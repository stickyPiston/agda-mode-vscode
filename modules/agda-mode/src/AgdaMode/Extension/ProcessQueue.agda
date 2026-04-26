module AgdaMode.Extension.ProcessQueue where

open import Data.Bool
import Data.List as List
open List using ([_] ; _∷_ ; [] ; filter)
open import Data.IO
open import Data.Maybe
open import Data.Maybe.Effectful
open import Data.Product
open import Function
open import Agda.Builtin.Unit
open import Data.JSON
open import Data.Nat
open import Data.Map
open import Data.JSON.Decode

open import AgdaMode.Extension.Model
open import AgdaMode.Extension.Display

open import Vscode.SemanticTokensProvider
open import Vscode.Common
open import Vscode.Window
open import Vscode.Logging
open import Vscode.Command

open import Node.Process

private
  postulate trace : {A : Set} → A → IO ⊤
  {-# COMPILE JS trace = A => a => async () => { console.log(a); return b => b["tt"]() } #-}

module Queue where
  open import Data.List hiding (null?)
  open import Data.List using (null?) public

  private variable
    A : Set

  t : Set → Set
  t = List

  empty : t A
  empty = []

  enqueue : A → t A → t A
  enqueue a q = q ++ [ a ]
  {-# COMPILE JS enqueue = A => a => q => [...q, a] #-}

  dequeue : t A → Maybe (A × t A)
  dequeue [] = nothing
  dequeue (x ∷ q) = just (x , q)
  {-# COMPILE JS dequeue = A => q => {
    if (q.length) return a => a["just"]({ "_,_": b => b["_,_"](q[0], q.slice(1)) });
    else return a => a["nothing"]();
  } #-}

open import Effect.Monad
open Monad {{ ... }}
open List.TraversableM {{ ... }}

-- The job queue makes sure that only one job is executed at the same time.
module JobQueue where
  postulate t : Set

  Job : Set
  Job = IO ⊤

  postulate new : IO t
  {-# COMPILE JS new = async () => { return new AgdaModeImports.PQueue({ concurrency: 1 }); } #-}

  postulate push : Job → t → IO ⊤
  {-# COMPILE JS push = job => q => async () => { q.add(job); return a => a["tt"]() } #-}

  postulate await-push : {A : Set} → IO A → t → IO A
  {-# COMPILE JS await-push = A => job => q => async () => { return await q.add(job) } #-}

module AgdaProcess where
  open import Data.String

  record t : Set where field
    process : Ref.t Process.t
    response-queue : JobQueue.t
  open t public

  -- We can send as many interactions to the Agda compiler without compromising the order of the responses or the
  -- the quality of the messages. This means we don't need to make and use a separate `JobQueue.t`.
  send-command : AgdaInteraction.t → t → IO ⊤
  send-command intr t = do
    trace (AgdaInteraction.show intr)
    Ref.get (t .process) >>= Process.write (AgdaInteraction.show intr) 

  private
    -- Some responses from the interactio mode might start with "JSON> ", so we strip
    -- those if they are present to make sure the JSON parser can parse the actual JSON.
    strip-prompt : String → String
    strip-prompt s = s |> _starts-with "JSON> " |> λ where
      true → slice 6 ∥ s ∥ s
      false → s

    -- Once we have a response string, we can parse it into JSON and run one of the many
    -- response decoders and handler on it. These will update the model mutable variable,
    -- which is safe since this handler will be executed as a job in the `JobQueue.t`.
    handle-response-string : Ref.t Model → String → Ref.t Process.t → JobQueue.Job
    handle-response-string model-ref response proc-ref = parse-json response |> λ where
        -- TODO: We should probably log that Agda sent something we don't understand
        nothing → pure tt
        (just parsed-response) → do
          model ← Ref.get model-ref
          new-model ← handle-agda-message (λ intr → Ref.get proc-ref >>= Process.write (AgdaInteraction.show intr)) model parsed-response or-else pure model
          Ref.set model-ref new-model

    spawn-proc : OutputChannel.t → IO Process.t
    spawn-proc output-chan = do
      -- We reload the config every time spawn is called, so that when the user issues a reload agda command,
      -- the possibly updated configuration from the vscode settings is also applied.
      config ← Config.load
      let args = "--interaction-json" ∷ filter (λ s → ∥ s ∥ > 0) (split (config .extra-args) " ")
      let cmd-name = config .agda-path or-else "agda"
      OutputChannel.trace ("Spawning process: " ++ cmd-name ++ " " ++ intercalate " " args) output-chan
      Process.spawn cmd-name args

    -- Whenever a buffer of data is received, we append to it to previously read and unparsed data.
    -- This complete buffer is split on newlines and the "JSON >" prompts are removed. All of the
    -- full lines should be JSON strings with responses, and jobs to parse and handle them will be pushed
    -- to the job queue.
    on-data-handler : Ref.t Process.t → JobQueue.t → Ref.t Model → OutputChannel.t → Ref.t String → Buffer.t → IO ⊤
    on-data-handler proc-ref res-queue model-ref output-chan stdout-buffer buffer =
      (List.unsnoc ∘ List.map strip-prompt ∘ lines ∘ (_++ Buffer.to-string buffer) <$> Ref.get stdout-buffer) >>= λ where
        nothing → OutputChannel.error "Could not unsnoc response string" output-chan
        (just (responses , new-buffer)) → do
          trace responses
          Ref.set stdout-buffer new-buffer
          tt <$ forM responses λ response → do
            trace response
            res-queue |> JobQueue.push (handle-response-string model-ref response proc-ref)

    {-# TERMINATING #-}
    mutual
      -- A child process calls this handler when:
      -- - The process failed to spawn or has unexpectedly exited
      -- - A message fails to send
      -- There is not much we do can other than allow the user to restart the process.
      on-error-handler : Ref.t Process.t → JobQueue.t → Ref.t Model → OutputChannel.t → String → Bool → IO ⊤
      on-error-handler proc-ref res-queue model-ref output-chan msg is-ENOENT = do
        OutputChannel.error ("Agda process error: " ++ msg) output-chan

        -- When the binary could not be spawed, give a specicialsed message and an additional button in the
        -- error to let the user know the path to the Agda binary might not be correct, or the PATH is not
        -- set up correctly.
        let message = if is-ENOENT
              then "Could not spawn Agda process. Are you sure Agda has correctly been installed on your path, or is the path to the binary in the settings correct?"
              else "Agda process error: " ++ msg
        let options = if is-ENOENT then "Open settings" ∷ "Restart Agda" ∷ [] else [ "Restart Agda" ]
        
        Window.show-error-message message options >>= λ where
          (just "Restart Agda") → do
            proc ← spawn-proc output-chan
            Ref.set proc-ref proc
            setup-handlers proc-ref res-queue model-ref output-chan
          (just "Open settings") → execute-command "workbench.action.openSettings" "agda-mode.agda-path"
          _ → pure tt

      setup-handlers : Ref.t Process.t → JobQueue.t → Ref.t Model → OutputChannel.t → IO ⊤
      setup-handlers proc-ref res-queue model-ref output-chan = do
        proc ← Ref.get proc-ref
        stdout-buffer ← Ref.new ""

        -- The handlers for these event listeners have been extracted because of a compilation bug in Agda.
        -- When pattern matching directly in this function, the code in the pattern match arms is not
        -- executed for some reason.
        Process.on-data proc (on-data-handler proc-ref res-queue model-ref output-chan stdout-buffer)
        Process.on-error proc (on-error-handler proc-ref res-queue model-ref output-chan)

  stop : OutputChannel.t → t → IO ⊤
  stop output-chan t = do
    OutputChannel.trace "Stopping currently running agda process" output-chan
    Ref.get (t .process) >>= Process.kill

  -- We first kill the currently running process, then spawn a new one and put it
  -- in the mutable variable. Then we set up all the handlers again.
  restart : OutputChannel.t → Ref.t Model → t → IO ⊤
  restart output-chan model-ref t = do
    stop output-chan t
    proc ← spawn-proc output-chan
    Ref.set (t .process) proc
    setup-handlers (t .process) (t .response-queue) model-ref output-chan

  -- Spawn a new `AgdaProcess.t` and immediately bind to the output on the stdout channel, which
  -- When messages arrive on the output channel, they will be handled via a `JobQueue.t`.
  spawn : OutputChannel.t → Ref.t Model → IO (t × Disposable.t)
  spawn output-chan model-ref = do
    proc ← spawn-proc output-chan
    proc-ref ← Ref.new proc
    res-queue ← JobQueue.new 
    setup-handlers proc-ref res-queue model-ref output-chan

    -- The disposable for an `AgdaProcess.t` calls `Process.kill`, which kills the child process gracefully.
    -- It will also cancel the on-data event listener created earlier.
    pure $ record { process = proc-ref ; response-queue = res-queue } , Disposable.new (Process.kill proc)
open AgdaProcess using (response-queue ; process) public
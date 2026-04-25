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

open import AgdaMode.Extension.Model
open import AgdaMode.Extension.Display

open import Vscode.SemanticTokensProvider
open import Vscode.Common
open import Vscode.Logging

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

private
  postulate timeout : Nat → IO ⊤
  {-# COMPILE JS timeout = n => () => new Promise(resolve => setTimeout(() => resolve(), Number(n))) #-}

-- The job queue makes sure that only one job is executed at the same time.
module JobQueue where
  Job : Set
  Job = IO ⊤

  -- private record r : Set where field
  --   busy? : Bool -- Are we currently processing a job? Invariant: is true when job-queue isn't empty
  --   job-queue : Queue.t Job
  --   event : EventEmitter.t ⊤
  -- open r public

  -- t : Set
  -- t = Ref.t r

  -- private
  --   -- Once a job has finished processing, we need to check whether the queue can provide
  --   -- a new job to run immediately after. Otherwise we stop executing and wait for `push`
  --   -- to call this function when a new job arrives.
  --   handle-completed-job : t → IO ⊤
  --   handle-completed-job q = Ref.get q >>= λ r → r .job-queue |> Queue.dequeue |> λ where
  --     (just (job , new-queue)) → do
  --       Ref.set q record r { job-queue = new-queue }
  --       trace "Started job"
  --       job
  --       trace "Finished job"
  --       -- Firing the event emitter triggers a run of this function again,
  --       -- but it allows to unroll the stack this execution is in, and reset the stack depth.
  --       EventEmitter.fire (r .event) tt
  --     nothing → Ref.set q record r { busy? = false }

  -- -- When pushing a new job, we check if the queue is already processing a job. If it is,
  -- -- then we enqueue the job, otherwise we immediately set this job as the currently running job.
  -- push : Job → t → IO ⊤
  -- push job q = Ref.get q >>= λ r → do
  --   Ref.set q record r { job-queue = Queue.enqueue job (r .job-queue) }
  --   unless r .busy? then handle-completed-job q

  -- -- Create a new `JobQueue.t`, it also provides a disposable that will clean up the event handler.
  -- new : IO (t × Disposable.t)
  -- new = do
  --   emitter ← EventEmitter.new
  --   job-queue ← Ref.new record { busy? = false ; job-queue = Queue.empty ; event = emitter }
  --   disposable ← Event.listen (EventEmitter.event emitter) λ _ → handle-completed-job job-queue
  --   pure (job-queue , disposable)

  postulate t : Set

  postulate new : IO t
  {-# COMPILE JS new = async () => { return new AgdaModeImports.PQueue({ concurrency: 1 }); } #-}

  postulate push : Job → t → IO ⊤
  {-# COMPILE JS push = job => q => async () => { q.add(job); return a => a["tt"]() } #-}

  postulate await-push : {A : Set} → IO A → t → IO A
  {-# COMPILE JS await-push = A => job => q => async () => { return await q.add(job) } #-}

module AgdaProcess where
  open import Data.String

  record t : Set where field
    process : Process.t
    response-queue : JobQueue.t
  open t public

  -- We can send as many interactions to the Agda compiler without compromising the order of the responses or the
  -- the quality of the messages. This means we don't need to make and use a separate `JobQueue.t`.
  send-command : AgdaInteraction.t → t → IO ⊤
  send-command intr t = do
    trace (AgdaInteraction.show intr)
    Process.write (t .process) (AgdaInteraction.show intr) 

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
    handle-response-string : Ref.t Model → String → Process.t → JobQueue.Job
    handle-response-string model-ref response proc = parse-json response |> λ where
      -- TODO: We should probably log that Agda sent something we don't understand
      nothing → pure tt
      (just parsed-response) → do
        model ← Ref.get model-ref
        trace parsed-response
        new-model ← handle-agda-message (λ intr → Process.write proc (AgdaInteraction.show intr)) model parsed-response or-else pure model
        Ref.set model-ref new-model

  -- Spawn a new `AgdaProcess.t` and immediately bind to the output on the stdout channel, which
  -- When messages arrive on the output channel, they will be handled via a `JobQueue.t`.
  spawn : OutputChannel.t → Ref.t Model → IO (t × Disposable.t)
  spawn output-chan model-ref = do
    -- We reload the config every time spawn is called, so that when the user issues a reload agda command,
    -- the possibly updated configuration from the vscode settings is also applied.
    config ← Config.load
    let args = "--interaction-json" ∷ filter (λ s → ∥ s ∥ > 0) (split (config .extra-args) " ")
    let cmd-name = config .agda-path or-else "agda"
    OutputChannel.trace ("Spawning process: " ++ cmd-name ++ " " ++ intercalate " " args) output-chan

    proc ← Process.spawn cmd-name args
    res-queue ← JobQueue.new 

    -- Whenever a buffer of data is received, we append to it to previously read and unparsed data.
    -- This complete buffer is split on newlines and the "JSON >" prompts are removed. All of the
    -- full lines should be JSON strings with responses, and jobs to parse and handle them will be pushed
    -- to the job queue.
    stdout-buffer ← Ref.new ""
    Process.on-data proc λ buffer → do
      just (responses , new-buffer) ← List.unsnoc ∘ List.map strip-prompt ∘ lines ∘ (_++ Buffer.to-string buffer)
        <$> Ref.get stdout-buffer where _ → pure tt
      Ref.set stdout-buffer new-buffer
      tt <$ forM responses λ response → res-queue |> JobQueue.push (handle-response-string model-ref response proc)

    -- The disposable for an `AgdaProcess.t` calls `Process.disconnect`, which allows the child process to
    -- exit once there are no more references to it. It will also cancel the on-data event listener created
    -- earlier.
    pure $ record { process = proc ; response-queue = res-queue } , Disposable.new (Process.disconnect proc)
open AgdaProcess using (response-queue ; process) public
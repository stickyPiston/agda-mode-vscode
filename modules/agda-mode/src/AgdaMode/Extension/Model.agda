module AgdaMode.Extension.Model where

open import Agda.Builtin.Unit

open import Data.Maybe
open import Data.String
open import Data.Map
open import Data.List

open import AgdaMode.Extension.Highlighting
open import AgdaMode.Extension.Keymap

open import Vscode.Panel
open import Vscode.Window
open import Vscode.Common
open import Vscode.SemanticTokensProvider
open import Vscode.TextEditor

open import Node.Process

record Model : Set where field
  panel : Maybe (Panel.t ⊤)
  status-bar-item : StatusBarItem.t
  input-mode-status-item : StatusBarItem.t
  agda : Maybe Process.t
  stdout-buffer : String
  current-doc : Maybe TextDocument.t
  loaded-files : StringMap.t (List Token)
  tokens-request-emitter : EventEmitter.t ⊤
  running-info : String
  underline-decoration : DecorationType.t
  keymap : Trie.t

open Model public

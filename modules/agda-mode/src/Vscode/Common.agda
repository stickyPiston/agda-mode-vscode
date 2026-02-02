module Vscode.Common where

open import Data.List
open import Data.String
open import Agda.Builtin.Nat

open import Data.IO

postulate Disposable : Set

module Uri where
  postulate t : Set

  postulate join-path : t → List String → IO t

  postulate path scheme : t → String
  {-# COMPILE JS path = uri => uri.path #-}
  {-# COMPILE JS scheme = uri => uri.scheme #-}

module Position where
    postulate t : Set
    postulate new : Nat → Nat → IO t
    postulate line char : t → Nat

    {-# COMPILE JS new = line => char => ({ vscode }, cont) => cont(new vscode.Position(Number(line), Number(char))) #-}
    {-# COMPILE JS line = pos => BigInt(pos.line) #-}
    {-# COMPILE JS char = pos => BigInt(pos.character) #-}

module Range where
    postulate t : Set
    postulate new : Position.t → Position.t → IO t
    postulate start end : t → Position.t

    {-# COMPILE JS new = start => end => ({ vscode }, cont) => cont(new vscode.Range(start, end)) #-}
    {-# COMPILE JS start = range => range.start #-}
    {-# COMPILE JS end = range => range.end #-}

module TextLine where
    postulate t : Set

    postulate range : t → Range.t
    {-# COMPILE JS range = line => line.range #-}

module TextDocument where
    postulate t : Set

    postulate uri : t → Uri.t
    {-# COMPILE JS uri = doc => doc.uri #-}

    postulate get-text : t → String
    {-# COMPILE JS get-text = doc => doc.getText() #-}

    postulate position-at : t → Nat → Position.t
    {-# COMPILE JS position-at = doc => n => doc.positionAt(Number(n)) #-}

    postulate line-at : t → Nat → TextLine.t
    {-# COMPILE JS line-at = doc => n => doc.lineAt(Number(n)) #-}

    postulate file-name : t → String
    {-# COMPILE JS file-name = doc => doc.fileName #-}

module ExtensionContext where
  postulate t : Set

  postulate get : IO t
  {-# COMPILE JS get = ({context}, cont) => cont(context) #-}

  postulate extension-uri : t → Uri.t
  {-# COMPILE JS extension-uri = ctx => ctx.extensionUri #-}

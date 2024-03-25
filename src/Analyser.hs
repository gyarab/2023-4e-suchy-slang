{-# LANGUAGE NamedFieldPuns #-}

module Analyser (TypedObject(..), extractType) where

import Parser as P
import qualified Data.Map as Map
import Data.Map (Map)
import Types (Type, TypedObject(Function))
import qualified Types as T
import Data.Maybe (isNothing)

type TypingResult = Either String TypedAST -- error or ok

type TypedAST = (T.Type, ASTNode)

builtInTypes :: Map String TypedObject
builtInTypes = Map.fromList [
    ("printf", Function [T.ref T.Char, T.Ellipsis] T.I32)
  ]

ntimes 1 f a = f a
ntimes n f a = f (ntimes (n-1) f a)

extractType P.ExternFunction { name, arugments, retVal } = (name, T.Function arugments retVal)
extractType P.Stream { name, arugments, retVal } = (name, T.Function arugments retVal)

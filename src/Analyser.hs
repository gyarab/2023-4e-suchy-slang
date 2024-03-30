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

extractType P.ExternFunction { name, arguments, retVal } = (name, T.Function arguments retVal)
extractType P.Stream { name, inType, outType } = (name, T.Stream inType outType)
extractType P.Struct { name, attributes } = (name, T.Structure attributes)

{-# LANGUAGE NamedFieldPuns #-}

module Analyser (addType, addType', getTypeError, getTypeError', TypedObject(..), extractType, forceSpecificType) where

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


checkArguments :: Map String TypedObject -> [T.Type] -> [P.ASTNode] -> Either String ()

checkArguments types [] [] = Right ()
checkArguments types [] a = Left ("got " ++ show (length a) ++ " more arguments than expected")
checkArguments types [T.Ellipsis] a = Right ()
checkArguments types ft [] = Left ("got " ++ show (length ft) ++ " less arguments than expected")

checkArguments types (ft:funcTypes) (a:args) = do
  (ta, _) <- addType types a
  if ft == T.Ellipsis then
    Right ()
  else
    if ft == ta then checkArguments types funcTypes args
    else Left $ "expecting type " ++ show ft ++ ", but got " ++ show ta

derefMaybe (Just t) = T.deref t
derefMaybe Nothing = Nothing

addType :: Map String TypedObject -> P.ASTNode -> TypingResult
addType types node =
  case node of
    P.ConstInt {} -> Right (T.I64, node)
    P.ConstString {} -> Right (T.ref T.Char, node)
    P.ConstChar {} -> Right (T.Char, node)
    P.ConstFloat {} -> Right (T.Float, node)
    -- P.Add l r -> forceSameType types node l r
    -- P.Subtract l r -> forceSameType types node l r
    -- P.Multiply l r -> forceSameType types node l r
    -- P.Divide l r -> forceSameType types node l r

    P.Negate n -> do
      t <- fst <$> addType types n
      if t `elem` [T.I64, T.I32] then Right (t, node) else Left $ "cannot negate" ++ show t

    P.Reference i n -> do
      t <- fst <$> addType types n
      Right (ntimes i T.ref t, n)

    P.Dereference i n -> do
      t <- fst <$> addType types n
      case ntimes i derefMaybe (Just t) of
        Just t -> Right (t, n)
        Nothing -> Left "cannot dereference, type is not a pointer" -- we should probably tell how much we are dereferencing and shit idk

    P.Call args callable -> do
      case callable of
        P.Identifier i -> do
          if length i > 1 then
            Left (show i ++ " is not callable")
          else
            case Map.lookup (head i) types of
              Nothing -> Left ("unknown callable: " ++ show i)
              Just (Function argsT retT) ->
                checkArguments types argsT args *> Right (retT, node)
        t ->
          Left (show callable ++ " is not callable")

    P.Identifier (v:xs) ->
      case Map.lookup v types of
        Just (T.Variable t) -> Right (t, node)
        Nothing -> Left ("invalid reference: " ++ v)

    n -> Left $ show n ++ " has no type"

forceSpecificType types node t = do
  (tn, _) <- addType types node
  if tn == t then Right (t, node) else Left $ "incompatible type " ++ show tn ++ "(must be " ++ show t ++ ")"

getTypeError builtInTypes (P.Declare identifier iType Nothing) = Nothing
getTypeError builtInTypes (P.Declare identifier iType (Just value)) =
  case addType builtInTypes value of
    Right (t, n) ->
      if iType == t then Nothing else Just $ "cannot assign " ++ show t ++ " to " ++ show iType
    Left s -> Just s

getTypeError builtInTypes (P.IfElse cond _ _) =
  case addType builtInTypes cond of
    Right {} -> Nothing
    Left s -> Just s

getTypeError builtInTypes P.ExternFunction {} = Nothing
getTypeError builtInTypes P.Struct {} = Nothing


getTypeError builtInTypes (P.Stream { body = Nothing }) = Nothing
getTypeError builtInTypes (P.Stream { body = Just body }) = getFirstTE body
  where
    getFirstTE [] = Nothing
    getFirstTE (stmt:body) =
      let err = getTypeError builtInTypes stmt in if isNothing err then getFirstTE body else err

-- must be an expression
getTypeError builtInTypes node =
  case addType builtInTypes node of
    Right {} -> Nothing
    Left err -> Just err

addType' = addType builtInTypes
getTypeError' = getTypeError builtInTypes

extractType P.ExternFunction { name, arugments, retVal } = (name, T.Function arugments retVal)
extractType P.Stream { name, arugments, retVal } = (name, T.Function arugments retVal)

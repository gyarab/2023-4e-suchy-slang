{-# LANGUAGE NamedFieldPuns #-}

module Codegen (testAssembleModule) where

import qualified Parser as P
import qualified Lexer as L
import qualified Data.Text as T
import Types (llvmType)
import Control.Monad.State
import qualified Analyser as A
import qualified Types

import qualified Data.Map as Map
import Data.Map(Map)
import Data.Maybe (isNothing)

data Module = Module {
   constants :: ![(String, String)],
   definitions :: ![String]
 }


type TypeMap = Map String Types.TypedObject

data Assembled = Inline !Int | Referenced !Int | Global !String
  deriving (Eq)

newtype ExceptT e m a = ExceptT (m (Either e a))

type AsState = StateT AssemblerState (Either String)

instance Show Assembled where
  show (Inline i) = show i
  show (Referenced i) = '%':show i
  show (Global s) = '@':s

data AssemblerState = AssemblerState {
    num :: !Int,
    global :: !Int,
    assembly :: ![T.Text],
    declarations :: ![T.Text], -- strings and other declarations that are needed before declaring the stream
    types :: !TypeMap,
    locals :: !(Map String Assembled)
  }
  deriving (Show, Eq)

failOnFirst :: [Either a b] -> Either a [b]
failOnFirst [] = Right []
failOnFirst (x:xs) =
  case x of
    Left err -> Left err
    Right ix ->
      case failOnFirst xs of
        Left err -> Left err
        Right rest -> Right (ix:rest)

fArgs args = if not (null args) then (llvmType . head) args ++ concatMap ((", "++) . llvmType) (tail args) else ""
concatWith delim args = if not (null args) then head args ++ concatMap (delim++) (tail args) else ""

assemble :: P.ASTNode -> AsState Assembled
assemble (P.ConstInt i) = pure (Inline i)

-- XXX: nezvládá unicode :(
assemble (P.ConstString s) = do
  modify incrGlobal
  n <- gets (Global . ("str" ++) . show . global)
  modify $ addDeclaration (T.pack (show n ++ " = private constant [" ++ show (length s + 1) ++ " x i8] c\"" ++ concatMap escapeChar s ++ "\\00\", align 1"))
  return n
  where
    escapeChar c
      | c == '\n' = "\\0A"
      | c == '\r' = "\\0D"
      | otherwise = [c]


assemble (P.Negate t) = assembleBinaryOp "sub" (P.ConstInt 0) t -- 0-t = -t
assemble (P.Add l r) = assembleBinaryOp "add" l r
assemble (P.Subtract l r) = assembleBinaryOp "sub" l r
assemble (P.Multiply l r) = assembleBinaryOp "mul" l r
assemble (P.Divide l r) = assembleBinaryOp "sdiv" l r

assemble (P.Call args func) = do
  types <- gets types

  case mapM (A.addType types) args of
    Left err -> lift $ Left err
    Right targs -> do
      aargs <- mapM (assemble . snd) targs

      modify incrNum
      n <- gets (Referenced . num)

      let P.Identifier (funcName:_) = func

      typ <- case Map.lookup funcName types of
        Nothing -> lift $ Left ("invalid reference: " ++ funcName)
        Just x -> lift $ Right x

      case typ of
        Types.Function funcTypes retType -> do

          let argTypes = map (llvmType . fst) targs
          let argsWithTypes = zipWith (\ t r -> t ++ " " ++ show r) argTypes aargs
          modify $ addAssembly (mk n funcName retType (map llvmType funcTypes) argsWithTypes)
          return n
        f -> lift $ Left (funcName ++ " is not callable")

    where
      mk n funcName retType targs aargs =
        T.pack (show n ++ " = " ++ "call " ++ llvmType retType ++ " (" ++ concatWith ", " targs ++ ") @" ++ funcName ++ "(" ++ concatWith ", " aargs ++ ")")

assemble (P.Declare name typ val) = do
  modify incrNum
  n <- gets (Referenced . num)

  modify $ addAssembly (allocation n)

  modify $ setVariable name typ n

  case val of
    Nothing -> return n
    Just val -> do
      ass <- assemble val
      modify $ addAssembly (store n ass)
      return n

  where
    allocation ref = T.pack (show ref ++ " = alloca " ++ llvmType typ)
    store ptr ass = T.pack ("store " ++ llvmType typ ++ " " ++ show ass ++ ", ptr " ++ show ptr)

assemble (P.Identifier (v:xs)) = do
  modify incrNum

  n <- gets (Referenced . num)
  mVal <- gets (Map.lookup v . locals)
  mTyp <- gets (Map.lookup v . types)
  if isNothing mVal then
    lift $ Left ("invalid reference: " ++ v)
  else do
    let (Just val) = mVal
    let (Just typ) = mTyp

    typ <- case typ of
      Types.Variable v -> lift $ Right v
      f -> lift $ Left (v ++ " is not a variable") 

    modify $ addAssembly (load n typ val)
    return n

  where
    load n t v = T.pack (show n ++ " = load " ++ llvmType t ++ ", ptr " ++ show v)


assembleBody :: [P.ASTNode] -> AsState Assembled
assembleBody stmts = do
  assembled <- mapM assemble stmts
  return $ last assembled


assembleBinaryOp :: String -> P.ASTNode -> P.ASTNode -> AsState Assembled
assembleBinaryOp instr left right = do
  l <- assemble left
  r <- assemble right

  types <- gets types

  (t, _) <- case A.addType types left of -- we dont have to check for equality, since we did it earlier
    Right t -> lift $ Right t
    err -> lift err

  modify incrNum

  n <- gets (Referenced . num)
  modify $ addAssembly (mk n l r t)

  return n

  where
    mk n l r t = T.pack (show n ++ " = " ++ instr ++ " " ++ llvmType t ++ " " ++ show l ++ ", " ++ show r)

addDeclaration dec (AssemblerState n g a d t l) = AssemblerState n g a (d++[dec]) t l
incrNum (AssemblerState n g a d t l) = AssemblerState (n+1) g a d t l
incrGlobal (AssemblerState n g a d t l) = AssemblerState n (g+1) a d t l
addAssembly ass (AssemblerState n g a d t l) = AssemblerState n g (a++[ass]) d t l
setVariable name typ ref (AssemblerState n g a d t l) = AssemblerState n g a d (Map.insert name (Types.Variable typ) t) (Map.insert name ref l)

assemblerStateEmpty t = AssemblerState 0 0 [] [] t Map.empty

testAssembleModule :: TypeMap -> [P.ASTNode] -> Either String T.Text
testAssembleModule types streams = do
  streams <- mapM (assembleStream types) streams
  return $ T.unlines streams

assembleStream :: TypeMap -> P.ASTNode -> Either String T.Text
assembleStream types (P.Stream name args rv (Just body)) = do
  (_, as) <- runStateT (assembleBody body) (assemblerStateEmpty types)
  return $ T.unlines [
      T.unlines (declarations as),
      T.pack ("define " ++ llvmType rv ++ " @" ++ name ++ "(" ++ fArgs args ++ ") noinline optnone {"),
      T.unlines (map (T.append (T.pack "  ")) (assembly as)),
      T.pack ("  ret " ++ llvmType rv ++ " 0\n}")
    ]

assembleStream _ (P.ExternFunction name args retVal) = pure $ T.pack ("declare " ++ llvmType retVal ++ " @" ++ name ++ "(" ++ fArgs args ++ ")")

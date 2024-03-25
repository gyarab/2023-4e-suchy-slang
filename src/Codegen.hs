{-# LANGUAGE NamedFieldPuns #-}

module Codegen (testAssembleModule) where

import qualified Parser as P
import qualified Lexer as L
import qualified Data.Text as T
import Types (llvmType)
import Control.Monad.State
import qualified Types

import qualified Data.Map as Map
import Data.Map(Map)
import Data.Maybe (isNothing, isJust)

data Module = Module {
   constants :: ![(String, String)],
   definitions :: ![String]
 }


type TypeMap = Map String Types.TypedObject

data Assembled = Inline !Int | Referenced !Int | Global !String | Dummy
  deriving (Eq)

type TAssembled = (Types.Type, Assembled)

newtype ExceptT e m a = ExceptT (m (Either e a))

type AsState = StateT AssemblerState (Either String)

instance Show Assembled where
  show (Inline i) = show i
  show (Referenced i) = '%':show i
  show (Global s) = '@':s
  show Dummy = "USED DUMMY SOMEWHERE. NICHT GOOD"

data AssemblerState = AssemblerState {
    labelN :: !Int,
    num :: !Int,
    global :: !Int,
    assembly :: ![T.Text],
    declarations :: ![T.Text], -- strings and other declarations that are needed before declaring the stream
    types :: !TypeMap,
    locals :: !(Map String Assembled)
  }
  deriving (Show, Eq)

--- Fail on first error
failOnFirst :: [Either a b] -> Either a [b]
failOnFirst [] = Right []
failOnFirst (x:xs) =
  case x of
    Left err -> Left err
    Right ix ->
      case failOnFirst xs of
        Left err -> Left err
        Right rest -> Right (ix:rest)

lookupVar :: String -> AsState (Types.TypedObject, Assembled)
lookupVar v = do
  mVal <- gets (Map.lookup v . locals)
  mTyp <- gets (Map.lookup v . types)

  if isNothing mVal then
    lift $ Left ("invalid reference: " ++ v)
  else do
    let (Just val) = mVal
    let (Just typ) = mTyp

    return (typ, val)

--- Replace an error message with another
infix 6 <?>
(<?>) :: AsState a -> String -> AsState a
(<?>) m newErr = do
  s <- get
  case runStateT m s of
    Left err -> lift $ Left newErr
    Right (x, s1) -> do
      put s1
      return x

forceSameType :: Types.Type -> Types.Type -> AsState ()
forceSameType tl tr =
  lift $ if tl == tr then Right () else Left $ show tl ++ " and " ++ show tr ++ " must be of the same type"

replaceType :: Types.Type -> TAssembled -> TAssembled
replaceType t (_, a) = (t, a)

fArgs args = if not (null args) then (llvmType . head) args ++ concatMap ((", "++) . llvmType) (tail args) else ""
concatWith delim args = if not (null args) then head args ++ concatMap (delim++) (tail args) else ""

assemble :: P.ASTNode -> AsState TAssembled
assemble (P.ConstInt i) = pure (Types.I64, Inline i)

-- XXX: nezvládá unicode :(
assemble (P.ConstString s) = do
  modify incrGlobal
  n <- gets (Global . ("str" ++) . show . global)
  modify $ addDeclaration (T.pack (show n ++ " = private constant [" ++ show (length s + 1) ++ " x i8] c\"" ++ concatMap escapeChar s ++ "\\00\", align 1"))
  return (Types.ref Types.Char, n)
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

assemble (P.Eq l r) = replaceType Types.Boolean <$> assembleBinaryOp "icmp eq" l r
assemble (P.Neq l r) = replaceType Types.Boolean <$> assembleBinaryOp "icmp neq" l r
assemble (P.Geq l r) = replaceType Types.Boolean <$> assembleBinaryOp "icmp sge" l r
assemble (P.Leq l r) = replaceType Types.Boolean <$> assembleBinaryOp "icmp sle" l r
assemble (P.Gtr l r) = replaceType Types.Boolean <$> assembleBinaryOp "icmp sgt" l r
assemble (P.Lsr l r) = replaceType Types.Boolean <$> assembleBinaryOp "icmp slt" l r

assemble (P.Call args func) = do
  aargs <- mapM assemble args

  modify incrNum
  n <- gets (Referenced . num)

  let P.Identifier (funcName:_) = func

  types <- gets types
  typ <- case Map.lookup funcName types of
    Nothing -> lift $ Left ("invalid reference: " ++ funcName)
    Just x -> lift $ Right x

  case typ of
    Types.Function funcTypes retType -> do

      let argTypes = map (llvmType . fst) aargs
      let argsWithTypes = zipWith (\ t r -> t ++ " " ++ show (snd r)) argTypes aargs

      modify $ addAssembly (mk n funcName retType (map llvmType funcTypes) argsWithTypes)

      return (retType, n)
    f -> lift $ Left (funcName ++ " is not callable")

    where
      mk n funcName retType targs aargs =
        T.pack (show n ++ " = " ++ "call " ++ llvmType retType ++ " (" ++ concatWith ", " targs ++ ") @" ++ funcName ++ "(" ++ concatWith ", " aargs ++ ")")

assemble (P.Declare name typ val) = do
  modify incrNum
  n <- gets (Referenced . num)
  types <- gets types

  modify $ addAssembly (allocation n)

  modify $ setVariable name typ n

  case val of
    Nothing -> return (typ, n)
    Just val -> (typ, n) <$ assemble (P.Assign (P.Identifier [name]) val) -- throw away reference to value, return reference to var instead

  where
    allocation ref = T.pack (show ref ++ " = alloca " ++ llvmType typ)

assemble (P.Identifier (v:xs)) = do
  modify incrNum
  n <- gets (Referenced . num)

  (typ, val) <- lookupVar v

  typ <- case typ of
    Types.Variable v -> lift $ Right v
    f -> lift $ Left (v ++ " is not a variable")

  modify $ addAssembly (load n typ val)
  return (typ, n)

  where
    load n t v = T.pack (show n ++ " = load " ++ llvmType t ++ ", ptr " ++ show v)

assemble (P.Assign (P.Identifier (var:_)) val) = do
  (typ, ref) <- lookupVar var

  typ <- case typ of
    Types.Variable v -> lift $ Right v
    f -> lift $ Left (var ++ " is not a variable")

  (valTyp, valAss) <- assemble val

  forceSameType typ valTyp <?> ("cannot assign " ++ show valTyp ++ " to " ++ show typ)

  modify $ addAssembly (store typ valAss ref)

  return (valTyp, valAss)

  where
    store typ ass ptr = T.pack ("store " ++ llvmType typ ++ " " ++ show ass ++ ", ptr " ++ show ptr)

assemble (P.IfElse cond a b) = do
  (tcond, ascond) <- assemble cond

  forceSameType Types.Boolean tcond <?> "condition of if statement must be of type Boolean"

  modify (incrLabel . incrLabel . incrLabel)

  label <- gets labelN

  modify $ addAssembly (T.pack ("br i1 " ++ show ascond ++ ", label %lbl" ++ show (label - 2) ++ ", label %lbl" ++ if isJust b then show (label - 1) else show label))
  modify $ addAssembly (T.pack ("lbl" ++ show (label - 2) ++ ":"))

  assembleBlock a

  modify $ addAssembly (T.pack ("br label %lbl" ++ show label))

  when (isJust b) $ do
    let (Just c) = b
    modify $ addAssembly (T.pack ("lbl" ++ show (label - 1) ++ ":"))
    assembleBlock c
    modify $ addAssembly (T.pack ("br label %lbl" ++ show label))

  modify $ addAssembly (T.pack ("lbl" ++ show label ++ ":"))

  return (Types.Void, Dummy)

assemble (P.While cond loop) = do
  modify (incrLabel . incrLabel . incrLabel)

  label <- gets labelN

  modify $ addAssembly (T.pack ("br label %lbl" ++ show (label - 2)))
  modify $ addAssembly (T.pack ("lbl" ++ show (label - 2) ++ ":"))

  (tcond, ascond) <- assemble cond
  forceSameType Types.Boolean tcond <?> "condition of while statement must be of type Boolean"

  modify $ addAssembly (T.pack ("br i1 " ++ show ascond ++ ", label %lbl" ++ show (label - 1) ++ ", label %lbl" ++ show label))
  modify $ addAssembly (T.pack ("lbl" ++ show (label - 1) ++ ":"))
  assembleBlock loop
  modify $ addAssembly (T.pack ("br label %lbl" ++ show (label - 2)))
  modify $ addAssembly (T.pack ("lbl" ++ show label ++ ":"))

  return (Types.Void, Dummy)
  

assembleBlock :: [P.ASTNode] -> AsState TAssembled
assembleBlock stmts = do
  locals <- gets locals
  assembled <- mapM assemble stmts
  modify $ setLocals locals -- exit block (throw away local vars)
  return $ last assembled

assembleBinaryOp :: String -> P.ASTNode -> P.ASTNode -> AsState TAssembled
assembleBinaryOp instr left right = do
  (tl, al) <- assemble left
  (tr, ar) <- assemble right

  forceSameType tl tr <?> ("cannot operate " ++ show tl ++ " with " ++ show tr)

  modify incrNum

  n <- gets (Referenced . num)
  modify $ addAssembly (mk n al ar tl)

  return (tl, n)

  where
    mk n l r t = T.pack (show n ++ " = " ++ instr ++ " " ++ llvmType t ++ " " ++ show l ++ ", " ++ show r)

addDeclaration dec (AssemblerState ln n g a d t l) = AssemblerState ln n g a (d++[dec]) t l
incrNum (AssemblerState ln n g a d t l) = AssemblerState ln (n+1) g a d t l
incrLabel (AssemblerState ln n g a d t l) = AssemblerState (ln+1) n g a d t l
incrGlobal (AssemblerState ln n g a d t l) = AssemblerState ln n (g+1) a d t l
addAssembly ass (AssemblerState ln n g a d t l) = AssemblerState ln n g (a++[ass]) d t l
setVariable name typ ref (AssemblerState ln n g a d t l) = AssemblerState ln n g a d (Map.insert name (Types.Variable typ) t) (Map.insert name ref l)
setLocals locals (AssemblerState ln n g a d t l) = AssemblerState ln n g a d t locals

assemblerStateEmpty t = AssemblerState 0 0 0 [] [] t Map.empty

testAssembleModule :: TypeMap -> [P.ASTNode] -> Either String T.Text
testAssembleModule types streams = do
  streams <- mapM (assembleStream types) streams
  return $ T.unlines streams

assembleStream :: TypeMap -> P.ASTNode -> Either String T.Text
assembleStream types (P.Stream name args rv (Just body)) = do
  (_, as) <- runStateT (assembleBlock body) (assemblerStateEmpty types)
  return $ T.unlines [
      T.unlines (declarations as),
      T.pack ("define " ++ llvmType rv ++ " @" ++ name ++ "(" ++ fArgs args ++ ") noinline optnone {"),
      T.unlines (map (T.append (T.pack "  ")) (assembly as)),
      T.pack ("  ret " ++ llvmType rv ++ " 0\n}")
    ]

assembleStream _ (P.ExternFunction name args retVal) = pure $ T.pack ("declare " ++ llvmType retVal ++ " @" ++ name ++ "(" ++ fArgs args ++ ")")

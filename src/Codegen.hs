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
    locals :: !(Map String Int),
    localNumber :: ![Types.Type],
    stream :: !P.ASTNode,
    blockN :: !Int
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

lookupVar :: String -> AsState (Types.TypedObject, Int)
lookupVar v = do
  mVal <- gets (Map.lookup v . locals)
  mTyp <- gets (Map.lookup v . types)

  if isNothing mVal then
    lift $ Left ("invalid reference: " ++ v)
  else do
    let (Just val) = mVal
    let (Just typ) = mTyp

    return (typ, val)

nextNum :: AsState Int
nextNum = modify incrNum *> gets num

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

memCpy :: Types.Type -> Assembled -> Assembled -> AsState ()
memCpy typ from to = do
  n <- Referenced <$> nextNum
  n2 <- Referenced <$> nextNum
  -- get size of element into n2
  modify $ addAssembly (T.pack (show n ++ " = getelementptr " ++ llvmType typ ++ ", ptr null, i32 1"))
  modify $ addAssembly (T.pack (show n2 ++ " = ptrtoint ptr " ++ show n ++ " to i32"))
  -- call memcpy intrinsic
  modify $ addAssembly (T.pack ("call void @llvm.memcpy.p0.p0.i32(ptr " ++ show to ++ ", ptr " ++ show from ++ ", i32 " ++ show n2 ++ ", i1 0)"))
  lift $ Right ()

store :: Types.Type -> Assembled -> Assembled -> AsState ()
store typ what whereTo =
  if Types.isFirstClass typ then do
    modify $ addAssembly (T.pack ("store " ++ llvmType typ ++ " " ++ show what ++ ", ptr " ++ show whereTo))
  else
    memCpy typ what whereTo

    

assemble :: P.ASTNode -> AsState TAssembled
assemble (P.ConstInt i) = pure (Types.I64, Inline i)

assemble (P.ConstBoolean b) = pure (Types.Boolean, Inline (if b then 1 else 0))

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

  n <- Referenced <$> nextNum

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
  types <- gets types

  case (typ, val) of
    (Just t, Just v) -> do
      (vtyp, vass) <- assemble v
      if t == vtyp then
        declareAndStore t vass
      else
        lift $ Left ("declaration of " ++ show name ++ ": cannot assign " ++ show v ++ " to declared type " ++ show t)
    (Nothing, Just v) -> do
      (vtyp, vass) <- assemble v
      declareAndStore vtyp vass
    (Just t, Nothing) -> do
      modify $ declareVar name t
      return (t, Dummy)
    (Nothing, Nothing) -> lift $ Left ("declaration of " ++ show name ++ ": must be declared with either a type or a value")
      

  where
    getPtr name n v = T.pack (show n ++ " = getelementptr %stream_"++name++"_locals, ptr %locals, i32 0, i32 " ++ show v)

    declareAndStore typ ass = do
      modify $ declareVar name typ
      n <- Referenced <$> nextNum
      ref <- gets (length . localNumber)
      sname <- gets (P.name . stream)
      modify $ addAssembly (getPtr sname n (ref-1))
      store typ ass n
      return (typ, Dummy)

assemble (P.Identifier (v:xs)) = do
  _ <- nextNum
  n <- nextNum
  name <- gets (P.name . stream)

  (typ, val) <- lookupVar v

  typ <- case typ of
    Types.Variable v -> lift $ Right v
    f -> lift $ Left (v ++ " is not a variable")

  modify $ addAssembly (getPtr name n val)
  modify $ addAssembly (load n typ val)
  return (typ, Referenced n)

  where
    getPtr name n v = T.pack (show (Referenced (n-1)) ++ " = getelementptr %stream_"++name++"_locals, ptr %locals, i32 0, i32 " ++ show v)
    load n t v = T.pack (show (Referenced n) ++ " = load " ++ llvmType t ++ ", ptr " ++ show (Referenced (n-1)))

assemble (P.Assign (P.Identifier (var:_)) val) = do
  name <- gets (P.name . stream)
  (typ, ref) <- lookupVar var

  typ <- case typ of
    Types.Variable v -> lift $ Right v
    f -> lift $ Left (var ++ " is not a variable")

  (valTyp, valAss) <- assemble val

  forceSameType typ valTyp <?> ("cannot assign " ++ show valTyp ++ " to " ++ show typ)

  modify incrNum

  n <- gets num

  modify $ addAssembly (getPtr name n ref)
  store typ valAss (Referenced n)

  return (valTyp, valAss)

  where
    getPtr name n v = T.pack (show (Referenced n) ++ " = getelementptr %stream_"++name++"_locals, ptr %locals, i32 0, i32 " ++ show v)

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

assemble (P.Catch what) = do
  case what of
    (P.Identifier ["in"]) -> do
      n <- Referenced <$> nextNum
      n2 <- Referenced <$> nextNum
      n3 <- Referenced <$> nextNum
      n4 <- Referenced <$> nextNum
      n5 <- Referenced <$> nextNum
      n6 <- Referenced <$> nextNum
      t <- gets (P.inType . stream)
      name <- gets (P.name . stream)
      -- allocate return object
      modify $ addAssembly (T.pack (show n ++ " = alloca " ++ llvmType t))
      -- get their locals
      modify $ addAssembly (T.pack (show n2 ++ " = getelementptr %stream_" ++ name ++ "_locals, ptr %locals, i32 1"))
      -- get next call
      modify $ addAssembly (T.pack (show n3 ++ " = getelementptr %clt, ptr %call_list, i32 1"))
      -- get next block
      modify $ addAssembly (T.pack (show n4 ++ " = getelementptr i8*, i8** %block, i32 1"))
      -- load this call
      modify $ addAssembly (T.pack (show n5 ++ " = load ptr, ptr %call_list"))
      -- call next in pipeline
      modify $ addAssembly (T.pack (show n6 ++ " = call i1 " ++ show n5 ++ "(ptr " ++ show n2 ++ ", ptr " ++ show n ++ ", ptr " ++ show n3 ++ ", ptr " ++ show n4 ++ ")"))
      -- if the stream did not return anything, do not return anything as well
      modify incrLabel
      l <- gets labelN
      modify $ addAssembly (T.pack ("br i1 " ++ show n6 ++ ", label %lbl" ++ show l ++ ", label %.blockblocked"))
      modify $ addAssembly (T.pack ("lbl" ++ show l ++ ":"))

      if Types.isFirstClass t then do
        n7 <- Referenced <$> nextNum
        modify $ addAssembly (T.pack (show n7 ++ " = load " ++ llvmType t ++ ", ptr " ++ show n))
        lift $ Right (t, n7)
      else
        lift $ Right (t, n)

    _ -> lift $ Left "catch not implemented"
assemble (P.Pipe what to) = do
  (twhat, aswhat) <- assemble what
  name <- gets (P.name . stream)
  case to of
    (P.Identifier ["out"]) -> do
      modify incrBlock
      bn <- gets blockN
      modify $ addAssembly (T.pack ("store " ++ llvmType twhat ++ " " ++ show aswhat ++ ", ptr %return_ptr"))
      modify $ addAssembly (T.pack ("store i8* blockaddress(@stream_" ++ name ++ ", %.block" ++ show bn ++ "), i8** %block"))
      modify $ addAssembly (T.pack "ret i1 1")
      modify $ addAssembly (T.pack (".block" ++ show bn ++ ":"))
      lift $ Right (Types.Void, Dummy)
    _ -> lift $ Left "pipe not implemented"


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

addDeclaration dec (AssemblerState ln n g a d t l locn sn bn) =
  AssemblerState ln n g a (d++[dec]) t l locn sn bn
incrNum (AssemblerState ln n g a d t l locn sn bn) =
  AssemblerState ln (n+1) g a d t l locn sn bn
incrLabel (AssemblerState ln n g a d t l locn sn bn) =
  AssemblerState (ln+1) n g a d t l locn sn bn
incrGlobal (AssemblerState ln n g a d t l locn sn bn) =
  AssemblerState ln n (g+1) a d t l locn sn bn
incrBlock (AssemblerState ln n g a d t l locn sn bn) =
  AssemblerState ln n g a d t l locn sn (bn + 1)
addAssembly ass (AssemblerState ln n g a d t l locn sn bn) =
  AssemblerState ln n g (a++[ass]) d t l locn sn bn
declareVar name typ (AssemblerState ln n g a d t l locn sn bn) =
  AssemblerState ln n g a d (Map.insert name (Types.Variable typ) t) (Map.insert name (length locn) l) (locn ++ [typ]) sn bn
setLocals locals (AssemblerState ln n g a d t l locn sn bn) =
  AssemblerState ln n g a d t locals locn sn bn

assemblerStateEmpty t name = AssemblerState 0 1 0 [] [] t Map.empty [] name 0

testAssembleModule :: TypeMap -> [P.ASTNode] -> Either String T.Text
testAssembleModule types streams = do
  streams <- mapM (assembleStream types) streams
  return $ T.unlines [defs, T.unlines streams, runtime]

assembleStream :: TypeMap -> P.ASTNode -> Either String T.Text
assembleStream types (P.Stream name inT outT (Just body)) = do
  (_, as) <- runStateT (assembleBlock body) (assemblerStateEmpty types (P.Stream name inT outT Nothing))
  return $ T.unlines [
      T.unlines (declarations as),
      T.pack ("%stream_"++ name ++"_locals = type {" ++ concatWith ", " (map llvmType (localNumber as)) ++ "}"),
      T.pack ("define i1" ++ " @stream_" ++ name ++ "(ptr %locals, ptr %return_ptr, %clt* %call_list, i8** %block) noinline {"),
      T.pack ("  %1 = load ptr, ptr %block\n  indirectbr ptr %1, [ " ++ concatWith ", " (map (\x -> "label %.block" ++ show x) [0..(blockN as)]) ++ ", label %.blockblocked ]\n\n.block0:"),
      T.unlines (map (T.append (T.pack "  ")) (assembly as)),
      T.pack ("  br label %.block0\n\n.blockblocked:\n  store i8* blockaddress(@stream_" ++ name ++ ", %.blockblocked), i8** %block\n  ret i1 0\n}")
    ]

assembleStream _ (P.ExternFunction name args retVal) = pure $ T.pack ("declare " ++ llvmType retVal ++ " @" ++ name ++ "(" ++ fArgs args ++ ")")

assembleStruct _ (P.Struct name fields) = pure $ T.pack ""

-- generated with: sed 's/$/\\n/g' runtime.ll | tr -d '\n'
defs = T.pack "%clt = type { i1(ptr,ptr,%clt*,i8**)* }\n"
runtime = T.pack "define i1 @const_args(ptr %input, ptr %return_ptr, %clt* %call_list, i8** %block) {\n  %1 = load i8*, i8** %block\n  indirectbr i8* %1, [label %.block0, label %.block1, label %.blockblocked ]\n\n.block0:\n  %2 = getelementptr {i32, ptr}, ptr null, i32 1\n  %3 = ptrtoint ptr %2 to i32\n  call void @llvm.memcpy.p0.p0.i32(ptr %return_ptr, ptr %input, i32 %3, i1 0)\n  store i8* blockaddress(@const_args, %.block1), i8** %block\n  ret i1 1\n\n.block1:\n  br label %.blockblocked ; break equivalent\n\n.blockblocked:\n  ; make sure we are blocked - we can now just jump to .blockblocked to immediately block ourselves\n  store i8* blockaddress(@const_args, %.blockblocked), i8** %block\n  ret i1 0\n}\n\n@pipeline_main = internal constant [2 x %clt*] [%clt* @stream_main, %clt* @const_args]\n\n%pipeline_stack_main = type { %stream_main_locals, i32 }\n\ndefine i32 @main(i32 %argc, i8** %argv) noinline {\n  %1 = alloca i32\n\n  ; init pipeline\n  %2 = getelementptr %clt, %clt* @pipeline_main, i32 1\n\n  %3 = load ptr, ptr @pipeline_main\n\n  %4 = alloca %pipeline_stack_main\n  %5 = alloca [2 x i8*]\n\n  %6 = getelementptr i8*, i8** %5, i32 0\n  store i8* blockaddress(@stream_main, %.block0), i8** %6\n  %7 = getelementptr i8*, i8** %5, i32 1\n  store i8* blockaddress(@const_args, %.block0), i8** %7\n\n  ; init args input\n  %8 = alloca {i32,ptr}\n  %9 = getelementptr {i32,ptr}, ptr %8, i32 0, i32 0\n  %10 = getelementptr {i32,ptr}, ptr %8, i32 0, i32 1\n  store i32 %argc, ptr %9\n  store ptr %argv, ptr %10\n\n  ; call main stream\n  %11 = call i1 %3(ptr %4, ptr %1, %clt* %2, i8** %5)\n\n  ; load output\n  br i1 %11, label %success, label %fail\n\nsuccess:\n  %12 = load i32, ptr %1\n  ret i32 %12\n\nfail:\n  ret i32 1\n}\n\ndeclare void @llvm.memcpy.p0.p0.i32(ptr, ptr, i32, i1)\n"

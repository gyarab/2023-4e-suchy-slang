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
import Text.Read (readMaybe)
import Data.Char (ord)

data Module = Module {
   constants :: ![(String, String)],
   definitions :: ![String]
 }


type TypeMap = Map String Types.TypedObject

data Assembled = Inline !Int | Referenced !Int | Global !String | Dummy | Pipeline !Int !Int | Raw !String
  deriving (Eq)

type TAssembled = (Types.Type, Assembled)

type AsState = StateT AssemblerState (Either String)

instance Show Assembled where
  show (Inline i) = show i
  show (Referenced i) = '%':show i
  show (Global s) = '@':s
  show Dummy = "USED DUMMY SOMEWHERE. NICHT GOOD"
  show (Pipeline i _) = "pipeline_" ++ show i
  show (Raw s) = s

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

getStackPtr :: Int -> AsState Assembled
getStackPtr v = do
  name <- gets (P.name . stream)
  n <- Referenced <$> nextNum
  modify $ addAssembly (T.pack (show n ++ " = getelementptr %stream_"++name++"_locals, ptr %locals, i32 0, i32 " ++ show v))
  return n

sizeOf :: Types.Type -> AsState TAssembled
sizeOf typ = do
  n <- Referenced <$> nextNum
  n2 <- Referenced <$> nextNum
  -- get size of element into n2
  modify $ addAssembly (T.pack (show n ++ " = getelementptr " ++ llvmType typ ++ ", ptr null, i32 1"))
  modify $ addAssembly (T.pack (show n2 ++ " = ptrtoint ptr " ++ show n ++ " to i32"))
  return (Types.I32, n2)

memCpy :: Types.Type -> Assembled -> Assembled -> AsState ()
memCpy typ from to = do
  (_, size) <- sizeOf typ
  -- call memcpy intrinsic
  modify $ addAssembly (T.pack ("call void @llvm.memcpy.p0.p0.i32(ptr " ++ show to ++ ", ptr " ++ show from ++ ", i32 " ++ show size ++ ", i1 0)"))
  lift $ Right ()

memSet :: Types.Type -> Assembled -> Int -> AsState ()
memSet typ what byte = do
  (_, size) <- sizeOf typ
  -- call memset intrinsic
  modify $ addAssembly (T.pack ("call void @llvm.memset.p0.i32(ptr " ++ show what ++ ", i8 " ++ show byte ++ ", i32 " ++ show size ++ ", i1 0)"))
  lift $ Right ()

store :: Types.Type -> Assembled -> Assembled -> AsState ()
store typ what whereTo =
  if Types.isFirstClass typ then do
    modify $ addAssembly (T.pack ("store " ++ llvmType typ ++ " " ++ show what ++ ", ptr " ++ show whereTo))
  else
    memCpy typ what whereTo

pipeToOut :: Types.Type -> Assembled -> AsState TAssembled
pipeToOut twhat aswhat = do
  name <- gets (P.name . stream)
  outType <- gets (P.outType . stream)
  if twhat == outType then do
    modify incrBlock
    bn <- gets blockN
    if Types.isFirstClass twhat then do
      modify $ addAssembly (T.pack ("store " ++ llvmType twhat ++ " " ++ show aswhat ++ ", ptr %return_ptr"))
    else do
      memCpy twhat aswhat (Raw "%return_ptr") 
    modify $ addAssembly (T.pack ("store i8* blockaddress(@stream_" ++ name ++ ", %.block" ++ show bn ++ "), i8** %block"))
    modify $ addAssembly (T.pack "ret i1 1")
    modify $ addAssembly (T.pack (".block" ++ show bn ++ ":"))
    lift $ Right (Types.Void, Dummy)
  else
    lift $ Left ("cannot output " ++ show twhat ++ " to " ++ show outType)

allocateOnStack :: Types.Type -> AsState Int
allocateOnStack t = do
  l <- gets locals
  localNumComb <- gets (length . localNumber)
  modify $ declareVar "x" t
  modify $ setLocals l -- kind of a hack to allocate memory on the stack without declaring the variable
  return localNumComb

loadIfNeeded :: Types.Type -> Assembled -> AsState TAssembled
loadIfNeeded typ ptr = do
  if Types.isFirstClass typ then do
      n <- Referenced <$> nextNum
      modify $ addAssembly (load n ptr typ)
      return (typ, n)
  else
    return (typ, ptr)

  where
    load n ptr t = T.pack (show n ++ " = load " ++ llvmType t ++ ", ptr " ++ show ptr)

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

assemble (P.ConstChar c) = pure (Types.Char, Inline (ord c))


assemble (P.Negate t) = assembleBinaryOp [
    (Types.I64, "sub"),
    (Types.I32, "sub"),
    (Types.Char, "sub"),
    (Types.Boolean, "sub"),
    (Types.Float, "fsub")
  ] (P.ConstInt 0) t -- 0-t = -t
assemble (P.Add l r) = assembleBinaryOp [
    (Types.I64, "add"),
    (Types.I32, "add"),
    (Types.Char, "add"),
    (Types.Boolean, "add"),
    (Types.Float, "fadd")
  ] l r
assemble (P.Subtract l r) = assembleBinaryOp [
    (Types.I64, "sub"),
    (Types.I32, "sub"),
    (Types.Char, "sub"),
    (Types.Boolean, "sub"),
    (Types.Float, "fsub")
  ] l r
assemble (P.Multiply l r) = assembleBinaryOp [
    (Types.I64, "mul"),
    (Types.I32, "mul"),
    (Types.Char, "mul"),
    (Types.Boolean, "mul"),
    (Types.Float, "fmul")
  ] l r
assemble (P.Divide l r) = assembleBinaryOp [
    (Types.I64, "sdiv"),
    (Types.I32, "sdiv"),
    (Types.Char, "sdiv"),
    (Types.Boolean, "sdiv"),
    (Types.Float, "fdiv")
  ] l r
assemble (P.Modulo l r) = assembleBinaryOp [
    (Types.I64, "srem"),
    (Types.I32, "srem"),
    (Types.Char, "srem"),
    (Types.Boolean, "srem"),
    (Types.Float, "frem")
  ] l r


assemble (P.Eq l r) = replaceType Types.Boolean <$> assembleBinaryOp [
    (Types.I64, "icmp eq"),
    (Types.I32, "icmp eq"),
    (Types.Char, "icmp eq"),
    (Types.Boolean, "icmp eq"),
    (Types.Float, "fcmp eq")
  ] l r
assemble (P.Neq l r) = replaceType Types.Boolean <$> assembleBinaryOp [
    (Types.I64, "icmp ne"),
    (Types.I32, "icmp ne"),
    (Types.Char, "icmp ne"),
    (Types.Boolean, "icmp ne"),
    (Types.Float, "fcmp ne")
  ] l r
assemble (P.Geq l r) = replaceType Types.Boolean <$> assembleBinaryOp [
    (Types.I64, "icmp sge"),
    (Types.I32, "icmp sge"),
    (Types.Char, "icmp sge"),
    (Types.Boolean, "icmp sge"),
    (Types.Float, "fcmp sge")
  ] l r
assemble (P.Leq l r) = replaceType Types.Boolean <$> assembleBinaryOp [
    (Types.I64, "icmp sle"),
    (Types.I32, "icmp sle"),
    (Types.Char, "icmp sle"),
    (Types.Boolean, "icmp sle"),
    (Types.Float, "fcmp sle")
  ] l r
assemble (P.Gtr l r) = replaceType Types.Boolean <$> assembleBinaryOp [
    (Types.I64, "icmp sgt"),
    (Types.I32, "icmp sgt"),
    (Types.Char, "icmp sgt"),
    (Types.Boolean, "icmp sgt"),
    (Types.Float, "fcmp sgt")
  ] l r
assemble (P.Lsr l r) = replaceType Types.Boolean <$> assembleBinaryOp [
    (Types.I64, "icmp slt"),
    (Types.I32, "icmp slt"),
    (Types.Char, "icmp slt"),
    (Types.Boolean, "icmp slt"),
    (Types.Float, "fcmp slt")
  ] l r

assemble (P.And l r) = assembleBinaryOp [(Types.Boolean, "and")] l r
assemble (P.Or l r) = assembleBinaryOp [(Types.Boolean, "or")] l r
assemble (P.Not v) = assembleBinaryOp [(Types.Boolean, "xor")] (P.ConstBoolean False) v -- 0^x = !x

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
        lift $ Left ("declaration of " ++ show name ++ ": cannot assign " ++ show vtyp ++ " to declared type " ++ show t)
    (Nothing, Just v) -> do
      (vtyp, vass) <- assemble v
      case vass of
        Pipeline pn ln -> do
          modify $ declarePipeline name (Types.Pipeline pn vtyp) ln
          return (vtyp, Dummy)
        _ -> declareAndStore vtyp vass
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

assemble (P.Identifier a) = assembleIdentifier False (P.Identifier a)

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

assemble (P.Dereference times val) = do
  (tval, assval) <- assemble val
  dereference tval times assval
  where
    dereference typ times ptr = do
      case typ of
        Types.Pointer t -> do
          n <- Referenced <$> nextNum
          modify $ addAssembly (T.pack (show n ++ " = load " ++ llvmType t ++ ", ptr " ++ show ptr))
          if times > 1
            then dereference t (times-1) n
            else return (t, n)
        _ -> lift $ Left ("cannot dereference " ++ show typ)


assemble (P.Catch what) = do
  (twhat, aswhat) <- assemble what
  (retType, ref) <- case aswhat of
    Pipeline (-1) _ -> do
      name <- gets (P.name . stream)
      let Types.PipelineT _ t = twhat
      -- allocate return object
      n <- allocateOnStack t >>= getStackPtr
      n2 <- Referenced <$> nextNum
      n3 <- Referenced <$> nextNum
      n4 <- Referenced <$> nextNum
      n5 <- Referenced <$> nextNum
      n6 <- Referenced <$> nextNum
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
      return (t, n)

    Pipeline pn ln -> do
      let Types.PipelineT _ t = twhat
      let combType = Types.StructureStub ("%pipeline_" ++ show pn ++ "_stack_comb")

      funcPtr <- Referenced <$> nextNum
      modify $ addAssembly (T.pack (show funcPtr ++ " = load ptr, ptr @pipeline_"++show pn))
      -- allocate return object
      n <- allocateOnStack t >>= getStackPtr

      stack <- getStackPtr ln

      blockPtr <- Referenced <$> nextNum
      modify $ addAssembly (T.pack (show blockPtr ++ " = getelementptr " ++ llvmType combType ++ ", ptr " ++ show stack ++ ", i32 0, i32 1"))

      -- get next call
      nxtCall <- Referenced <$> nextNum
      modify $ addAssembly (T.pack (show nxtCall ++ " = getelementptr %clt, %clt* @pipeline_" ++ show pn ++ ", i32 1"))

      retVal <- Referenced <$> nextNum
      modify $ addAssembly (T.pack (show retVal ++ " = call i1 " ++ show funcPtr ++ "(ptr " ++ show stack ++ ", ptr " ++ show n ++ ", ptr " ++ show nxtCall ++ ", ptr " ++ show blockPtr ++ ")"))

      modify incrLabel
      l <- gets labelN
      modify $ addAssembly (T.pack ("br i1 " ++ show retVal ++ ", label %lbl" ++ show l ++ ", label %.blockblocked"))
      modify $ addAssembly (T.pack ("lbl" ++ show l ++ ":"))

      return (t, n)
      
    d -> lift $ Left ("can only catch pipelines, but got " ++ show d)


  if Types.isFirstClass retType then do
    n <- Referenced <$> nextNum
    modify $ addAssembly (T.pack (show n ++ " = load " ++ llvmType retType ++ ", ptr " ++ show ref))
    return (retType, n)
  else
    return (retType, ref)


assemble (P.Pipe what to) = do
  let calls = collapse what
  let toOut = case to of
        (P.Identifier ["out"]) -> True
        _ -> False

  let (input:pipeline) = reverse (if toOut then calls else to:calls)

  boobies <- mapM checkStream (reverse pipeline)

  if null boobies then do
    (twhat, awhat) <- assemble input
    pipeToOut twhat awhat
  else do
    (tinput, vinput) <- assemble input
    case tinput of
      Types.PipelineT {} -> lift $ Left "first node in a pipeline must be some concrete input (can be ())"
      _ -> return ()

    modify incrGlobal
    pipelineNumber <- gets global
    let callList = Global ("pipeline_" ++ show pipelineNumber)
    -- Create a global call list. This is a constant that represents the order of the streams, together with addresses to the functions.
    modify $ addDeclaration (T.pack (show callList ++ " = internal constant [" ++ show (length boobies + 1) ++ " x %clt*] [" ++ formatCallList boobies ++ ", %clt* @const_copy]"))

    let combType = Types.StructureStub ("%pipeline_" ++ show pipelineNumber ++ "_stack_comb")
    let stackType = Types.StructureStub ("%pipeline_" ++ show pipelineNumber ++ "_stack")

    -- Create a call stack type. This will have to be initialized for the pipeline's local variables.
    modify $ addDeclaration (T.pack (llvmType stackType ++ " = type {" ++ formatCallStack boobies ++ ", %sptr, " ++ llvmType tinput ++ "}"))

    -- Create a combined type with the pipeline's blocks for ease of use
    modify $ addDeclaration (T.pack (llvmType combType ++ " = type { " ++ llvmType stackType ++ ", [" ++ show (length boobies + 1) ++ " x i8*] }"))

    localNumComb <- allocateOnStack combType
    -- init blocks
    combPtr <- getStackPtr localNumComb
    blockPtr <- Referenced <$> nextNum
    modify $ addAssembly (T.pack (show blockPtr ++ " = getelementptr " ++ llvmType combType ++ ", ptr " ++ show combPtr ++ ", i32 0, i32 1"))
    foldM_ (addBlockInit blockPtr) 0 boobies

    -- init const copy block
    constBlockPtr <- Referenced <$> nextNum
    modify $ addAssembly (T.pack (show constBlockPtr ++ " = getelementptr i8*, ptr " ++ show blockPtr ++ ", i32 " ++ show (length boobies)))
    modify $ addAssembly (T.pack ("store i8* blockaddress(@const_copy, %.block0), i8** " ++ show constBlockPtr))

    (_, inputSize) <- sizeOf tinput
    inpSizePtr <- Referenced <$> nextNum
    modify $ addAssembly (T.pack (show inpSizePtr ++ " = getelementptr " ++ llvmType stackType ++ ", ptr " ++ show combPtr ++ ", i32 0, i32 " ++ show (length boobies)))
    inpPtrPtr <- Referenced <$> nextNum
    modify $ addAssembly (T.pack (show inpPtrPtr ++ " = getelementptr %sptr, ptr " ++ show inpSizePtr ++ ", i32 0, i32 1"))
    inputPtr <- Referenced <$> nextNum
    modify $ addAssembly (T.pack (show inputPtr ++ " = getelementptr " ++ llvmType stackType ++ ", ptr " ++ show combPtr ++ ", i32 0, i32 " ++ show (length boobies + 1)))
    modify $ addAssembly (T.pack ("store i32 " ++ show inputSize ++ ", ptr " ++ show inpSizePtr))
    modify $ addAssembly (T.pack ("store ptr " ++ show inputPtr ++ ", ptr " ++ show inpPtrPtr))

    -- copy input
    if Types.isFirstClass tinput then do
      modify $ addAssembly (T.pack ("store " ++ llvmType tinput ++ " " ++ show vinput ++ ", ptr " ++ show inputPtr))
    else do
      modify $ addAssembly (T.pack ("call void @llvm.memcpy.p0.p0.i32(ptr " ++ show inputPtr ++ ", ptr " ++ show vinput ++ ", i32 " ++ show inputSize ++ ", i1 0)"))


    if toOut then
      lift $ Left "pipeline into out not impl"
    else 
      let (Types.Stream inT _) = snd . head $ boobies
          (Types.Stream _ outT) = snd . last $ boobies
          in return (Types.PipelineT inT outT, Pipeline pipelineNumber localNumComb)

  where
    collapse :: P.ASTNode -> [P.ASTNode]
    collapse (P.Pipe w t) = t:collapse w
    collapse x = [x]

    checkStream :: P.ASTNode -> AsState (String, Types.TypedObject)
    checkStream (P.Identifier (name:xs)) = do
      t <- gets (Map.lookup name . types)
      if isJust t then let (Just x) = t in lift $ Right (name, x) else lift $ Left ("undefined stream: " ++ name)


    checkStream _ = lift $ Left "only streams can be in a pipeline"

    formatCallList pipeline = concatWith ", " (map (\x -> "%clt* @stream_" ++ fst x) pipeline)

    formatCallStack pipeline = concatWith ", " (map (\x -> "%stream_" ++ fst x ++ "_locals") pipeline)

    addBlockInit :: Assembled -> Int -> (String, Types.TypedObject) -> AsState Int
    addBlockInit blockPtr counter node = do
      n <- Referenced <$> nextNum
      modify $ addAssembly (T.pack (show n ++ " = getelementptr i8*, ptr " ++ show blockPtr ++ ", i32 " ++ show counter))
      modify $ addAssembly (T.pack ("store i8* blockaddress(@stream_" ++ fst node ++ ", %.block0), i8** " ++ show n))
      return (counter + 1)

assemble (P.Cast typ val) = do
  (tval, assval) <- assemble val
  case (tval, typ) of
    (Types.Pointer x, Types.Pointer y) -> return (typ, assval)
    (Types.I32, Types.I64) -> extend tval typ assval
    (Types.Char, Types.I64) -> extend tval typ assval
    (Types.Boolean, Types.I64) -> extend tval typ assval
    (Types.Char, Types.I32) -> extend tval typ assval
    (Types.Boolean, Types.I32) -> extend tval typ assval
    (Types.Boolean, Types.Char) -> extend tval typ assval
    (Types.I64, Types.I32) -> truncate tval typ assval
    (Types.I64, Types.Char) -> truncate tval typ assval
    (Types.I64, Types.Boolean) -> truncate tval typ assval
    (Types.I32, Types.Char) -> truncate tval typ assval
    (Types.I32, Types.Boolean) -> truncate tval typ assval
    (Types.Char, Types.Boolean) -> truncate tval typ assval
    _ -> lift $ Left ("cannot cast " ++ show tval ++ " to " ++ show typ)

  where
    extend t1 t2 ref = do
      n <- Referenced <$> nextNum
      modify $ addAssembly (T.pack (show n ++ " = zext " ++ llvmType t1 ++ " " ++ show ref ++ " to " ++ llvmType t2))
      return (t2, n)
    truncate t1 t2 ref = do
      n <- Referenced <$> nextNum
      modify $ addAssembly (T.pack (show n ++ " = trunc " ++ llvmType t1 ++ " " ++ show ref ++ " to " ++ llvmType t2))
      return (t2, n)

assemble (P.MakeTuple vals) = do
  assembled <- mapM assemble vals
  let t = Types.Tuple (map fst assembled)
  ref <- allocateOnStack t >>= getStackPtr
  foldM_ (ptrAndStore t ref) 0 assembled
  return (t, ref)

  where
    ptrAndStore t ref counter (typ, val) = do
      n <- Referenced <$> nextNum
      modify $ addAssembly (T.pack (show n ++ " = getelementptr " ++ llvmType t ++ ", ptr " ++ show ref ++ ", i32 0, i32 " ++ show counter))
      modify $ addAssembly (T.pack ("store " ++ llvmType typ ++ " " ++ show val ++ ", ptr " ++ show n))
      return (counter + 1)

assemble (P.Index a b) = assembleIndex False (P.Index a b)

-- dont reference multiple times :)
assemble (P.Reference times val) =
  case val of
    P.Identifier {} -> assembleIdentifier True val
    P.Index {} -> assembleIndex True val
    _ -> lift $ Left "can only reference indexed vars or identifiers"
    
assembleIdentifier :: Bool -> P.ASTNode -> AsState TAssembled
assembleIdentifier forceNoLoad (P.Identifier (v:attrs)) = do
  name <- gets (P.name . stream)

  (typ_obj, val) <- lookupVar v

  case typ_obj of
    Types.Variable t -> do
      ptr <- getStackPtr val
      (typ, ptr) <- if forceNoLoad then return (Types.ref t, ptr) else loadIfNeeded t ptr

      foldM getAttribute (typ, ptr) attrs

    Types.Pipeline n t -> return (t, Pipeline n val)

    _ -> lift $ Left (v ++ " is not a variable")

  where
    getAttribute (typ, ptr) attr = do
      n <- Referenced <$> nextNum
      case typ of
        Types.Tuple vals -> do
          case readMaybe attr :: Maybe Int of
            Just a ->
              if a < length vals then do
                let t = vals !! a
                modify $ addAssembly (T.pack (show n ++ " = getelementptr " ++ llvmType typ ++ ", ptr " ++ show ptr ++ ", i32 0, i32 " ++ attr))
                if forceNoLoad then return (Types.ref t, n) else loadIfNeeded t n
              else
                lift $ Left ("no attribute " ++ show attr ++ " on " ++ show typ)
            Nothing ->
              lift $ Left ("no attribute " ++ show attr ++ " on " ++ show typ)
        _ -> lift $ Left ("no attribute " ++ show attr ++ " on " ++ show typ)

assembleIndex :: Bool -> P.ASTNode -> AsState TAssembled
assembleIndex forceNoLoad (P.Index idx val) = do
  (tidx, asidx) <- assemble idx
  forceSameType Types.I32 tidx <?> ("index must be of type I32 (got " ++ show tidx ++ ")")
  (tval, asval) <- assemble val
  t <- case Types.deref tval of
    Just v -> return v
    Nothing -> lift $ Left ("indexed value must be ptr (got " ++ show tval ++ ")")
  
  n <- Referenced <$> nextNum
  modify $ addAssembly (T.pack (show n ++ " = getelementptr " ++ llvmType tval ++ ", ptr " ++ show asval ++ ", i32 " ++ show asidx))

  if forceNoLoad then return (Types.ref t, n) else loadIfNeeded t n

assembleBlock :: [P.ASTNode] -> AsState TAssembled
assembleBlock stmts = do
  locals <- gets locals
  assembled <- mapM assemble stmts
  modify $ setLocals locals -- exit block (throw away local vars)
  return $ last assembled

assembleBinaryOp :: [(Types.Type, String)] -> P.ASTNode -> P.ASTNode -> AsState TAssembled
assembleBinaryOp instr left right = binOp (Map.fromList instr) left right
  where
    binOp typeMap left right = do
      (tl, al) <- assemble left
      (tr, ar) <- assemble right
      instr <- case Map.lookup tr typeMap of
        Just x -> return x
        Nothing -> lift $ Left ("operation not defined on " ++ show tl)

      forceSameType tl tr <?> ("cannot operate " ++ show tl ++ " with " ++ show tr)

      modify incrNum

      n <- gets (Referenced . num)
      modify $ addAssembly (mk instr n al ar tr)

      return (tl, n)

      where
        mk instr n l r t = T.pack (show n ++ " = " ++ instr ++ " " ++ llvmType t ++ " " ++ show l ++ ", " ++ show r)

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
declarePipeline name obj lnn (AssemblerState ln n g a d t l locn sn bn) =
  AssemblerState ln n g a d (Map.insert name obj t) (Map.insert name lnn l) locn sn bn
setLocals locals (AssemblerState ln n g a d t l locn sn bn) =
  AssemblerState ln n g a d t locals locn sn bn

assemblerStateEmpty globalCounter t name = AssemblerState 0 1 globalCounter [] [] t (Map.fromList [("in", -1), ("out", -2)]) [] name 0

testAssembleModule :: TypeMap -> [P.ASTNode] -> Either String T.Text
testAssembleModule types streams = do
  (streams, g) <- foldM ass (T.empty, 0) streams
  return $ T.unlines [defs, streams, runtime]

  where
    ass (assembledStreams, counter) stream = do
      (ass, g) <- assembleStream types counter stream
      return (T.unlines [assembledStreams, ass], g)

assembleStream :: TypeMap -> Int -> P.ASTNode -> Either String (T.Text, Int)
assembleStream types globalCounter (P.Stream name inT outT (Just body)) = do
  let types1 = Map.insert "in" (Types.Pipeline (-1) (Types.PipelineT Types.Void inT)) types
  let types2 = Map.insert "out" (Types.Pipeline (-2) (Types.PipelineT outT Types.Void)) types1
  (_, as) <- runStateT (assembleBlock body) (assemblerStateEmpty globalCounter types2 (P.Stream name inT outT Nothing))
  return (T.unlines [
      T.unlines (declarations as),
      T.pack ("%stream_"++ name ++"_locals = type {" ++ concatWith ", " (map llvmType (localNumber as)) ++ "}"),
      T.pack ("define i1" ++ " @stream_" ++ name ++ "(ptr %locals, ptr %return_ptr, %clt* %call_list, i8** %block) noinline {"),
      T.pack ("  %1 = load ptr, ptr %block\n  indirectbr ptr %1, [ " ++ concatWith ", " (map (\x -> "label %.block" ++ show x) [0..(blockN as)]) ++ ", label %.blockblocked ]\n\n.block0:"),
      T.unlines (map (T.append (T.pack "  ")) (assembly as)),
      T.pack ("  br label %.block0\n\n.blockblocked:\n  store i8* blockaddress(@stream_" ++ name ++ ", %.blockblocked), i8** %block\n  ret i1 0\n}")
    ], global as)

assembleStream _ c (P.ExternFunction name args retVal) = pure (T.pack ("declare " ++ llvmType retVal ++ " @" ++ name ++ "(" ++ fArgs args ++ ")"), c)

assembleStruct _ (P.Struct name fields) = pure $ T.pack ""

defs = T.pack "%sptr = type {i32,ptr}\n%clt = type { i1(ptr,ptr,%clt*,i8**)* }\ndeclare void @llvm.memset.p0.i32(ptr,i8,i32,i1)\n@strdbg = internal constant [8 x i8] c\"dbg %d\\0A\\00\"\n"
-- generated with: sed 's/\\/\\\\/g;s/$/\\n/g;s/"/\\"/g' runtime.ll | tr -d '\n'
runtime = T.pack "; a const stream that just copies the input once and blocks itself\ndefine i1 @const_copy(ptr %input, ptr %return_ptr, %clt* %call_list, i8** %block) {\n  %1 = load i8*, i8** %block\n  indirectbr i8* %1, [label %.block0, label %.block1, label %.blockblocked ]\n\n.block0:\n  %2 = load i32, ptr %input\n  %3 = getelementptr %sptr, ptr %input, i32 0, i32 1\n  %4 = load ptr, ptr %3\n  %5 = load i64, ptr %4\n  call void @llvm.memcpy.p0.p0.i32(ptr %return_ptr, ptr %4, i32 %2, i1 1)\n  store i8* blockaddress(@const_copy, %.block1), i8** %block\n  ret i1 1\n\n.block1:\n  br label %.blockblocked ; break equivalent\n\n.blockblocked:\n  ; make sure we are blocked - we can now just jump to .blockblocked to immediately block ourselves\n  store i8* blockaddress(@const_copy, %.blockblocked), i8** %block\n  ret i1 0\n}\n\n@pipeline_main = internal constant [2 x %clt*] [%clt* @stream_main, %clt* @const_copy]\n\n%pipeline_stack_main = type { %stream_main_locals, %sptr, {i32,ptr} }\n\ndefine i32 @main(i32 %argc, i8** %argv) noinline {\n  %1 = alloca i32\n\n  ; init pipeline\n  %2 = getelementptr %clt, %clt* @pipeline_main, i32 1\n\n  %3 = load ptr, ptr @pipeline_main\n\n  %4 = alloca %pipeline_stack_main\n  %5 = alloca [2 x i8*]\n\n  %6 = getelementptr i8*, i8** %5, i32 0\n  store i8* blockaddress(@stream_main, %.block0), i8** %6\n  %7 = getelementptr i8*, i8** %5, i32 1\n  store i8* blockaddress(@const_copy, %.block0), i8** %7\n\n  ; init args input\n  %8 = getelementptr {i32,ptr}, ptr null, i32 1\n  %9 = ptrtoint ptr %8 to i32\n\n  ; store length to copy\n  %10 = getelementptr %pipeline_stack_main, ptr %4, i32 0, i32 1\n  %11 = getelementptr %sptr, ptr %10, i32 0, i32 1\n  store i32 %9, ptr %10\n\n  ; store input data (argc, argv)\n  %12 = getelementptr %pipeline_stack_main, ptr %4, i32 0, i32 2\n  store ptr %12, ptr %11\n  %13 = getelementptr {i32,ptr}, ptr %12, i32 0, i32 0\n  %14 = getelementptr {i32,ptr}, ptr %12, i32 0, i32 1\n  store i32 %argc, ptr %13\n  store ptr %argv, ptr %14\n\n\n  ; call main stream\n  %15 = call i1 %3(ptr %4, ptr %1, %clt* %2, i8** %5)\n\n  ; load output\n  br i1 %15, label %success, label %fail\n\nsuccess:\n  %16 = load i32, ptr %1\n  ret i32 %16\n\nfail:\n  ret i32 1\n}\n\ndeclare void @llvm.memcpy.p0.p0.i32(ptr, ptr, i32, i1)\n"

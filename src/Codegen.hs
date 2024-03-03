module Codegen(assemble, assembleTest, testAssembleStream) where

import qualified Parser as P
import qualified Data.Text as T

data Module = Module {
   constants :: ![(String, String)],
   declarations :: ![String],
   definitions :: ![String]
 }

data Assembled = Inline !Int | Referenced !Int
  deriving (Eq)

instance Show Assembled where
  show (Inline i) = show i
  show (Referenced i) = '%':show i

data AssemblerState = AssemblerState {
    num :: !Int,
    assembly :: !T.Text
  }
  deriving (Show, Eq)

assemblerStateEmpty = AssemblerState 0 T.empty


emptyModule = Module [] [] []

assemble :: AssemblerState -> P.ASTNode -> (AssemblerState, Assembled)

assemble as (P.ConstInt i) = (as, Inline i)

assemble as (P.Negate t) = assembleBinaryOp as "sub" (P.ConstInt 0) t -- 0-t = -t
assemble as (P.Add l r) = assembleBinaryOp as "add" l r
assemble as (P.Subtract l r) = assembleBinaryOp as "sub" l r
assemble as (P.Multiply l r) = assembleBinaryOp as "mul" l r
assemble as (P.Divide l r) = assembleBinaryOp as "sdiv" l r

-- create a "function" that can print i64s
assemble as (P.Call [arg] (P.Identifier ["print"])) = (as2, ref2)
  where
    (as1, ref1) = assemble as arg
    ref2 = Referenced (num as1 + 1)
    as2 = AssemblerState (i+1) newt
      where
        (AssemblerState i t) = as1
        newt = T.append t instruction
        instruction = T.pack ('\n':show ref2 ++ " = " ++ "call i32 (ptr, ...) @printf(ptr @.str, i64 " ++ show ref1 ++ ")")

assembleBinaryOp :: AssemblerState -> String -> P.ASTNode -> P.ASTNode -> (AssemblerState, Assembled)
assembleBinaryOp as instr left right = (as3, ref3)
  where
    (as1, ref1) = assemble as left
    (as2, ref2) = assemble as1 right
    ref3 = Referenced (num as2 + 1)
    as3 = AssemblerState (i+1) newt
      where
        (AssemblerState i t) = as2
        newt = T.append t instruction
        instruction = T.pack ('\n':show ref3 ++ " = " ++ instr ++ " i64 " ++ show ref1 ++ ", " ++ show ref2)

assembleUnaryOp :: AssemblerState -> String -> P.ASTNode -> (AssemblerState, Assembled)
assembleUnaryOp as instr inner = (as2, ref2)
  where
    (as1, ref1) = assemble as inner
    ref2 = Referenced (num as1 + 1)
    as2 = AssemblerState (i+1) newt
      where
        (AssemblerState i t) = as2
        newt = T.append t instruction
        instruction = T.pack ('\n':show ref2 ++ " = " ++ instr ++ " i64 " ++ show ref1)


decls = T.pack "@.str = private constant [4 x i8] c\"%d\\0A\\00\", align 1\ndeclare i32 @printf(ptr, ...)"

testAssembleStream :: P.ASTNode -> T.Text
testAssembleStream (P.Stream name args rv [s]) = T.unlines [decls, defineStart, assembly as, defineEnd]
  where
    (as, _) = assemble assemblerStateEmpty s
    defineStart = T.pack "define i32 @main() noinline optnone {"
    defineEnd = T.pack "ret i32 0\n}"

assembleTest = assemble (AssemblerState 0 T.empty) (P.Divide (P.Add (P.ConstInt 0) (P.Negate (P.ConstInt 1))) (P.ConstInt 2))

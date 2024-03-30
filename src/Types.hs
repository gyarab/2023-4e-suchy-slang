module Types (Type(..), ref, deref, llvmType, TypedObject(..), isFirstClass) where
import Data.Map(Map)

data Type
  = I64
  | I32
  | Char
  | Float
  | Boolean
  | Ellipsis -- only used for extern functions
  | Pointer !Type
  | StructureStub !String -- structure data stored separately
  | Void
  | Tuple ![Type]
  | PipelineT !Type !Type
  deriving (Show, Eq, Ord)

data TypedObject
  = Structure ![(String, Type)]
  | Function ![Type] !Type
  | Stream !Type !Type
  | Variable !Type
  | Pipeline !Int !Type
  deriving (Show, Eq, Ord)

-- XXX: we could techically dereference an ellipsis here :(
ref :: Type -> Type
ref = Pointer

deref :: Type -> Maybe Type
deref (Pointer t) = Just t
deref _ = Nothing

concatWith delim args = if not (null args) then head args ++ concatMap (delim++) (tail args) else ""

llvmType I64 = "i64"
llvmType I32 = "i32"
llvmType Char = "i8"
llvmType Boolean = "i1" -- :)
llvmType Ellipsis = "..."
llvmType Void = "void"
llvmType (Pointer _) = "ptr"
llvmType (StructureStub s) = s -- prepend struct_ to not collide with generated tuple types
llvmType (Tuple types) = "{" ++ concatWith "," (map llvmType types) ++ "}" -- inline LLVM structure type

-- determine if the type is first-class in LLVM
isFirstClass :: Type -> Bool
isFirstClass Void = False
isFirstClass StructureStub {} = False
isFirstClass Tuple {} = False
isFirstClass _ = True

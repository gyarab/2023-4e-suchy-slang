module Types (Type(..), ref, deref, llvmType, TypedObject(..)) where
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
  deriving (Show, Eq, Ord)

data TypedObject
  = Structure !(Map String Type)
  | Function ![Type] !Type
  | Variable !Type
  deriving (Show, Eq, Ord)

-- XXX: we could techically dereference an ellipsis here :(
ref :: Type -> Type
ref = Pointer

deref :: Type -> Maybe Type
deref (Pointer t) = Just t
deref _ = Nothing

llvmType I64 = "i64"
llvmType I32 = "i32"
llvmType Char = "i8"
llvmType Boolean = "i8" -- :)
llvmType Ellipsis = "..."
llvmType (Pointer _) = "ptr"

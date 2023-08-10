module Type where

data Type = 
  -- DataType String | 
  TVar String | TInt | TBool | TUnit | TFun Type Type deriving (Eq, Show)

data TypeDecl = TypeDecl  {
    typeDeclName  :: String,
    signature :: Type
}
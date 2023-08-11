module Syntax  where
import Type (Type (..))
import Data.Map 


-- type check -> desugar -> interpret or compile

-- type Field = Type
  -- data type name, data constructor, field type
-- data DataType = Data String String [Type]

-- choice typechecking before desugar or after desugar
-- haskell choose the former
-- typed lambda calculus
data Expr
  = -- DataCon String [Type]
  Var String 
  | App Expr Expr
  | Lam String Expr
  | Let [Decl] Expr
  deriving (Eq, Show)






data Decl
  = Decl { 
           declName  :: String
         , declRhs   :: Expr
         , declWhere :: Maybe [Decl]
         , declType :: Maybe Type
         }
  deriving (Eq, Show)




-- untyped lambda calculus
data ExprE
  = VarE String
  | AppE ExprE ExprE
  | LamE String ExprE
  deriving (Eq, Show)

data DeclE
  = DeclE { declNameE  :: String
         , declRhsE   :: ExprE
         }
  deriving (Eq, Show)

type Program = [Decl]

declToE :: Decl -> DeclE
declToE (Decl n r Nothing _) = DeclE n (eToE r)
declToE (Decl n r (Just w) _) = DeclE n $ eToE (Let w r)


eToE :: Expr -> ExprE
eToE (Var n) = VarE n
eToE (App f x) = AppE (eToE f) (eToE x)
eToE (Lam n b) = LamE n (eToE b)
eToE (Let (d:ds) b) = let dd = declToE d in  AppE (LamE (declNameE dd) (eToE (Let ds b))) (declRhsE dd)
eToE (Let [] b) = eToE b
eToE _ = error "not implemented"


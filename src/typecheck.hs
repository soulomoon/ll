{-# LANGUAGE DeriveFunctor #-}
module TypeCheck where
import Syntax
import Control.Monad.Reader (Reader, ask, local)
import qualified Data.Map as Map

data TypeF =
  TypeVar String
  | TypeAll String TypeF
  -- | TypeSome String TypeF 
  | TypeFun TypeF TypeF deriving (Eq, Show)

type TypeEnv = Map.Map String TypeF
type TcMonad = Reader TypeEnv


-- system F
data ExprF
  = VarF String
  -- abstraction
  | AbsF String TypeF ExprF
  -- type abstraction
  | TAbsF String ExprF
  -- application
  | AppF ExprF ExprF
  -- type application
  | TAppF ExprF TypeF

eToF :: Expr -> TcMonad ExprF
eToF (Var n) = return $ VarF n
eToF (App f x) = AppF <$> eToF f <*> eToF x
eToF (Lam n b) = (AbsF n <$> freshTypeVar) <*> eToF b
-- todo introduce type here
eToF (Let (d:ds) b) = AppF <$> (AbsF (declName d) <$> freshTypeVar <*> eToF (Let ds b)) <*> eToF (declRhs d)
eToF (Let [] b) = eToF b


freshTypeVar :: TcMonad TypeF
freshTypeVar = do
    env <- ask
    let n = Map.size env
    return $ TypeVar $ "t" ++ show n


lookUp :: String -> TcMonad TypeF
lookUp s = do
    env <- ask
    case Map.lookup s env of
        Just t -> return t
        Nothing -> error $ "Not in scope: " ++ s

inferType :: ExprF -> TcMonad TypeF
inferType (VarF s) =  lookUp s
inferType (AppF f x) = do
    tf <- inferType f
    case tf of
        (TypeFun ta tb) -> do
            tx <- inferType x
            if ta == tx then return tb else error "Type mismatch"
        _ -> error "expect function type in application"
inferType (AbsF s tv b) = local (Map.insert s tv) $ TypeFun tv <$> inferType b
inferType (TAbsF s b) = do
    tv <- freshTypeVar
    local (Map.insert s tv) $ TypeAll s <$> inferType b
inferType (TAppF e t) = do
    te <- inferType e
    case te of
        (TypeAll s b) -> return $ substF s t b
        _ -> error "expect forall type in type application"

substF :: String -> TypeF -> TypeF -> TypeF
substF s t (TypeVar s') = if s == s' then t else TypeVar s'
substF s t (TypeAll s' b) = if s == s' then TypeAll s' b else TypeAll s' (substF s t b)
substF s t (TypeFun a b) = TypeFun (substF s t a) (substF s t b)

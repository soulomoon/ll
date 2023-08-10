{-# LANGUAGE DeriveFunctor #-}
module TypeCheck where
import Type
import Syntax
import Control.Monad.Reader (Reader, ask, local)
import qualified Data.Map as Map

type TypeEnv = Map.Map String Type
type TcMonad = Reader TypeEnv 

freshTypeVar :: TcMonad Type
freshTypeVar = do
    env <- ask
    let n = Map.size env
    return $ TVar $ "t" ++ show n


lookUp :: String -> TcMonad Type
lookUp s = do
    env <- ask 
    case Map.lookup s env of
        Just t -> return t
        Nothing -> error $ "Not in scope: " ++ s

inferType :: ExprE -> TcMonad Type
inferType (VarE s) =  lookUp s
inferType (AppE f x) = do
    tf <- inferType f
    case tf of
        (TFun ta tb) -> do 
            tx <- inferType x
            if ta == tx then return tb else error "Type mismatch"
        _ -> error "expect function type in application"
inferType (LamE s b) = do 
    var <- freshTypeVar
    local (Map.insert s var) $ TFun var <$> inferType b

    
--   case tb of 
--   Just b' -> Just $ TFun TInt b'
--   _ -> Nothing
-- -- 
-- inferType (Let (d:ds) b) = undefined
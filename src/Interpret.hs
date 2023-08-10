{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Interpret where

import Syntax
import Control.Monad.Reader (ReaderT, Reader, MonadReader (ask))
import Control.Monad.State (StateT)
-- import Control.Monad.Trans (MaybeT)
import qualified Data.Map as Map
import Control.Applicative

-- normalization monad
-- small step evaluation
-- evaluation monad

data Eval a =  NeedEval a | Fin a deriving (Show, Functor, Eq)

data Cxt = Top | L Cxt ExprE | R ExprE Cxt
type Loc = (ExprE, Cxt)

toLoc :: ExprE -> Loc
toLoc expr = (expr, Top)


-- goDown :: Loc -> Eval Loc
goDown (AppE f x, cxt) fail =
  goDown (f, L cxt x) (goDown (x, R f cxt)
    (case f of
      LamE n b -> Fin (subst n x b, cxt)
      _ -> error $ show f ++ "not a function"
    ))
goDown (LamE n b, cxt) fail = fail
goDown (x@(VarE n), cxt) fail = fail

nextLoc :: Loc -> Loc
nextLoc (expr, ctx) = case expr of
  AppE f x -> (f, L ctx x)
  VarE n -> case ctx of
    Top -> error "no next loc"
    L cxt x -> error "no next loc"
    R f cxt -> (f, L ctx expr)
  LamE n b -> undefined


goUp :: Loc -> Loc
goUp (expr, Top) = (expr, Top)
goUp (expr, L cxt x) = (AppE expr x, cxt)
goUp (expr, R f cxt) = (AppE f expr, cxt)



-- smallStep3 :: ExprE -> Cxt -> (ExprE, Cxt)
-- smallStep3 expr Top = case expr of
--   AppE f x -> (f, L Top x)
--   _ -> (expr, Top)


instance Applicative Eval where
  pure = NeedEval
  (<*>) f a = case (f, a) of
    (Fin f', Fin a') -> error "Both fin means over stepping"
    (Fin f', NeedEval a') -> Fin $ f' a'
    (NeedEval f', Fin a') -> Fin $ f' a'
    (NeedEval f', NeedEval a') -> NeedEval $ f' a'

instance Alternative Eval where
  empty = error "empty"
  (<|>) a b = case (a, b) of
    (Fin a', _) -> Fin a'
    (_, Fin b') -> Fin b'
    (result, _) -> result

instance Monad Eval where
  (>>=) :: Eval a -> (a -> Eval b) -> Eval b
  (>>=) (Fin a) f = f a
  (>>=) (NeedEval a) f = f a

-- | ev : special combinator transformer 
-- that transforms the eval function to a combinator
ev :: (t -> Eval t) -> Eval (t -> a) -> t -> Eval a
ev _ (Fin g) a = Fin g <*> pure a
ev f g a = (g <*> f a) <|> (g <*> pure a)


smallStep2 :: ExprE -> Eval ExprE
smallStep2 expr = case expr of
  AppE f x -> case (smallStep2 f, smallStep2 x) of
    (Fin f', _)-> Fin $ AppE f' x
    (_, Fin x') -> Fin $ AppE f x'
    _ -> case f of
      LamE n b -> Fin $ subst n x b
      _ -> NeedEval expr
  _ -> NeedEval expr


(<***>) :: Eval (ExprE -> a) -> ExprE -> Eval a
(<***>) = ev smallStep
(<$$>) :: (ExprE -> a) -> ExprE -> Eval a
(<$$>) = (<***>) . pure

-- >>> smallStep (AppE  (VarE "y") (VarE "y"))
-- Error "variable y not in scope and variable y not in scope and VarE \"y\"not a function"

smallStep :: ExprE -> Eval ExprE
smallStep expr = case expr of
  AppE f x -> (AppE <$$> f <***> x)
    <|> (case f of
          LamE n b -> Fin $ subst n x b
          _ -> error $ show f ++ "not a function")
  _ -> NeedEval expr

showSteps :: ExprE -> IO ()
showSteps expr = do
        putStrLn "-----------steps-----------"
        print expr
        go expr
        putStrLn "-----------steps done-----------"
  where
    go expr = case smallStep expr of
      NeedEval _ -> error "cannot evaluate"
      Fin x -> print x >> go x

bigStep :: ExprE -> Eval ExprE
bigStep x = case smallStep x of
  Fin x' -> bigStep x'
  NeedEval x' -> error $ "cannot evaluate " ++ show x'

notInScope :: String -> String
notInScope n = "variable " ++ n ++ " not in scope"


-- >>> smallStep $ AppE (LamE "x" (VarE "x")) (VarE "y")
-- Fin (VarE "y")
-- >>> smallStep $ AppE (AppE (LamE "x" (LamE "x" (VarE "x"))) (VarE "y")) (VarE "z")
-- Fin (AppE (LamE "x" (VarE "x")) (VarE "z"))
-- >>> smallStep $ AppE (LamE "x" (VarE "x")) (AppE (LamE "x" (VarE "z")) (VarE "k"))
-- Fin (AppE (LamE "x" (VarE "x")) (VarE "z"))
-- >>> bigStep $ AppE (LamE "x" (VarE "x")) (AppE (LamE "x" (VarE "x")) (VarE "k"))
-- cannot evaluate VarE "k"

-- >>> fmap smallStep $ smallStep $ AppE (AppE (LamE "x" (LamE "x" (VarE "x"))) (VarE "y")) (VarE "z")
-- Fin (Fin (VarE "z"))


subst :: String -> ExprE -> ExprE -> ExprE
subst n x b = case b of
  VarE m -> if m == n then x else b
  AppE f y -> AppE (subst n x f) (subst n x y)
  LamE m b' -> if m == n then b else LamE m (subst n x b')

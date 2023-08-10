# Small step normalization

## Introduction

The goal is to do a small step normalization to the lambda calculus.
We are writing the small step combinator to make normalization clear and nice.

The lambda calculus expression contains three constructs:

- variables (x)
- functions(abstractions) (λx. e)
- application (e1 e2)

```haskell
data ExprE
  = VarE String
  | AppE ExprE ExprE
  | LamE String ExprE
```

We can see the only way to reduce the expression is to apply a function to an argument.
And application is the construct that contains two sub-expressions,

We need to reduce the sub-expression first and then reduce the whole expression. It is called the applicative order (opposed to the normal order).

## Small step

Small step make sure only one step is reduced at a time.
To represent the process of small step reduction, we can define
a data type

```haskell
data Eval a =  NeedEval a | Fin a | Error String 
```

The `NeedEval` constructor means the expression is not touched,
the `Fin` constructor means the small step reduction is finished,

## Naive implementation

```haskell
subst :: String -> ExprE -> ExprE -> ExprE
subst n x b = case b of
    VarE m -> if m == n then x else b
    AppE f y -> AppE (subst n x f) (subst n x y)
    LamE m b' -> if m == n then b else LamE m (subst n x b')

smallStep2 :: ExprE -> Eval ExprE
smallStep2 expr = case expr of
  AppE f x -> case (smallStep2 f, smallStep2 x) of
    (Fin f', _)-> Fin $ AppE f' x
    (_, Fin x') -> Fin $ AppE f x'
    _ -> case f of 
      LamE n b -> Fin $ subst n x b 
      _ -> Error $ show f ++ "not a function"
  VarE n -> Error $ "variable " ++ n ++ " not in scope"
  LamE n b -> Error "cannot evaluate lambda"
```

The naive implementation of small step reduction is to do a pattern match on the expression, which look stupid.
So I decide to use the `Functor, Applicative, Alternative` type class to make the code more elegant, in a combinator style !

## Functor and Applicative

Obviously, the `Eval` is a functor, we can define the `fmap` function

```haskell
instance Functor Eval where
  fmap f (NeedEval a) = NeedEval (f a)
  fmap f (Fin a) = Fin (f a)
  fmap f (Error s) = Error s
```

The `Applicative` instance is also easy to define.
It is defined in this way to ensure only one Fin is allowed.
And the Error is propagated.

```haskell
instance Applicative Eval where
  pure = NeedEval
  (<*>) f a = case (f, a) of
    (Fin f', Fin a') -> Error "Both fin means over stepping"
    (Fin f', NeedEval a') -> Fin $ f' a'
    (NeedEval f', Fin a') -> Fin $ f' a'
    (NeedEval f', NeedEval a') -> NeedEval $ f' a'
    (Error s, _) -> Error s
    (_, Error s) -> Error s
```

## Alternative

The `Alternative` provides a way to combine two `Eval` values.
It is a rank-2 monoid.
It provide alternation, the alternation selects the first `Fin, NeedEval, Error` values in the order listed.

```haskell
instance Alternative Eval where
  empty = Error "empty"
  (<|>) a b = case (a, b) of
    (Fin a', _) -> Fin a'
    (_, Fin b') -> Fin b'
    (NeedEval a', _) -> NeedEval a'
    (_, NeedEval b') -> NeedEval b'
    (Error s1, Error s2) -> Error $ s1 ++ " and " ++ s2
```

## Applicative Combinator

Now we can write a general small step combinator.

```haskell
ev :: (t -> Eval t) -> Eval (t -> a) -> t -> Eval a
ev _ (Fin g) a = Fin g <*> pure a
ev f g a = g <*> f a
```

`ev` combinator is defined in an applicative style, it handles the actual reduction by providing the `f` argument.

First case is the normalization process is finished.
The rest part in the expression should remain untouched.

Second case is the normalization process is not finished.
we can try to do a small step normalization to the rest expression
and then apply the function to the argument.

Finally we can write the small step reduction function subject to the
`Eval` type.

```haskell
smallStep :: ExprE -> Eval ExprE
smallStep expr = case expr of
  AppE f x -> (AppE <$$> f <**> x) 
    <|> (case f of 
            LamE n b -> Fin $ subst n x b 
            _ -> Error $ show f ++ "not a function")
  VarE n -> Error $ "variable " ++ n ++ " not in scope"
  LamE n b -> Error "cannot reduce lambda"
  where 
    (<**>) = ev smallStep
    (<$$>) = (<**>) . pure
```

good job! It look much better than the naive implementation.
Since it saves two lines of code in the body of the main logic!

## Big step and examples

```haskell
bigStep :: ExprE -> Eval ExprE
bigStep x = case smallStep x of
  Fin x' -> bigStep x'
  NeedEval x' -> Error $ "cannot reduce" ++ show x'
  Error s -> Fin x

-- >>> smallStep $ AppE (LamE "x" (VarE "x")) (VarE "y")
-- Fin (VarE "y")
-- >>> smallStep $ AppE (AppE (LamE "x" (LamE "x" (VarE "x"))) (VarE "y")) (VarE "z")
-- Fin (AppE (LamE "x" (VarE "x")) (VarE "z"))
-- >>> smallStep $ AppE (LamE "x" (VarE "x")) (AppE (LamE "x" (VarE "z")) (VarE "k"))
-- Fin (AppE (LamE "x" (VarE "z")) (VarE "k"))
-- >>> bigStep $ AppE (LamE "x" (VarE "x")) (AppE (LamE "x" (VarE "x")) (VarE "k"))
-- Fin (VarE "k")

-- >>> fmap smallStep $ smallStep $ AppE (AppE (LamE "x" (LamE "x" (VarE "x"))) (VarE "y")) (VarE "z")
-- Fin (Fin (VarE "z"))
```

Happy hacking!

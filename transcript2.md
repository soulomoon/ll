# 小步规约（Small Step Normalization）

## 简介

我们的目标是对 Lambda 演算进行小步规约，为此我们将编写小步组合子，使规约过程清晰明了。

Lambda 演算表达式包含三种构造：

- 变量（x）
- 函数（抽象）（λx. e）
- 应用（e1 e2）

```haskell
data ExprE
  = VarE String
  | AppE ExprE ExprE
  | LamE String ExprE
```

我们可以看到，唯一的规约方式是将一个函数应用到一个参数上。而应用是由两个子表达式组成的构造。

我们需要先规约子表达式，然后再规约整个表达式。这称为应用序（与正常序相对）。

## 小步规约

小步规约确保每次只规约一步。
为了表示小步规约的过程，我们可以定义一个数据类型：

```haskell
data Eval a =  NeedEval a | Fin a | Error String 
```

`NeedEval` 构造器表示表达式未变动，
`Fin` 构造器表示小步规约已完成，
`Error` 构造器表示出现错误。

## 初始实现

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

小步规约的初始实现是对表达式进行模式匹配，看起来有些愚蠢。
所以我决定使用 `Functor, Applicative, Alternative` 类型类来使代码更加优雅，以一种组合子风格来实现！

## Functor 和 Applicative

显然，`Eval` 是一个 `Functor`，我们可以定义 `fmap` 函数：

```haskell
instance Functor Eval where
  fmap f (NeedEval a) = NeedEval (f a)
  fmap f (Fin a) = Fin (f a)
  fmap f (Error s) = Error s
```

`Applicative` 实例的定义也很简单。
它是这样定义的，以确保只允许一个 `Fin`。
并且错误会被传播。

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

`Alternative` 提供了一种组合两个 `Eval` 值的方式。
它是一个二阶单子，提供了选择运算，即从左至右选择第一个 `Fin, NeedEval, Error` 值。

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

## Applicative 组合子

现在我们可以写一个通用的小步组合子。

```haskell
ev :: (t -> Eval t) -> Eval (t -> a) -> t -> Eval a
ev _ (Fin g) a = Fin g <*> pure a
ev f g a = g <*> f a
```

`ev` 组合子以 Applicative 风格定义，它通过提供 `f` 参数来处理实际的规约。

第一种情况是规约过程已经完成。
表达式的其余部分应保持不变。

第二种情况是规约过程尚未完成。
我们可以尝试对其余表达式进行小步规约，然后将函数应用到参数上。

最后，我们可以编写遵循 `Eval` 类型的小步规约函数。

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

干得好！它看起来比初始实现好多了。
在主要逻辑的代码体中，它节省了两行代码！
闲的蛋疼。

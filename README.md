# A New Functional Programming Language: A Lightweight, Haskell-inspired Approach

## Introduction

We introduce a new functional programming language that draws inspiration from Haskell while offering a simplified and more accessible experience for developers. This language aims to retain the essential features of Haskell, making it powerful, yet easy to read and write.

## Key Features

Our language incorporates the following key features:

### 1. First-Class Functions and Currying

Functions are treated as first-class citizens, enabling higher-order functions and allowing for partial application through currying.

### 2. Algebraic Data Types and Pattern Matching

Algebraic data types provide a structured approach to data modeling, complemented by pattern matching, including the 'or' pattern for increased flexibility.

### 3. Type Inference

Type inference is a fundamental aspect of our language, which ensures concise code without sacrificing type safety.

### 4. Type Classes

Type classes allow for ad-hoc polymorphism, facilitating the creation of generic algorithms that can operate on different data types.

### 5. Lazy Evaluation

Lazy evaluation enhances performance by evaluating expressions only when necessary, optimizing resource utilization.

### 6. Indentation-based Syntax

We adopt an indentation-based syntax to enhance code readability, promoting a clean and consistent coding style.

## Motivation

While Haskell is a powerful language, its extensive feature set can present challenges for newcomers and compiler implementors alike. Our goal is to create a lightweight language that captures the essence of Haskell, making it more approachable for developers while retaining its expressiveness and functional paradigm.

By providing a simpler version of Haskell, we aim to bridge the gap between powerful functional programming and ease of learning. Our language will enable developers to leverage the benefits of functional programming without overwhelming complexity.

In conclusion, our new functional programming language draws inspiration from Haskell but offers a more streamlined and accessible approach. It empowers developers to embrace functional programming principles with confidence, and we strive to create a community where innovation and simplicity coexist harmoniously.

## Syntax

* declarations

support nested declarations
two way to nest, one is to use indentation, the other is to use braces

```haskell
x :: Int
x = z 
    t where
      y = 2
      x = 3
      where 
        z = x + y
```

* type declarations


## Road map

* untyped lambda calculus
* add types and type checking
* type inference
  * hindley-milner
  * bidirectional type checking
* algebraic data type
  * pattern matching
  * recursive data type
* type classes

# Type Constraints

Notes I want to hit

- Typeclasses
- ConstraintKind

## Examples while I think about this bit.

### Constraining instance head so that type 'a' has a show instance.

```haskell
data Tree a = Leaf a | Node a

instance Show (Tree a) where
    show (Node x) = show x
    show (Leaf x) = ""
```

This example produces this error from ghc (8.8.4):

```
treeshow.hs:5:21: error:
    * No instance for (Show a) arising from a use of `show'
      Possible fix:
        add (Show a) to the context of the instance declaration
    * In the expression: show x
      In an equation for `show': show (Node x) = show x
      In the instance declaration for `Show (Tree a)'
  |
5 |     show (Node x) = show x
  |                     ^^^^^^
```

To fix this we can make a show instance for Tree for types 'a' with an existing show instance by constraining the instance head.

```haskell
data Tree a = Leaf a | Node a

instance (Show a) => Show (Tree a) where
    show (Node x) = show x
    show (Leaf x) = ""
```

### Constraint Kind example from Obelisk

```haskell
{-# LANGUAGE ConstraintKinds #-} --Allows this type constraint alias
type SDLCanDraw m = (SDLRenderer m, MonadReader Config m)

drawGrid :: SDLCanDraw m => Int -> GridTransform -> m ()
```

### Hylogen type level kludging example

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

--For OrMatVec MulR type-level machinery
{-# LANGUAGE TypeFamilies #-}

type family OrMatVec' a where
    OrMatVec' (Expr (FloatMat n m)) = 'True
    OrMatVec' (Expr (FloatVec n)) = 'True

type OrMatVec t = (OrMatVec' t ~ 'True)

type family MulR a b where
    MulR (Expr (FloatVec 2)) (Expr (FloatMat 2 2)) = Expr (FloatVec 2)
    MulR (Expr (FloatVec 3)) (Expr (FloatMat 3 3)) = Expr (FloatVec 3)
    MulR (Expr (FloatVec 4)) (Expr (FloatMat 4 4)) = Expr (FloatVec 4)

    MulR (Expr (FloatMat 2 2)) (Expr (FloatVec 2)) = Expr (FloatVec 2)
    MulR (Expr (FloatMat 3 3)) (Expr (FloatVec 3)) = Expr (FloatVec 3)
    MulR (Expr (FloatMat 4 4)) (Expr (FloatVec 4)) = Expr (FloatVec 4)

    MulR (Expr (FloatMat 2 2)) (Expr (FloatMat 2 2)) = Expr (FloatMat 2 2)
    MulR (Expr (FloatMat 3 3)) (Expr (FloatMat 3 3)) = Expr (FloatMat 3 3)
    MulR (Expr (FloatMat 4 4)) (Expr (FloatMat 4 4)) = Expr (FloatMat 4 4)

--The tilde stuff was because it wasn't able to deduce enough from just this
mul :: (OrMatVec a, OrMatVec b, MulR a b ~ Expr c, b ~ Expr b0, a ~ Expr a0, ToGLSLType a0, ToGLSLType b0, ToGLSLType c) => a -> b -> MulR a b
mul = op2 "*"
```

For reference here is the impl for op2 and the other type machinery kludged around from [Expr.hs in Hylogen](https://github.com/adpextwindong/hylogen/blob/protofeature/matrix/hylogen/src/Hylogen/Expr.hs).

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Binary operator.
-- Most generally typed.
op2 :: forall a b c
       . (ToGLSLType a, ToGLSLType b, ToGLSLType c)
       => String -> Expr a -> Expr b -> Expr c
op2 str a b = Expr t (Tree (Op2, toGLSLType t, str) [toMono a, toMono b])
  where t = tag :: c

class ToGLSLType  ty where
  -- | Gives us dependence from typed singleton tags to untyped tags
  toGLSLType :: ty -> GLSLType
  -- | Singleton tag
  tag :: ty -- TODO: fill in!

-- | Internal type tag
data GLSLType = GLSLFloat
              | GLSLVec2
              | GLSLVec3
              | GLSLVec4
              | GLSLMat2
              | GLSLMat3
              | GLSLMat4
              | GLSLBool
              | GLSLTexture
              deriving (Eq, Ord)
```

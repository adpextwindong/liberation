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

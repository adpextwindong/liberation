--TODO vim macro for lhs code block
# Reading type errors and falling into the language extension hole. Part 1

[Haskell Version 8.8.4](https://downloads.haskell.org/~ghc/8.8.4/docs/html/users_guide/glasgow_exts.html#lexically-scoped-type-variables)

This example comes from recent discussion on the [fp discord](https://discord.gg/K6XHBSh).

A user was implementing a Boolean Any function using foldr with a helper function within a where clause accompanied with a type signature.

```haskell
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr g False
  where
    g :: a -> Bool -> Bool
    g = \ x acc -> f x || acc
```

The resulting error:

```
TypeError.lhs:61:22: error:
    ? Couldn't match expected type ‘a’ with actual type ‘a1’
      ‘a1’ is a rigid type variable bound by
        the type signature for:
          g :: forall a1. a1 -> Bool -> Bool
        at TypeError.lhs:60:5-26
      ‘a’ is a rigid type variable bound by
        the type signature for:
          myAny :: forall a. (a -> Bool) -> [a] -> Bool
        at TypeError.lhs:57:1-35
    ? In the first argument of ‘f’, namely ‘x’
      In the first argument of ‘(||)’, namely ‘f x’
      In the expression: f x || acc
    ? Relevant bindings include
        x :: a1 (bound at TypeError.lhs:61:11)
        g :: a1 -> Bool -> Bool (bound at TypeError.lhs:61:5)
        f :: a -> Bool (bound at TypeError.lhs:58:7)
        myAny :: (a -> Bool) -> [a] -> Bool (bound at TypeError.lhs:58:1)
```

If you're unfamiliar with type errors in Haskell, let alone reading a huge error like this, it can be daunting. 

This is probably one of the first places a beginner encounters language extensions in Haskell in a rougher than preferred way.
Now I try to liberally use type signatures w/ type holes (like I expressed in [another post](Constraints.md)) to help guide my implementation. Especially when I'm dealing with composition that I don't fully grasp (foldl vs foldr is learnt by braile) or intuit that there might be smelly types signatures down the road. Unfortunately in this case it led us to a wall of text.

Lets begin with:

```
    ? Couldn't match expected type ‘a’ with actual type ‘a1’

```

What's the deal? And where is a1 coming from?

```
      ‘a1’ is a rigid type variable bound by
        the type signature for:
          g :: forall a1. a1 -> Bool -> Bool
        at TypeError.lhs:60:5-26
```

Whenever it talks about a rigid type variable, it is based on something you are supplying. An expression within is forcing the compiler to make it "rigid" to that type variable. In this case its the type signature we supplied.

```
g :: a -> Bool -> Bool
```


This can shadow. There are scoping rules.

The solution:

```haskell
{-# LANGUAGE ScopedTypeVariables #-}

myAny :: forall a. (a -> Bool) -> [a] -> Bool
myAny f = foldr g False
  where
    g :: a -> Bool -> Bool
    g = \ x acc -> f x || acc

```

NOTE:
Morrow — Today at 12:12 AM
By the way, ScopedTypeVariables will soon be on by default, in the next major GHC version (9.2).

![Type Checked!]
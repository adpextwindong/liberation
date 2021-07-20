# Type Constraints

This post is a work in progress showcase of some type level programming that I have been enjoying over the past five months since I've started writing Haskell seriously in February. Its geared towards my imperative programmer friends (who are hopefully seeking liberation) and prospective interviewers interested in my engineering values. I'm not a Haskell developer but I play one on TV if you haven't noticed by Woodwright's Shop theme by now.

Now the topic of the day is type constraints, where they show up and why I enjoy them.

## TOC/NOTES
--Notes I want to hit

- Picking a top tier (Language)
- Aside - [(Type) Hole-Driven Haskell](https://www.youtube.com/watch?v=52VsgyexS8Q)
- Typeclasses
- ConstraintKind
- On type safety: the classic Phantom type expr example

## Picking a top tier (...a language rant)

[![Sanford Kelly's timeless words](https://img.youtube.com/vi/sGh4ZU4H5Hk/0.jpg)](https://youtu.be/sGh4ZU4H5Hk?t=96)

```
"I'm tired of it. Fuck working, who wants to work?

I want the money to work for me. I'm tired of working for the money." - Sanford Kelly 2009.
```
Its 2021. We're not programming on PDP-11s, 286s or even Pentium 4s anymore. Most developers are working on problems that can tolerate a garbage collector during runtime. If not, then Rust is a good language. (I'm not here to argue for Rust, I'm a different kind of Bible salesman) Alternatively use FFI into a systems language if possible. Plenty of good software rests on that kind of interop. If all of the above cannot apply then you're either SOL or you know what you're doing TM.

Eitherway, a vast amount of programmers on this earth are subjected to the torture that is JavaScript/(C++ || Java)/Python. (I would include PHP but thats a different part of hell to discuss another time)

Here are my gripes with them in ascending order of pain:

Python is dynamically typed. Writing anything past a page will result in type drift eventually. Python is great for teaching algorithmns and FFI interop frontends but past that it can be a real liability when runtime type issues can eat up a lot of time.

Java is the epitome of OOP cult programming practices and has clouded the minds of generations. Javonic is a legitimate adjective.

C++ being a design by commitee kitchen sink language puts it in a hard spot. There is plenty of good reasons to use C++ if you know what you're doing and want C interop. There are also plenty of footguns in it. The language is so old and vast that finding consistent resources on it is a pitfall for beginner programmers.

Java and C++ both have enough type system features to express things you'd like for average CRUD/Engine work but often force you through awkward language features. Like a square peg through a round hole. Try writing an algebraic datatype for an AST by hand in those languages.

On a serious note my experience with C++/Java taught at college was shocking. Leading up to college I already had programming experience so I got to watch beginners sink or swim. They force feed you this idea of "polymorphism" and "overloading" without being precise about it. Then if those language features aren't enough for you they siege your mind endlessly with object orientated programming drills that abuse class inherientance everywhere.

It wasn't until I tried Haskell that I learned the difference between Parametric Polymorphism and Ad-hoc Polymorphism.

Lastly Javascript...

Javascript is the lowest common denominator language in terms of tooling/implementors/library users and even libraries. The sad truth about the internet is that a dynamically typed language is needed in the browser. But that does not and should not lead people to believe it belongs on the backend. The virtual machine work done for Javascript is impressive. But Javascript imo largely serves employers wanting a RAID.

**R**edundant **A**nd **I**nexpensive **D**eveloper **A**array.

---------
!TODO Memory semantics, Rust lifetimes and the CString library.

You might be thinking "HEY THIS JACKASS JUST HATES DYNAMICALLY TYPED LANGUAGES".

Why yes. I do.

The only dynamically typed languages I appreciate are Lisp and Prolog. And to be fair those languages are aimming for homoiconicity which is worth respecting.

!TODO

## Aside- Hole Driven Haskell

A technique that a lot of introductory Haskell material fails to show is Hole Driven Haskell.

Here's a talk by Matthew Brecknell from 2013 on it:

[![Matthew Brecknell - Hole-Driven Haskell](https://img.youtube.com/vi/52VsgyexS8Q/0.jpg)](https://www.youtube.com/watch?v=52VsgyexS8Q)

The gist of it is that if you don't know what the compiler is expecting just ask by placing an underscore. This can be done at both the type level and value level.

Type level example:

```haskell
foo :: a -> _ -> b
foo a baz = baz a
```

```
    * Found type wildcard `_' standing for `a -> b'
      Where: `a', `b' are rigid type variables bound by
               the inferred type of foo :: a -> (a -> b) -> b
               at typehole.hs:3:1-18
      To use the inferred type, enable PartialTypeSignatures
    * In the type `a -> _ -> b'
      In the type signature: foo :: a -> _ -> b
  |
3 | foo :: a -> _ -> b
  |             ^
``` 

Alternatively if we had this code where we generally knew the signature but now how to use the arguments:

```haskell
foo :: a -> (a->b) -> b
foo x baz = _ x
```

```
    * Found hole: _ :: a -> b
      Where: `a', `b' are rigid type variables bound by
               the type signature for:
                 foo :: forall a b. a -> (a -> b) -> b
               at typehole.hs:3:1-23
    * In the expression: _
      In the expression: _ x
      In an equation for `foo': foo x baz = _ x
    * Relevant bindings include
        baz :: a -> b
          (bound at typehole.hs:4:7)
        x :: a
          (bound at typehole.hs:4:5)
        foo :: a -> (a -> b) -> b
          (bound at typehole.hs:4:1)
      Valid hole fits include
        baz :: a -> b
          (bound at typehole.hs:4:7)
  |
4 | foo x baz = _ x
  |             ^
```

It says a valid hole fits include baz!

Now this isn't to say this approach solves all your problems. Thinking will be necessary to get the typechecker to guide you closer to a valid expression. This gives us a mechanism to refine our types and build up to an appropriate type signature.

Another point to note is that you can define other functions with type holes and partially apply them in expression to "force" rigid type variables. !TODO

Ultimately to add to the previous topic about picking a top tier, type inference in Haskell can do a lot of work for you. Leveraging that to build type safe expressions is key.

## Examples while I think about this bit.

### Constraining instance head so that type 'a' has a show instance.

This example problem popped up in the FP discord today and is probably the first encounter people have with type constraints.

```haskell
data Tree a = Leaf a | Node a

instance Show (Tree a) where
    show (Node x) = show x
    show (Leaf x) = ""
```

When compiled it produces this error from ghc (8.8.4):

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

!TODO

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

!TODO show the type inference error when the constraints aren't included.

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

### Attempts at Natural Deduction AST that started all of this type level monkey buisness

The core idea was to use DataKinds, type families and type operators to mark types as "Cursed" and "Uncursed" or "Burdened" and "Unburdened".

Example from [here](https://github.com/adpextwindong/NDNotes/blob/main/app/Main.hs).


```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

data PTaintedness -- Burden of Proof
    = Burdened
    | Unburdened
    deriving (Eq, Show, Typeable)

type family CombinePTaintedness (a :: PTaintedness) (b :: PTaintedness) :: PTaintedness where
    'Burdened `CombinePTaintedness` _ = 'Burdened
    _ `CombinePTaintedness` 'Burdened = 'Burdened
    _ `CombinePTaintedness` _ = 'Unburdened
```

TODO see if literate haskell can be used to make sure these type check

Now this approach was an attempt to represent Gentzen's Natural Deduction Logic mostly in the type level. I couldn't get ImplyIntro to type check (yet).

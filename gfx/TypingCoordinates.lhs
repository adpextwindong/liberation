- Typelevel Coordinate Systems in Haskell

A hobby project of mine since this summer has been a [Wolf3D style raycasting engine](https://github.com/adpextwindong/obelisk). I am hoping to replicate [Godbolt's presentation](https://www.youtube.com/watch?v=eOCQfxRQ2pY) all "in-engine" as a sort of developer commentary. This was all inspired by [Fabien Sanglard's Game Engine Black Book on Wolf3d](https://fabiensanglard.net/gebbwolf3d.pdf).

I will be extremely honest. I am not a productive gamedev at all, and at best I'm a mediocre engine grease-monkey. Writing nearly everything from scratch for education purposes has felt extremely labor intensive.

Nevertheless it has lead to some productive experiences which my other projects will benefit from. In particular I managed to encode coordinate system transformations at the typelevel.

Why? Because I am terrible at keeping track of which coordinate space something is in. The type system should do my bidding anyways.

-- Preface on the Graphics DSL

Leading up to this I built a small DSL for composing SDL-gfx graphics primitives. This let me build diagrams for how the raycasting engine is actually working.

\begin{code}
{-# LANGUAGE GADTs #-}

data Shape t
data CInt
data M22Affine t

data Graphic a where
    Prim :: Shape Float -> Graphic (Shape Float)
    GroupPrim :: String -> [Graphic (Shape Float)] -> Graphic (Shape Float)
    AffineT :: M22Affine Float -> Graphic a -> Graphic a
    EvaldP :: Shape CInt -> Graphic (Shape CInt)
    EvaldGP :: String -> [Graphic (Shape CInt)] -> Graphic (Shape CInt)
\end{code}


It uses GADTs to maintain a distinction between evaluated screen render ready graphics and to be evaluated graphics. (This handles a SDL2 rendering detail which I dont like exposing here but its good enough for government work as my dad would say.)

The AffineT constructor is where the magic happens as it allows me to compose diagrams of whats going on in world space and apply Affine Transformations to project them into screen space coordinates.

This let me write pure DebugUI graphics that are composable too, independent of any screen details as long as I'd handle that with an AffineT later.

-- The beef

This worked well enough for my tastes until I would take a break from the project for a while and come back confused as to which coordinate space I'm working in.

![Foley's Fundamentals of Interactive Computer Graphics](foleyCover.jpg)

I impulse bought this book recently and it made me realize I could annotate my Affine Transformations with coordinate space types.

Now I don't believe comments should act in replacement of types. If you're reasoning about invariants, in this case which coordinate system you're _working_ in, then these invariants should be leveraged at the typelevel to prevent fuckups.

--TODO https://raw.githubusercontent.com/adpextwindong/obelisk/main/src/Obelisk/Math/Hierarchy.hs
--TODO coerces

Is it perfect? No.
Does it work for all solutions? Yeah.
Does it prevent me from _working_ too hard on wrangling coordinate systems in this project? Absolutely.

--TODO Daan Leijen, Meijer Domain Specific Embedded Compilers
--TODO Read First Class Phantom Types - Cheney Hinze 2003
--TODO Rust Vulkan/OpenGL backend example

A long long time ago in a classroom far far away I wrote a [wrapper around SFML](https://github.com/adpextwindong/OrionCloneLoadingUtility) to provide a uniform interface to loading graphical assets for a game (that was never completed naturally). I wanted to write a simple single threaded level loading screen given a list of asset load targets.

If you pay attention to the ReadMe.txt and [code style in sfmlLoadUtil.h](https://github.com/adpextwindong/OrionCloneLoadingUtility/blob/master/sfmlLoadUtil.h) you can tell I drew A LOT of inspiration (for the lack of a better word) from the C standard library and Java practices. The C++ Template usage was merely fighting against C++ itself, having heard about reflection in Java being used for Runescape bot metaprogramming.


Be deliberate with the expressions you write.


To continue my post on [algebraic datatypes from last time.](ad_example/SeeingTheRoundPegsAndTheSquareHoles.md),

Your expressions should express invariants at the type level that you would will enevitably rely on to write correct code.

My dissatisfaction with many programming languages
\begin{code}

data Expression = Number Int
                | Negate Expression
                | Add Expression Expression
                | Minus Expression Expression
                | Mult Expression Expression
                | Divide Expression Expression

--TODO GADT version



--TODO experience adding Matrix's to Hylogen.
--Ideally I would add optimization pass flags to it, debugging SSA form isn't feasible really.

Ultimately I believe most arguments for dynamically typed languages (other than Prolog) amount to

\end{code}

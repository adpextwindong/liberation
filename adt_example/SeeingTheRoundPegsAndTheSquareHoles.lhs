# Seeing the Round Pegs and the Square holes. Part 1.

This article is a reflection on my programming upbringing being primarily C/C++ and the things I couldn't express until now. If you're in Mr.Bartlo's class this article is aimmed towards you. And Mr.Bartlo if you're reading this, thanks for changing my life.

##Sapir-Whorf Hypothesis related to Programming Languages

The other day I saw [this tweet](https://twitter.com/fchollet/status/1425337663914057731).

```
A programming language has a dual purpose: to enable you to express programs, and to shape the way you think about programming.

Languages program you.
-@fchollet
```

This reminded me about how I used to view programming in my beginner years versus now. If you've heard about the [Church-Turing thesis](https://www.youtube.com/watch?v=GTPwGBIoASM), the notion that Turing machines and the Lambda Calculus is equivalent in computational power, it really changes your thinking about what computation is. Then if you look at Haskell which is a [stone's throw away from Lambda Calculus](https://en.wikipedia.org/wiki/System_F), it makes you wonder what are we really expressing. In constrast I believe the C language (and its derivatives) is more Turing machine like in the way that its forces you to worry about manipulating a machine rather than building up expressions. This is due to the language design limitations and type system.

While you can express the same computation, its not necessarily as fun, clean and safe to do in one or the other.

An example I always carry around in the back of my mind while teaching people programming is [algebraic data types](https://eniki/Algebraic_da.wikipedia.org/wta_type).

## Representing an ADT in C/Haskell/Rust

\begin{code}
data BTree a = Leaf a
             | Node (BTree a) (BTree a)

foo :: BTree Int
foo = Leaf 5

bar :: BTree Int
bar = Node foo (Node
                    (Leaf 7)
                    (Leaf 6))
\end{code}

If we print foo and bar we get:

```haskell

```

Representing this in a concise manner using C requires the "Tagged Union" construct. In other languages and more rigorous contexts we call it a Sum type.

```C
//TODO FILL IN
```

You might think well jeeze thats a simple data constructor, whats the big deal?


What if we want to represent more complicated expressions with a similarly constructed type? Such as:

\begin{code}
data Expression = Number Int
                | Add Expression Expression
                | Minus Expression Expression
                | Mult Expression Expression
                | Divide Expression Expression
\end{code}

Consider the potential mistakes one could make or even a whole engineering team could make. Any manipulation of the C version requires machinery to inspect the tag and avoid touching uninitialized fields in the union. If you were to try and match on a subset of tags (for example all binary operators or unary operator expressions) you would have to build much even more machinery to do this safely. In a language like Haskell pattern matching deals with this cleanly for you. (This doesn't mean you can't write incorrect pattern matches however)




--Representing command line modes

Here's a concrete example from one of my coreutil ports to Haskell:

When using any language you have to be cognizant of what you're trying to express and the limitations of any language features employed to express that.


--TODO Rustlang version

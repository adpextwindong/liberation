# Seeing the Round Pegs and the Square holes. Part 1.

This article is a reflection on my programming upbringing being primarily C/C++ and the things I couldn't express until way later. If you're in Mr.Bartlo's class this article is aimmed towards you. And Mr.Bartlo if you're reading this, thanks for changing my life.

## Sapir-Whorf Hypothesis related to Programming Languages

The other day I saw [this tweet](https://twitter.com/fchollet/status/1425337663914057731).

```
A programming language has a dual purpose: to enable you to express programs, and to shape the way you think about programming.

Languages program you.
-@fchollet
```

This reminded me about how I used to view programming in my beginner years versus now. In the beginning I treated programming largely as the exercise/organization of bytes/bits. It wasn't until much later in college that I divorced myself from this low level outlook after I took [Theory of Computation](http://web.archive.org/web/20210421112403/https://web.engr.oregonstate.edu/~rosulekm/cs321/) and [Programming Language Fundamentals](http://web.archive.org/web/20180602045127/http://web.engr.oregonstate.edu:80/~erwig/cs381/).

If you've heard about the [Church-Turing thesis](https://www.youtube.com/watch?v=GTPwGBIoASM), the notion that Turing machines and the Lambda Calculus are equivalent in computational power, really changes your thinking about what programming languages and computations are. If you look at Haskell, which is a [stone's throw away from Lambda Calculus](https://en.wikipedia.org/wiki/System_F), it makes you wonder what we are really expressing. In constrast if you look at C and the things it forces you to do, I believe its Turing machine-esque.

In particular the way that its burdens you to carefully manipulate state machines with _statements_ rather than building up _meaningful expressions_. Part of this is due to the language design limitations, type system and origin as a language to bootstrap Unix development on the PDP-11. (This isn't to bash C)

While you can do the same computations its not necessarily as fun, clean and safe to do in one or the other all the time. (Don't worry there's stuff in Haskell I struggle to write. Like [this](https://www.gamedev.net/reference/articles/article806.asp) which I have on the backburner for a Haskell port)

Here's an example I always carry around in the back of my mind while teaching people programming.

## Representing a BTree Algebraic Data Type in C vs Haskell

Here we have a simple Binary tree data constructor in Haskell. It has a type parameter a so you can have a binary tree of Int's or Float's for example.

NOTE: [Literate Haskell Version for running in GHCI](SeeingTheRoundPegsAndTheSquareHoles.lhs)

```haskell
data BTree a = Leaf a
             | Node (BTree a) (BTree a)

tx :: BTree Int
tx = (Node
       (Leaf 5)
       (Leaf 6))
```

Representing this in a concise manner using C requires the "Tagged Union" construct. In other languages and more rigorous contexts we call it a [Sum type](https://en.wikipedia.org/wiki/Tagged_union). C Unions combined with a "tag" enum denoting which variant it is can act as a Sum Type. C Structs on the other hand act as [Product types](https://en.wikipedia.org/wiki/Product_type). In our Haskell example Node is one as it has multiple fields to it. Sum and Product types together form [Algebraic Data Types](https://en.wikipedia.org/wiki/Algebraic_data_type).

Here's the C equivilant:

Note: For the sake of being a single statement (to mirror the Haskell expression) this C example uses [Compound Literal Syntax](https://en.wikipedia.org/wiki/C_syntax#Compound_literals) avalible since C99. Compiled with "gcc -std=c99 adt.c -o main_adt.exe"

```C
//BTree Union Tag
enum BTREE_TAG {
    NODE,
    LEAF
};

//BTree DataType
struct BTree {
    enum BTREE_TAG tag;
    union {
        struct Node {
            struct BTree * left;
            struct BTree * right;
        } node;
        int value;
    };
} BTree;

int main(void){
    
    //A BTree assignment equal to a fully evaluated tx in the Haskell example
    struct BTree tx = ((struct BTree) {
        .tag = NODE, .node = ((struct Node){
            &(struct BTree){ .tag = LEAF, .value = 5},
            &(struct BTree){ .tag = LEAF, .value = 6}
        })
    });

    return 0;
}
```

You might think well jeeze thats a simple data constructor, whats the big deal?

Notice that BTREE_TAG and the union within struct BTREE are seperate statements. Idealy these are organized right next to eachother and kept in sync. Secondly anytime you want to inspect a tree you have to manually handle the type tag. The language doesn't hold your hand for this. This bit in particular is what I call juggling at the value level. Ideally a language would handle this for you.

What if we want to represent more complicated expressions? Such as:

```haskell
data Expression = Number Int
                | Negate Expression
                | Add Expression Expression
                | Minus Expression Expression
                | Mult Expression Expression
                | Divide Expression Expression
```

Consider the potential mistakes one could make or even a whole engineering team could make. Any manipulation of the C version requires machinery to inspect the tag and avoid touching uninitialized fields in the union. If you were to try and match on a subset of tags (for example all binary operators or unary operator expressions) you would have to build much even more machinery to do this safely. Then tuck everything behind a few functions that users have to be informed about.

In a language like Haskell pattern matching deals with this cleanly for you. This doesn't mean you can't write incorrect pattern matches however.

For example:

```haskell
isBinOp :: Expression -> Bool
isBinOp (Number _) = False
isBinOp (Negate _) = False
isBinOp _ = True
```

We pattern match on the Expression data constructors and for the unary ones we return False. Then use the underscore wildcard pattern to match on the remaining binary operators.

## Representing command line modes

Back to my original point. What can we start doing with Algebraic Datatypes once we shed this mental burden and tedious labor of oiling delicate C ADT machinery?

Here's a concrete example from one of my [coreutil ports to Haskell](https://github.com/adpextwindong/hcoreutils/blob/master/src/uniq.hs). 

```haskell
data DuplicateHandling = NoAdjacentDups | -- default
                         OneEachDups | -- d
                         AllDups | -- D
                         AllRepeated SeperationHandling | --all-repeated
                         NoDups -- u

data SeperationHandling = Prepend | Separate

data UniqOpts = UniqOpts {                                  -- Corresponding GNU Uniq flags
                    style            :: DuplicateHandling,  -- -d --all-repeated
                    ignoreCase       :: Bool,               -- -i --ignore-case
                    skipFields       :: Int,                -- -f --skip-fields=N
                    checkChars       :: Maybe Int,          -- -w --check-chars=N
                    skipChars        :: Int,                -- -s --skip-chars=N
                    outputCount      :: Bool,               -- -c --count
                    nullTerminated   :: Bool                -- -z --zero-terminated
                }
```

SeperationHandling in the AllRepeated data constructor show cases a missing pain point in Haskell that hand written C would deal with. If I were to add more seperation handling modes to the SeperationHandling type I do it right there. Then for any functions handling SeperationHandling directly the non-exhaustive pattern match warning can indicate if I haven't handled it. Also notice how its all nested inside the UniqOpts datatype.

Other pitfalls in the C version to note is that to have a BTree type be generic in its value type (without preprocessor schenanigans) value must be of type void *. Doing this however diverges it meaning from the Haskell equivlant seen here. Any leaf could contain an arbitrary void * to a heterogenous type. Any interface you build would have to maintain this invariance if you want a BTree of a specific value type.

Going beyond this example, an excerise left to the reader is to look at abstract syntax trees in compilers and embedded domain specific languages. There's a lot you can express and manipulate easily when you aren't caught up in language cobbwebs. Alternatively how would you do this in C++/Java?

## Conclusion

When using any language you have to be cognizant of what you're trying to express and the limitations of any language features employed to express that. Sometimes the language you're using is a round peg in a square hole.

Always take the opportunity to look at new language concepts and stick it in the back of your mind for later on. See enough tricks and maybe you'll be as handy as this guy:

[![The Woodwright's Shop Intro (2017)](https://img.youtube.com/vi/I7IXq-qmt70/0.jpg)](https://www.youtube.com/watch?v=I7IXq-qmt70)

Note: If you're interested in ADT's in C99 theres a recent library [Datatype99](https://github.com/Hirrolot/datatype99).

## BONUS

The original C example was compiled with:

```
gcc -std=c99 adt.c -o main_adt.exe
```

What happens if we return the BTree, as declared, from a function with optimization passes turned on? Try compiling the following code with -O turned on.

```C
#include <stdio.h>

//BTree Union Tag
enum BTREE_TAG {
    NODE,
    LEAF
};

//BTree DataType
struct BTree {
    enum BTREE_TAG tag;
    union {
        struct Node {
            struct BTree * left;
            struct BTree * right;
        } node;
        int value;
    };
} BTree;

struct BTree foo(void){
    struct BTree ret = ((struct BTree) {
        .tag = NODE, .node = ((struct Node){
            &(struct BTree){ .tag = LEAF, .value = 5},
            &(struct BTree){ .tag = LEAF, .value = 6}
        })
    });

    return ret;
}
int main(void){
    struct BTree tx = foo();

    printf("%d\n", tx.node.left->value);

    return 0;
}
```

Can you explain whats happening?

Hint: Why are the left and right nodes pointers? How do we get around this in the compound literal expression and why is that a potential pitfall?

```C
struct Node {
    struct BTree * left;
    struct BTree * right;
} node;
```
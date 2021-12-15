# Algebraic Data Type Design for a C99 flatlander

## Preface

I talk a lot about Algebraic Data Types (ADTs) to my non-Functional Programmer friends. This article is a recollection of the conceptual gap about datatypes that I _slowly_ crossed over the years, and a motivating example.

Its worth noting I crossed this gap very slowly too. I had seen [LYAH](http://learnyouahaskell.com/chapters) in highschool but it didnt click until my Junior year of uni with [Martin Erwig's class](https://web.archive.org/web/20180602045127/http://web.engr.oregonstate.edu:80/~erwig/cs381).

As beginner I thought for a long time that datatypes were just bits laid out in memory. Reading "C Programming: A Modern Approach" by K. N King influenced this view a lot.

That view isn't entirely wrong. __Most__ data does get written out to memory in some fashion. In some cases your application _could_ be sensitive to it, in others its just an operational detail.

As I went on learning about programming and watching conference talks I would hear about algebraic data types and wonder "what the hell could be _algebraic_ about data types?".

At this point in my life I knew three things about C data types (which I will use as the lingua franca)

- Structs
- Unions
- Enums

My mental model so far was that structs join types together into a record. Union's "union" types into an allocation space based on the largest member. And finally, enums let you define a set of values (integers practically speaking) with nice names as a type. And every good kid wishing to be on Santa's list uses enums liberally over raw integers for typesafety.

This view is consistent with "Chapter 16. Structures, Unions, and Enumerations" in King's book.

--TODO tagged union pg400

A keen eye will catch that this enum tag is called a "discriminant" and with some further wikipedia'ing [Discriminated Union](https://en.wikipedia.org/wiki/Discriminated_union) will lead you to [Tagged Union](https://en.wikipedia.org/wiki/Discriminated_union) (which also talks about sum types...).

So ultimately we can really think about unions of structs augmenting enums seperately from structs.

- Structs
- Tagged Unions

If you squint really hard at the [Algebraic Data Type](https://en.wikipedia.org/wiki/Algebraic_data_type) wiki page you'll realize we have

- Product Types
- Sum Types

--TODO why, rich datatypes

## Motivating Example

Lets say we were to reimplement GNU's uniq coreutil and we'd like to model the different line handling behaviors.

As we skim through the [options documentation](https://www.gnu.org/software/coreutils/manual/html_node/uniq-invocation.html#uniq-invocation) we can begin with two modes that catch our eye: The Default and All Repeated

```
By default, uniq prints its input lines, except that it discards all but the first of adjacent repeated lines, so that no output lines are repeated.
```

```
‘--all-repeated[=delimit-method]’

Do not discard the second and subsequent repeated input lines, but discard lines that are not repeated.
```

Reading this far along we could naively use a mode boolean and a spare enum for the different delimit methods.

```c99
#include <stdbool.h>

enum DelimitMethod {
    None = 0,
    ...
};

static bool mode = false;
static enum DelimitMethod method = None;
```


```
The optional delimit-method, supported with the long form option, specifies how to delimit groups of repeated lines, and must be one of the following:

‘none’
Do not delimit groups of repeated lines. This is equivalent to --all-repeated (-D).

‘prepend’
Output a newline before each group of repeated lines. With --zero-terminated (-z), use a zero byte (ASCII NUL) instead of a newline as the delimiter.

‘separate’ ...
```

--TODO
```
enum Handling {
    NoAdjacentDuplicates, // default
    AllRepeated           // --all-repeated
};

```

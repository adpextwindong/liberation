# Algebraic Data Type Design for a C99 flatlander

## Preface

I talk a lot about Algebraic Data Types (ADTs) to my non-Functional Programmer friends. This article is a recollection of the conceptual gap about datatypes that I _slowly_ crossed over the years, a classic example and a motivating example.

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

King's section titled "Using Unions to Build Mixed Data Structures" pg399 describes further usage of unions.

```
Unions have another important application: creating data structures that contain a mixture of data of different types. Let's say that we need an array whose elements are am ixture of `int` and `double` values. Since the elemtns of an array must be of the same type, it seems impossible to create such an array. Using unions though, it's relatively easy. First, we define a union type whose members represent the different kinds of data to be stored in the array:

typedef union {
    int i;
    double d;
} Number;
```

King goes onto describe adding a "Tag Field" to the union to distinguish between the fields.

```c
typedef struct {
    enum { INT_KIND, DOUBLE_KIND } kind;
    union {
        int i;
        double d;
    } u;
} Number;
```

A keen eye will catch that this enum tag is called a "discriminant" and with some further wikipedia'ing [Discriminated Union](https://en.wikipedia.org/wiki/Discriminated_union) will lead you to [Tagged Union](https://en.wikipedia.org/wiki/Discriminated_union) (which also talks about sum types...).

While I love King's book from a pedagogical perspective and how it deals with programming pitfalls...

![Landspeed Record](res/blueflame.png)

it BLOWS by the importance of discriminated unions like they're setting the land speed record.

† GAP: At this point the relationship between enums and unions is starting to emerge. In my view enums can be seen as a special case of tagged unions for integer _values_. This way we can look at enums and tagged unions in a unified manner and stare at the type composition mechanisms again:

- Structs
- Tagged Unions

If you squint really hard at the [Algebraic Data Type](https://en.wikipedia.org/wiki/Algebraic_data_type) wiki page you'll realize these correspond to:

- Product Types
- Sum Types

"Sums" and "Products" is where the "Algebraic" bit comes from.

### So wheres the beef?

The importance of Algebraic data types comes in two parts.

- Your domain models can be decomposed using these two actions
- Tagged unions and structs can then bundle necessary data to inform desired data transformations

This leads to semantically rich datatypes.

Now the former isn't immediately obvious. Requires practice and shines primarily in languages where ADTs and pattern matching is king.

† GAP: This is why "pattern matching" and "case analysis" in functional programming languages is important. Languages without such features can be tedious to work with or too often rely on dynamic dispatch.

Being able to act on sum type inhabitants differently is also critical to using ADTs effectively. If we move the last Tagged Union example away from simple numeric types to more complex domains then things will become more noticable.

## Expression Example

A classic example for ADTs within the FP world is the arithemtic expression example.

```
((4 + (0 - 1)) * 2)
```

In this example we have

- Integer Literals
- Addition
- Subtraction
- Multiplication

Each have their own meanings and interpretations but they're all arithemtic expressions so lets model that.

```c
enum ExprTag {
    LitE,
    AddE,
    SubE,
    MulE
};

typedef struct Expr {
    enum ExprTag Tag;
    union {
        int value;
        struct binExpr {
            struct Expr * left;
            struct Expr * right;
        } b;
    } u;
} Expr;
```

Evaluating any Expr then involves inspecting the tag and recursively evaluating and applying the respective operator.

```c
int eval(Expr * e){
    switch(e->Tag){
        case LitE:
            return e->u.value;
        case AddE:
            return (eval (e->u.b.left)) + (eval (e->u.b.right));
        case SubE:
            return (eval (e->u.b.left)) - (eval (e->u.b.right));
        case MulE:
            return (eval (e->u.b.left)) * (eval (e->u.b.right));
    }
}
```

An example Expr and its evaluated value

```c
#include <stdio.h>

int main(void){

    Expr lit5 = {
        LitE , { 5 }
    };
    Expr lit6 = {
        LitE , { 6 }
    };

    Expr val = {
        AddE, .u.b = { &lit5 , &lit6  }
    };

    printf("Eval val = %d\n", eval(&val));

    return 0;
}
```

Gives us 11 as we expect.

This is already getting hard to read. An even more complex expression would be hard to write or would require parsing. How many of these source file characters are working for us instead of appeasing the parser? What does this look like in an FP language like Haskell?

```haskell
data Expr = LitE Int
          | AddE Expr Expr
          | SubE Expr Expr
          | MulE Expr Expr
```

The vertical bar denotes another inhabitant of a sum type. The first symbol after the bar being the constructor, similar to the tag. The types following the constructor combine to form a product type.

To add onto what I said about pattern matching ergonomics, eval would look like this in Haskell.

```haskell
eval :: Expr -> Int
eval (LitE v) = v
eval (AddE l r) = eval l + eval r
eval (SubE l r) = eval l - eval r
eval (MulE l r) = eval l * eval r
```

With the main looking as such.

```
main :: IO ()
main = print $ eval (AddE (LitE 5) (LitE 7))
```

## Coreutil Example

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

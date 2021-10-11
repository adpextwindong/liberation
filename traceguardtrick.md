# Trace Pattern Guard Trick

There was a comment on r/haskell recently that showed a nice way of using Debug.trace. I ended up using this in my Raycaster project the next week.

```haskell
fib :: Integer -> Integer
fib n | trace ("fib " ++ show n) False = undefined
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

The example u/Noughtmare used was a simple recursive fibonacci function. The first line being the trace trick. Trace's type signature is:

```haskell
trace :: String -> a -> a
```

It takes a string to print and an argument to return. Combined with a pattern guard we can print the string and return False to resume pattern matching on other cases. This is why we supply undefined as the RHS for this.

## Obelisk example

In my engine I had a simple mistake oh mismatched arguments. In epsilonBump the arguments were swapped and I easily caught this by inspecting with the trace pattern guard trick.

```haskell
sampleWalkPaths :: WorldTiles
sampleWalkPaths world playerpos ray (step: path) = if accessMapV world checkInds == FW
                                                   then Just cPair
                                                   else sampleWalkRayPaths world player pos ray path
    where
        cPair@(checkSpot, checkInds) = epsilonBump ray step
```

NOTE FROM DOCUMENTATION: Debug.Trace should only be used for debugging, or for monitoring executiong. The function is not referentially transparent: its type indicates that it is a pure function but it has the side effect of outputting the trace message.

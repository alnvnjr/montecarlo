# Basic Haskell Implementation of Monte Carlo Options Pricing

### status: ROUGH

## Implied Volatility
The Monte Carlo pricing method is dependent on an accurate implied volatility (IV) determination. None is more accurate than Peter Jaeckel's Let's Be Rational implementation. A haskell wrapper for the open source C code is available here or under my other account feio-ai.

## Random Number Generation
Several different methods for generating random numbers were tested. Ultimately the method below was used:
```Haskell
main = do
  vs <- withSystemRandom . asGenST $ \gen -> uniformVector gen 20
  print (vs :: Vector Int)
``` 

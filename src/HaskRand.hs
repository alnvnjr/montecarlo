{-# LANGUAGE ScopedTypeVariables #-}
-- Method of Random Number Generation
module HaskRand where

import System.Random.MWC
import Data.Vector.Unboxed
import Control.Monad.ST

main = do
  vs <- withSystemRandom . asGenST $ \gen -> uniformVector gen 20
  print (vs :: Vector Int)
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Lib

import System.Random.MWC
import Data.Vector.Unboxed
import Control.Monad.ST

main :: IO ()
main = do
  vs <- withSystemRandom $ \(gen::GenST s) -> uniformVector gen 20 :: ST s (Vector Int)
  putStrLn (countVec vs)
  print vs

countVec :: Vector Int -> [Char]
countVec input = show (Data.Vector.Unboxed.length input)
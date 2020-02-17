{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Lib

import System.Random.MWC
import Data.Vector.Unboxed as DV
import Control.Monad.ST



main :: IO ()
main = do
    iter <- getLine
    let ind = convToInt iter
    let oldVec = createVecIO ind
    the_nums <- sequence oldVec -- [IO Float] -> IO [Float]
    putStrLn (show the_nums)


createVecIO :: Int -> [IO Float]
createVecIO ind 
    | ind <= 0      = []
    | otherwise     = do
        let va = withSystemRandom $ \(gen::GenST s) -> uniform gen :: ST s (Float)
        va : createVecIO (ind-1)

convToInt :: [Char] -> Int
convToInt input = read input :: Int

---------------------------------------------------------------------------------------
-- Vec Utils
countVec :: Vector Int -> [Char]
countVec input = show (DV.length input)

normalizeVec :: Vector Int -> Vector Double
normalizeVec input = DV.map f convec
    where 
        f x = x / tots
        tots = DV.sum convec
        convec = convertVec input

convertVec :: Vector Int -> Vector Double
convertVec = DV.map f
    where
        f x = fromIntegral $ abs x


------------------------------------------------------------------------------------------------
-- Monte given a time step
montePrice :: Double -> Double -> Double -> Double -> Double -> [Double]
montePrice so sig r eps dt = Prelude.map f times
    where
        times = timeStep dt
        f x = so * (exp (y + z))
          where
              y = (r - ((sig**2) / 2)) * x
              z = (sig * sqrt x) * eps
timeStep :: Double -> [Double]
timeStep dt 
    | dt >= 1     = []
    | otherwise   = [0, dt .. 1]

---------------------------------------------------------------------------------------------------

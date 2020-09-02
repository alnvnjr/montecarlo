{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Lib

import System.Random.MWC
import Data.Vector.Unboxed as DV hiding (replicateM, (++))
import Control.Monad.ST
import Control.Monad

import Functions.Exp.LBR

-- Get args from user
main :: IO ()
main = do
    putStrLn "How Many Iterations? (Total Runs)"
    iter <- getLine
    let iter' = read iter :: Float
    putStrLn "Time Step? (0.01, 0.02, 0.03)"
    dt <- getLine
    let dt' = read dt :: Float
    putStrLn "Stock Price:"
    so <- getLine
    let so' = read so :: Float
    putStrLn "Implied Volatility"
    sig <- getLine
    let sig' = read sig :: Float
    putStrLn "Rate"
    r <- getLine
    let r' = read r :: Float

    writeFile "results.txt" ("")

    let num_steps = numIter dt
    let i = convToInt iter
    let loop = do
            let oldVec = createVecIO num_steps
            the_nums <- sequence oldVec
            appendFile "results.txt" ((show $ monteBundled so' sig' r' dt' the_nums) ++ "\n")
    replicateM i loop
    putStrLn "End Results"
  
numIter :: [Char] -> Float
numIter step = 1 / x
    where
        x = read step :: Float

createVecIO :: Float -> [IO Float]
createVecIO ind 
    | ind <= 0      = []
    | otherwise     = do
        let va = withSystemRandom $ \(gen::GenST s) -> uniform gen :: ST s (Float)
        va : createVecIO (ind-1)

------------------------------------------------------------------------------------------------
-- Monte Carlo Implementation
monteBundled :: Float -> Float -> Float -> Float -> [Float] -> [(Float, Float)]
monteBundled so sig r dt eps = Prelude.zip times f
    where    
        f = montePrice so sig r (timeEpsZip eps dt)
        times = timeStep dt

montePrice :: Float -> Float -> Float -> [(Float, Float)] -> [Float]
montePrice _ _ _ [] = []
montePrice so sig r (x:xs) = so * (exp (y + z)) : montePrice so sig r xs
    where
        y = (r - ((sig**2) / 2)) * t
        z = (sig * sqrt t) * eps
        t = fst x
        eps = snd x
        
timeEpsZip :: [Float] -> Float -> [(Float,Float)]
timeEpsZip eps dt = Prelude.zip times eps
    where
        times = timeStep dt

timeStep :: Float -> [Float]
timeStep dt 
    | dt >= 1     = []
    | otherwise   = [0, dt .. 1]

---------------------------------------------------------------------------------------
-- | Vec Utils (Not all used / Referential)

convToInt :: [Char] -> Int
convToInt input = read input :: Int

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


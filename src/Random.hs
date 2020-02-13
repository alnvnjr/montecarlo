{-# LANGUAGE ForeignFunctionInterface #-}

module Random(
    randNumList
) where

import Foreign.Ptr
import Foreign.C

foreign import ccall "math.h rand"
    c_rand :: Int -> Int

randNumList :: Int -> Int -> [Float]
randNumList x n
    | n<= 0     = []
    | otherwise = f :randNumList y (n-1)
        where 
            f = fromIntegral (y `mod` 10) :: Float
            y = c_rand x

norm_randNumList :: [Float] -> [Float]
norm_randNumList input = map (f) input
    where
        f x = x /  (sum input)


sumList :: [Int] -> Float
sumList (x:xs) = fromIntegral $ sum (x:xs)
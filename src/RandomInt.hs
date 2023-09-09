module RandomInt (randomInts, toRange) where

import System.Random (Random, randomR, mkStdGen)

randomsR :: Random a => Int -> (a,a) -> [a]
randomsR seed range = go (mkStdGen seed)
  where
    go gen = let (a,next) = randomR range gen
             in next `seq` a : go next

randomInts :: Int -> [Int]
randomInts seed = randomsR seed (minBound :: Int, maxBound :: Int)

toRange :: Int -> (Int, Int) -> Int
toRange x (a,b)
  | a > b = error "RandomInt.toRange"
  | a == b = a
  | otherwise = a + mod x (b-a+1)

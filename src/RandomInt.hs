module RandomInt (randomInts) where

import System.Random (Random, randomR, mkStdGen)

randomsR :: Random a => Int -> (a,a) -> [a]
randomsR seed range = go (mkStdGen seed)
  where
    go gen = let (a,next) = randomR range gen
             in next `seq` a : go next

randomInts :: Int -> [Int]
randomInts seed = randomsR seed (minBound :: Int, maxBound :: Int)

module Util where

consIf :: (a -> Bool) -> a -> [a] -> [a]
consIf p x xs | p x = x : xs
              | otherwise = xs

singleton :: [a] -> a
singleton [x] = x

-- sieve f g zs = (xs, ys)
-- reverse xs = filter f zs
-- reverse ys = filter g (filter (not . f) zs)

sieve :: (a -> Bool) -> (a -> Bool) -> [a] -> ([a], [a])
sieve f g zs = go zs [] []
  where
    go (a:as) xs ys
      | f a = go as (a:xs) ys
      | g a = go as xs (a:ys)
      | otherwise = go as xs ys
    go [] xs ys = (xs, ys)

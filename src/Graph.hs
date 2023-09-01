module Graph where

import Global
import Data.List (nub)
import Data.Maybe

data Edge v = Edge v v deriving Show

type Graph v = [Edge v]

outgoing :: Eq v => Graph v -> [v] -> [v]
outgoing es vs = mapMaybe match es
  where
    match (Edge a b) | elem a vs = Just b
                     | otherwise = Nothing

-- Naive and for DAG only.
future :: Eq v => Graph v -> [v] -> [v]
future es vs = nub $
  concat $
  takeWhile (not . null) $
  tail $
  iterate (outgoing es) vs

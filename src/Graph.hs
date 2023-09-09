module Graph where

import Global
import Data.List (nub, intersect)
import Data.Maybe

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M

data Edge v = Edge v v deriving Show

source :: Edge v -> v
source (Edge a _) = a

hasTarget :: Eq v => v -> Edge v -> Bool
hasTarget y (Edge _ b) = y == b

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

predecessors :: Eq v => Graph v -> v -> [(v,[v])]
predecessors es exit = map go vs
  where
    vs = nub (map source es) ++ [exit]
    go v = let preds = nub $ map source $ filter (hasTarget v) es
           in (v,preds)

-- assuming tsort
dominators :: Eq v => Graph v -> v -> v
dominators es exit = head $ intersect tsort uips
  where
    (start,[]):preds = predecessors es exit
    go acc (b,as) = let dom:doms = map (fromJust . flip lookup acc) as
                        curr = b : foldl intersect dom doms
                    in (b,curr):acc
    tsort = reverse $ nub (map source es)
    (ex,uips):_ = foldl go [(start,[start])] preds

assertTopologicalSort :: Eq v => Graph v -> v -> Bool
assertTopologicalSort es exit = all good es
  where
    good (Edge a b) = elem b $ tail $ dropWhile (/= a) vs
    vs = nub (map source es) ++ [exit]

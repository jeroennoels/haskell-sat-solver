module ConflictAnalysis (
  Destination(..), satisfiable,
  analyzeConflict) where

import Global
import Util (sameIgnoringOrder)
import Assignment (Assignment)
import UnitPropagation (Implied (Implied))
import Data.List (partition)
import Data.IntSet (IntSet)
import qualified Data.IntSet as S
import Data.IntMap (IntMap)
import qualified Data.IntMap as M


data Destination = Sat Assignment | Conflict Lit [Clause] [[Implied]]
  deriving Show

satisfiable :: Destination -> Bool
satisfiable (Sat _) = True
satisfiable _ = False

analyzeConflict :: Destination -> String
analyzeConflict (Conflict lastDecision conflicts implieds)
  | optimized = show index
  | verifyIndex index = "verified " ++ show graph
  | otherwise = error "analyzeConflict"
  where
    flatten = concat (reverse implieds)
    set = buildCurrentSet lastDecision flatten
    index = buildIndex set flatten
    conflict = antecedentOfConflict set $ head conflicts -- only use the first
    graph = buildImplicationGraph lastDecision index conflict

antecedentOfConflict :: IntSet -> Clause -> [Lit]
antecedentOfConflict set (Clause xs) = filter keep (map negation xs)
  where
    keep (Lit i) = S.member i set

-- This set is used to quickly distinguish the current set of implied
-- literals from previously assigned literals. The former become the
-- vertices of the implication graph. The latter will contribute to
-- the learned clause.
-- We explicitly add the last decision literal to this set.
buildCurrentSet :: Lit -> [Implied] -> IntSet
buildCurrentSet (Lit j) implieds = S.fromList $ j : map extract implieds
  where
    extract (Implied _ (Lit i)) = i


antecedent :: Implied -> [Lit]
antecedent (Implied (Clause xs) x) = map negation (filter (/= x) xs)

consequent :: Implied -> Lit
consequent (Implied _ x) = x

-- Split the antecedent into young and old literals.
-- The last decision literal is consider part of the young generation.
-- Keep a reference to the @Implied@ for assertions and debugging.
data Split = Split [Lit] [Lit] Implied deriving Show

pair :: IntSet -> Implied -> (Int, Split)
pair set implied = (key, Split young old implied)
  where
    Lit key = consequent implied
    isYoung (Lit i) = S.member i set
    (young, old) = partition isYoung (antecedent implied)

-- index by consequent
buildIndex :: IntSet -> [Implied] -> IntMap Split
buildIndex set = M.fromList . map (pair set)


data Edge = Edge Lit Lit deriving Show

edgeTo :: Lit -> Lit -> Edge
edgeTo = flip Edge

requires :: Lit -> IntMap Split -> Lit -> ([Lit], [Edge])
requires lastDecision index implied@(Lit i)
  | implied == lastDecision = ([], [])
  | otherwise = (young, map (edgeTo implied) young)
  where
    Split young _ _ = index M.! i

requires' :: Lit -> IntMap Split -> [Lit] -> ([Lit], [Edge])
requires' lastDecision index ys = (concat as, concat bs)
  where
    (as, bs) = unzip $ map (requires lastDecision index) ys

step :: Lit -> IntMap Split -> ([Lit], [Edge]) -> ([Lit], [Edge])
step lastDecision index (ys, acc) = (xs, edges ++ acc)
  where
    (xs, edges) = requires' lastDecision index ys

buildImplicationGraph :: Lit -> IntMap Split -> [Lit] -> [Edge]
buildImplicationGraph lastDecision index xs = snd first
  where
    steps = iterate (step lastDecision index) (xs, [])
    done = null . fst
    first = head (filter done steps)


--------------------------
-- tests and assertions --
--------------------------

assertSplit :: Lit -> Split -> Bool
assertSplit y (Split young old (Implied (Clause xs) x)) = good
  where
    recombined = y : map negation (young ++ old)
    good = x == y && recombined `sameIgnoringOrder` xs

verifyIndex :: IntMap Split -> Bool
verifyIndex index = all good (M.toList index)
  where
    good = uncurry (assertSplit . Lit)

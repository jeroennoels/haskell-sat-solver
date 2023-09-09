module ConflictAnalysis (
  Conflict(..), Learn(..),
  analyzeConflict) where

import Global
import Graph
import Util (sameIgnoringOrder, unique)
import Assignment (Assignment)
import UnitPropagation (Implied (Implied))
import Data.List (partition, nub)
import Data.IntSet (IntSet)
import qualified Data.IntSet as S
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M


data Conflict = Conflict Lit [Clause] [[Implied]]
  deriving Show

-- Separate the UIP from the rest.
data Learn = Learn Lit [Lit] deriving Show

analyzeConflict :: Conflict -> Learn
analyzeConflict (Conflict lastDecision conflicts implieds)
  | optimized = learn
  | verifyIndex index && assertSize && assertTopologicalSort graph Kappa = learn
  | otherwise = error "analyzeConflict"
  where
    flatten = concat (reverse implieds)
    set = buildCurrentSet lastDecision flatten
    index = buildIndex set flatten
    conflict = head conflicts  -- only use the first one, for now
    Split young old _ = antecedentOfConflict set conflict
    graph = buildImplicationGraph lastDecision index young
    learn = learnedClause lastDecision index graph old
    assertSize = let (a,b,c) = (length flatten, M.size index, S.size set)
                 in a == b && c == a+1


antecedentOfConflict :: IntSet -> Clause -> Split
antecedentOfConflict set (Clause xs) = Split young old undefined
  where
    isYoung (Lit i) = S.member i set
    (young, old) = partition isYoung (map negation xs)

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

data V = Vertex Lit | Kappa deriving (Eq, Show)

edgeTo :: Lit -> Lit -> Edge V
edgeTo b a = Edge (Vertex a) (Vertex b)

edgeToKappa :: Lit -> Edge V
edgeToKappa a = Edge (Vertex a) Kappa

requires :: Lit -> IntMap Split -> Lit -> ([Lit], [Edge V])
requires lastDecision index implied@(Lit i)
  | implied == lastDecision = ([], [])
  | otherwise = (young, map (edgeTo implied) young)
  where
    Split young _ _ = index M.! i

requires' :: Lit -> IntMap Split -> [Lit] -> ([Lit], [Edge V])
requires' lastDecision index ys = (concat as, concat bs)
  where
    (as, bs) = unzip $ map (requires lastDecision index) ys

step :: Lit -> IntMap Split -> ([Lit], [Edge V]) -> ([Lit], [Edge V])
step lastDecision index (ys, acc) = (xs, edges ++ acc)
  where
    (xs, edges) = requires' lastDecision index ys

buildImplicationGraph :: Lit -> IntMap Split -> [Lit] -> [Edge V]
buildImplicationGraph lastDecision index xs = snd first
  where
    start = (xs, map edgeToKappa xs)
    steps = iterate (step lastDecision index) start
    done = null . fst
    first = head (filter done steps)

oldGeneration :: IntMap Split -> V -> [Lit]
oldGeneration _ Kappa = []
oldGeneration index (Vertex (Lit i)) = old
  where
    Split _ old _ = index M.! i

learnedClause :: Lit -> IntMap Split -> [Edge V] -> [Lit]-> Learn
learnedClause lastDecision index graph oldFromConflict
  | optimized = learn
  | unique vertices = learn
  | otherwise = error "learnedClause"
  where
    Vertex uip = dominators graph Kappa --lastDecision  -- todo
    vertices = future graph [Vertex uip]
    sufficientForConflict = oldFromConflict ++
                            concatMap (oldGeneration index) vertices
    learn = Learn (negation uip) $ map negation $ nub sufficientForConflict



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

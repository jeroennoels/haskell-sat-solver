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
  | verifyIndex index = "verified"
  | otherwise = error "analyzeConflict"
  where
    flatten = concat (reverse implieds)
    index = buildIndex lastDecision (buildCurrentSet flatten) flatten

-- This set is used to quickly distinguish the current set of implied
-- literals from previously assigned literals. The former become the
-- vertices of the implication graph. The latter will contribute to
-- the learned clause.
buildCurrentSet :: [Implied] -> IntSet
buildCurrentSet = S.fromList . map extract
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

pair :: Lit -> IntSet -> Implied -> (Int, Split)
pair lastDecision set implied = (key, Split young old implied)
  where
    Lit key = consequent implied
    isYoung x | x == lastDecision = True
    isYoung (Lit i) = S.member i set
    (young, old) = partition isYoung (antecedent implied)

-- index by consequent
buildIndex :: Lit -> IntSet -> [Implied] -> IntMap Split
buildIndex lastDecision set = M.fromList . map (pair lastDecision set)



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

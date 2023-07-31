module UnitPropagation (test_unitPropagation) where

import Data.List (nub)
import Util (sieve)
import Global
import Database
import Assignment

data Eval = Satisfied | Unit Clause Lit | Conflict Clause | Unresolved
  deriving Show

isSatisfied :: Eval -> Bool
isSatisfied Satisfied = True
isSatisfied _ = False

isUnresolved :: Eval -> Bool
isUnresolved Unresolved = True
isUnresolved _ = False

isConflict :: Eval -> Bool
isConflict (Conflict _) = True
isConflict _ = False

isUnit :: Eval -> Bool
isUnit (Unit _ _) = True
isUnit _ = False

fromUnit :: Eval -> Lit
fromUnit (Unit _ x) = x

evaluate :: Assignment -> Clause -> Eval
evaluate a clause
  | any (isAssigned a) xs = Satisfied
  | null reduced = Conflict clause
  | null (tail reduced) = Unit clause (head xs)
  | otherwise = Unresolved
  where
    xs = literals clause
    falsified = isAssigned a . negation
    reduced = filter (not . falsified) xs

-- assuming the literals @xs@ are assigned in @a@
evaluations :: Database -> Assignment -> [Lit] -> [Eval]
evaluations db a xs = map (evaluate a) relevant
  where
    -- we only care about clauses in which one of the given
    -- literals occurs falsified
    relevant = concatMap (clausesWith db) (map negation xs)

data Analysis = Units [Eval]
              | Conflicts [Clause]
              | Inconsistencies [Var]
              deriving Show

-- quadratic
inconsistent :: [Lit] -> [Var]
inconsistent xs = nub $ map variable $ filter bad xs
  where
    bad x = elem (negation x) xs

analyze :: [Eval] -> Analysis
analyze evals = Units []
  where
   (units, conflicts) = sieve isUnit isConflict evals
   xs = map fromUnit units



test_unitPropagation :: Bool
test_unitPropagation = test_evaluate && test_inconsistent

test_evaluate :: Bool
test_evaluate =
     isSatisfied (eval [6,1,9])
  && isUnresolved (eval [6,-3,5,7])
  && isConflict (eval [-4,5,-2])
  && isUnit (eval [-2,5,9])
  where
    eval = evaluate a . Clause . map Lit
    a = extend emptyAssignment $ map Lit [1,2,3,4,-5]

test_inconsistent :: Bool
test_inconsistent =
     check [-5,-4,1,2,4,6] [4]
  && check [1,2,3] []
  && check [-2,-1,1,2,3] [2,1]
  where
    check input output = inconsistent (map Lit input) == map Var output

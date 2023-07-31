module UnitPropagation (test_unitPropagation) where

import Data.List (nub, splitAt)
import Util (sieve)
import Global
import Database
import Assignment

-- Eq instance for testing purposes only
data Eval = Satisfied | Unit Clause Lit | Conflict Clause | Unresolved
  deriving (Show, Eq)

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

-- Assuming @Unit@ argument.
fromUnit :: Eval -> Lit
fromUnit (Unit _ x) = x

-- Assuming @Unit@ ore @Conflict@ argument.
antecedent :: Eval -> Clause
antecedent (Unit clause _) = clause
antecedent (Conflict clause) = clause

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

-- Assuming the literals @xs@ are assigned in @a@.
evaluations :: Database -> Assignment -> [Lit] -> [Eval]
evaluations db a xs = map (evaluate a) relevant
  where
    -- We only care about clauses in which one of the given
    -- literals occurs falsified.
    relevant = concatMap (clausesWith db) (map negation xs)


-- We distinguish two kinds of conflict: direct and mutual.
-- A direct conflict is simply a clause that is entirely falsified.
-- A mutual conflict arises when two clauses become simultaneously
-- unit with opposite conclusions.

-- Eq instance for testing purposes only
data Analysis = Units [Eval]
              | Conflicts [Clause]
              deriving (Show, Eq)

-- quadratic
contradictions :: [Lit] -> [Var]
contradictions xs = nub $ map variable $ filter bad xs
  where
    bad x = elem (negation x) xs

-- Assuming @Unit@ argument.
sameVariable :: Var -> Eval -> Bool
sameVariable var (Unit _ x) = variable x == var

-- Assuming the argument list is filtered for @Unit@.
-- Returns one of the clauses that contribute to a mutual conflict.
mutualConflict :: [Eval] -> Var -> Clause
mutualConflict units var = antecedent u
  where  -- There must be at least two units for this variable:
    (u:_:_) = filter (sameVariable var) units

-- Assuming the argument list is filtered for @Unit@.
mutualConflicts :: [Eval] -> [Clause]
mutualConflicts units = map (mutualConflict units) vars
  where
    xs = map fromUnit units
    vars = contradictions xs

analyze :: [Eval] -> Analysis
analyze evals
  | null conflicts && null mutuals = Units units
  | otherwise = Conflicts $ map antecedent conflicts ++ mutuals
  where
    (units, conflicts) = sieve isUnit isConflict evals
    mutuals = mutualConflicts units


test_unitPropagation :: Bool
test_unitPropagation = test_evaluate
  && test_contradictions
  && test_mutualConflicts
  && test_analyze

test_evaluate :: Bool
test_evaluate =
     isSatisfied (eval [6,1,9])
  && isUnresolved (eval [6,-3,5,7])
  && isConflict (eval [-4,5,-2])
  && isUnit (eval [-2,5,9])
  where
    eval = evaluate a . Clause . map Lit
    a = extend emptyAssignment $ map Lit [1,2,3,4,-5]

test_contradictions :: Bool
test_contradictions =
     check [-5,-4,1,2,4,6] [4]
  && check [1,2,3] []
  && check [-2,-1,1,2,2,2,3] [2,1]
  where
    check input output = contradictions (map Lit input) == map Var output

-- Fake but good enough to assert that the correct clauses are selected.
testClauses :: [Clause]
testClauses = makeClauses $ map (:[]) [1..]

test_mutualConflicts :: Bool
test_mutualConflicts = mutualConflicts units == take 2 testClauses
  where
    xs = map Lit [8,5,-8,4,-5,8,2]  -- contradictions for 8 and for 5
    units = zipWith Unit testClauses xs

test_analyze :: Bool
test_analyze =
  -- order depends on implementation details, but good enough for now
  reverse output == take 5 (tail evals)
  && conflicts == [b,c]
  where
    (as, (b:c:_)) = splitAt 5 testClauses
    xs = map Lit [1..5]
    x = Lit (-3)
    evals = [Satisfied] ++ zipWith Unit as xs ++ [Unresolved]
    more = evals ++ [Conflict b, Unit c x]  -- direct and mutual
    Units output = analyze evals
    Conflicts conflicts = analyze more

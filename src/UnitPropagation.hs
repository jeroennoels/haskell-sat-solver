module UnitPropagation (
  Propagated(Propagated), propagate,
  test_unitPropagation) where

import Data.List (nub, splitAt)
import Util (sieve, singleton)
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

-- Assuming @Unit@ or @Conflict@ argument.
antecedent :: Eval -> Clause
antecedent (Unit clause _) = clause
antecedent (Conflict clause) = clause

evaluate :: Assignment -> Clause -> Eval
evaluate a clause
  | any (isAssigned a) xs = Satisfied
  | not ok = error "evaluate"
  | null reduced = Conflict clause
  | null (tail reduced) = Unit clause (singleton reduced)
  | otherwise = Unresolved
  where
    xs = literals clause
    falsified = isAssigned a . negation
    reduced = filter (not . falsified) xs
    ok = optimized || null reduced == all falsified xs

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

data Mutual = Mutual Eval Eval deriving (Show, Eq)

-- Eq instance for testing purposes only
data Analysis = Units [Eval]
              | Conflicts [Clause] [Mutual]
              deriving (Show, Eq)

-- quadratic
contradictions :: [Lit] -> [Var]
contradictions xs = nub $ map variable $ filter bad xs
  where
    bad x = elem (negation x) xs

-- Assuming @Unit@ argument.
positive :: Var -> Eval -> Bool
positive (Var i) (Unit _ (Lit j)) = i == j

-- Assuming @Unit@ argument.
negative :: Var -> Eval -> Bool
negative (Var i) (Unit _ (Lit j)) = i == negate j

-- Assuming the argument list is filtered for @Unit@.
-- Returns one of the clauses that contribute to a mutual conflict.
mutualConflict :: [Eval] -> Var -> Mutual
mutualConflict units var = Mutual a b
  where  -- there must be at least one positive and one negative
    (a:_, b:_) = sieve (positive var) (negative var) units

-- Assuming the argument list is filtered for @Unit@.
mutualConflicts :: [Eval] -> [Mutual]
mutualConflicts units = map (mutualConflict units) vars
  where
    xs = map fromUnit units
    vars = contradictions xs

analyze :: [Eval] -> Analysis
analyze evals
  | null conflicts && null mutuals = Units units
  | otherwise = Conflicts (map antecedent conflicts) mutuals
  where
    (units, conflicts) = sieve isUnit isConflict evals
    mutuals = mutualConflicts units

-- For verification only.
fullAnalysis :: Database -> Assignment -> Analysis
fullAnalysis db a = analyze $ map (evaluate a) (allClauses db)

-- Assuming the argument list is filtered for @Unit@.
uniqueLiterals :: [Eval] -> [Lit]
uniqueLiterals = nub . map fromUnit

-- Nested list of @Eval@ just to avoid needless flattening.
data Propagated = Propagated Analysis Assignment [[Eval]] deriving Show

recurse :: Database -> Assignment -> [Lit] -> [[Eval]] -> Propagated
recurse db a xs acc =
  case analysis of
    Units [] -> Propagated analysis aa acc
    Units units -> recurse db aa (uniqueLiterals units) (evals:acc)
    Conflicts _ _ -> Propagated analysis aa acc
  where
    aa = extend a xs
    evals = evaluations db aa xs
    analysis = analyze evals

propagate :: Database -> Assignment -> Lit -> Propagated
propagate db a x = recurse db a [x] []


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
test_mutualConflicts = [e8, e5] == [Lit 8, Lit 5]
  -- order depends on implementation details, but good enough for now
  where
    xs = map Lit [8,5,-8,4,-5,8,2]  -- contradictions for 8 and for 5
    units = zipWith Unit testClauses xs
    [Mutual (Unit _ e8) _, Mutual (Unit _ e5) _] = mutualConflicts units

test_analyze :: Bool
test_analyze =
  -- order depends on implementation details, but good enough for now
  reverse output == take 5 (tail evals)
  && conflict == b
  && antecedent mutual2 == c && mutual1 == evals !! 3
  where
    (as, (b:c:_)) = splitAt 5 testClauses
    xs = map Lit [1..5]
    x = Lit (-3)
    evals = [Satisfied] ++ zipWith Unit as xs ++ [Unresolved]
    more = evals ++ [Conflict b, Unit c x]  -- direct and mutual
    Units output = analyze evals
    Conflicts [conflict] [Mutual mutual1 mutual2] = analyze more

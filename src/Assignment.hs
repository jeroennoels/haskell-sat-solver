module Assignment (
  Assignment,
  makeEmptyAssignment, isAssigned, extend,
  randomUnassignedVariable, isComplete,
  test_assignment) where

import Global
import Data.List (sort, intersect)
import Data.IntSet (IntSet)
import qualified Data.IntSet as Fast
import Data.Set (Set)
import qualified Data.Set as Slow

-- The first set tracks assigned literals, the second one tracks unassigned
-- variables. We need it to efficiently pick a random unassigned variable.
data Assignment = Assignment IntSet (Set Int) deriving Show

makeEmptyAssignment :: [Var] -> Assignment
makeEmptyAssignment allVariables = Assignment Fast.empty (Slow.fromList is)
  where
    unwrap (Var i) = i
    is = map unwrap allVariables

isComplete :: Assignment -> Bool
isComplete (Assignment _ set) = Slow.null set

randomUnassignedVariable :: RandomInt -> Assignment -> Var
randomUnassignedVariable rand (Assignment _ set)
  | n > 0 = Var (Slow.elemAt idx set)
  | otherwise = error "no unassigned variables"
  where
    n = Slow.size set
    idx = mod rand n

isAssigned :: Assignment -> Lit -> Bool
isAssigned (Assignment set _) (Lit i) = Fast.member i set

-- only for testing at the moment
isUnassigned :: Assignment -> Var -> Bool
isUnassigned (Assignment _ set) (Var i) = Slow.member i set

assign :: Assignment -> Lit -> Assignment
assign (Assignment a b) (Lit i) = Assignment aa bb
  where
    aa = Fast.insert i a
    bb = Slow.delete (abs i) b  -- absolute value!

extend :: Assignment -> [Lit] -> Assignment
extend
  | optimized = foldl assign
  | otherwise = foldl (flip safe)
  where
    safe x = assertAssigned x . flip assign x . assertNotAssigned x


--------------------------
-- tests and assertions --
--------------------------

verify :: Assignment -> [Var]
verify (Assignment a b)
  | disjoint = sort (as ++ bs)
  | otherwise = error "Assignment.verify"
  where
    as = map (variable . Lit) (Fast.elems a)
    bs = map Var (Slow.elems b)
    disjoint = null (intersect as bs)

assertNotAssigned :: Lit -> Assignment -> Assignment
assertNotAssigned x a
  | isAssigned a (negation x) = error $ "conflicting assignment for " ++ show x
  | isAssigned a x = error $ "reassigning " ++ show x
  | not consistent = error $ "inconsistent assignment with " ++ show x
  | otherwise = a
  where
    consistent = isUnassigned a (variable x)

assertAssigned :: Lit -> Assignment -> Assignment
assertAssigned x a
  | isAssigned a x && consistent = a
  | otherwise = error $ "assertAssigned: " ++ show x
  where
    consistent = not $ isUnassigned a (variable x)

test_assignment :: Bool
test_assignment = isAssigned a (Lit 4)
  && not (isAssigned a (Lit (-6)))
  && isAssigned b (Lit (-6))
  && isUnassigned b (Var 7)
  && isAssigned c (Lit 7)
  && not (isAssigned b (Lit 6))
  && all (isUnassigned empty) allVariables
  && all (not . isUnassigned c) allVariables
  && verify empty == allVariables
  && verify a == allVariables
  && verify b == allVariables
  && verify c == allVariables
  && not (isComplete b)
  && isComplete c
  && randomUnassignedVariable 0 b `elem` map Var [7,-8,9]
  where
    allVariables = map Var [1..9]
    empty = makeEmptyAssignment allVariables
    a = extend empty (map Lit [1..5])
    b = extend a [Lit (-6)]
    c = extend b (map Lit [7,-8,9])

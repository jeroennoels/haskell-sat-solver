module UnitPropagation (test_evaluate) where

import Global
import Database
import Assignment

data Eval = Satisfied | Unit Clause Lit | Conflict Clause | Unresolved
  deriving Show

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



isSatisfied :: Eval -> Bool
isSatisfied Satisfied = True
isSatisfied _ = False

isUnresolved :: Eval -> Bool
isUnresolved Unresolved = True
isUnresolved _ = False

test_evaluate :: Bool
test_evaluate = isSatisfied (evaluate a (asClause [6,1,9]))
  && isUnresolved (evaluate a (asClause [6,-3,5,7]))
  where
    asClause = Clause . map Lit
    a = extend emptyAssignment (map Lit [1,2,3,4,-5])

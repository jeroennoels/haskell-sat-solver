module Drive where

import Data.List (nub, sort)
import Data.Maybe
import RandomInt
import Global
import Assignment (Assignment, assign, isComplete, isAssigned,
                   randomUnassignedVariable)
import Database (Database, emptyAssignment, addLearnedClause, learnedClauses)
import UnitPropagation
import ConflictAnalysis


data Outcome = Sat Assignment Database | Unsat | Debug Learn deriving Show

drive :: [RandomInt] -> Database -> Outcome
drive rands db = recurse rands [empty] db Nothing
  where
    empty = emptyAssignment db

extractConflict :: Lit -> Result -> Conflict
extractConflict lastDecision result = conflict
  where
    Result (Conflicting details) _ implieds = result
    clauses = map conflictClause details
    conflict = Conflict lastDecision clauses implieds

decide :: RandomInt -> Assignment -> Lit
decide rand a = makeLiteral var bit
  where
    bit = rand > 0     -- using the same RandomInt twice is a bit
    var = randomUnassignedVariable rand a  -- questionable but ok

good :: Learn -> Assignment -> Bool
good (Learn _ xs) a = all (isAssigned a) (map negation xs)

-- Inefficient, but good enough for now.
backjump :: Database -> [Assignment] -> Learn -> [Assignment]
backjump db _ (Learn _ xs) | null xs = [emptyAssignment db]
backjump db (a:b:cs) learn
  | good learn b = backjump db (b:cs) learn
  | optimized = a:b:cs
  | good learn a && not (good learn b) = a:b:cs

canonical :: Clause -> Clause
canonical (Clause xs) = Clause (sort xs)

assertFixpoint :: Database -> Assignment -> Assignment
assertFixpoint db a
  | optimized = a
  | isFixpoint db a = a

-- Imagine an assignment such that for a variable v, both v and -v
-- propagate to conflict with no other new decisions involved.
-- Then we must avoid oscillating between these two.
-- So after a conflict, try the opposite decision.
recurse :: [RandomInt] -> [Assignment] -> Database -> Maybe Lit -> Outcome
recurse (rand:rands) trail db opposite
  | isComplete a = Sat a db
  | isConflict && null trail = Unsat  -- I have yet to understand this
  | isConflict && isNothing opposite = recurse rands bj db' (Just x)
  | isConflict = recurse rands (tail trail) db' Nothing  -- backtrack
  | otherwise = recurse rands (assertFixpoint db aa : trail) db Nothing
  where
    a = head trail
    decision = fromMaybe (decide rand a) opposite
    result@(Result summary aa _) = propagate db a decision
    isConflict = isConflicting summary
    -- only forced in case of conflict
    learn@(Learn x xs) = analyzeConflict $ extractConflict decision result
    db' = addLearnedClause db $ Clause (x:xs)
    bj = backjump db (undefined:trail) learn



testDrive :: Database -> Int -> (Bool, Bool, Int)
testDrive db seed = (checkSAT db a, checkLearnedSAT db a, length (learnedClauses db'))
  where
    Sat a db' = drive (randomInts seed) db

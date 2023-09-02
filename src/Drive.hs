module Drive where

import RandomInt
import Global
import Assignment (Assignment, assign, isComplete, isAssigned,
                   randomUnassignedVariable)
import Database (Database, emptyAssignment, addLearnedClause, learnedClauses)
import UnitPropagation
import ConflictAnalysis


data Outcome = Sat Assignment | Unsat | Debug Learn deriving Show

drive :: [RandomInt] -> Database -> Outcome
drive rands db = recurse rands [empty] db empty
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

-- Inefficient, but good enough for now.
backjump :: Database -> [Assignment] -> Learn -> [Assignment]
backjump db _ (Learn _ xs) | null xs = [emptyAssignment db]
backjump db (a:b:cs) learn@(Learn x xs)
  | all (isAssigned b) (map negation xs) = backjump db (b:cs) learn
  | otherwise = let aa = assign a x in aa:b:cs


recurse :: [RandomInt] -> [Assignment] -> Database -> Assignment -> Outcome
recurse (rand:rands) trail db a
  | isComplete a = Sat a
  | isConflicting summary = recurse rands bj db' (head bj)
  | otherwise = recurse rands (aa:trail) db aa
  where
    decision = decide rand a
    result@(Result summary aa implieds) = propagate db a decision
    conflict = extractConflict decision result
    learn@(Learn x xs) = analyzeConflict conflict
    db' = addLearnedClause db $ Clause (x:xs)
    bj = backjump db (undefined:trail) learn


testDrive :: Database -> Int -> Outcome
testDrive db seed = drive (randomInts seed) db

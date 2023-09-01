module Drive where

import RandomInt
import Global
import Assignment (Assignment, isComplete, randomUnassignedVariable)
import Database (Database, emptyAssignment)
import UnitPropagation
import ConflictAnalysis


data Outcome = Sat Assignment | Unsat | Debug Learn deriving Show

drive :: [RandomInt] -> Database -> Outcome
drive rands db = recurse rands db (emptyAssignment db)

extractConflict :: Lit -> Result -> Destination
extractConflict lastDecision result = destination
  where
    Result (Conflicting details) _ implieds = result
    clauses = map conflictClause details
    destination = Conflict lastDecision clauses implieds

decide :: RandomInt -> Assignment -> Lit
decide rand a = makeLiteral var bit
  where
    bit = rand > 0     -- using the same RandomInt twice is a bit
    var = randomUnassignedVariable rand a  -- questionable but ok

recurse :: [RandomInt] -> Database -> Assignment -> Outcome
recurse (rand:rands) db a
  | isComplete a = Sat a
  | isConflicting summary = Debug learn
  | otherwise = recurse rands db aa
  where
    decision = decide rand a
    destination = extractConflict decision result
    learn = analyzeConflict destination
    result@(Result summary aa implieds) = propagate db a decision


testDrive :: Database -> Int -> Outcome
testDrive db seed = drive (randomInts seed) db

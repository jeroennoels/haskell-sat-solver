module Drive where

import RandomInt
import Global
import Assignment (Assignment, isComplete, randomUnassignedVariable)
import Database (Database, emptyAssignment)
import UnitPropagation
import ConflictAnalysis

drive :: [RandomInt] -> Database -> Destination
drive rands db = recurse rands db (emptyAssignment db)

extractConflict :: Lit -> Result -> Destination
extractConflict lastDecision result = destination
  where
    Result (Conflicting details) _ implieds = result
    clauses = map conflictClause details
    destination = Conflict lastDecision clauses implieds

recurse :: [RandomInt] -> Database -> Assignment -> Destination
recurse (rand:rands) db a
  | isComplete a = Sat a
  | isConflicting summary = extractConflict decision result
  | otherwise = recurse rands db aa
  where
    bit = rand > 0     -- using the same RandomInt twice is a bit
    var = randomUnassignedVariable rand a  -- questionable but ok
    decision = makeLiteral var bit
    result@(Result summary aa implieds) = propagate db a decision


testDrive :: Database -> Int -> String
testDrive db seed
  | satisfiable destination = "satisfiable"
  | otherwise = show lastDecision ++ " " ++ analyzeConflict destination
  where
    destination = drive (randomInts seed) db
    Conflict lastDecision _ _ = destination

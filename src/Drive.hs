module Drive where

import RandomInt
import Global
import Assignment
import Database
import UnitPropagation
import ConflictAnalysis


drive :: [RandomInt] -> Database -> Result
drive rands db = recurse rands db (emptyAssignment db)

recurse :: [RandomInt] -> Database -> Assignment -> Result
recurse (rand:rands) db a
  | isConflicting summary = result
  | otherwise = recurse rands db aa
  where
    bit = rand > 0     -- using the same RandomInt twice is a bit
    var = randomUnassignedVariable rand a  -- questionable but ok
    decision = makeLiteral var bit
    result@(Result summary aa implieds) = propagate db a decision


testDrive db seed = analyzeConflict $ drive (randomInts seed) db

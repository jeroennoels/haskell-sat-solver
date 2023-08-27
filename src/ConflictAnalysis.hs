module ConflictAnalysis (
  Destination(..), satisfiable,
  analyzeConflict) where

import Global
import Assignment
import UnitPropagation

data Destination = Sat Assignment | Conflict Lit [Clause] [[Implied]]
  deriving Show

satisfiable :: Destination -> Bool
satisfiable (Sat _) = True
satisfiable _ = False

analyzeConflict :: Destination -> [Clause]
analyzeConflict (Conflict lastDecision clauses implieds) = clauses

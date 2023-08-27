module ConflictAnalysis (analyzeConflict) where

import Global
import Assignment
import UnitPropagation

analyzeConflict :: Result -> [Clause]
analyzeConflict (Result (Conflicting details) assignment implieds) = conflicts
  where
    conflicts = map conflictClause details

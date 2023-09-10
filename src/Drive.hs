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


data Stats = Stats Int Int Int deriving Show

registerStep :: Stats -> Stats
registerStep (Stats s c r) = Stats (s+1) c r

registerConflict :: Stats -> Stats
registerConflict (Stats s c r) = Stats s (c+1) r

registerRestart :: Stats -> Stats
registerRestart (Stats s c r) = Stats s c (r+1)

data Outcome = Sat Assignment Database Stats
             | Unsat Stats
             | Debug Learn Stats
             deriving Show

drive :: [RandomInt] -> Database -> Outcome
drive rands db = recurse (Stats 0 0 0) rands [(undefined,empty)] db []
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

good :: Learn -> (Lit, Assignment) -> Bool
good (Learn _ xs) (_, a) = all (isAssigned a) (map negation xs)

type Trail = [(Lit, Assignment)]

-- Inefficient, but good enough for now.
-- Smallest assignment where the learned clause is still unit.
backjump :: Database -> Trail -> Learn -> Trail
backjump db _ (Learn _ xs) | null xs = [(undefined, emptyAssignment db)]
backjump db (a:b:cs) learn
  | good learn b = backjump db (b:cs) learn
  | optimized = a:b:cs
  | good learn a && not (good learn b) = a:b:cs

assertFixpoint :: Database -> Assignment -> Assignment
assertFixpoint db a
  | optimized = a
  | isFixpoint db a = a

-- Imagine an assignment such that for a variable v, both v and -v
-- propagate to conflict with no other new decisions involved.
-- Then we restart to avoid oscillating between these two.
recurse :: Stats -> [RandomInt] -> Trail -> Database -> [Clause] -> Outcome
recurse stats (rand:rands) trail db previousLearns
  | isComplete a = Sat a db stats
  | isConflict && conflictFollowup = recurse (registerRestart stats) rands restart db []
  | isConflict && null (tail trail) = Unsat stats -- I have yet to understand this
  | isConflict = recurse (registerConflict stats) rands bj db' [learnedClause]
  | otherwise = recurse (registerStep stats) rands ((decision, assertFixpoint db aa) : trail) db []
  where
    (keep, a) = head trail
    conflictFollowup = not (null previousLearns)
    decision = if conflictFollowup then keep else decide rand a
    -- When following up on conflict we propagate the learned clause
    -- rather than a new decision.
    propagateDecision = if conflictFollowup then [] else [decision]
    result@(Result summary aa _) = propagate db a previousLearns propagateDecision
    isConflict = isConflicting summary
    restart = [(undefined, emptyAssignment db)]
    -- Only forced in case of conflict:
    learn@(Learn x xs) = head $ analyzeConflicts $ extractConflict decision result
    learnedClause = Clause (x:xs)
    db' = addLearnedClause db learnedClause
    bj = backjump db (undefined:trail) learn


countUnique :: [Clause] -> (Int, Int)
countUnique cs = (length cs, length cs')
  where
    cs' = nub $ map canonical cs
    canonical (Clause xs) = Clause (sort xs)


testDrive db seed = (checkSAT db a, checkLearnedSAT db a,
                     stats, countUnique (learnedClauses db'))
  where Sat a db' stats = drive (randomInts seed) db

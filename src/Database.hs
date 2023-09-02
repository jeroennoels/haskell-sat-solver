module Database (
  Database, makeClauses, makeDatabase, addLearnedClause,
  originalClauses, learnedClauses, clausesWith, emptyAssignment,
  test_database) where

import Assignment (Assignment, makeEmptyAssignment, randomUnassignedVariable)

import Data.List (nub)
import Global
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M

-- all clauses that contain a given literal
type Index = IntMap [Clause]

data Database = Database [Clause] Index Assignment [Clause]

instance Show Database where
  show (Database _ index vars _) =
    "clause database for " ++ show (M.size index) ++ " literals"

cons :: a -> Maybe [a] -> Maybe [a]
cons a (Just as) = Just (a:as)
cons a Nothing = Just [a]

insertClause :: Clause -> Index -> Lit -> Index
insertClause clause index (Lit i) = M.alter (cons clause) i index

indexClause :: Index -> Clause -> Index
indexClause index clause = foldl (insertClause clause) index (literals clause)

indexAll :: [Clause] -> Index
indexAll = foldl indexClause M.empty

makeClauses :: [[Int]] -> [Clause]
makeClauses = map (Clause . map Lit)

makeDatabase :: [[Int]] -> Database
makeDatabase prims = Database clauses index empty learned
  where
    index = indexAll clauses
    clauses = makeClauses prims
    literals = map Lit (M.keys index)
    empty = makeEmptyAssignment (map variable literals)
    learned = []

addLearnedClause :: Database -> Clause -> Database
addLearnedClause (Database orig idx empty learn) new =
  Database orig (indexClause idx new) empty (new:learn)

originalClauses :: Database -> [Clause]
originalClauses (Database clauses _ _ _) = clauses

learnedClauses :: Database -> [Clause]
learnedClauses (Database _ _ _ learned) = learned

clausesWith :: Database -> Lit -> [Clause]
clausesWith (Database _ index _ _) (Lit i) = index M.! i

emptyAssignment :: Database -> Assignment
emptyAssignment (Database _ _ empty _) = empty


--------------------------
-- tests and assertions --
--------------------------

test_database :: Bool
test_database = length (originalClauses db) == 3
  && check (clausesWith db (Lit 1)) [c,a]
  && check (clausesWith db (Lit 4)) [c,b]
  && check (clausesWith db (Lit (-6))) [c]
  && check (clausesWith db' (Lit (-6))) [d,c]
  && check (clausesWith db' (Lit 2)) [d]
  && randomUnassignedVariable 0 (emptyAssignment db) `elem` map Var [1..6]
  where
    a = [1,-2,3]
    b = [4,5]
    c = [1,4,-6]
    d = [2,-6]
    db = makeDatabase [a,b,c]
    db' = addLearnedClause db $ Clause (map Lit d)
    check clauses raw = clauses == makeClauses raw

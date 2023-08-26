module Database (
  Database, makeClauses, makeDatabase,
  allClauses, clausesWith, allVariables,
  test_database) where

import Data.List (nub)
import Global
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M

-- all clauses that contain a given literal
type Index = IntMap [Clause]

data Database = Database [Clause] Index [Var]

instance Show Database where
  show (Database _ index vars) =
    "clause database for " ++ show (M.size index) ++
    " literals and " ++ show (length vars) ++ " variables"

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
makeDatabase prims = Database clauses index variables
  where
    index = indexAll clauses
    clauses = makeClauses prims
    literals = map Lit (M.keys index)
    variables = nub $ map variable literals

allClauses :: Database -> [Clause]
allClauses (Database clauses _ _) = clauses

clausesWith :: Database -> Lit -> [Clause]
clausesWith (Database _ index _) (Lit i) = index M.! i

allVariables :: Database -> [Var]
allVariables (Database _ _ vars) = vars

test_database :: Bool
test_database = length (allClauses db) == 3
  && check (clausesWith db (Lit 1)) [c,a]
  && check (clausesWith db (Lit 4)) [c,b]
  && check (clausesWith db (Lit (-6))) [c]
  where
    a = [1,-2,3]
    b = [4,5]
    c = [1,4,-6]
    db = makeDatabase [a,b,c]
    check clauses raw = clauses == makeClauses raw

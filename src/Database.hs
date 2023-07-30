module Database (Database, makeDatabase, allClauses, clausesWith) where

import Global
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M

-- all clauses that contain a given literal
type Index = IntMap [Clause]

data Database = Database [Clause] Index

instance Show Database where
  show (Database _ index) = let n = M.size index in
    "clause database for " ++ show n ++ " literals"

cons :: a -> Maybe [a] -> Maybe [a]
cons a (Just as) = Just (a:as)
cons a Nothing = Just [a]

insertClause :: Clause -> Index -> Lit -> Index
insertClause clause index (Lit i) = M.alter (cons clause) i index

indexClause :: Index -> Clause -> Index
indexClause index clause = foldl (insertClause clause) index (literals clause)

indexAll :: [Clause] -> Index
indexAll = foldl indexClause M.empty

fromPrimitives :: [[Int]] -> [Clause]
fromPrimitives = map (Clause . map Lit)

makeDatabase :: [[Int]] -> Database
makeDatabase prims = Database clauses (indexAll clauses)
  where
    clauses = fromPrimitives prims

allClauses :: Database -> [Clause]
allClauses (Database clauses _) = clauses

clausesWith :: Database -> Lit -> [Clause]
clausesWith (Database _ index) (Lit i) = index M.! i

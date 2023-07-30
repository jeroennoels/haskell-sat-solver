module Assignment (Assignment, emptyAssignment, isAssigned, extend) where

import Global
import Data.IntSet (IntSet)
import qualified Data.IntSet as S

newtype Assignment = Assignment IntSet deriving Show

emptyAssignment :: Assignment
emptyAssignment = Assignment S.empty

isAssigned :: Assignment -> Lit -> Bool
isAssigned (Assignment set) (Lit i) = S.member i set

assign :: Assignment -> Lit -> Assignment
assign (Assignment set) (Lit i) = Assignment (S.insert i set)

extend :: Assignment -> [Lit] -> Assignment
extend
  | optimized = foldl assign
  | otherwise = foldl (flip safe)
  where
    safe x = assertAssigned x . flip assign x . assertNotAssigned x


assertNotAssigned :: Lit -> Assignment -> Assignment
assertNotAssigned x a
  | isAssigned a (negation x) = error $ "conflicting assignment for " ++ show x
  | isAssigned a x = error $ "reassigning " ++ show x
  | otherwise = a

assertAssigned :: Lit -> Assignment -> Assignment
assertAssigned x a
  | isAssigned a x = a
  | otherwise = error $ "assertAssigned: " ++ show x

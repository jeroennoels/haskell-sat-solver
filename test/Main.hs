module Main where

import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as BS

import ReadCNF
import Database
import Global
import Assignment

main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run ["test"] = runTests
run ["read"] = readDatabase >>= print

readDatabase :: IO Database
readDatabase = makeDatabase `fmap` readCNF "./local/problem.cnf"

runTests :: IO ()
runTests = sequence_ $ map putStrLn [
  show test_assignment,
  show test_linesToClauses,
  show test_database]

test_assignment :: Bool
test_assignment = isAssigned a (Lit 4)
  && not (isAssigned a (Lit (-6)))
  && isAssigned b (Lit (-6))
  where
    a = extend emptyAssignment (map Lit [1..5])
    b = extend a [Lit (-6)]

test_linesToClauses :: Bool
test_linesToClauses = clauses == [[48,-58,63], [-58,63]]
  where
    dimacs = ["c comment", "p cnf 123 456", "48 -58 63 0", "-58 63 0"]
    clauses = linesToClauses (map BS.pack dimacs)

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
    check clauses prims = map literals clauses == map (map Lit) prims

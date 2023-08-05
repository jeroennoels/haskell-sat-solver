module Main where

import System.Environment (getArgs)

import ReadCNF
import Database
import Global
import Assignment
import UnitPropagation

main :: IO ()
main = fmap head getArgs >>=  run

run :: String -> IO ()
run "test" = runTests
run "read" = readDatabase >>= print
run "go" = readDatabase >>= print . go

go db = propagate db emptyAssignment (Lit 3)

readDatabase :: IO Database
readDatabase = makeDatabase `fmap` readCNF "./local/problem.cnf"

runTests :: IO ()
runTests = sequence_ $ map putStrLn [
  show test_assignment,
  show test_linesToClauses,
  show test_database,
  show test_unitPropagation]

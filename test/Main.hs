module Main where

import System.Environment (getArgs)

import ReadCNF
import Database
import Global
import Assignment
import UnitPropagation
import RandomInt
import Drive
import ConflictAnalysis

main :: IO ()
main = warn >> fmap head getArgs >>= run

warn :: IO ()
warn = putStrLn $
  if optimized
  then "optimized = true"
  else "[WARNING] Global.optimized = false"

run :: String -> IO ()
run "test" = runTests
run "read" = readDatabase >>= print
run "go" = readDatabase >>= print . go

go db = map (testDrive db) [1..50]

readDatabase :: IO Database
readDatabase = makeDatabase `fmap` readCNF "./local/problem.cnf"

runTests :: IO ()
runTests = sequence_ $ map putStrLn [
  show test_assignment,
  show test_linesToClauses,
  show test_database,
  show test_unitPropagation]

module Main where

import System.Environment (getArgs)

import ReadCNF
import Database
import Global
import Assignment
import UnitPropagation
import Drive
import ConflictAnalysis

main :: IO ()
main = fmap head getArgs >>=  run

run :: String -> IO ()
run "test" = runTests
run "read" = readDatabase >>= print
run "go" = readDatabase >>= print . go

fakeRandoms :: [RandomInt]
fakeRandoms = [4..]

go db = drive fakeRandoms db

quux a lit db = propagate db a lit

readDatabase :: IO Database
readDatabase = makeDatabase `fmap` readCNF "./local/problem.cnf"

runTests :: IO ()
runTests = sequence_ $ map putStrLn [
  show test_assignment,
  show test_linesToClauses,
  show test_database,
  show test_unitPropagation]

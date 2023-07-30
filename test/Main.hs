module Main where

import System.Environment (getArgs)

import ReadCNF (readCNF)
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
runTests = sequence_ $ map putStrLn ["hello", "world"]

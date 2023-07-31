module ReadCNF (linesToClauses, readCNF) where

import qualified Data.ByteString.Char8 as C

-- Local type synonyms for code readability.
-- From the outside [Clause] is simply [[Int]].

type Literal = Int
type Clause = [Literal]

isClauseLine :: String -> Bool
isClauseLine ('c':_) = False
isClauseLine ('p':_) = False
isClauseLine _ = True

parseLiteral :: String -> Literal
parseLiteral = read

mapDropLast :: Eq a => a -> (a -> b) -> [a] -> [b]
mapDropLast terminator f (x:xs)
  | not (null xs) = f x : mapDropLast terminator f xs
  | x == terminator = []
  | otherwise = error "last not terminator"

lineToClause :: String -> Clause
lineToClause = mapDropLast "0" parseLiteral . words

linesToClauses :: [C.ByteString] -> [Clause]
linesToClauses = map lineToClause . filter isClauseLine . map C.unpack

readLines :: String -> IO [C.ByteString]
readLines filepath = C.lines `fmap` C.readFile filepath

readCNF :: String -> IO [[Int]]
readCNF filepath = linesToClauses `fmap` readLines filepath

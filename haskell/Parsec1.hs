-- Following RealWorldHaskell for this
-- at http://book.realworldhaskell.org/read/using-parsec.html

module ParseCSV where

import Text.ParserCombinators.Parsec
import Text.Parsec.Prim hiding (try)
import Data.Functor.Identity

csvFile :: GenParser Char st [[String]]
csvFile = line `sepBy` eol

line :: GenParser Char st [String]
line = cell `sepBy` char ','

cell :: GenParser Char st String
cell = many (noneOf ",\n")

eol :: ParsecT String u Identity Char
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV = parse csvFile "(unknown)"


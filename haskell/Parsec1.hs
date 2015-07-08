-- Following RealWorldHaskell for this
-- at http://book.realworldhaskell.org/read/using-parsec.html

module ParseCSV where

import Text.ParserCombinators.Parsec
import Text.Parsec.Prim
import Data.Functor.Identity

csvFile :: GenParser Char st [[String]]
csvFile =
  do result <- many line
     eof
     return result

line :: GenParser Char st [String]
line =
  do result <- cells
     eol
     return result

cells :: GenParser Char st [String]
cells =
  do first <- cellContent
     next <- remainingCells
     return (first : next)

remainingCells :: GenParser Char st [String]
remainingCells =
  (char ',' >> cells)
  <|> return []

cellContent :: GenParser Char st String
cellContent =
  many (noneOf ",\n")

eol :: ParsecT String u Identity Char
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV = parse csvFile "(unknown)"

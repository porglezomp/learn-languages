{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- I figured this one out mostly on my own, and with docs

module ParseJSON where

import Text.Parsec
import Control.Applicative ((<$>))

data JSON = JStr String
          | JInt Int
          | JFloat Float
          | JBool Bool
          | JNull
          | JArr [JSON]
          | JObj [(String, JSON)]
          deriving Show

jsonFile = jsonItem
jsonItem = object <|> array <|> value

value =     jstring
        <|> try int
        <|> try float
        <|> bool
        <|> jnull
        <?> "JSON value"

float = do start <- many1 digit
           char '.'
           rest <- many digit
           let numStr = start ++ '.':rest
           return $ JFloat . read $ numStr
           
int = JInt . read <$> many1 digit
bool =     (string "true" >> return (JBool True))
       <|> (string "false" >> return (JBool False))
jnull = string "null" >> return JNull
jstring = do char '"'
             body <- many quotedChar
             char '"'
             return $ JStr body
  where quotedChar = noneOf "\"\\"
                     <|> (string "\\\"" >> return '"')
                     <|> (string "\\\\" >> return '\\')

object = do char '{'
            elements <- objElem `sepBy` char ','
            char '}'
            return $ JObj elements
         <?> "JSON object"
  where objElem = do JStr key <- jstring
                     char ':'
                     val <- jsonItem
                     return (key, val)

array = do char '['
           elements <- jsonItem `sepBy` char ','
           char ']'
           return $ JArr elements
        <?> "JSON array"

parseJSON :: String -> Either ParseError JSON
parseJSON = parse jsonFile "(unknown)" 

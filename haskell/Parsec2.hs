{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- I figured this one out mostly on my own, and with docs

module ParseJSON where

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Control.Applicative ((<$>), (<*))

data JSON = JStr String
          | JInt Integer
          | JFloat Double
          | JBool Bool
          | JNull
          | JArr [JSON]
          | JObj [(String, JSON)]
          deriving Show

jsonFile = P.whiteSpace lang >> jsonItem <* eof
jsonItem = P.lexeme lang (jObject <|> jArray <|> value)

value =     jstring
        <|> try jint
        <|> try jfloat
        <|> jbool
        <|> jnull
        <?> "JSON value"

jfloat = JFloat <$> P.float lang
jint = JInt <$> P.integer lang
jbool =    (P.symbol lang "true" >> return (JBool True))
       <|> (P.symbol lang "false" >> return (JBool False))
jnull = P.symbol lang "null" >> return JNull
jstring = JStr <$> P.stringLiteral lang
  
jObject = JObj <$> P.braces lang (P.commaSep lang objElem) <?> "JSON object"
  where objElem = do JStr key <- jstring
                     P.colon lang
                     val <- jsonItem
                     return (key, val)

jArray = JArr <$> P.brackets lang (P.commaSep lang jsonItem) <?> "JSON array"

parseJSON :: String -> Either ParseError JSON
parseJSON = parse jsonFile "(unknown)" 

lang = P.makeTokenParser haskellDef

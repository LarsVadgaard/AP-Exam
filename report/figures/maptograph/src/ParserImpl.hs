module ParserImpl where

-- put your parser in this file. Do not change the types of the following
-- exported functions

import Text.Parsec.Prim hiding (token)
import Text.Parsec.Char hiding (spaces)
import Text.Parsec.String
import Text.Parsec.Combinator

import Control.Monad (when, unless, foldM)

import Data.Functor (($>))
import Data.Char (toLower, toUpper)


----------------------
-- AST
----------------------

type District = D Int [(String, Int)]

type Territory = T [District]


----------------------
-- Functions to export
----------------------

parseTerritory :: String -> Either ErrMsg Database
parseTerritory s = case parse (spaces *> territory <* eof) "" s of
  Left e -> Left $ show e
  Right v -> Right v


------------------
-- Generic parsers
------------------

-- whitespace
spaces :: Parser String
spaces = many space <* skipMany cmt

spaces1 :: Parser String
spaces1 = many1 space <* skipMany cmt <|> skipMany1 cmt $> ""

-- comments
cmt :: Parser String
cmt = string "--" >> (try (manyTill anyChar (oneOf "\r\n"))
                     <|>  manyTill anyChar eof) <* spaces

-- token - ignore whitespace after
token :: Parser a -> Parser a
token p = p <* spaces

token1 :: Parser a -> Parser a
token1 p = p <* spaces1

-- symbols - strings
symbol :: String -> Parser String
symbol = token . string

symbol1 :: String -> Parser String
symbol1 = token1 . string

-- delimiters
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

quots :: Parser a -> Parser a
quots = between (char '\"') (symbol "\"")


-- Grammar

-- Database
territory :: Parser Territory
territory = T <$> (char '#' >> braces (sepBy district (symbol ",")))

district :: Parser District
district = do
  num <- read <$> many1 digit
  symbol "=>"

neighbour = do


-- versions
vnum :: Parser VNum
vnum = do
  num <- read <$> many1 digit
  when (num < minVN || num > maxVN)
    $ unexpected $ "version number (allowed range: " ++ show minVN ++ "-" ++ show maxVN ++ ")"
  VN num <$> suffix

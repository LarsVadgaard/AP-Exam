module ParserImpl where

-- put your parser in this file. Do not change the types of the following
-- exported functions

import Text.Parsec.Prim hiding (token)
import Text.Parsec.Char hiding (spaces)
import Text.Parsec.Error
import Text.Parsec.String
import Text.Parsec.Combinator

import Control.Monad (when, foldM)

import Data.Functor (($>))
import Data.Char (toLower, toUpper)

import Defs
import Utils

parseVersion :: String -> Either ErrMsg Version
parseVersion s = case parse (spaces *> version <* eof) "" s of
  Left e -> Left $ show e
  Right v -> Right v

parseDatabase :: String -> Either ErrMsg Database
parseDatabase s = case parse (spaces *> database <* eof) "" s of
  Left e -> Left $ show e
  Right v -> Right v

-- Utility

keyword :: String -> Parser String
keyword = token1 . mapM (\c -> char (toLower c) <|> char (toUpper c))

-- Whitespace
spaces :: Parser String
spaces = many space <* skipMany cmt

spaces1 :: Parser String
spaces1 = many1 space <* skipMany cmt <|> skipMany1 cmt $> ""

-- Comments
cmt :: Parser String
cmt = string "--" *> (try (manyTill anyChar (oneOf "\r\n")) <|> manyTill anyChar eof) <* spaces

  --   <|> manyTill anyChar eof) <* spaces

-- Token
token :: Parser a -> Parser a
token p = p <* spaces

token1 :: Parser a -> Parser a
token1 p = p <* spaces1

-- Symbols
symbol :: String -> Parser String
symbol = token . string

symbol1 :: String -> Parser String
symbol1 = token1 . string

-- Delimiters
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

quots :: Parser a -> Parser a
quots = between (char '\"') (char '\"')


-- Grammar

-- Database
database :: Parser Database
database = DB <$> many package

-- Package
package :: Parser Pkg
package = do
  keyword "package"
  clauses <- checkClauses =<< braces (sepEndBy clause (symbol ";"))
  checkDeps =<< foldM (flip genAction) start clauses

  where start = Pkg{name=P "",ver=stdV,desc="",deps=[]}

checkClauses :: [Clause] -> Parser [Clause]
checkClauses cs = do
  when (n /= 1) $ unexpected "number of name clauses (exactly 1)"
  when (v > 1)  $ unexpected "number of version clauses (at most 1)"
  when (d > 1)  $ unexpected "number of description clauses (at most 1)"
  return cs

  where (n,v,d) = foldl countClause (0,0,0) cs

checkDeps :: Pkg -> Parser Pkg
checkDeps pkg = do
  when (any (\(p,_) -> p == name pkg) $ deps pkg) $
    unexpected "self-referential dependencies"
  return pkg

countClause :: (Int,Int,Int) -> Clause -> (Int,Int,Int)
countClause (n,v,d) Name{}        = (n+1,v,d)
countClause (n,v,d) Version{}     = (n,v+1,d)
countClause (n,v,d) Desc{}        = (n,v,d+1)
countClause (n,v,d) Constraints{} = (n,v,d)

-- Using a temporary type to keep track of the clauses
data Clause = Name PName | Version Version | Desc String | Constraints Constrs

clause :: Parser Clause
clause = (keyword "name"        >> Name <$> pname)
     <|> (keyword "version"     >> Version <$> version)
     <|> (keyword "description" >> Desc <$> str)
     <|> (Constraints <$> constr)

genAction :: Clause -> Pkg -> Parser Pkg
genAction (Name id)        pkg = return $ pkg{name=id}
genAction (Version v)      pkg = return $ pkg{ver=v}
genAction (Desc d)         pkg = return $ pkg{desc=d}
genAction (Constraints cs) pkg = case deps pkg `merge` cs of
  Nothing  -> unexpected "inconsistence between constraints"
  Just cs' -> return $ pkg{deps=cs'}

-- PName
pname :: Parser PName
pname = token $ (do
      first <- letter
      after <- concat <$> many pname'
      return $ P (first:after))
  <|> P <$> str

pname' :: Parser String
pname' = do
  c <- string "-" <|> return ""
  a <- alphaNum
  return $ c++[a]

-- Version
version :: Parser Version
version = token $ V <$> sepBy1 vnum (char '.')

vnum :: Parser VNum
vnum = do
  num <- read <$> many1 digit
  when (num < minVNum || num > maxVNum)
    (unexpected $ "version number (allowed range: " ++ show minVNum ++ "-" ++ show maxVNum ++ ")")
  VN num <$> suffix

  where (minVNum,maxVNum) = case (minV,maxV) of
                (V (VN min _:_),V (VN max _:_)) -> (min,max-1)

suffix :: Parser String
suffix = do
  s <- many lower
  if length s > 4
  then unexpected "number of letters in suffix (max 4)"
  else return s

-- Strings
str :: Parser String
str = token $ quots $ concat <$> many c
  where c = return <$> noneOf "\"" <|>
            try (string "\"\"" $> "\"")

-- Constraints
constr :: Parser Constrs
constr = do
  b  <- (keyword "requires" $> True) <|> (keyword "conflicts" $> False)
  map (\(name, vmin, vmax) -> (name, (b, vmin, vmax))) <$> bounds b

bound :: Bool -> Parser (PName, Version, Version)
bound b = do
  name <- pname
  (do lt <- (symbol "<" $> True) <|> (symbol ">=" $> False)
      v <- version
      if lt == b then return (name, minV, v)
                 else return (name, v, maxV))
    <|> return (name, minV, if b then maxV else minV)

bounds :: Bool -> Parser [(PName, Version, Version)]
bounds b = sepBy1 (bound b) (symbol ",")

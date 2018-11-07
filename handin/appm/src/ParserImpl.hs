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

import Defs
import Utils

----------------------
-- Functions to export
----------------------

parseVersion :: String -> Either ErrMsg Version
parseVersion s = case parse (spaces *> version <* eof) "" s of
  Left e -> Left $ show e
  Right v -> Right v

parseDatabase :: String -> Either ErrMsg Database
parseDatabase s = case parse (spaces *> database <* eof) "" s of
  Left e -> Left $ show e
  Right v -> Right v


------------------
-- Generic parsers
------------------

-- keywords
keyword :: String -> Parser String
keyword = token1 . mapM (\c -> char (toLower c) <|> char (toUpper c))

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
database :: Parser Database
database = DB <$> many package

-- Package
data Clause = Name PName | Ver Version | Desc String | Deps Constrs

package :: Parser Pkg
package = do
  keyword "package"
  checkDeps =<< genPackage
            =<< checkClauses
            =<< braces clauses

clauses :: Parser [Clause]
clauses = sepEndBy clause (symbol ";")

clause :: Parser Clause
clause = (keyword "name"        >> Name <$> pname   )
     <|> (keyword "version"     >> Ver  <$> (checkPkgVer =<< version) )
     <|> (keyword "description" >> Desc <$> str     )
     <|> (                         Deps <$> constr  )

checkPkgVer :: Version -> Parser Version
checkPkgVer v = do
  unless (v < maxV) $
    unexpected ("Version number of package (max " ++ show (maxVN - 1) ++ ")")
  return v

checkClauses :: [Clause] -> Parser [Clause]
checkClauses cls = do
  when (n /= 1) $ unexpected "number of name clauses (exactly 1)"
  when (v > 1)  $ unexpected "number of version clauses (at most 1)"
  when (d > 1)  $ unexpected "number of description clauses (at most 1)"
  return cls

  where (n,v,d) = countClauses cls

countClauses :: [Clause] -> (Int,Int,Int)
countClauses = foldl count (0,0,0)
  where
    count (n,v,d) Name {} = (n+1,v,d)
    count (n,v,d) Ver  {} = (n,v+1,d)
    count (n,v,d) Desc {} = (n,v,d+1)
    count (n,v,d) Deps {} = (n,v,d)

checkDeps :: Pkg -> Parser Pkg
checkDeps pkg = do
  when (any (\(p,_) -> p == name pkg) $ deps pkg) $
    unexpected "self-referential dependencies"
  return pkg

genPackage :: [Clause] -> Parser Pkg
genPackage cls =
  let start = Pkg{name=P "",ver=stdV,desc="",deps=[]}
  in foldM applyClause start cls

applyClause :: Pkg -> Clause -> Parser Pkg
applyClause pkg (Name p ) = return $ pkg{ name = p }
applyClause pkg (Ver  v ) = return $ pkg{ ver  = v }
applyClause pkg (Desc d ) = return $ pkg{ desc = d }
applyClause pkg (Deps cs) = case deps pkg `merge` cs of
  Nothing  -> unexpected "inconsistence between constraints"
  Just cs' -> return $ pkg{ deps = cs' }


-- package names
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


-- versions
version :: Parser Version
version = token $ V <$> sepBy1 vnum (char '.')

vnum :: Parser VNum
vnum = do
  num <- read <$> many1 digit
  when (num < minVN || num > maxVN)
    $ unexpected $ "version number (allowed range: " ++ show minVN ++ "-" ++ show maxVN ++ ")"
  VN num <$> suffix

suffix :: Parser String
suffix = do
  s <- many lower
  if length s > 4
  then unexpected "number of letters in suffix (max 4)"
  else return s


-- strings
str :: Parser String
str = quots $ concat <$> many c
  where c = return <$> noneOf "\"" <|>
            try (string "\"\"" $> "\"")

-- deps
constr :: Parser Constrs
constr = do
  b  <- (keyword "requires" $> True) <|> (keyword "conflicts" $> False)
  map (\(p, vmin, vmax) -> (p, (b, vmin, vmax))) <$> bounds b

bound :: Bool -> Parser (PName, Version, Version)
bound b = do
  p <- pname
  (do lt <- (symbol "<" $> True) <|> (symbol ">=" $> False)
      v <- version
      if lt == b then return (p, minV, v)
                 else return (p, v, maxV))
    <|> return (p, minV, if b then maxV else minV)

bounds :: Bool -> Parser [(PName, Version, Version)]
bounds b = sepBy1 (bound b) (symbol ",")

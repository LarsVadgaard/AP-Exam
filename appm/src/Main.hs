module Main where

import Defs
import Parser (parseDatabase)
import Solver (normalize, install)

import System.Environment (getArgs)
import Data.List (intercalate)


prettyVersion :: Version -> String
prettyVersion (V l) =
  intercalate "." [show n ++ s | VN n s <- l]

check :: String -> Either String a -> IO a
check s (Left e) = error $ s ++ ": " ++ e
check _ (Right a) = return a

main :: IO ()
main = do
  args <- getArgs
  case args of
    "-p":dbfile:b ->
      do s <- readFile dbfile
         db <- check "Parsing" $ parseDatabase s
         putStrLn . prettyDB $ db
         putStrLn . (if b == ["p"] then prettyDB else show) $ db
    "-n":dbfile:b ->
      do s <- readFile dbfile
         db <- check "Parsing" $ parseDatabase s
         db' <- check "Normalizing" $ normalize db
         putStrLn . (if b == ["p"] then prettyDB else show) $ db'
    [dbfile, pkg] ->
      do s <- readFile dbfile
         db <- check "Parsing" $ parseDatabase s
         db' <- check "Normalizing" $ normalize db
         case install db' (P pkg) of
            Nothing -> error "Cannot solve constraints"
            Just l ->
              do putStrLn "installing packages:"
                 mapM_ (\(P p,v) -> putStrLn $ p ++ " (" ++ prettyVersion v ++ ")") l
    _ -> error "Usage: appm DATABASE.db PACKAGE"

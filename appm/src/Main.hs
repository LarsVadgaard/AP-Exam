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
check s (Left e) = error $ s ++ ":" ++ e
check s (Right a) = return a

doError s = putStrLn $ "Error: " ++ s

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-p", dbfile] ->
      do s <- readFile dbfile
         db <- check "Parsing" $ parseDatabase s
         print db
    [dbfile, pkg] ->
      do s <- readFile dbfile
         db <- check "Parsing" $ parseDatabase s
         db' <- check "Normalizing" $ normalize db
         case install db' (P pkg) of
            Nothing -> doError "Cannot solve constraints"
            Just l ->
              do putStrLn "installing packages:"
                 mapM_ (\(P p,v) -> putStrLn $ p ++ " (" ++ prettyVersion v ++ ")") l
    _ -> doError "Usage: appm DATABASE.db PACKAGE"

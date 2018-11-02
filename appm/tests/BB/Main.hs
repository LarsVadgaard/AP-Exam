module Main where

-- Put your black-box tests in this file

import Defs
import Parser (parseDatabase)
import Solver (install)

import Test.Tasty
import Test.Tasty.HUnit

-- just a sample; feel free to replace with your own structure
tests = testGroup "Unit tests"
  [testGroup "Parser tests"
     [testCase "tiny" $
        parseDatabase "package {name foo}" @?= Right db],
   testGroup "Solver tests"
     [testCase "tiny" $
        install db pname @?= Just [(pname, ver)] ] ]
  where
    pname = P "foo"
    ver = V [VN 1 ""]
    db = DB [Pkg pname ver "" []]

main = defaultMain tests


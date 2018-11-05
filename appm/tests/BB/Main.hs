module Main where

-- Put your black-box tests in this file

import Test.Tasty
import Test.Tasty.HUnit

import Defs
import qualified TestParser as Parser
import qualified TestUtil   as Util
import qualified TestSolver as Solver

-- just a sample; feel free to replace with your own structure
tests = testGroup "Unit tests"
  [ Util.tests
  , Parser.tests
  , Solver.tests
  ]

main = defaultMain tests


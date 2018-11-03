module TestSolver where

import Test.Tasty
import Test.Tasty.HUnit

import Defs
import Solver (install)


tests :: TestTree
tests = testGroup "Solver tests"
    [ testCase "tiny" $
       install db1 pname1 @?= Just [(pname1, ver1)]
    ]


-- Test 1
db1 = DB [Pkg pname1 ver1 "" []]
pname1 = P "foo"
ver1 = V [VN 1 ""]

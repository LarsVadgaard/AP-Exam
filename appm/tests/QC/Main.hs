module Main where

import Defs
import Properties
import Solver (install)

import Test.Tasty
import Test.Tasty.QuickCheck

-- The following is just a sample; feel free to replace with your
-- own structure

instance Arbitrary PName where
  -- not very random...
  arbitrary = elements [P "foo", P "bar"]

instance Arbitrary Database where
  -- even less so
  arbitrary = return $ DB [Pkg (P "foo") (V [VN 1 ""]) "" []]

prop_install_c' db p = install_c' db p (install db p)
  
tests = testGroup "QC tests"
          [testProperty "simple" $ prop_install_c']

main = defaultMain tests

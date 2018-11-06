module Main where

import Gen

import Defs
import Properties
import Solver (install)

import Test.Tasty
import Test.Tasty.QuickCheck


----------
-- Testing
----------

prop_install_a (DB db) = let pkg = head db in install_a (DB db) (name pkg) (install (DB db) $ name pkg)
prop_install_b (DB db) = let pkg = head db in install_b (DB db) (name pkg) (install (DB db) $ name pkg)
prop_install_c (DB db) = let pkg = head db in install_c (DB db) (name pkg) (install (DB db) $ name pkg)
prop_install_d (DB db) = let pkg = head db in install_d (DB db) (name pkg) (install (DB db) $ name pkg)
prop_install_e (DB db) = let pkg = head db in install_e (DB db) (name pkg) (install (DB db) $ name pkg)
prop_install_f (DB db) = let pkg = head db in install_f (DB db) (name pkg) (install (DB db) $ name pkg)
prop_install_g (DB db) = let pkg = head db in install_g (DB db) (name pkg) (install (DB db) $ name pkg)

prop_parse_db  = parses_db

tests = testGroup "QC tests"
          [ testProperty "prop a"  prop_install_a
          , testProperty "prop b"  prop_install_b
          , testProperty "prop c"  prop_install_c
          , testProperty "prop d"  prop_install_d
          , testProperty "prop e"  prop_install_e
          , testProperty "prop f"  prop_install_f
          , testProperty "prop g"  prop_install_g
          , testProperty "parsing" prop_parse_db
          ]

main = defaultMain tests

module Main where

import Defs
import Properties
import Solver (install)

import Test.Tasty
import Test.Tasty.QuickCheck

import Control.Monad (foldM, (<=<))

import Data.Char (isLower)

-- The following is just a sample; feel free to replace with your
-- own structure

lowers = ['a'..'z']
uppers = ['A'..'Z']
digits = ['1'..'9']
letters   = lowers ++ uppers
alphanums = letters ++ digits

-- PNames
instance Arbitrary PName where
  arbitrary = P <$> pname

pname = oneof [simple, general]

simple  = do
  first <- choose ('a','z')
  after <- resize 8 $
                listOf $ frequency
                  [ (5, do {
                          l <- elements letters;
                          return [l]             })
                  , (3, do {
                          l <- elements digits;
                          return [l]             })
                  , (1, do {
                          l <- elements alphanums;
                          return ['-',l]          })
                  ]
  return $ first : concat after

general = resize 10 $ listOf1 arbitrary


-- Versions
instance Arbitrary VNum where
  arbitrary = do
    i <- sized $ \n -> choose (1,n*2)
    suf <- frequency [(5, return ""),(1,lowerString4)]
    return $ VN i suf

    where lowerString4 = resize 4 (listOf1 (elements lowers))

instance Arbitrary Version where
  arbitrary = V <$> resize 5 (listOf1 arbitrary)


-- Packages
instance Arbitrary Pkg where
  arbitrary = do
    p <- arbitrary
    v <- arbitrary
    desc <- resize 15 $ listOf1 arbitrary
    return $ Pkg p v desc []


-- The database
instance Arbitrary Database where
  arbitrary = DB <$> (genDeps <=< trim <=< listOf) arbitrary

-- remove duplicates even though the chance of such is tiny
trim :: [Pkg] -> Gen [Pkg]
trim (Pkg p c d1 d2:pkgs) = do
  trimmed <- trim $ filter (\(Pkg p' _ _ _) -> p /= p') pkgs
  return $ Pkg p c d1 d2 : trimmed
trim [] = return []

-- generating dependencies
genDeps :: [Pkg] -> Gen [Pkg]
genDeps pkgs =
  mapM (\(Pkg p v d _) -> do
    pdeps <- take 5 <$> sublistOf pkgs
    cs <- foldM (\cs (Pkg p v _ _) -> do
        v1 <- arbitrary
        v2 <- arbitrary
        b  <- arbitrary
        return $ (p, (b, min v1 v2, max v1 v2)) : cs
      ) [] pdeps
    return $ Pkg p v d cs
  ) pkgs


-- Testing
prop_install_a db p = install_a db p (install db p)
prop_install_b db p = install_b db p (install db p)
prop_install_c db p = install_c db p (install db p)
prop_install_d db p = install_d db p (install db p)

tests = testGroup "QC tests"
          [ testProperty "simple" prop_install_a
          , testProperty "simple" prop_install_b
          , testProperty "simple" prop_install_c
          , testProperty "simple" prop_install_d
          ]

main = defaultMain tests

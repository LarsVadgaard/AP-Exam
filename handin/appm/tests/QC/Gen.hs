module Gen where

import Defs
import Test.Tasty
import Test.Tasty.QuickCheck

import Control.Monad (foldM, (<=<))
import Utils (sortPkgs, merge)

import Data.List (nub,nubBy,delete)


-- characters
lowers = ['a'..'z']
uppers = ['A'..'Z']
digits = ['1'..'9']
letters   = lowers ++ uppers
alphanums = letters ++ digits


-- package names
instance Arbitrary PName where
  arbitrary = P <$> simple

-- we only generate simple names
simple  = do
  first <- elements lowers
  after <- resize 7 $ listOf $ frequency
      [ (5, do { l <- elements letters;
                 return [l]              })
      , (2, do { l <- elements digits;
                 return [l]              })
      , (1, do { l <- elements alphanums;
                 return ['-',l]          })
      ]
  return $ first : concat after


-- versions numbers
instance Arbitrary VNum where
  arbitrary = do
    i <- sized $ \n -> choose (1,n)
    suf <- frequency [(5, return ""),(1,lowerString4)]
    return $ VN i suf

    where lowerString4 = resize 4 (listOf1 (elements lowers))


-- actual versions
instance Arbitrary Version where
  arbitrary = V <$> resize 5 (listOf1 arbitrary)


-- packages
instance Arbitrary Pkg where
  arbitrary = do
    v <- frequency [(4,arbitrary),(1,return stdV)]
    desc <- frequency [ (3, return "")
                      , (1,take <$> choose (1,15)
                                <*> listOf1 arbitraryASCIIChar) ]
    return $ Pkg (P "") v desc []


-- the database
instance Arbitrary Database where
  arbitrary = DB . sortPkgs
          <$> ( genAllDeps
            <=< trimDatabase
            <=< genNames
            <=< listOf1
           ) arbitrary
  shrink (DB db) = DB <$> shrink db

-- generate names for packages
genNames :: [Pkg] -> Gen [Pkg]
genNames pkgs = do
  validPNames <- nub <$> vectorOf (length pkgs) arbitrary
  mapM (\pkg -> do
         name' <- elements validPNames
         return $ pkg{name = name'}
       ) pkgs

-- remove duplicates
trimDatabase :: [Pkg] -> Gen [Pkg]
trimDatabase (pkg:pkgs) =
  let rest = filter (\pkg' -> name pkg /= name pkg' || ver pkg /= ver pkg') pkgs
  in (:) pkg <$> trimDatabase rest
trimDatabase [] = return []

-- generating dependencies
genAllDeps :: [Pkg] -> Gen [Pkg]
genAllDeps pkgs =
  mapM (\pkg -> do
    cs <- pkg `genAvailDeps` pkgs
    return $ pkg{deps = cs}
  ) pkgs

genAvailDeps :: Pkg -> [Pkg] -> Gen Constrs
genAvailDeps pkg pkgs = do
      n <- choose (0,4)
      pkgs' <- take n <$> sublistOf (filter (\pkg' -> name pkg' /= name pkg) pkgs)
      cs <- mapM (\pkg' -> do
          b <- arbitrary
          (vmin,vmax) <- genRange (ver pkg')
          return (name pkg',(b,vmin,vmax))
        ) pkgs'
      case mergeMultiple cs of
        Nothing     -> return []
        Just merged -> return merged

-- merge individual constraints in a constraint list
mergeMultiple :: Constrs -> Maybe Constrs
mergeMultiple [] = Just []
mergeMultiple (c:cs) = foldM (\cs' c' -> merge cs' [c']) [] cs

-- generate version range
genRange :: Version -> Gen (Version, Version)
genRange v = frequency [(4,do
      vmin <- suchThat arbitrary (<= v)
      vmax <- suchThat arbitrary (> v)
      return (vmin,vmax))
    , (1, suchThat arbitrary (uncurry (<)))]

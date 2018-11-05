module SolverImpl where

-- Put your solver implementation in this file.
-- Do not change the types of the following exported functions

import Control.Monad
import Control.Arrow ((&&&)) -- just a small convenience

import Data.Maybe (mapMaybe)
import Data.List (sort, partition, groupBy)

import Defs
import Utils


--------------
-- Normalizing
--------------

-- for each package, find all packages with the same name,
-- check that they are semantically equivalent, group them
-- together, and run recursively on the rest of rest
normalize :: Database -> Either String Database
normalize (DB db) =
  let groups = groupSame db
  in if all allEquiv groups
     then return $ (DB . sortPkgs . map head) groups
     else Left "The database is not consistent"

-- equivalence between all packages in a list
allEquiv :: [Pkg] -> Bool
allEquiv (pkg:pkgs) =
  foldl (\b pkg' -> b && pkg' `equiv` pkg) True pkgs
allEquiv [] = False -- shouldn't ever happen

-- equivalence between two packages
equiv :: Pkg -> Pkg -> Bool
equiv pkg1 pkg2 =
  name pkg1 == name pkg2 &&
  desc pkg1 == desc pkg2 &&
  sort (deps pkg1) == sort (deps pkg2)


-------------------------
-- Solving and installing
-------------------------

-- solving constraints given a solution
solve :: Database -> Constrs -> Sol -> [Sol]
solve _ cs sol | sol `satisfies` cs = [sol]
solve db cs sol =
  let req     = getRequired cs sol db
      groups  = groupName req
      combos  = sequence groups
      sat     = getSats cs sol combos
  in concatMap (uncurry $ solve db) sat


-- installing:
-- fetch all versions of the wanted package and find
-- all solutions for each. then fetch the first sat-
-- isfying solution - if one exists
install :: Database -> PName -> Maybe Sol
install db p = do
  cands <- getWithName p db
  sols  <- getSols db cands
  return $ head sols

-- get all solutions for a list of packages to be installed
getSols :: Database -> [Pkg] -> Maybe [Sol]
getSols db pkgs =
  let sols = map (\pkg ->
          solve db (deps pkg) [(name pkg, ver pkg)]
        ) pkgs
  in case concat sols of
      []   -> Nothing
      sols -> Just sols


------------------------------
-- Secondary utility functions
------------------------------

-- get required packages from the context
getRequired :: Constrs -> Sol -> Database -> [Pkg]
getRequired cs sol (DB pkgs) = filter (\pkg ->
      pkg `isRequired` cs && not (pkg `inSol` sol)
    ) pkgs

-- get all packages with given name in the database
getWithName :: PName -> Database -> Maybe [Pkg]
getWithName p (DB db) =
  case filter (\pkg -> p == name pkg) db of
    []   -> Nothing
    pkgs -> Just pkgs

-- grouping packages with same name
groupName :: [Pkg] -> [[Pkg]]
groupName = groupBy (\pkg1 pkg2 -> name pkg1 == name pkg2)

-- grouping packages with same name and version
groupSame :: [Pkg] -> [[Pkg]]
groupSame = groupBy (\pkg1 pkg2 ->
            name pkg1 == name pkg2 &&
            ver  pkg1 == ver  pkg2)

-- for a list of lists of packages, convert the lists of packages
-- that can be added consistently to a constraint/solution tuple
getSats :: Constrs -> Sol -> [[Pkg]] -> [(Constrs,Sol)]
getSats cs sol = mapMaybe (`takeIfSat` (cs,sol))

  where takeIfSat pkgs (cs,sol) | newsol <- sol ++ toSol pkgs = do
              unless (newsol `satisfies` cs) Nothing
              cs' <- (mergeAll cs . map deps) pkgs
              return (cs',newsol)

-- merge all constraints
mergeAll :: Constrs -> [Constrs] -> Maybe Constrs
mergeAll = foldM merge

-- convert a list of packages to a solution
toSol :: [Pkg] -> Sol
toSol = map (name &&& ver)

-- check if a package is required by a set of constraints
isRequired :: Pkg -> Constrs -> Bool
isRequired pkg = any (\(p,(b,_,_)) ->
              b && name pkg == p)

-- check if a package is already in the solution
inSol :: Pkg -> Sol -> Bool
inSol pkg = any (\(p,_) -> p == name pkg)

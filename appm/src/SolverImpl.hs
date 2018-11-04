module SolverImpl where

-- Put your solver implementation in this file.
-- Do not change the types of the following exported functions

import qualified Data.Map as M

import Control.Monad (foldM,when)
import Control.Arrow ((&&&))

import Debug.Trace

import Data.Ord (comparing, Ordering(..))
import Data.List (sort, sortBy, minimumBy)

import Defs
import Utils


--------------
-- Normalizing
--------------
normalize :: Database -> Either String Database
normalize (DB db) = DB <$> normalize' db

normalize' :: [Pkg] -> Either String [Pkg]
normalize' (pkg:db) = do
  (eq,rest) <- normalize'' pkg db ([pkg],[])
  (++) (sortBy (flip compare) eq) <$> normalize' rest
normalize' [] = return []

normalize'' _ [] buf = Right buf
normalize'' pkg (pkg':db) (eq,rest) | name pkg == name pkg' =
  if pkg `equiv` pkg'
  then normalize'' pkg db (pkg':eq,rest)
  else Left $ (show . name) pkg ++ " is not consistent"
normalize'' pkg (pkg':db) (eq,rest) = normalize'' pkg db (eq,rest++[pkg']) -- append not necessary, just keeps the order and makes it easier to do simple tests

equiv :: Pkg -> Pkg -> Bool
equiv (Pkg p1 _ _ deps1) (Pkg p2 _ _ deps2) =
  sort deps1 == sort deps2 -- sort to match the individual constraints


-------------
-- The Solver
-------------

solve :: Database -> Constrs -> Sol -> [Sol]
solve (DB db) cs sol | sol `solSatisfies` cs = [sol]
solve (DB db) cs sol =
  let cand   = filter (not . (`inSol` sol)) db
      combos = subsets cand
      sat = foldl (\spkgs combo -> case combo `pkgsSatisfy` cs of
              Nothing -> spkgs
              Just cs'-> (combo,cs') : spkgs
            ) [] combos
      m = foldl (\s (pkgs,cs') ->
            s ++ solve (DB db) cs' (toSol pkgs ++ sol)
          ) [] sat
  in sortBy (flip quality) m


install :: Database -> PName -> Maybe Sol
install (DB db) p = do
  let cands = filter (\pkg -> name pkg == p) db
  when (null cands) Nothing
  let pkg  = (maximum . filter (\pkg -> name pkg == p)) cands
      sols = solve (DB db) (deps pkg) [(name pkg, ver pkg)]
  when (null sols) Nothing
  return $ head sols


--------------------
-- Utility functions
--------------------

-- an ordering for quality of solutions
-- better orderings are greater
quality :: Sol -> Sol -> Ordering
quality sol1 sol2 | length sol1 < length sol2 = GT
quality sol1 sol2 | length sol1 > length sol2 = LT
quality sol1 sol2 =
  let zipped  = zip (sort sol1) (sort sol2)
      (lt,gt) = foldl (\(lt,gt) (l,r) ->
                        if l < r
                        then (lt+1,gt)
                        else if l > r
                        then (lt,gt+1)
                        else (lt,gt)
                ) (0,0) zipped
  in if lt > gt then LT
     else if lt < gt  then GT
     else EQ


-- check if a package is already in the solution
inSol :: Pkg -> Sol -> Bool
inSol pkg = elem (name pkg,ver pkg)

-- convert a list of packages to a solution
toSol :: [Pkg] -> Sol
toSol = map (name &&& ver)


-- generate a list of subsets
subsets :: [a] -> [[a]]
subsets []  = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)


-- check if a list af packages satisfies some constraints
-- if it does, merge all the constraints and return the
-- resulting constraint list
pkgsSatisfy :: [Pkg] -> Constrs -> Maybe Constrs
pkgsSatisfy pkgs cs =
  let cs' = mergeAll pkgs cs
  in if all (pkgs `pkgsSatisfy'`) cs then cs' else Nothing

  where mergeAll pkgs cs = foldM merge cs $ map deps pkgs

pkgsSatisfy' :: [Pkg] -> (PName,PConstr) -> Bool
pkgsSatisfy' pkgs (p,(b,vmin,vmax)) | b =
  any ((`reqCheck` (p,vmin,vmax)) . (name &&& ver)) pkgs
pkgsSatisfy' pkgs (p,(_,vmin,vmax)) =
  not $ any ((`conflictCheck` (p,vmin,vmax)) . (name &&& ver)) pkgs


-- check if a solution satisfies some constraints
solSatisfies :: Sol -> Constrs -> Bool
solSatisfies sol = all (sol `solSatisfies'`)

solSatisfies' :: Sol -> (PName,PConstr) -> Bool
solSatisfies' sol (p,(b,vmin,vmax)) | b =
  any (`reqCheck` (p,vmin,vmax)) sol
solSatisfies' sol (p,(_,vmin,vmax)) =
  not $ any (`conflictCheck` (p,vmin,vmax)) sol

reqCheck (p',v) (p,vmin,vmax) =
  p' == p && v >= vmin && v < vmax
conflictCheck (p',v) (p,vmin,vmax) =
  p' == p && (v < vmin || v >= vmax)

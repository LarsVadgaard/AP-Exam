module SolverImpl where

-- Put your solver implementation in this file.
-- Do not change the types of the following exported functions

import qualified Data.Map as M

import Control.Monad
import Control.Arrow ((&&&)) -- just a small convenience

import Data.Maybe (mapMaybe)
import Data.Ord (comparing, Ordering(..))
import Data.List (sort, sortBy, minimumBy, groupBy, partition)

import Defs
import Utils


--------------
-- Normalizing
--------------

-- for each package, find all packages with the same name,
-- check that they are semantically equivalent, and group
-- them together
normalize :: Database -> Either String Database
normalize (DB db) = DB <$> normalize' db

-- helper to avoid handling the DB-constructor
normalize' :: [Pkg] -> Either String [Pkg]
normalize' (pkg:db) = do
  (eq,rest) <- normalize'' pkg db ([pkg],[])
  let eq' = sortBy (flip compare) eq
    in (++) eq' <$> normalize' rest
normalize' [] = return []

-- helper to do so for each package
normalize'' _ [] buf = return buf
normalize'' pkg (pkg':db) (eq,rest)
      | name pkg == name pkg' && ver pkg == ver pkg' =
  if pkg `equiv` pkg'
  then normalize'' pkg db (pkg':eq,rest)
  else Left $ (show . name) pkg ++ " is not consistent"
normalize'' pkg (pkg':db) (eq,rest) = normalize'' pkg db (eq,pkg':rest)

-- sort to match the individual constraints;
-- the internal constraints have been merged
-- when parsed, so we don't need to do anything
-- special to check deeper semantic equivalence
equiv :: Pkg -> Pkg -> Bool
equiv pkg1 pkg2 = desc pkg1 == desc pkg2 &&
                  sort (deps pkg1) == sort (deps pkg2)


-------------
-- The Solver
-------------

type Context = ([Sol],Database)

newtype Solver a = S { runSolver :: Context -> (a,[Sol])}

execSolver :: Solver a -> Context -> [Sol]
execSolver act = snd . runSolver act

instance Monad Solver where
  return a = S $ \(s,_) -> (a,s)
  m >>= f = S $ \(s,db) ->
    let (a,s') = runSolver m (s,db)
    in runSolver (f a) (s',db)

instance Functor Solver where
   fmap = liftM

instance Applicative Solver where
   pure  = return
   (<*>) = ap

get :: Solver Database
get = S $ \(s,db) -> (db,s)

put :: Sol -> Solver ()
put s' = S $ \(s,_) -> ((), s':s)


-- base case: the solution satisfies the constraints
-- else, compute all subset of required packages and
-- check each bruteforce - we do this recursively
--solve :: Database -> Constrs -> Sol -> [Sol]
--solve _ cs sol | sol `satisfies` cs = [sol]
--solve (DB db) cs sol =
--  let req     = filter (\pkg -> pkg `isRequired` cs && not (pkg `inSol` sol)) db
--      groups  = groupByName req
--      combos  = sequence groups
--      sat     = mapMaybe (`takeIfSat` (cs,sol)) combos
--      branch  = concatMap (uncurry $ solve (DB db)) sat
--  in sortBy (flip compare) branch
solve :: Constrs -> Sol -> Solver ()
solve cs sol | sol `satisfies` cs = put sol
solve cs sol = do
  DB db <- get
  let req     = filter (\pkg -> pkg `isRequired` cs && not (pkg `inSol` sol)) db
      groups  = groupByName req
      combos  = sequence groups
      sat     = mapMaybe (`takeIfSat` (cs,sol)) combos
  mapM_ (uncurry solve) sat


-- fetch all versions of the wanted package and find
-- all solutions for each. then fetch the first sat-
-- isfying solution - if one exists
install :: Database -> PName -> Maybe Sol
install db p = do
  cands <- getPkgs p db
  sols  <- getSols db cands
  return $ head sols


--------------------
-- Utility functions
--------------------

-- get all packages with given name
getPkgs :: PName -> Database -> Maybe [Pkg]
getPkgs p (DB db) =
  case sortBy (flip $ comparing ver) . filter ((p ==) . name) $ db of
    []   -> Nothing
    pkgs -> Just pkgs

-- get all solutions for a list of packages to be installed
getSols :: Database -> [Pkg] -> Maybe [Sol]
getSols db pkgs =
  case concatMap (\pkg -> execSolver (solve (deps pkg) [(name pkg, ver pkg)]) ([],db)) pkgs of
    []   -> Nothing
    sols -> Just sols

-- group all packages by name apparently 'group' and 'groupBy' don't have this
-- exact behaviour
groupByName :: [Pkg] -> [[Pkg]]
groupByName [] = []
groupByName (pkg:pkgs) =
  let (g,r) = partition ((name pkg ==) . name) pkgs
  in sort (pkg:g) : groupByName r

-- check if a list of packages can be added consistently
-- if so, return the new partial solution and the constraints
takeIfSat :: [Pkg] -> (Constrs, Sol) -> Maybe (Constrs,Sol)
takeIfSat pkgs (cs,sol) | newsol <- sol ++ toSol pkgs = do
  unless (newsol `satisfies` cs) Nothing
  cs' <- mergeAll pkgs cs
  return (cs',newsol)

  where mergeAll pkgs cs = foldM merge cs $ map deps pkgs

-- check if a package is required by a set of constraints
isRequired :: Pkg -> Constrs -> Bool
isRequired pkg = any (\(p,(b,_,_)) ->
              b && name pkg == p)

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
inSol pkg = any (\(p,_) -> p == name pkg)

-- convert a list of packages to a solution
toSol :: [Pkg] -> Sol
toSol = map (name &&& ver)

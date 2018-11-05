module Properties where

import Defs
import Utils
import Parser
import Solver

import Control.Monad (foldM,mapM)

import Data.List (partition, delete, sort)

type InstallProp = Database -> PName -> Maybe Sol -> Bool

---------------------
-- Install properties
---------------------

-- a) all packages (with the indicated versions) are actually available in the
-- database
install_a :: InstallProp
install_a _ _ Nothing   = True
install_a _db _ (Just sol) = all (`isInDB` _db ) sol


-- b) any package name may only occur once in the list; in particular, it is not
-- possible to install two different versions of the same package
-- simultaneously
install_b :: InstallProp
install_b _ _ Nothing = True
install_b _db _p (Just sol) =
  let groups = groupByName sol
  in not . any (\group -> length group > 1) $ groups


-- c) the package requested by the user is in the list
install_c :: InstallProp
install_c _ _ Nothing = True
install_c _db _p (Just sol) = any (\(p,_) -> _p == p) sol


-- d) for any package in the list, all the packages it requires are also in the
-- list
install_d :: InstallProp
install_d _ _ Nothing = True
install_d _db _p (Just sol) =
  all (\pv -> case getReqs pv _db of
          Nothing   -> False
          Just reqs -> all (`isReqInSol` sol) reqs
      ) sol


-- e) for any package in the list, all other packages in the list should
-- satisfy it
install_e _ _ Nothing = True
install_e _db _p (Just sol) =
  all (\pv -> sat (delete pv sol) pv _db) sol

-- does the solution satisfy the constraints of a given package?
sat :: Sol -> (PName, Version) -> Database -> Bool
sat sol pv db = case getReqs pv db of
  Nothing -> False
  Just cs -> sol `satisfies` cs


-- f) you should not be able to remove a package without breaking consistency
install_f _ _ Nothing = True
install_f _db _p (Just sol) =
  all (\pv -> fst pv == _p || -- dont remove the package we want to install
      let sol' = delete pv sol
      in case genConstrs _db sol' of
        Nothing -> True
        Just cs -> not $ sol' `satisfies` cs
    ) sol


-- g) you should not be able to replace a package with a newer version without
-- breaking consistency
install_g _ _ Nothing = True
install_g _db _p (Just sol) =
  all (\pv -> case getNewerVers pv _db of
      []   -> True
      pkgs ->
        let sol' = delete pv sol
        in all (\pv -> let sol'' = pv:sol' in
             case genConstrs _db sol'' of
               Nothing -> True
               Just cs -> not $ sol'' `satisfies` cs ) pkgs
      ) sol -- so sorry for this layout


--------------------
-- Parser properties
--------------------

parses_db _db = case parseDatabase (prettyDB _db) of
  Right db -> db `dbEquiv` _db
  Left _ -> False

dbEquiv :: Database -> Database -> Bool
dbEquiv (DB db1) (DB db2) =
  length db1 == length db2 &&
    all (\(pkg1,pkg2) ->
            name pkg1 == name pkg2 &&
            ver pkg1 == ver pkg2   &&
            desc pkg1 == desc pkg2 &&
            sort (deps pkg1) == sort (deps pkg2)
        ) (zip (sort db1) (sort db2))


--------------------
-- Utility functions
--------------------

-- group solution tuples by name
groupByName :: [(PName,Version)] -> [[(PName,Version)]]
groupByName [] = []
groupByName ((p,v):sol) =
  let (g,r) = partition ((p ==) . fst) sol
  in ((p,v):g) : groupByName r

-- is a given package name and version in the database?
isInDB :: (PName,Version) -> Database -> Bool
isInDB (p,v) (DB db) = any (\pkg -> name pkg == p && ver pkg == v) db

-- is a given package required?
isReqInSol :: (PName,PConstr) -> Sol -> Bool
isReqInSol (p,(b, vmin, vmax)) sol | b =
  any (\(p',v) -> p' == p  && v >= vmin && v < vmax) sol
isReqInSol _ _ = True

-- get requirements of a single package
getReqs :: (PName,Version) -> Database -> Maybe Constrs
getReqs _ (DB []) = Nothing
getReqs (p,v) (DB (pkg':ps))
  | name pkg' == p && ver pkg' == v =
      Just $ deps pkg'
getReqs (p,v) (DB (_:ps)) = getReqs (p,v) (DB ps)

-- generate constraints from a solution
genConstrs :: Database -> Sol -> Maybe Constrs
genConstrs db sol =
  foldM merge [] =<< mapM (`getReqs` db) sol

-- get a list of newer versions of a given package
getNewerVers :: (PName,Version) -> Database -> [(PName,Version)]
getNewerVers _ (DB []) = []
getNewerVers (p,v) (DB (pkg':ps))
  | name pkg' == p && ver pkg' > v =
      (name pkg', ver pkg') : getNewerVers (p,v) (DB ps)
getNewerVers pv (DB (_:ps)) = getNewerVers pv (DB ps)

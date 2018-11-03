module Properties where

import Defs

type InstallProp = Database -> PName -> Maybe Sol -> Bool

-- for reference; may discard after implementing full install_c
install_c' :: InstallProp
install_c' _db _p Nothing = True
install_c' _db _p (Just []) = False
install_c' _db _p (Just _) = True

-- if you don't implement one or more of these, leave them as 'undefined'

-- all packages (with the indicated versions) are actually available in the
-- database
install_a :: InstallProp
install_a _ _ Nothing   = True
install_a _ _ (Just []) = True
install_a _db _p (Just ((p,_):sol)) = case getReqs p _db of
  Just reqs -> all (`isInDB` _db) reqs && install_a _db _p (Just sol)
  Nothing   -> False

isInDB :: (PName,PConstr) -> Database -> Bool
isInDB (p,(True, vmin, vmax)) (DB db) = any (\pkg -> name pkg == p && ver pkg >= vmin && ver pkg < vmax) db
isInDB _ _ = True

-- any package name may only occur once in the list; in particular, it is not
-- possible to install two different versions of the same package
-- simultaneously
install_b :: InstallProp
install_b _ _ Nothing               = True
install_b _ _ (Just [])             = True
install_b _db _p (Just ((p,_):sol)) = count p sol == 0 && install_b _db _p (Just sol)

count p = foldl (\c (p',_) -> if p == p' then c+1 else c) 0

-- the package requested by the user is in the list
install_c :: InstallProp
install_c _ _ Nothing       = True
install_c _db _p (Just sol) = any (\(p,_) -> p == _p) sol

-- for any package in the list, all the packages it requires are also in the
-- list
install_d :: InstallProp
install_d _ _ Nothing    = True
install_d _ _ (Just [])  = True
install_d _db _ (Just ((p,v):sol)) = case getReqs p _db of
  Just reqs -> all (`isInSol` sol) reqs
  Nothing   -> False

isInSol :: (PName,PConstr) -> Sol -> Bool
isInSol (p,(True, vmin, vmax)) sol = any (\(p',v) -> p' == p && v >= vmin && v < vmax) sol
isInSol _ _ = True


-- getting requirements; shared between a) and d)
getReqs :: PName -> Database -> Maybe Constrs
getReqs p (DB [])     = Nothing
getReqs p (DB (pkg':ps)) | name pkg' == p = Just $ deps pkg'
getReqs p (DB (_:ps)) = getReqs p (DB ps)

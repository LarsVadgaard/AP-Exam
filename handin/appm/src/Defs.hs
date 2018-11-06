module Defs where

import Data.List (intercalate)


--------
-- Types
--------

type ErrMsg = String -- for all human-readable error messages

newtype PName = P String
  deriving (Eq, Ord, Show, Read)

data VNum = VN Int String
  deriving (Eq, Show, Read, Ord)

newtype Version = V [VNum]
  deriving (Eq, Show, Read, Ord) -- the default ordering works perfectly

minVN, maxVN, stdVN :: Int
stdVN = 1
minVN = 0
maxVN = 1000000

minV, maxV, stdV :: Version
minV = V [VN minVN ""]  -- inclusive lower bound
maxV = V [VN maxVN ""]  -- exclusive upper bound
stdV = V [VN stdVN ""]

type PConstr = (Bool, Version, Version) -- req'd; allowed interval [lo,hi)
type Constrs = [(PName, PConstr)]

data Pkg = Pkg {name :: PName,
                ver :: Version,
                desc :: String,
                deps :: Constrs}
  deriving (Eq, Show, Read)

newtype Database = DB [Pkg]
  deriving (Eq, Read)

type Sol = [(PName, Version)]


----------------
-- Own instances
----------------

-- define package ordering
instance Ord Pkg where
  l <= r = (name l < name r) || (name l == name r && ver l <= ver r)

-- define the database to be pretty printed using show
instance Show Database where
  show = prettyDB


-------------------------------
-- Pretty printer for databases
-------------------------------

-- used to quickcheck the parser and better read output
-- not a pretty implementation, but the output is OK

prettyDB :: Database -> String
prettyDB (DB pkgs) = (intercalate "\n\n" . map prettyPkg) pkgs

prettyPkgs :: [Pkg] -> String
prettyPkgs = intercalate "\n\n" . map prettyPkg

prettyPkg :: Pkg -> String
prettyPkg (Pkg n v des dep) =
  "package {\n" ++
  "  name " ++ prettyPName n ++ ";\n"   ++
     (if v /= stdV then "  version " ++ prettyVer v   ++ ";\n" else "") ++
     (if null des then "" else "  description \"" ++ escape des ++ "\";\n") ++
      prettyConstrs dep ++
  "}"

  where escape = concatMap escapeChar
        escapeChar '"' = "\"\""
        escapeChar c = [c]

prettyPName :: PName -> String
prettyPName (P s) = s

prettyVer :: Version -> String
prettyVer (V vnums) = intercalate "." . map prettyVNum $ vnums

prettyVNum :: VNum -> String
prettyVNum (VN n s) = show n ++ s

prettyConstrs :: Constrs -> String
prettyConstrs = concatMap prettyConstr

prettyConstr :: (PName,PConstr) -> String
prettyConstr (p,(b,vmin,vmax)) =
  (if b then "  requires " else "  conflicts ")  ++
  prettyPName p ++ (if b && vmin > minV then " >= " else " < ") ++
  prettyVer vmin ++ ";\n" ++
  (if b then "  requires " else "  conflicts ")  ++
  prettyPName p ++ (if b then " < " else " >= ") ++
  prettyVer vmax ++ ";\n"

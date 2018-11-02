module Utils where

-- Any auxiliary code to be shared by Parser, Solver, or tests
-- should be placed here.

import Defs

import qualified Data.Map.Strict as M


instance Ord Version where
  -- Made VNum an instance of Ord, should work fine
  (V v1) <= (V v2) = v1 <= v2

merge :: Constrs -> Constrs -> Maybe Constrs
merge c1 c2 = return $ foldl merge' (Just c1) c2

merge' cs (pname, constr) = merge'' pname constr m

merge'' (b1, vmin1, vmax1) (b2, vmin2, vmax2) = Just (b1 || b2, vmin1, vmax2)

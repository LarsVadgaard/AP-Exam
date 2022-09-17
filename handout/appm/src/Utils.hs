module Utils where

-- Any auxiliary code to be shared by Parser, Solver, or tests
-- should be placed here.

import Defs

instance Ord Version where
  (<=) = undefined   -- or define 'compare' instead

merge :: Constrs -> Constrs -> Maybe Constrs
merge = undefined

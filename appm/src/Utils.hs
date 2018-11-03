module Utils where

-- Any auxiliary code to be shared by Parser, Solver, or tests
-- should be placed here.

import Defs

import Control.Monad
import Data.List


--instance Ord Version where
--  -- Made VNum an instance of Ord, should work fine
--  (V v1) <= (V v2) = v1 <= v2 -- Maybe just derive Ord then?

merge :: Constrs -> Constrs -> Maybe Constrs
merge c1 c2 = foldM merge' [] (c1 ++ c2)

merge' ((n,c):lst) (n',c') | n == n' = do
  new <- merge'' c c'
  return $ (n,new) : lst
merge' (c:lst) c' = (:) c <$> merge' lst c'
merge' [] c = return [c]

merge'' (b1, vmin1, vmax1) (b2, vmin2, vmax2) =
  let (vmin',vmax') = (max vmin1 vmin2, min vmax1 vmax2)
  in if vmax' <= vmin'
     then Nothing
     else return (b1 || b2, vmin', vmax')

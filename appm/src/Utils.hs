module Utils where

-- Any auxiliary code to be shared by Parser, Solver, or tests
-- should be placed here.

import Defs

import Control.Monad
import Data.List


instance Ord Version where
  -- Made VNum an instance of Ord, should work fine
  (V v1) <= (V v2) = v1 <= v2 -- Maybe just derive Ord then?

--merge :: Constrs -> Constrs -> Maybe Constrs
--merge c1 c2 =
--  let merged = filter (\(_,(b,min,max)) -> b || max <= min) $ M.toList $ foldl merge' (M.fromList c1) c2
--  in if any (\(_,(_,min,max)) -> max <= min) merged
--     then Nothing
--     else Just merged
--

merge :: Constrs -> Constrs -> Maybe Constrs
merge c1 c2 =
  let (req,con) = partition (\(_,(b,_,_)) -> b) (c1 ++ c2)
  in foldM merge' [] req >>= (\mrg -> foldM merge' mrg con)
    --return $ filter (\(_,(b,_,_)) -> b) merged

merge' ((n,c):lst) (n',c') | n == n' = do
  new <- merge'' c c'
  return $ (n,new) : lst
merge' (c:lst) c' = (:) c <$> merge' lst c'
merge' [] c = return [c]

-- Both are conflicts or required
merge'' (b1, vmin1, vmax1) (b2, vmin2, vmax2) | b1 == b2 =
  let (vmin',vmax') = (max vmin1 vmin2, min vmax1 vmax2)
  in if vmax' <= vmin'
     then Nothing
     else return (b1, vmin', vmax')

-- Both are conflicts
--merge'' (False, vmin1, vmax1) (False, vmin2, vmax2) =
--  let (vmin',vmax') = (max vmin1 vmin2, min vmax1 vmax2)
--  in if vmax' <= vmin' -- might not be necessary
--     then Nothing
--     else return (False, vmin', vmax')

-- One is a conflict, the other is required
merge'' (b1, vmin1, vmax1) (b2, vmin2, vmax2) | b1 =
  let (vmin',vmax') = if vmax1 > vmax2 then (max vmin1 vmax2, vmax1) else (vmin1, min vmax1 vmin2)
  in if vmax' <= vmin'
     then Nothing
     else return (b1, vmin', vmax')
merge'' c1 c2 = merge'' c2 c1

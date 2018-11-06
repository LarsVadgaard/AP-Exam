module Utils where

import Defs

import Control.Monad (foldM)

import Data.List (sortBy)


-- merging constraint lists
merge :: Constrs -> Constrs -> Maybe Constrs
merge c1 c2 = foldM merge' [] (c1 ++ c2)

merge' :: Constrs -> (PName,PConstr) -> Maybe Constrs
merge' ((p,c):lst) (p',c') | p == p' = do
  new <-  c `intersection` c'
  return $ (p,new) : lst
merge' (c:lst) c' = (:) c <$> merge' lst c'
merge' [] c = return [c]

-- compute the intersection between two version ranges
intersection :: PConstr -> PConstr -> Maybe PConstr
intersection (b1, vmin1, vmax1) (b2, vmin2, vmax2) =
  let (vmin',vmax') = (max vmin1 vmin2, min vmax1 vmax2)
  in if vmax' <= vmin'
     then Nothing
     else return (b1 || b2, vmin', vmax')


-- check if a solution satisfies some constraints
satisfies :: Sol -> Constrs -> Bool
satisfies sol = all (sol `satisfies'`)

satisfies' :: Sol -> (PName,PConstr) -> Bool
satisfies' sol (p,(b,vmin,vmax)) | b =
  any (\(p',v) -> p' == p && (v >= vmin && v < vmax)) sol
satisfies' sol (p,(_,vmin,vmax)) =
  not $ any (\(p',v) -> p' == p && (v < vmin || v >= vmax)) sol


-- sorting packages (newer version first)
sortPkgs :: [Pkg] -> [Pkg]
sortPkgs = sortBy (flip compare)

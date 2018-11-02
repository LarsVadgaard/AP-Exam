module SolverImpl where

-- Put your solver implementation in this file.
-- Do not change the types of the following exported functions

import Defs
import Utils


normalize :: Database -> Either String Database
normalize = undefined

solve :: Database -> Constrs -> Sol -> [Sol]
solve = undefined

install :: Database -> PName -> Maybe Sol
install = undefined

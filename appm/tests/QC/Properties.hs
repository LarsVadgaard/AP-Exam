module Properties where

import Defs

type InstallProp = Database -> PName -> Maybe Sol -> Bool

-- for reference; may discard after implementing full install_c
install_c' :: InstallProp
install_c' _db _p Nothing = True
install_c' _db _p (Just []) = False
install_c' _db _p (Just _) = True

-- if you don't implement one or more of these, leave them as 'undefined'

install_a :: InstallProp
install_a = undefined

install_b :: InstallProp
install_b = undefined

install_c :: InstallProp
install_c = undefined

install_d :: InstallProp
install_d = undefined


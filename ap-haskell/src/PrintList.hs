module PrintList where

import Control.Monad

printList :: Show a => [a] -> IO ()
printList = mapM_ print

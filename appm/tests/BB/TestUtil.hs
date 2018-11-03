module TestUtil where

import Test.Tasty
import Test.Tasty.HUnit

import Defs
import Utils


tests :: TestTree
tests = testGroup "Util"
    [ testGroup "Version comparison"

      [ testCase "1.0.0 <= 1.0" $
          v1 <= v2 @?= False
      , testCase "1.0 <= 1.0.0" $
          v2 <= v1 @?= True
      , testCase "1.0.0 > 1.0" $
          v1 > v2 @?= True
      , testCase "1.0 > 1.0.0" $
          v2 > v1 @?= False
      , testCase "1.0 > 1.0.0" $
          v2 > v1 @?= False
      , testCase "1.0.0 <= 2.0" $
          v1 <= v3 @?= True
      , testCase "2.0.0 <= 2.0.0wow" $
          v4 <= v5 @?= True
      , testCase "2.0.0wow > 2.0" $
          v5 > v3 @?= True
      , testCase "1.0oh.0 > 1.0" $
          v6 > v2 @?= True
      , testCase "1.0oh.0 > 1.0.0" $
          v6 > v1 @?= True
      , testCase "3.4z > 3.5a" $
          v7 > v8 @?= False
      ]

    , testGroup "Merging constraints"
      [ testCase "4 + 4 : Nothing" $
          merge c1l c1r @?= c1Out
      , testCase "4 + 4 : Just" $
          merge c2l c2r @?= c2Out
      , testCase "4 + 4 : Nothing" $
          merge c3l c3r @?= c3Out
      , testCase "4 + 4 : Just" $
          merge c4l c4r @?= c4Out
      , testCase "4 + 4 : Nothing" $
          merge c5l c5r @?= c5Out
      ]

    ]

------------------------------
-- Comparison between versions
------------------------------

-- Helper function to generate versions
listToVersion :: [(Int,String)] -> Version
listToVersion = V . map (uncurry VN)

-- Test 1
v1    = listToVersion [(1,""),(0,""),(0,"")]
v2    = listToVersion [(1,""),(0,"")]
v3    = listToVersion [(2,""),(0,"")]
v4    = listToVersion [(2,""),(0,""),(0,"")]
v5    = listToVersion [(2,""),(0,""),(0,"wow")]
v6    = listToVersion [(1,""),(0,"oh"),(0,"")]
v7    = listToVersion [(3,""),(4,"z")]
v8    = listToVersion [(3,""),(5,"a")]


------------------------------
-- Merging two constrain lists
------------------------------

-- Test 1
c1l   = [ (P "i3-wm",  (True, V [VN 2 "",VN 0 "",VN 4 ""], V [VN 5 "",VN 2 "",VN 2 ""]))
        , (P "emacs",  (True,  V [VN 4 "",VN 1 ""], V [VN 4 "",VN 6 ""]))
        , (P "ghc",    (True, V [VN 1 "",VN 1 "",VN 25 ""], V [VN 3 "",VN 6 ""]))
        , (P "vim",    (True,  V [VN 3 "",VN 0 ""], V [VN 6 "",VN 0 "",VN 2 "",VN 0 ""]))
        ]
c1r   = [ (P "uxterm", (True,  V [VN 7 "",VN 2 ""], V [VN 9 "",VN 0 "",VN 0 ""]))
        , (P "emacs",  (True,  V [VN 2 "",VN 3 ""], V [VN 4 "",VN 3 ""]))
        , (P "mono",   (True,  V [VN 4 "",VN 1 "",VN 1 ""], V [VN 5 "",VN 8 ""]))
        , (P "vim",    (True,  V [VN 6 "",VN 1 "",VN 3 ""], V [VN 7 "",VN 0 "",VN 2 "",VN 0 ""]))
        ]
c1Out = Nothing

-- Test 2
c2l   = [ (P "i3-wm",  (True,  V [VN 2 "",VN 0 "",VN 4 ""], V [VN 5 "",VN 2 "",VN 2 ""]))
        , (P "emacs",  (True,  V [VN 4 "",VN 1 ""], V [VN 4 "",VN 6 ""]))
        , (P "ghc",    (True,  V [VN 1 "",VN 1 "",VN 25 ""], V [VN 3 "",VN 6 ""]))
        , (P "vim",    (False, V [VN 3 "",VN 0 ""], V [VN 6 "",VN 0 "",VN 2 "",VN 0 ""]))
        ]
c2r   = [ (P "uxterm", (True,  V [VN 7 "",VN 2 ""], V [VN 9 "",VN 0 "",VN 0 ""]))
        , (P "emacs",  (False, V [VN 2 "",VN 3 ""], V [VN 4 "",VN 3 ""]))
        , (P "mono",   (True,  V [VN 4 "",VN 1 "",VN 1 ""], V [VN 5 "",VN 8 ""]))
        , (P "vim",    (True,  V [VN 5 "",VN 1 "",VN 3 ""], V [VN 7 "",VN 0 "",VN 2 "",VN 0 ""]))
        ]
c2Out = Just
        [ (P "i3-wm",  (True,  V [VN 2 "",VN 0 "",VN 4 ""], V [VN 5 "",VN 2 "",VN 2 ""]))
        , (P "emacs",  (True,  V [VN 4 "",VN 3 ""], V [VN 4 "",VN 6 ""]))
        , (P "ghc",    (True,  V [VN 1 "",VN 1 "",VN 25 ""], V [VN 3 "",VN 6 ""]))
        , (P "uxterm", (True,  V [VN 7 "",VN 2 ""], V [VN 9 "",VN 0 "",VN 0 ""]))
        , (P "mono",   (True,  V [VN 4 "",VN 1 "",VN 1 ""], V [VN 5 "",VN 8 ""]))
        , (P "vim",    (True,  V [VN 6 "",VN 0 "",VN 2 "",VN 0 ""], V [VN 7 "",VN 0 "",VN 2 "",VN 0 ""]))
        ]

-- Test 3
c3l   = [ (P "i3-wm",  (True, V [VN 2 "",VN 0 "",VN 4 ""], V [VN 5 "",VN 2 "",VN 2 ""]))
        , (P "emacs",  (True,  V [VN 4 "",VN 1 ""], V [VN 4 "",VN 6 ""]))
        , (P "ghc",    (True, V [VN 1 "",VN 1 "",VN 25 ""], V [VN 3 "",VN 6 ""]))
        , (P "vim",    (False,  V [VN 2 "",VN 0 ""], V [VN 7 "",VN 0 "",VN 2 "",VN 0 ""]))
        ]
c3r   = [ (P "uxterm", (True,  V [VN 7 "",VN 2 ""], V [VN 9 "",VN 0 "",VN 0 ""]))
        , (P "emacs",  (True,  V [VN 2 "",VN 3 ""], V [VN 4 "",VN 3 ""]))
        , (P "mono",   (True,  V [VN 4 "",VN 1 "",VN 1 ""], V [VN 5 "",VN 8 ""]))
        , (P "vim",    (True,  V [VN 4 "",VN 1 "",VN 3 ""], V [VN 5 "",VN 0 "",VN 2 "",VN 0 ""]))
        ]
c3Out = Nothing

-- Test 4
c4l   = [ (P "i3-wm",  (True,  V [VN 2 "",VN 0 "",VN 4 ""], V [VN 5 "",VN 2 "",VN 2 ""]))
        , (P "emacs",  (False, V [VN 3 "",VN 1 ""], V [VN 4 "",VN 6 ""]))
        , (P "ghc",    (True,  V [VN 1 "",VN 1 "",VN 25 ""], V [VN 3 "",VN 6 ""]))
        , (P "vim",    (False, V [VN 3 "",VN 0 ""], V [VN 6 "",VN 0 "",VN 2 "",VN 0 ""]))
        ]
c4r   = [ (P "uxterm", (True,  V [VN 7 "",VN 2 ""], V [VN 9 "",VN 0 "",VN 0 ""]))
        , (P "emacs",  (False, V [VN 2 "",VN 3 ""], V [VN 4 "",VN 2 ""]))
        , (P "mono",   (True,  V [VN 4 "",VN 1 "",VN 1 ""], V [VN 5 "",VN 8 ""]))
        , (P "vim",    (True,  V [VN 5 "",VN 1 "",VN 3 ""], V [VN 7 "",VN 0 "",VN 2 "",VN 0 ""]))
        ]
c4Out = Just
        [ (P "i3-wm",  (True,  V [VN 2 "",VN 0 "",VN 4 ""], V [VN 5 "",VN 2 "",VN 2 ""]))
        , (P "ghc",    (True,  V [VN 1 "",VN 1 "",VN 25 ""], V [VN 3 "",VN 6 ""]))
        , (P "uxterm", (True,  V [VN 7 "",VN 2 ""], V [VN 9 "",VN 0 "",VN 0 ""]))
        , (P "mono",   (True,  V [VN 4 "",VN 1 "",VN 1 ""], V [VN 5 "",VN 8 ""]))
        , (P "vim",    (True,  V [VN 6 "",VN 0 "",VN 2 "",VN 0 ""], V [VN 7 "",VN 0 "",VN 2 "",VN 0 ""]))
        , (P "emacs",  (False,  V [VN 3 "",VN 1 ""], V [VN 4 "",VN 2 ""]))
        ]

-- Test 3
c5l   = [ (P "i3-wm",  (True,  V [VN 2 "",VN 0 "",VN 4 ""], V [VN 5 "",VN 2 "",VN 2 ""]))
        , (P "emacs",  (False, V [VN 1 "",VN 1 ""], V [VN 2 "",VN 6 ""]))
        , (P "ghc",    (True,  V [VN 1 "",VN 1 "",VN 25 ""], V [VN 3 "",VN 6 ""]))
        , (P "vim",    (False, V [VN 3 "",VN 0 ""], V [VN 6 "",VN 0 "",VN 2 "",VN 0 ""]))
        ]
c5r   = [ (P "uxterm", (True,  V [VN 7 "",VN 2 ""], V [VN 9 "",VN 0 "",VN 0 ""]))
        , (P "emacs",  (False, V [VN 2 "",VN 9 ""], V [VN 4 "",VN 3 ""]))
        , (P "mono",   (True,  V [VN 4 "",VN 1 "",VN 1 ""], V [VN 5 "",VN 8 ""]))
        , (P "vim",    (True,  V [VN 5 "",VN 1 "",VN 3 ""], V [VN 7 "",VN 0 "",VN 2 "",VN 0 ""]))
        ]
c5Out = Nothing

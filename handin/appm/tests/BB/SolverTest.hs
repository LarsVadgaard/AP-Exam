module SolverTest where

import Test.Tasty
import Test.Tasty.HUnit

import Defs
import Solver (install)


tests :: TestTree
tests = testGroup "Solver tests"
    [ testCase "tiny" $
       install db1 pname1 @?= Just [(pname1, ver1)]
    , testCase "larger" $
       install db2 pname2 @?= Just out2
    , testCase "vim old version" $
       install db3 pname3 @?= Just out3
    , testCase "vim new version" $
       install db4 pname3 @?= Just out4
    , testCase "empty database" $
       install db5 pname3 @?= Nothing
    , testCase "doesn't exist" $
       install db6 pname6 @?= Nothing
    , testCase "newer version" $
       install db7 pname7 @?= Just out7
    , testCase "conflict with newer version" $
       install db8 pname7 @?= Just out8
    ]


-- Test 1
db1 = DB [Pkg pname1 ver1 "" []]
pname1 = P "foo"
ver1 = V [VN 1 ""]


-- Test 2
db2  = DB [ Pkg { name = P "foo", ver = V [VN 2 "",VN 3 ""]
               , desc = "The foo application"
               , deps = []
           }
         , Pkg { name = P "beef", ver = V [VN 2 "",VN 0 ""]
               , desc = "The beef library - new API, same dependencies"
               , deps = [(P "baz",(True,V [VN 3 "",VN 4 ""],V [VN 5 "",VN 0 "",VN 3 ""]))]
           }
         , Pkg { name = P "bar", ver = V [VN 2 "",VN 1 ""]
               , desc = "The bar library, new API"
               , deps = []
           }
         , Pkg { name = P "baz", ver = V [VN 4 "",VN 1 "",VN 2 ""]
               , desc = ""
               , deps = [(P "bar",(True,V [VN 0 "",VN 4 ""],V [VN 5 "",VN 0 "",VN 3 ""]))]
           }
         , Pkg { name = P "beef", ver = V [VN 1 "",VN 0 ""]
               , desc = "The beef library"
               , deps = [(P "baz",(True,V [VN 3 "",VN 4 ""],V [VN 5 "",VN 0 "",VN 3 ""]))]
           }
         , Pkg { name = P "bar", ver = V [VN 1 "",VN 1 ""]
               , desc = "The bar library, new API"
               , deps = []
           }
         ]
pname2 = P "beef"
out2 = [(pname2, V [VN 2 "", VN 0 ""]),(P "baz", V [VN 4 "", VN 1 "", VN 2 ""]),(P "bar", V [VN 2 "", VN 1 ""])]


-- Test 3
db3  = DB [ Pkg { name = P "foo", ver = V [VN 2 "",VN 3 ""]
               , desc = "The foo application"
               , deps = [ (P "vim", (False, minV, V [VN 9 "",VN 0 ""]))
                        , (P "bar", (True, minV, maxV))]
           }
         , Pkg { name = P "bar", ver = V [VN 2 "",VN 1 ""]
               , desc = "The bar library, new API"
               , deps = []
           }
         , Pkg { name = P "vim", ver = V [VN 11 "",VN 1 "",VN 2 ""]
               , desc = "neovim"
               , deps = [(P "foo",(True,V [VN 0 "",VN 4 ""],V [VN 5 "",VN 0 "",VN 3 ""]))]
           }
         , Pkg { name = P "baz", ver = V [VN 4 "",VN 1 "",VN 2 ""]
               , desc = ""
               , deps = [(P "bar",(True,V [VN 0 "",VN 4 ""],V [VN 5 "",VN 0 "",VN 3 ""]))]
           }
         , Pkg { name = P "vim", ver = V [VN 8 "",VN 1 "",VN 2 ""]
               , desc = ""
               , deps = [(P "foo",(True,V [VN 0 "",VN 4 ""],V [VN 5 "",VN 0 "",VN 3 ""]))]
           }
         , Pkg { name = P "beef", ver = V [VN 1 "",VN 0 ""]
               , desc = "The beef library"
               , deps = [(P "vim",(True,V [VN 3 "",VN 4 ""],V [VN 9 "",VN 0 "",VN 3 ""]))]
           }
         , Pkg { name = P "bar", ver = V [VN 1 "",VN 1 ""]
               , desc = "The bar library, new API"
               , deps = []
           }
         ]
pname3 = P "vim"
out3 = [(pname3, V [VN 8 "", VN 1 "", VN 2 ""]),(P "foo", V [VN 2 "", VN 3 ""]),(P "bar", V [VN 2 "", VN 1 ""])]


-- Test 4
db4  = DB [ Pkg { name = P "foo", ver = V [VN 2 "",VN 3 ""]
               , desc = "The foo application"
               , deps = [ (P "vim", (False, minV, V [VN 12 "",VN 0 ""]))
                        , (P "bar", (True, minV, maxV))]
           }
         , Pkg { name = P "bar", ver = V [VN 2 "",VN 1 ""]
               , desc = "The bar library, new API"
               , deps = []
           }
         , Pkg { name = P "vim", ver = V [VN 11 "",VN 1 "",VN 2 ""]
               , desc = "neovim"
               , deps = [(P "foo",(True,V [VN 0 "",VN 4 ""],V [VN 5 "",VN 0 "",VN 3 ""]))]
           }
         , Pkg { name = P "baz", ver = V [VN 4 "",VN 1 "",VN 2 ""]
               , desc = ""
               , deps = [(P "bar",(True,V [VN 0 "",VN 4 ""],V [VN 5 "",VN 0 "",VN 3 ""]))]
           }
         , Pkg { name = P "vim", ver = V [VN 8 "",VN 1 "",VN 2 ""]
               , desc = ""
               , deps = [(P "foo",(True,V [VN 0 "",VN 4 ""],V [VN 5 "",VN 0 "",VN 3 ""]))]
           }
         , Pkg { name = P "beef", ver = V [VN 1 "",VN 0 ""]
               , desc = "The beef library"
               , deps = [(P "vim",(True,V [VN 3 "",VN 4 ""],V [VN 9 "",VN 0 "",VN 3 ""]))]
           }
         , Pkg { name = P "bar", ver = V [VN 1 "",VN 1 ""]
               , desc = "The bar library, new API"
               , deps = []
           }
         ]
out4= [(pname3, V [VN 11 "", VN 1 "", VN 2 ""]),(P "foo", V [VN 2 "", VN 3 ""]),(P "bar", V [VN 2 "", VN 1 ""])]


-- Test 5
db5 = DB []

-- Test 6
db6  = DB [ Pkg { name = P "foo", ver = V [VN 2 "",VN 3 ""]
               , desc = "The foo application"
               , deps = [ (P "vim", (False, minV, V [VN 12 "",VN 0 ""]))
                        , (P "bar", (True, minV, maxV))]
           }
         , Pkg { name = P "bar", ver = V [VN 2 "",VN 1 ""]
               , desc = "The bar library, new API"
               , deps = []
           }
         , Pkg { name = P "vim", ver = V [VN 11 "",VN 1 "",VN 2 ""]
               , desc = "neovim"
               , deps = [(P "foo",(True,V [VN 0 "",VN 4 ""],V [VN 5 "",VN 0 "",VN 3 ""]))]
           }
         , Pkg { name = P "baz", ver = V [VN 4 "",VN 1 "",VN 2 ""]
               , desc = ""
               , deps = [(P "bar",(True,V [VN 0 "",VN 4 ""],V [VN 5 "",VN 0 "",VN 3 ""]))]
           }
         , Pkg { name = P "vim", ver = V [VN 8 "",VN 1 "",VN 2 ""]
               , desc = ""
               , deps = [(P "foo",(True,V [VN 0 "",VN 4 ""],V [VN 5 "",VN 0 "",VN 3 ""]))]
           }
         , Pkg { name = P "beef", ver = V [VN 1 "",VN 0 ""]
               , desc = "The beef library"
               , deps = [(P "vim",(True,V [VN 3 "",VN 4 ""],V [VN 9 "",VN 0 "",VN 3 ""]))]
           }
         , Pkg { name = P "bar", ver = V [VN 1 "",VN 1 ""]
               , desc = "The bar library, new API"
               , deps = []
           }
         ]
pname6 = P "none"


-- Test 7
db7  = DB [ Pkg { name = P "foo", ver = V [VN 2 "",VN 3 ""]
               , desc = "The foo application"
               , deps = [ (P "vim", (False, minV, V [VN 12 "",VN 0 ""]))
                        , (P "bar", (True, minV, maxV))]
           }
         , Pkg { name = P "bar", ver = V [VN 2 "",VN 1 ""]
               , desc = "The bar library, new API"
               , deps = []
           }
         , Pkg { name = P "vim", ver = V [VN 11 "",VN 1 "",VN 2 ""]
               , desc = "neovim"
               , deps = [(P "foo",(True,V [VN 0 "",VN 4 ""],V [VN 5 "",VN 0 "",VN 3 ""]))]
           }
         , Pkg { name = P "baz", ver = V [VN 4 "",VN 1 "",VN 2 ""]
               , desc = ""
               , deps = [(P "bar",(True,V [VN 0 "",VN 4 ""],V [VN 5 "",VN 0 "",VN 3 ""]))]
           }
         , Pkg { name = P "vim", ver = V [VN 8 "",VN 1 "",VN 2 ""]
               , desc = ""
               , deps = [(P "foo",(True,V [VN 0 "",VN 4 ""],V [VN 5 "",VN 0 "",VN 3 ""]))]
           }
         , Pkg { name = P "beef", ver = V [VN 1 "",VN 0 ""]
               , desc = "The beef library"
               , deps = [(P "vim",(True,V [VN 3 "",VN 4 ""],V [VN 9 "",VN 0 "",VN 3 ""]))]
           }
         , Pkg { name = P "bar", ver = V [VN 1 "",VN 1 ""]
               , desc = "The bar library, new API"
               , deps = []
           }
         ]
pname7 = P "baz"
out7= [(pname7, V [VN 4 "", VN 1 "", VN 2 ""]),(P "bar", V [VN 2 "", VN 1 ""])]


-- Test 8
db8  = DB [ Pkg { name = P "foo", ver = V [VN 2 "",VN 3 ""]
               , desc = "The foo application"
               , deps = [ (P "vim", (False, minV, V [VN 12 "",VN 0 ""]))
                        , (P "bar", (True, minV, maxV))]
           }
         , Pkg { name = P "bar", ver = V [VN 7 "",VN 1 ""]
               , desc = "The bar library, new API"
               , deps = []
           }
         , Pkg { name = P "vim", ver = V [VN 11 "",VN 1 "",VN 2 ""]
               , desc = "neovim"
               , deps = [(P "foo",(True,V [VN 0 "",VN 4 ""],V [VN 5 "",VN 0 "",VN 3 ""]))]
           }
         , Pkg { name = P "baz", ver = V [VN 4 "",VN 1 "",VN 2 ""]
               , desc = ""
               , deps = [(P "bar",(True,V [VN 0 "",VN 4 ""],V [VN 5 "",VN 0 "",VN 3 ""]))]
           }
         , Pkg { name = P "vim", ver = V [VN 8 "",VN 1 "",VN 2 ""]
               , desc = ""
               , deps = [(P "foo",(True,V [VN 0 "",VN 4 ""],V [VN 5 "",VN 0 "",VN 3 ""]))]
           }
         , Pkg { name = P "beef", ver = V [VN 1 "",VN 0 ""]
               , desc = "The beef library"
               , deps = [(P "vim",(True,V [VN 3 "",VN 4 ""],V [VN 9 "",VN 0 "",VN 3 ""]))]
           }
         , Pkg { name = P "bar", ver = V [VN 1 "",VN 1 ""]
               , desc = "The bar library, new API"
               , deps = []
           }
         ]
out8= [(pname7, V [VN 4 "", VN 1 "", VN 2 ""]),(P "bar", V [VN 1 "", VN 1 ""])]

module TestParser where

import Test.Tasty
import Test.Tasty.HUnit

import Defs
import Parser (parseDatabase)


tests :: TestTree
tests = testGroup "Parser tests"
    [ testCase "tiny" $
       parseDatabase src1 @?= Right db1
    , testCase "small" $
       parseDatabase src2 @?= Right db2
    , testCase "large" $
       parseDatabase src3 @?= Right db3
    , testCase "multiple names" $
       parseDatabase src4 @?= Left "\"p\""
    , testCase "implicit version range (requires)" $
       parseDatabase src5 @?= Right db5
    , testCase "implicit version range (conflicts)" $
       parseDatabase src6 @?= Right db6
    ]


-- Test 1
src1 = "package {name foo}"
db1  = DB [Pkg (P "foo") (V [VN 1 ""]) "" []]

-- Test 2
src2 =
  "package { \n\
  \  name hej; \n\
  \  version 1.3.5f; \n\
  \  description \"LOL du \"\"hej\"\" med dig\"; \n\
  \  requires bar >= 1.0; \n\
  \  conflicts bar < 4.0.9 \n\
  \}"
db2  = DB [ Pkg { name = P "hej"
                , ver = V [VN 1 "",VN 3 "",VN 5 "f"]
                , desc = "LOL du \"hej\" med dig"
                , deps = [(P "bar",(True,V [VN 4 "",VN 0 "",VN 9 ""],maxV))]
            }
          ]

-- Test 3
src3 =
  "package { \n\
  \  name foo; \n\
  \  version 2.3; \n\
  \  description \"The foo application\"; \n\
  \  requires bar >= 1.0 \n\
  \} \n\n\
  \package { \n\
  \  name bar; \n\
  \  version 1.0; \n\
  \  description \"The bar library\" \n\
  \} \n\n\
  \package { \n\
  \  name bar; \n\
  \  version 2.1; \n\
  \  description \"The bar library, new API\"; \n\
  \  conflicts baz < 3.4, baz >= 5.0.3 \n\
  \} \n\n\
  \package { \n\
  \  name baz; \n\
  \  version 6.1.2; \n\
  \}"
db3  = DB [ Pkg { name = P "foo", ver = V [VN 2 "",VN 3 ""]
                , desc = "The foo application"
                , deps = [(P "bar",(True,V [VN 1 "",VN 0 ""],maxV))]
            }

          , Pkg { name = P "bar", ver = V [VN 1 "",VN 0 ""]
                , desc = "The bar library", deps = []
            }

          , Pkg { name = P "bar", ver = V [VN 2 "",VN 1 ""]
                , desc = "The bar library, new API"
                , deps = [(P "baz",(False,V [VN 3 "",VN 4 ""],V [VN 5 "",VN 0 "",VN 3 ""]))]
            }

          , Pkg { name = P "baz", ver = V [VN 6 "",VN 1 "",VN 2 ""]
                , desc = ""
                , deps = []
            }
          ]

-- Test 4
src4 =
  "package { \n\
  \  name foo; \n\
  \  version 2.3; \n\
  \  description \"The foo application\"; \n\
  \  requires bar >= 1.0 \n\
  \} \n\n\
  \package { \n\
  \  name bar; \n\
  \  name far; \n\
  \  version 1.0; \n\
  \  description \"The bar library\" \n\
  \} \n\n\
  \package { \n\
  \  name bar; \n\
  \  version 2.1; \n\
  \  description \"The bar library, new API\"; \n\
  \  conflicts baz < 3.4, baz >= 5.0.3 \n\
  \} \n\n\
  \package { \n\
  \  name baz; \n\
  \  version 6.1.2; \n\
  \}"

-- Test 5
src5 = "package {name foo; requires hej, dav < 9.2}"
db5  = DB [Pkg (P "foo") (V [VN 1 ""]) "" [(P "hej", (True,minV,maxV)),(P "dav", (True,minV,V [VN 9 "",VN 2 ""]))]]

-- Test 5
src6 = "package {name foo; conflicts hej, dav < 9.2}"
db6  = DB [Pkg (P "foo") (V [VN 1 ""]) "" [(P "hej", (False,minV,minV)),(P "dav", (False,V [VN 9 "",VN 2 ""],maxV))]]

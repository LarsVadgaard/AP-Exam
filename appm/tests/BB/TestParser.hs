module TestParser where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Either (isLeft)

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
       isLeft(parseDatabase src4) @?= True
    , testCase "implicit version range (requires)" $
       parseDatabase src5 @?= Right db5
    , testCase "implicit version range (conflicts)" $
       parseDatabase src6 @?= Right db6
    , testCase "comments" $
       parseDatabase src3cmt @?= Right db3
    , testCase "semicolon in string" $
       parseDatabase src7 @?= Right db7
    , testCase "case insensitive keywords" $
       parseDatabase src8 @?= Right db7
    , testCase "case sensitive pnames" $
       parseDatabase src9 @?= Right db9
    , testCase "no space after keyword" $
       isLeft (parseDatabase src10) @?= True
    , testCase "strings, general package names" $
       parseDatabase src11 @?= Right db11
    , testCase "name clause > 1" $
       isLeft (parseDatabase src12) @?= True
    , testCase "name clauses < 1" $
       isLeft (parseDatabase src12') @?= True
    , testCase "version clause" $
       isLeft (parseDatabase src13) @?= True
    , testCase "description clause" $
       isLeft (parseDatabase src14) @?= True
    , testCase "self-referential deps" $
       isLeft (parseDatabase src15) @?= True
    , testCase "inherently contradictory" $
       isLeft (parseDatabase src16) @?= True
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
src5 = "package {name foo-hoo; requires hej, dav < 9.2}"
db5  = DB [Pkg (P "foo-hoo") (V [VN 1 ""]) "" [(P "hej", (True,minV,maxV)),(P "dav", (True,minV,V [VN 9 "",VN 2 ""]))]]

-- Test 5
src6 = "package {name foo; conflicts hej, dav < 9.2}"
db6  = DB [Pkg (P "foo") (V [VN 1 ""]) "" [(P "hej", (False,minV,minV)),(P "dav", (False,V [VN 9 "",VN 2 ""],maxV))]]

-- Test 6
-- includes comment as only space after keyword
--        , comment until eof
--        , comment between packages
--        , etc.
src3cmt =
  "package { \n\
  \  name--test test\n\
  \    foo; -- this is the name \n\
  \  version 2.3; \n\
  \  description \"The foo application\"; \n\
  \  requires bar >= 1.0 \n\
  \} \n-- now i'm between packages\n\
  \package { \n\
  \  name bar; \n\
  \  version 1.0; --this is the version \n\
  \  description \"The bar library\" \n\
  \} \n\n\
  \package { \n\
  \  name bar; \n\
  \  version 2.1; \n\
  \  description \"The bar library, new API\"; \n\
  \  conflicts baz < 3.4, baz >= 5.0.3 \n\
  \} \n\n-- almost done!\n\n\
  \package { \n\
  \  name baz; \n\
  \  version 6.1.2; \n\
  \}  -- goodbye"

-- Test 7
src7 =
  "package { \n\
  \  name hej; \n\
  \  version 1.3.5f; \n\
  \  description \"LOL du; \"\"hej\"\" med dig\"; \n\
  \  requires bar >= 1.0; \n\
  \  conflicts bar < 4.0.9 \n\
  \}"
db7  = DB [ Pkg { name = P "hej"
                , ver = V [VN 1 "",VN 3 "",VN 5 "f"]
                , desc = "LOL du; \"hej\" med dig"
                , deps = [(P "bar",(True,V [VN 4 "",VN 0 "",VN 9 ""],maxV))]
            }
          ]

-- Test 8
src8 =
  "pAcKAge { \n\
  \  NAme hej; \n\
  \  VERSION 1.3.5f; \n\
  \  dEScrIption \"LOL du; \"\"hej\"\" med dig\"; \n\
  \  reQUIRES bar >= 1.0; \n\
  \  coNFlicts bar < 4.0.9 \n\
  \}"

-- Test 9
src9 =
  "pAcKAge { \n\
  \  NAme hEj; \n\
  \  VERSION 1.3.5f; \n\
  \  dEScrIption \"LOL du; \"\"hej\"\" med dig\"; \n\
  \  reQUIRES Bar >= 1.0; \n\
  \  coNFlicts Bar < 4.0.9 \n\
  \}"
db9  = DB [ Pkg { name = P "hEj"
                , ver = V [VN 1 "",VN 3 "",VN 5 "f"]
                , desc = "LOL du; \"hej\" med dig"
                , deps = [(P "Bar",(True,V [VN 4 "",VN 0 "",VN 9 ""],maxV))]
            }
          ]

-- Test 10
src10 =
  "pAcKAge { \n\
  \  NAmehEj; \n\
  \  VERSION 1.3.5f; \n\
  \  dEScrIption \"LOL du; \"\"hej\"\" med dig\"; \n\
  \  reQUIRES Bar >= 1.0; \n\
  \  coNFlicts Bar < 4.0.9 \n\
  \}"

-- Test 11
src11 =
  "pAcKAge { \n\
  \  NAme \"This is a general name!!1 --- \n\"\"test\"\"\"; \n\
  \  VERSION 1.3.5f; \n\
  \  dEScrIption \"LOL du; \"\"hej\"\" med dig\"; \n\
  \  reQUIRES Bar >= 1.0; \n\
  \  coNFlicts Bar < 4.0.9 \n\
  \}"
db11  = DB [ Pkg { name = P "This is a general name!!1 --- \n\"test\""
                , ver = V [VN 1 "",VN 3 "",VN 5 "f"]
                , desc = "LOL du; \"hej\" med dig"
                , deps = [(P "Bar",(True,V [VN 4 "",VN 0 "",VN 9 ""],maxV))]
            }
          ]

-- Test 12
src12 =
  "pAcKAge { \n\
  \  NAme \"This is a general name!!1 --- \n\"\"test\"\"\"; \n\
  \  NAme \"This is a general name!!1 --- \n\"\"test\"\"\"; \n\
  \  VERSION 1.3.5f; \n\
  \}"

src12' =
  "pAcKAge { \n\
  \  VERSION 1.3.5f; \n\
  \}"

-- Test 13
src13 =
  "pAcKAge { \n\
  \  NAme \"This is a general name!!1 --- \n\"\"test\"\"\"; \n\
  \  VERSION 1.3.5f; \n\
  \  VERSION 1.3.5f; \n\
  \}"

-- Test 14
src14 =
  "pAcKAge { \n\
  \  NAme \"This is a general name!!1 --- \n\"\"test\"\"\"; \n\
  \  dEScrIption \"LOL du; \"\"hej\"\" med dig\"; \n\
  \  dEScrIption \"LOL du; \"\"hej\"\" med dig\"; \n\
  \}"

-- Test 15
src15 =
  "pAcKAge { \n\
  \  NAme foo; \n\
  \  requires foo; \n\
  \}"

-- Test 16
src16 =
  "pAcKAge { \n\
  \  NAme \"This is a general name!!1 --- \n\"\"test\"\"\"; \n\
  \  dEScrIption \"LOL du; \"\"hej\"\" med dig\"; \n\
  \  requires bar < 3; \n\
  \  requires bar > 7.9 \n\
  \}"

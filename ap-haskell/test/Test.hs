{-
Example code produced for Advanced Programming lecture.

Main purpose is to show how to generate recursive types.
-}

import Test.Tasty
import Test.Tasty.HUnit

import Test.QuickCheck
import Control.Monad (liftM, liftM2, ap)
import Control.Applicative

-- Tests

tests :: TestTree
tests = testGroup "Tests"
    [ testCase "lol" $ 7 @?= 7 ]


-- Quickcheck
data Expr = Con Int
          | Add Expr Expr
     deriving (Eq, Show, Read, Ord)

eval :: Expr -> Int
eval (Con n) = n
eval (Add x y) = eval x + eval y


prop_com_add x y = eval (Add x y) == eval (Add y x)


-- expr =  oneof [ fmap Con arbitrary
--               , Add <$> expr <*> expr
--               ]


-- -- Take 2: using sized generators and shrinking
instance Arbitrary Expr where
   arbitrary = expr2

   shrink (Add e1 e2) =
     -- shrink to subterms
     [e1, e2] ++
     -- recursively shrink subterms
     [Add e1' e2' | (e1', e2') <- shrink (e1, e2)]
   shrink (Con n) = map Con $ shrink n


expr2 = sized exprN
exprN 0 = Con <$> arbitrary
exprN n = oneof [ Con <$> arbitrary
                , Add <$> subexpr <*> subexpr
                ]
  where subexpr = exprN (n `div` 2)

main = quickCheck prop_com_add >> defaultMain tests

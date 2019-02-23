module Problem032Spec where

import Test.Hspec
import Problem032

spec :: Spec
spec = 
  describe "pandigital product" $ do
    let m1 = 39
        m2 = 186
        ans = 7254
    testPandProd m1 m2 ans
    let m1 = 2
        m2 = 1357
        ans = 0
    testPandProd m1 m2 ans

testPandProd :: Int -> Int -> Int -> SpecWith ()
testPandProd m1 m2 ans =
  it ("the pandigital product of " ++ (show m1) ++
       " and " ++ (show m2) ++
       " is " ++ (show ans)) $ do
    pandProd m1 m2 `shouldBe` ans


module Problem125Spec where

import Test.Hspec
import Problem125

spec :: Spec
spec = do
  describe "The palindromic sum of all numbers less than nMax." $ do
    let nMax = 1000
        pSum = 4164
    it ("The palindromic sum for " ++ (show nMax) ++ " = " ++ (show pSum)) $ do
      palindromicSum nMax `shouldBe` pSum


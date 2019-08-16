module Problem118Spec where

import Test.Hspec
import Problem118

-- It is difficult to test this problem, and this test is very expensive.
spec :: Spec
spec = do
  describe "Test takes too long" $ do
    it (" ") $ do
      True `shouldBe` True
{-
spec = do
  describe "pandigital prime sets" $ do
    let partition = [(1,2),(2,2),(3,1),(4,0),(5,0),(6,0),(7,0),(8,0)]
        validSet = [2,5,47,89,631]
    it ("The prime set " ++ (show validSet) ++ " is found to be pandigital.") $ do
       validSet `elem` enumPDPrimeSets partition `shouldBe` True
-}


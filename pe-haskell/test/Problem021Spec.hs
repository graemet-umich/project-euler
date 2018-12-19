module Problem021Spec where

import Test.Hspec
import Problem021

spec :: Spec
spec = do
  describe "find amicable pair" $ do
    let n = 6
    it (show n ++ " is not in an amicable pair") $ do
      getAmicablePair n `shouldBe` []
    let n = 220
    it (show n ++ " is in an amicable pair") $ do
      getAmicablePair n `shouldBe` [220, 284]


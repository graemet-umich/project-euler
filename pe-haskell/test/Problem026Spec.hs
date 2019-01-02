module Problem026Spec where

import Test.Hspec
import Problem026

spec :: Spec
spec = do
  describe "test full reptend primes" $ do
    let p = 7
    it ((show p) ++ " is a full reptend prime") $ do
      isFullReptendPrime p `shouldBe` True
    let p = 13
    it ((show p) ++ " is not a full reptend prime") $ do
      isFullReptendPrime p `shouldBe` False


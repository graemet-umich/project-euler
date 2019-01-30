module Problem121Spec where

import Test.Hspec
import Problem121

spec :: Spec
spec = do
  describe "Prize fund for n turn game" $ do
    let n = 4
        ans = 10
    it ("A " ++ (show n) ++ " turn game requires a fund of £" ++ (show ans)) $ do
      prizeFund n `shouldBe` ans


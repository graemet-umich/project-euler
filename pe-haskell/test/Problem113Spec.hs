module Problem113Spec where

import Test.Hspec
import Problem113

spec :: Spec
spec = do
  describe "numbers below a power of 10 that are not bouncy" $ do
    let pow = 1
        ans = 9
    it ("There are " ++ (show ans) ++
        " non-bouncy numbers below 10^" ++ (show pow) ++ ".") $ do
      answer pow `shouldBe` ans
    -- All positive numbers <100 are non-bouncy.
    let pow = 2
        ans = 99
    it ("There are " ++ (show ans) ++
        " non-bouncy numbers below 10^" ++ (show pow) ++ ".") $ do
      answer pow `shouldBe` ans
    -- test cases
    let pow = 6
        ans = 12951
    it ("There are " ++ (show ans) ++
        " non-bouncy numbers below 10^" ++ (show pow) ++ ".") $ do
      answer pow `shouldBe` ans
    let pow = 10
        ans = 277032
    it ("There are " ++ (show ans) ++
        " non-bouncy numbers below 10^" ++ (show pow) ++ ".") $ do
      answer pow `shouldBe` ans


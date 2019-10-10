module Problem132Spec where

import Test.Hspec
import Problem132

spec :: Spec
spec = do
  describe "prime factors of R(k)" $ do
    let n = 4
        k = 10
        first4sum = 9414
    it ("The first " ++ (show n) ++ " prime factors of R(" ++ (show k) ++
       ") sum to " ++ (show first4sum)) $ do
      soln132 n k `shouldBe` first4sum


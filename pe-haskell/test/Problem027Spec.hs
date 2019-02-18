module Problem027Spec where

import Test.Hspec
import Problem027

spec :: Spec
spec = do
  describe "number of consecutive primes (ncp) and a*b for (a, b)" $ do
    let a = 1
        b = 41
        ncp = 40
        ans = (ncp, a * b)
    it ("(ncp, a*b) for (a, b) = " ++ (show (a, b)) ++ " is " ++ (show ans)) $ do
      ncpAndAB (a, b) `shouldBe` ans
    let a = (-79)
        b = 1601
        ncp = 80
        ans = (ncp, a * b)
    it ("(ncp, a*b) for (a, b) = " ++ (show (a, b)) ++ " is " ++ (show ans)) $ do
      ncpAndAB (a, b) `shouldBe` ans

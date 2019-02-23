module Problem034Spec where

import Test.Hspec
import Problem034
import Data.Ratio

spec :: Spec
spec = 
  describe "digit cancelling fractions" $ do
    let n   = 145
        ans = 145
    test_getDigitFactorial n ans
    let n   = 144
        ans = 0
    test_getDigitFactorial n ans

test_getDigitFactorial :: Integer -> Integer -> SpecWith ()
test_getDigitFactorial n ans =
  it ("the digit factorial of " ++ (show n) ++
      " is " ++ (show ans)) $ do
    getDigitFactorial n `shouldBe` ans


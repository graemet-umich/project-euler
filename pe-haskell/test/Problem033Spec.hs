module Problem033Spec where

import Test.Hspec
import Problem033
import Data.Ratio

spec :: Spec
spec = 
  describe "digit cancelling fractions" $ do
    let n = 49
        d = 98
        ans = [1%2]
    test_dcf n d ans
    let n = 30
        d = 50
        ans = []
    test_dcf n d ans

--test_dcf :: Ratio Integer ratio => Int -> Int -> ratio -> SpecWith ()
test_dcf n d ans =
  it ("the digit cancelling fraction(s) for " ++ (show n) ++
      "/" ++ (show d) ++
      " is " ++ (show ans)) $ do
    dcf n d `shouldBe` ans


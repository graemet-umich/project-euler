module Problem124Spec where

import Test.Hspec
import Problem124

spec :: Spec
spec = do
  describe "Find E(k), the kth element in the sorted n column" $ do
    let nMax = 10
        k = 4
        _Ek = 8
    it ("For nMax = " ++ (show nMax) ++ ", E(" ++ (show k) ++ ") = " ++ (show _Ek)) $ do
      _E nMax k `shouldBe` _Ek
    let k = 6
        _Ek = 9
    it ("For nMax = " ++ (show nMax) ++ ", E(" ++ (show k) ++ ") = " ++ (show _Ek)) $ do
      _E nMax k `shouldBe` _Ek


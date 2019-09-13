module Problem129Spec where

import Test.Hspec
import Problem129

spec :: Spec
spec = do
  describe "A(n) = k" $ do
    let n = 7
        k = 6
    it ("A(" ++ (show n) ++ ") = " ++ (show k)) $ do
      _A n `shouldBe` k
    let n = 41
        k = 5
    it ("A(" ++ (show n) ++ ") = " ++ (show k)) $ do
      _A n `shouldBe` k

  describe "The least n such that A(n) first exceeds k" $ do
    let n = 17
        k = 10
    it ("A(" ++ (show n) ++ ") first exceeds " ++ (show k)) $ do
      iAexceeds k `shouldBe` n


module Problem025Spec where

import Test.Hspec
import Problem025

spec :: Spec
spec = do
  describe "the index i of the first Fibonacci number F(i) to have n digits" $ do
    let n = 1
        i = 1
    it ("F(" ++ (show i) ++ ") has " ++ (show n) ++ " digit") $ do
      ift n `shouldBe` i
    let n = 2
        i = 7
    it ("F(" ++ (show i) ++ ") has " ++ (show n) ++ " digits") $ do
      ift n `shouldBe` i
    let n = 3
        i = 12
    it ("F(" ++ (show i) ++ ") has " ++ (show n) ++ " digits") $ do
      ift n `shouldBe` i

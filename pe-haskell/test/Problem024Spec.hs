module Problem024Spec where

import Test.Hspec
import Problem024

spec :: Spec
spec = do
  describe "the nth lexicographic permutation" $ do
    let lex = "012"
        n = 4
    it ("of " ++ (show lex) ++ " (n = " ++ (show n) ++ ")") $ do
      nthLexPerm n lex `shouldBe` "120"

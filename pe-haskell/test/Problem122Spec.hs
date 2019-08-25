module Problem122Spec where

import Test.Hspec
import Problem122

spec = do
  describe "m(k), the minimal amount of multiplications to compute n^k" $ do
    let k = 1
        m_k = 0
    it ("m(" ++ (show k) ++ ") = " ++ (show m_k)) $ do
       m k `shouldBe` m_k
    let k = 2
        m_k = 1
    it ("m(" ++ (show k) ++ ") = " ++ (show m_k)) $ do
       m k `shouldBe` m_k
    let k = 5
        m_k = 3
    it ("m(" ++ (show k) ++ ") = " ++ (show m_k)) $ do
       m k `shouldBe` m_k
    -- test case
    let k = 15
        m_k = 5
    it ("m(" ++ (show k) ++ ") = " ++ (show m_k)) $ do
       m k `shouldBe` m_k


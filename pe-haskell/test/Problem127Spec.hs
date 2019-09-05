module Problem127Spec where

import Test.Hspec
import Problem127

spec :: Spec
spec = do
  describe "Is an abc-hit." $ do
    let abc = (5, 27, 32)
        result = True
    it ("The abc triplet " ++ (show abc) ++
        " is an abc-hit: " ++ (show result) ++ ".") $ do
      is_abc_hit abc `shouldBe` result
    let abc = (3, 5, 8)
        result = False
    it ("The abc triplet " ++ (show abc) ++
        " is an abc-hit: " ++ (show result) ++ ".") $ do
      is_abc_hit abc `shouldBe` result

  describe "The number of abc-hits for c < cMax." $ do
    let cMax = 1000
        num_abc_hits = 31
    it ("There are " ++ (show num_abc_hits) ++
        " abc-hits less than " ++ (show cMax) ++ ".") $ do
      length (abc_hits cMax) `shouldBe` num_abc_hits

  describe "The sum of cs of abc-hits for c < cMax." $ do
    let cMax = 1000
        sum_cs = 12523 
    it ("The sum of cs of abc-hits for c < " ++ (show cMax) ++
        " is " ++ (show sum_cs) ++ ".") $ do
      sum_cs_abc_hits cMax `shouldBe` sum_cs




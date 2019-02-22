module Problem031Spec where

import Test.Hspec
import Problem031

spec :: Spec
spec = do
  describe "selected coin sums" $ do
    let denominations = [1,2,5,10,20,50,100,200]
    
    let amount = 0
        ans = 0
    testCoinSums amount denominations ans
    
    let amount = 1
        ans = 1
    testCoinSums amount denominations ans

    let amount = 2
        ans = 2
    testCoinSums amount denominations ans

    let amount = 3
        ans = 2
    testCoinSums amount denominations ans

    let amount = 4
        ans = 3
    testCoinSums amount denominations ans

    let amount = 5
        ans = 4
    testCoinSums amount denominations ans

testCoinSums amount denominations ans =
  it ("there are " ++ (show ans) ++
       " ways to make " ++ (show amount) ++
       "p using denominations " ++ (show denominations)) $ do
    coinSums amount denominations `shouldBe` ans
  

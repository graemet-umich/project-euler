module Problem133Spec where

import Test.Hspec
import Problem133

import Data.List ((\\))
import Data.Numbers.Primes (primes)

spec :: Spec
spec = do
  describe "prime (non) factors of R(k)" $ do
    let k = 10^4
        primeFactorsLT100 = [11, 17, 41, 73]
        allPrimesLT100 = takeWhile (< 100) primes
        primeNonFactorsLT100 = takeWhile (< 100) $ primeNonFactorsRk k
    it ("The only 4 primes below 100 that can be a factor of R(10^n) are " ++
        (show primeFactorsLT100)) $ do
      (allPrimesLT100 \\ primeNonFactorsLT100) `shouldBe` primeFactorsLT100


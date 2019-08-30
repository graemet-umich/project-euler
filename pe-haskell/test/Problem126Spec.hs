module Problem126Spec where

import Test.Hspec
import Problem126

spec :: Spec
spec = do
  describe "The number of cubes in cuboid layers." $ do
    let cuboid = (3, 2, 1)
        layers = [22, 46, 78, 118]
    it ("The first four layers of cuboid " ++ (show cuboid) ++
        " have " ++ (show layers) ++ " cubes.") $ do
      take 4 (enumLayerSizes cuboid) `shouldBe` layers
    let cuboid = (5, 1, 1)
        layer = 22
    it ("The first layer of cuboid " ++ (show cuboid) ++
        " has " ++ (show layer) ++ " cubes.") $ do
      head (enumLayerSizes cuboid) `shouldBe` layer
    let cuboid = (5, 3, 1)
        layer = 46
    it ("The first layer of cuboid " ++ (show cuboid) ++
        " has " ++ (show layer) ++ " cubes.") $ do
      head (enumLayerSizes cuboid) `shouldBe` layer
    let cuboid = (7, 2, 1)
        layer = 46
    it ("The first layer of cuboid " ++ (show cuboid) ++
        " has " ++ (show layer) ++ " cubes.") $ do
      head (enumLayerSizes cuboid) `shouldBe` layer
    let cuboid = (11, 1, 1)
        layer = 46
    it ("The first layer of cuboid " ++ (show cuboid) ++
        " has " ++ (show layer) ++ " cubes.") $ do
      head (enumLayerSizes cuboid) `shouldBe` layer

  describe "C(n) is the number of cuboids that contain n cubes in one of its layers." $ do
    let n = 22
        _C_n = 2
    it ("C(" ++ (show n) ++ ") = " ++ (show _C_n)) $ do
      _C n `shouldBe` _C_n
    let n = 46
        _C_n = 4
    it ("C(" ++ (show n) ++ ") = " ++ (show _C_n)) $ do
      _C n `shouldBe` _C_n
    let n = 78
        _C_n = 5
    it ("C(" ++ (show n) ++ ") = " ++ (show _C_n)) $ do
      _C n `shouldBe` _C_n
    let n = 118
        _C_n = 8
    it ("C(" ++ (show n) ++ ") = " ++ (show _C_n)) $ do
      _C n `shouldBe` _C_n

  describe "iC(nCuboids) = n is the least value of n for which C(n) = nCuboids." $ do
    let nCuboids = 2
        iC_nCuboids = 22
    it ("iC(" ++ (show nCuboids) ++ ") = " ++ (show iC_nCuboids)) $ do
      iC nCuboids `shouldBe` iC_nCuboids
    let nCuboids = 10
        iC_nCuboids = 154
    it ("iC(" ++ (show nCuboids) ++ ") = " ++ (show iC_nCuboids)) $ do
      iC nCuboids `shouldBe` iC_nCuboids

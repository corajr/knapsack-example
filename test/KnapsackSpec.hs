module KnapsackSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Maybe (isJust, isNothing)

import Knapsack

instance Arbitrary Good where
  arbitrary = do
    w <- choose (1, 5)
    (NonNegative p) <- arbitrary
    n <- arbitrary
    return $ Good w p n

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "a Knapsack 4" $ do
    it "can contain at most 4 lbs." $ property $
      \xs -> let weight = sum (map goodWeight xs)
             in isJust (mkKnapsack xs) === (weight <= 4)
  describe "exponentialStealOptimallyFrom" $ do
    it "returns the empty knapsack with no Goods" $
      exponentialStealOptimallyFrom [] `shouldBe` emptyKnapsack
    context "with one item" $ do
      it "returns a knapsack containing one Good iff that Good has weight <= 4" $ property $
        \x -> let w = goodWeight x
                  k = exponentialStealOptimallyFrom [x]
              in if w <= 4 then Just k === mkKnapsack [x] else k === emptyKnapsack
    context "with two items of weight 4" $ do
      it "returns a knapsack containing the item of higher value" $ property $
        \(NonNegative x) (NonNegative y) ->
          let goodX = Good 4 x "x"
              goodY = Good 4 y "y"
              k = exponentialStealOptimallyFrom [goodX, goodY]
          in (x /= y) ==> if x > y then Just k === mkKnapsack [goodX] else Just k === mkKnapsack [goodY]
  describe "a Thief in the night" $ do
    let guitar = Good 1 15 "guitar"
        stereo = Good 4 30 "stereo"
        laptop = Good 3 20 "laptop"
        house = [guitar, stereo, laptop]
        optimal = mkKnapsack [guitar, laptop]
    it "has a knapsack with a 4lb maximum" $ do
      mkKnapsack [] `shouldBe` Just emptyKnapsack
      mkKnapsack [guitar, laptop] `shouldSatisfy` isJust
      mkKnapsack [stereo, laptop] `shouldSatisfy` isNothing
    it "maximizes price by weight in stealing" $ do
      Just (stealOptimallyFrom house) `shouldBe` optimal
    it "gives the same result as an exponential time algorithm" $ property $
      \xs -> length xs <= 20 ==> stealOptimallyFrom xs === exponentialStealOptimallyFrom xs

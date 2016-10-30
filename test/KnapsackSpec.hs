module KnapsackSpec (main, spec) where

import Test.Hspec
import Data.Maybe (isJust, isNothing)

import Knapsack

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
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
      stealOptimallyFrom house `shouldBe` optimal

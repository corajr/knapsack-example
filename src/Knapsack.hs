{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Knapsack ( Good(..)
                , Knapsack
                , emptyKnapsack
                , mkKnapsack
                , getSackContents
                , stealOptimallyFrom
                ) where

import Control.Monad (foldM)
import GHC.TypeLits
import Data.Maybe (fromMaybe)

data Good = Good
  { goodWeight :: Int
  , goodPrice :: Int
  , goodName :: String
  } deriving (Eq, Show, Ord)

newtype Knapsack (n :: Nat) = Knapsack [Good]
  deriving (Eq, Show, Ord)

emptyKnapsack :: Knapsack 4
emptyKnapsack = Knapsack []

mkKnapsack :: [Good] -> Maybe (Knapsack 4)
mkKnapsack = fmap fst . foldM addGood (emptyKnapsack, 4)
  where addGood (Knapsack goods, n) x@(Good {goodWeight = w})
          | n >= w = Just (Knapsack (x:goods), n - w)
          | otherwise = Nothing

getSackContents :: Knapsack a -> [Good]
getSackContents (Knapsack xs) = xs

stealOptimallyFrom :: [Good] -> Maybe (Knapsack 4)
stealOptimallyFrom = undefined

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Knapsack ( Good(..)
                , Knapsack
                , emptyKnapsack
                , mkKnapsack
                , getSackContents
                , stealOptimallyFrom
                , exponentialStealOptimallyFrom
                ) where

import GHC.TypeLits
import Data.Proxy (Proxy(Proxy))

import Control.Monad (foldM)
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Data.List (maximumBy, subsequences)

data Good = Good
  { goodWeight :: Integer
  , goodPrice :: Integer
  , goodName :: String
  } deriving (Eq, Show, Ord)

newtype Knapsack (n :: Nat) = Knapsack [Good]
  deriving (Eq, Show, Ord)

emptyKnapsack :: Knapsack 4
emptyKnapsack = emptyKnapsack'

emptyKnapsack' :: forall n. (KnownNat n) => Knapsack n
emptyKnapsack' = Knapsack []

mkKnapsack :: [Good] -> Maybe (Knapsack 4)
mkKnapsack = mkKnapsack'

mkKnapsack' :: forall n. (KnownNat n) => [Good] -> Maybe (Knapsack n)
mkKnapsack' = fmap fst . foldM addGood (emptyKnapsack', natVal (Proxy :: Proxy n))
  where addGood (Knapsack goods, n) x@(Good {goodWeight = w})
          | n >= w = Just (Knapsack (x:goods), n - w)
          | otherwise = Nothing

getSackContents :: Knapsack n -> [Good]
getSackContents (Knapsack xs) = xs

stealOptimallyFrom :: [Good] -> Knapsack 4
stealOptimallyFrom = undefined

exponentialStealOptimallyFrom :: [Good] -> Knapsack 4
exponentialStealOptimallyFrom = maximumBy (compare `on` (sum . map goodPrice . getSackContents)) . mapMaybe mkKnapsack . subsequences

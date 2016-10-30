{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Knapsack ( Good(..)
                , Knapsack
                , emptyKnapsack
                , mkKnapsack
                , getSackContents
                , stealOptimallyFrom
                ) where

import Control.Monad (foldM)
import GHC.TypeLits
import Data.Proxy (Proxy(Proxy))
import Data.Maybe (fromMaybe)

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

getSackCapacity :: forall n. (KnownNat n) => Knapsack n -> Integer
getSackCapacity _ = natVal (Proxy :: Proxy n)

getSackContents :: Knapsack n -> [Good]
getSackContents (Knapsack xs) = xs

stealOptimallyFrom :: [Good] -> Maybe (Knapsack 4)
stealOptimallyFrom = undefined

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
import Data.Maybe (mapMaybe, fromJust)
import Data.List (maximumBy, subsequences)
import Data.Array (Array, listArray, range, (!))

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
stealOptimallyFrom = stealOptimallyFrom'

-- for approach, see: http://jelv.is/blog/Lazy-Dynamic-Programming/
stealOptimallyFrom' :: forall n. (KnownNat n) => [Good] -> Knapsack n
stealOptimallyFrom' xs = fromJust . mkKnapsack' . reverse . snd $ m n maxW
  where n = length xs
        maxW = natVal (Proxy :: Proxy n)
        xs' = listArray (1, n) xs
        m 0 _ = (0, [])
        m i w
          | w_i > w = go (i - 1) w Nothing
          | otherwise = maximum' [ go (i - 1) w Nothing
                                 , go (i - 1) (w - w_i) (Just good) ]
          where good@(Good {goodWeight = w_i}) = xs' ! i
                maximum' = maximumBy (compare `on` fst)
        go i w good = case good of
          Just good' -> (score + goodPrice good', good' : goods)
          Nothing -> (score, goods)
          where (score, goods) = mt ! (i, w)
        mt = listArray mtBounds [ m i w | (i, w) <- range mtBounds ]
        mtBounds = ((0, 0), (n, maxW))

exponentialStealOptimallyFrom :: [Good] -> Knapsack 4
exponentialStealOptimallyFrom = exponentialStealOptimallyFrom'

exponentialStealOptimallyFrom' :: forall n. (KnownNat n) => [Good] -> Knapsack n
exponentialStealOptimallyFrom' = maximumBy (compare `on` (sum . map goodPrice . getSackContents)) . mapMaybe mkKnapsack' . subsequences


{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

-- |
-- Module      :  Data.Accumulator
-- Copyright   :  (c) Francesco Gazzetta 2017
-- License     :  BSD3 (see the file LICENSE)
--
-- Maintainer  :  francygazz@gmail.org
-- Stability   :  stable
-- Portability :  portable
--
-- Accumulators, primarily useful for Polyvariadic

module Data.Accumulator
  ( Accumulator (..)
  , singleton
  , accumulateMany
  , AccumulatorSemigroup (..)
  ) where

import qualified Data.Set as Set
import           Data.Foldable (foldl')
import           Data.Semigroup

#if !MIN_VERSION_base(4,8,0)
import Data.Foldable
#endif

-- | An 'Accumulator c i' supports accumulation of elements of type i in it.
-- This is different from 'Semigroup' or 'Monoid', where '<>' acts between
-- two values with the same type.
class Accumulator acc x where
  accumulate :: x -> acc -> acc -- ^ Accumulate a value

-- | Accumulate a single value in a 'Monoid'
singleton :: (Accumulator acc x, Monoid acc) => x -> acc
singleton = flip accumulate mempty

-- | Strictly accumulate multiple values from a 'Foldable', from left to right
accumulateMany :: (Foldable f, Accumulator acc x) => f x -> acc -> acc
accumulateMany xs acc = foldl' (flip accumulate) acc xs

instance Accumulator [a] a where
  accumulate = (:)

instance Ord a => Accumulator (Set.Set a) a where
  accumulate = Set.insert

instance Semigroup m => Accumulator (AccumulatorSemigroup m) (AccumulatorSemigroup m) where
  accumulate = (<>)

-- | Lift a 'Semigroup' into an Accumulator. This is a newtype because (<>) isn't always the ideal way of accumulating
newtype AccumulatorSemigroup a = AccumulatorSemigroup {getAccumulatorSemigroup :: a}

instance Semigroup m => Semigroup (AccumulatorSemigroup m) where
  (<>) (AccumulatorSemigroup a) (AccumulatorSemigroup b) = AccumulatorSemigroup $ (<>) a b


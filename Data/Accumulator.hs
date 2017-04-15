{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Accumulator
  ( Accumulator (..)
  , singleton
  , AccumulatorMonoid (..)
  ) where

import qualified Data.Set as Set

-- | Lift a monoid into an Accumulator. This is a newtype because the mappend isn't always the ideal way of accumulating
newtype AccumulatorMonoid a = AccumulatorMonoid {getAccumulatorMonoid :: a}

instance Monoid m => Monoid (AccumulatorMonoid m) where
  mempty = AccumulatorMonoid mempty
  mappend (AccumulatorMonoid a) (AccumulatorMonoid b) = AccumulatorMonoid $ mappend a b

-- | An 'Accumulator c i' supports accumulation of elements of type i in it.
-- This is different from 'Semigroup' or 'Monoid', where '<>' acts between
-- two values with the same type.
class Accumulator acc x where
  accumulate :: x -> acc -> acc -- ^ Accumulate a value

-- | Accumulate a single value in a 'Monoid'
singleton :: (Accumulator acc x, Monoid acc) => x -> acc
singleton = flip accumulate mempty

instance Accumulator [a] a where
  accumulate = (:)

instance Ord a => Accumulator (Set.Set a) a where
  accumulate = Set.insert

instance Monoid m => Accumulator (AccumulatorMonoid m) (AccumulatorMonoid m) where
  accumulate = mappend


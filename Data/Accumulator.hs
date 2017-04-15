{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Accumulator where -- MAYBE rename to Accumulate

import qualified Data.Set as Set

-- | Lift a monoid into an Accumulator. This is a newtype because the mappend isn't always the ideal way of accumulating
newtype AccumulatorMonoid a = AccumulatorMonoid {getAccumulatorMonoid :: a}

instance Monoid m => Monoid (AccumulatorMonoid m) where
  mempty = AccumulatorMonoid mempty
  mappend (AccumulatorMonoid a) (AccumulatorMonoid b) = AccumulatorMonoid $ mappend a b

-- | An 'Accumulator c i' supports creation of an empty accumulator of type c
-- and accumulation of elements of type i in it.
-- This is different from 'Monoid', where '<>' acts between two values with the same type.
class Accumulator c i | c -> i where
  insert :: i -> c -> c -- ^ Accumulate a value
  --MAYBE empty is not necessaryly empty. Rename to new or something?
  --MAYBE remove entirely? Empty is not necessary anywhere in polyvariadic
  --and introduces a functional dependency. which is bad.
  empty :: c -- ^ Empty accumulator

-- | Accumulate a single value
singleton :: Accumulator c i => i -> c
singleton = flip insert empty

instance Accumulator [a] a where
  insert = (:)
  empty = []

instance Ord a => Accumulator (Set.Set a) a where
  insert = Set.insert
  empty = Set.empty

instance Monoid m => Accumulator (AccumulatorMonoid m) (AccumulatorMonoid m) where
  insert = mappend
  empty = mempty


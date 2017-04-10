{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Accumulator where -- MAYBE rename to Accumulate

import qualified Data.Set as Set

newtype AccumulatorMonoid a = AccumulatorMonoid {getAccumulatorMonoid :: a}

instance Monoid m => Monoid (AccumulatorMonoid m) where
  mempty = AccumulatorMonoid mempty
  mappend (AccumulatorMonoid a) (AccumulatorMonoid b) = AccumulatorMonoid $ mappend a b


class Accumulator c i | c -> i where
  insert :: i -> c -> c
  empty :: c
  singleton :: i -> c
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


{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Accumulator where -- MAYBE rename to Accumulate

import qualified Data.Set as Set

newtype AccumulatorMonoid a = AccumulatorMonoid {getAccumulatorMonoid :: a} deriving Monoid

class Accumulator c i | c -> i where
  insert :: i -> c -> c
  empty :: c
  singleton :: i -> c
  singleton = flip insert empty

instance Accumulator [a] a where
  insert = (:)
  empty = []

instance Ord a => Accumulator (Set.Set a) a where
  --TODO

instance Monoid m => Accumulator (AccumulatorMonoid m) (AccumulatorMonoid m) where
  insert = mappend
  empty = mempty


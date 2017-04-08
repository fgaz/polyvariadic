{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Accumulator where -- MAYBE rename to Accumulate

import qualified Data.Set as Set

newtype UnfoldableMonoid a = UnfoldableMonoid {getUnfoldableMonoid :: a} deriving Monoid

class Unfoldable c i | c -> i where
  insert :: i -> c -> c
  empty :: c
  singleton :: i -> c
  singleton = flip insert empty

instance Unfoldable [a] a where
  insert = (:)
  empty = []

instance Unfoldable (Set.Set a) a where
  --TODO

instance Monoid m => Unfoldable (UnfoldableMonoid m) (UnfoldableMonoid m) where
  insert = mappend
  empty = mempty


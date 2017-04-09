{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Function.Polyvariadic where

import Data.Foldable
import Data.Accumulator
--import Data.Collections


---- creation

applyToAccumulatedArgs :: (Polyvariadic a b x, Accumulator a i) => (a -> b) -> x
applyToAccumulatedArgs = polyvariadic empty
--argsToAccumulator :: Polyvariadic a a x => x
--argsToAccumulator = applyToAccumulatedArgs id

class Polyvariadic accumulator result x where
  polyvariadic :: accumulator -> (accumulator -> result) -> x

instance (Accumulator c i, Polyvariadic c b x) => Polyvariadic c b (i -> x) where
  polyvariadic a f x = polyvariadic (insert x a) f

instance Polyvariadic accumulator result result where
  polyvariadic a f = f a


---- application

class Apply a b x where
  apply' :: x -> [a] -> b

instance Apply a b b where
  apply' f _ = f

instance (Apply a b x) => Apply a b (a -> x) where
  apply' f (x:xs) = apply (f x) xs
  apply' _ _ = error "Not enough arguments in polyvariadic application"


apply :: (Apply a b x, Foldable t) => x -> t a -> b
apply f t = apply f $ toList t


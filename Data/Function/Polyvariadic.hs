{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Function.Polyvariadic where

import Data.Foldable
import Data.Accumulator
--import Data.Collections


---- creation

applyToAccumulatedArgs :: (Varargs a b x, Accumulator a i) => (a -> b) -> x
applyToAccumulatedArgs = varargs empty
--argsToAccumulator :: Varargs a a x => x
--argsToAccumulator = applyToAccumulatedArgs id

--MAYBE rename to Polyvariadic
class Varargs accumulator result x where
  varargs :: accumulator -> (accumulator -> result) -> x

instance (Accumulator c i, Varargs c b x) => Varargs c b (i -> x) where
  varargs a f x = varargs (insert x a) f

instance Varargs accumulator result result where
  varargs a f = f a


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


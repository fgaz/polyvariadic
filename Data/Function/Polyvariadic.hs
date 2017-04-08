{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Function.Polyvariadic where

import Data.Accumulator
--import Data.Collections

applyToAccumulatedArgs :: (Varargs a b x, Unfoldable a i) => (a -> b) -> x
applyToAccumulatedArgs = varargs empty
--argsToUnfoldable :: Varargs a a x => x
--argsToUnfoldable = applyToAccumulatedArgs id

--MAYBE rename to Polyvariadic
class Varargs accumulator result x where
  varargs :: accumulator -> (accumulator -> result) -> x

instance (Unfoldable c i, Varargs c b x) => Varargs c b (i -> x) where
  varargs a f x = varargs (insert x a) f

instance Varargs accumulator result result where
  varargs a f = f a


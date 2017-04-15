{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Function.Polyvariadic
  (
    -- * Creation
    Polyvariadic (polyvariadic)
  , applyToAccumulatedArgs
    -- * Application
  , Apply (apply')
  , apply
  ) where

import Data.Foldable
import Data.Accumulator


---- creation

-- | Creation of functions with an arbitrary number of arguments.
--
-- The arguments will be accumulated in the given 'Accumulator',
-- which will then be passed as an argument to the function.
--
-- ==== __Examples__
--
-- The classic @printf@ function, which takes an arbitrary amount of arguments
-- and inserts them in a string.
--
-- @
-- magicChar = \'%\'
-- notMagicChar c = c \/= magicChar
--
-- data PrintfAccum = PrintfAccum { done :: String, todo :: String }
--
-- instance Show x => Accumulator PrintfAccum x where
--   accumulate x (PrintfAccum done (_:todo)) = PrintfAccum
--                                               (done ++ show x ++ takeWhile notMagicChar todo)
--                                               (dropWhile notMagicChar todo)
--   accumulate _ acc = acc
--
-- printf' str = polyvariadic
--                (PrintfAccum (takeWhile notMagicChar str) (dropWhile notMagicChar str))
--                done
-- @
--
-- >>> printf' "aaa%bbb%ccc%ddd" "TEST" 123 True
-- "aaaTESTbbb123cccTrueddd"
class Polyvariadic accumulator result x where
  polyvariadic :: accumulator -> (accumulator -> result) -> x

-- | Accumulates the next argument
instance (Accumulator c i, Polyvariadic c b x) => Polyvariadic c b (i -> x) where
  polyvariadic a f x = polyvariadic (accumulate x a) f

-- | There are no more arguments to accumulate so the function is applied
-- to the 'Accumulator'
instance Polyvariadic accumulator result result where
  polyvariadic a f = f a

applyToAccumulatedArgs :: (Polyvariadic a b x, Monoid a) => (a -> b) -> x
applyToAccumulatedArgs = polyvariadic mempty
--argsToAccumulator :: Polyvariadic a a x => x
--argsToAccumulator = applyToAccumulatedArgs id


---- application

-- | Application of function with an arbitrary number of arguments to the
-- elements of a list.
--
-- __Will raise an error if the list doesn't have enough elements__
class Apply a b x where
  apply' :: x -> [a] -> b

-- | The final type is not reached yet and the application continues
instance (Apply a b x) => Apply a b (a -> x) where
  apply' f (x:xs) = apply (f x) xs
  apply' _ _ = error "Not enough arguments in polyvariadic application"

-- | The final type is reached and the application terminates
instance Apply a b b where
  apply' f _ = f

-- | Like 'apply'' but with an arbitrary 'Foldable' instead if a list
apply :: (Apply a b x, Foldable t) => x -> t a -> b
apply f t = apply f $ toList t


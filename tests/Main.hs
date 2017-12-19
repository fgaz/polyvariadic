{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
import Data.Function.Polyvariadic
import Data.Accumulator
import Control.Exception (assert)

#if !MIN_VERSION_base(4,8,0)
import Data.Foldable
import Data.Monoid
#endif

main = do
  putStrLn one
  assert (one == "[3,2,1]") $ return ()
  putStrLn two
  assert (two == "aaa\"TEST\"bbb123cccTrueddd") $ return ()
  putStrLn three
  assert (three == "3") $ return ()

one = show (polyvariadic mempty (id :: [Int] -> [Int]) (1::Int) (2::Int) (3::Int) :: [Int])
two = printf' "aaa%bbb%ccc%ddd" "TEST" 123 True 1.2
three = show (apply ((+) :: Int -> Int -> Int) ([1,2] :: [Int]) :: Int)

magicChar = '%'

data PrintfAccum = PrintfAccum { done :: String, todo :: String }

instance Show x => Accumulator PrintfAccum x where
  accumulate x (PrintfAccum done (_:todo)) = PrintfAccum (done ++ show x ++ takeWhile (/= magicChar) todo) (dropWhile (/= magicChar) todo)
  accumulate _ acc = acc

printf' str = polyvariadic (PrintfAccum (takeWhile (/= magicChar) str) (dropWhile (/= magicChar) str)) done


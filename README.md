# polyvariadic

**Creation and application of polyvariadic functions**

[![Build Status](https://travis-ci.org/fgaz/polyvariadic.svg?branch=master)](https://travis-ci.org/fgaz/polyvariadic)
[![Hackage](https://img.shields.io/hackage/v/polyvariadic.svg)]()

For example, the classic printf:

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
import Data.Function.Polyvariadic
import Data.Accumulator

magicChar = '%'
notMagicChar = (/= magicChar)

data PrintfAccum = PrintfAccum { done :: String, todo :: String }

instance Show x => Accumulator PrintfAccum x where
  accumulate x (PrintfAccum done (_:todo)) = PrintfAccum
                                              (done ++ show x ++ takeWhile notMagicChar todo)
                                              (dropWhile notMagicChar todo)
  accumulate _ acc = acc

printf' str = polyvariadic
               (PrintfAccum (takeWhile notMagicChar str) (dropWhile notMagicChar str))
               done
```

```haskell
>>> printf' "aaa%bbb%ccc%ddd" "TEST" 123 True
"aaa\"TEST\"bbb123cccTrueddd"
```


# polyvariadic

**Creation and application of polyvariadic functions**

For example, the classic printf:

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


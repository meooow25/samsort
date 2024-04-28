{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

import Control.DeepSeq (NFData(..), rwhnf)
import Control.Monad.Primitive (PrimMonad(..), RealWorld)
import Data.Primitive.Array
  ( MutableArray(..), newArray, sizeofMutableArray, writeArray )
import Data.Bits ((.&.))
import qualified Data.List as L
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector.Algorithms.Merge as Merge
import qualified Data.Vector.Algorithms.Tim as Tim
import GHC.Exts (sizeofMutableArray#)

import Criterion.Main (Benchmark, defaultMain, bench, bgroup, perRunEnv)

import qualified Data.SamSort as Sam

main :: IO ()
main = defaultMain $ map bgroupN sizes

bgroupN :: Int -> Benchmark
bgroupN n = bgroup (show n) $
  if n <= 1
  then
    [ bgroupMA "Same" (sameMA n)
    ]
  else
    [ bgroupMA "Asc" (ascMA n)
    , bgroupMA "Desc" (descMA n)
    , bgroupMA "UpDownAsc" (upDownAscMA n)
    , bgroupMA "UpDownDesc" (upDownDescMA n)
    , bgroupMA "BigRand" (bigRandMA n)
    , bgroupMA "SmallRand" (smallRandMA n)
    , bgroupMA "AscThenRand" (ascThenRandMA n)
    , bgroupMA "RandThenAsc" (randThenAscMA n)
    ]

bgroupMA :: String -> IO (MutableArray RealWorld Int) -> Benchmark
bgroupMA name mkma = bgroup name
  [ bench "SamSort" $
    perRunEnv (fmap WHNF mkma) $ \(WHNF ma) ->
      samSort ma
  , bench "vector-algorithms Intro" $
    perRunEnv (fmap WHNF mkma) $ \(WHNF ma) ->
      Intro.sort (MV.MVector 0 (sizeofMutableArray ma) ma)
  , bench "vector-algorithms Merge" $
    perRunEnv (fmap WHNF mkma) $ \(WHNF ma) ->
      Merge.sort (MV.MVector 0 (sizeofMutableArray ma) ma)
  , bench "vector-algorithms Tim" $
    perRunEnv (fmap WHNF mkma) $ \(WHNF ma) ->
      Tim.sort (MV.MVector 0 (sizeofMutableArray ma) ma)
  ]

sizes :: [Int]
sizes = [0, 1, 10, 1000, 100000]

samSort :: (PrimMonad m, Ord a) => MutableArray (PrimState m) a -> m ()
samSort (MutableArray ma#) = primitive $ \s ->
  case Sam.sortBy# compare ma# 0# (sizeofMutableArray# ma#) s of
    s1 -> (# s1, () #)

---------
-- Data
---------

bigRandList :: Int -> [Int]
bigRandList n = take n $ L.iterate' ((0xffffffff .&.) . (7654321 *)) n
{-# INLINE bigRandList #-}

sameMA :: Int -> IO (MutableArray RealWorld Int)
sameMA n = mutableArrayFromListN n (replicate n 0)

ascMA :: Int -> IO (MutableArray RealWorld Int)
ascMA n = mutableArrayFromListN n [1..n]

descMA :: Int -> IO (MutableArray RealWorld Int)
descMA n = mutableArrayFromListN n [n,n-1..1]

-- [2,1,4,3,6,5..]
upDownAscMA :: Int -> IO (MutableArray RealWorld Int)
upDownAscMA n = mutableArrayFromListN n (take n (map f [1..]))
  where
    f i = if odd i then i+1 else i-1

upDownDescMA :: Int -> IO (MutableArray RealWorld Int)
upDownDescMA n = mutableArrayFromListN n (take n (map f [1..]))
  where
    f i = n - (if odd i then i+1 else i-1)

bigRandMA :: Int -> IO (MutableArray RealWorld Int)
bigRandMA n = mutableArrayFromListN n (bigRandList n)

smallRandMA :: Int -> IO (MutableArray RealWorld Int)
smallRandMA n = mutableArrayFromListN n (take n rs)
  where
    rs = L.iterate' ((0xff .&.) . (7654321 *)) n

ascThenRandMA :: Int -> IO (MutableArray RealWorld Int)
ascThenRandMA n =
  mutableArrayFromListN n ([1 .. n `div` 2] ++ bigRandList ((n+1) `div` 2))

randThenAscMA :: Int -> IO (MutableArray RealWorld Int)
randThenAscMA n =
  mutableArrayFromListN n (bigRandList (n `div` 2) ++ [1 .. (n+1) `div` 2])

----------
-- Utils
----------

mutableArrayFromListN
  :: PrimMonad m => Int -> [a] -> m (MutableArray (PrimState m) a)
mutableArrayFromListN n xs = do
  ma <- newArray n (error "errorElement")
  let f x k !i = writeArray ma i x *> k (i+1)
  foldr f (\ !_ -> pure ()) xs 0
  pure ma
{-# INLINE mutableArrayFromListN #-}

newtype WHNF a = WHNF a

instance NFData (WHNF a) where
  rnf = rwhnf

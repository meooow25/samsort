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
main = defaultMain
  [ bgroup "Int" $ map (bgroupN id) sizes
  , bgroup "Maybe Int" $ map (bgroupN Just) sizes
  ]

sizes :: [Int]
sizes = [0, 1, 10, 1000, 100000]

bgroupN :: Ord a => (Int -> a) -> Int -> Benchmark
bgroupN f n = bgroup (show n) $
  if n <= 1
  then
    [ bgroupIOA "Same" (sameIOA f n)
    ]
  else
    [ bgroupIOA "Asc" (ascIOA f n)
    , bgroupIOA "Desc" (descIOA f n)
    , bgroupIOA "UpDownAsc" (upDownAscIOA f n)
    , bgroupIOA "UpDownDesc" (upDownDescIOA f n)
    , bgroupIOA "BigRand" (bigRandIOA f n)
    , bgroupIOA "SmallRand" (smallRandIOA f n)
    , bgroupIOA "AscThenRand" (ascThenRandIOA f n)
    , bgroupIOA "RandThenAsc" (randThenAscIOA f n)
    ]

bgroupIOA :: Ord a => String -> IO (IOArray a) -> Benchmark
bgroupIOA name mkma = bgroup name
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

samSort :: (PrimMonad m, Ord a) => MutableArray (PrimState m) a -> m ()
samSort (MutableArray ma#) = primitive $ \s ->
  case Sam.sortBy# compare ma# 0# (sizeofMutableArray# ma#) s of
    s1 -> (# s1, () #)

---------
-- Data
---------

sameIOA :: (Int -> a) -> Int -> IO (IOArray a)
sameIOA f n = mutableArrayFromListN n (replicate n (f 0))

ascIOA :: (Int -> a) -> Int -> IO (IOArray a)
ascIOA f n = mutableArrayFromListN n (map f [1..n])

descIOA :: (Int -> a) -> Int -> IO (IOArray a)
descIOA f n = mutableArrayFromListN n (map f [n,n-1..1])

-- [2,1,4,3,6,5..]
upDownAscIOA :: (Int -> a) -> Int -> IO (IOArray a)
upDownAscIOA f n = mutableArrayFromListN n (take n (map (f . g) [1..]))
  where
    g i = if odd i then i+1 else i-1

upDownDescIOA :: (Int -> a) -> Int -> IO (IOArray a)
upDownDescIOA f n = mutableArrayFromListN n (take n (map (f . g) [1..]))
  where
    g i = n - (if odd i then i+1 else i-1)

bigRandIOA :: (Int -> a) -> Int -> IO (IOArray a)
bigRandIOA f n = mutableArrayFromListN n (map f $ randomInts n)

smallRandIOA :: (Int -> a) -> Int -> IO (IOArray a)
smallRandIOA f n = mutableArrayFromListN n (take n $ map f rs)
  where
    rs = map (0xff .&.) $ randomInts n

ascThenRandIOA :: (Int -> a) -> Int -> IO (IOArray a)
ascThenRandIOA f n =
  mutableArrayFromListN
    n
    (map f $ [1 .. n `div` 2] ++ randomInts ((n+1) `div` 2))

randThenAscIOA :: (Int -> a) -> Int -> IO (IOArray a)
randThenAscIOA f n =
  mutableArrayFromListN
    n
    (map f $ randomInts (n `div` 2) ++ [1 .. (n+1) `div` 2])

----------
-- Utils
----------

type IOArray = MutableArray RealWorld

-- LCG
randomInts :: Int -> [Int]
randomInts n =
  take n $ L.iterate' (\i -> 0xffffffff .&. (i * 1103515245 + 12345)) n
{-# INLINE randomInts #-}

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

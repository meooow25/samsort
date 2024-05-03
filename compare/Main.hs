{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

import Control.DeepSeq (NFData(..), rwhnf)
import Control.Monad.Primitive (PrimMonad(..), RealWorld, stToPrim)
import Control.Monad.ST (stToIO)
import Data.Bits ((.&.))
import Data.Primitive.Array
  ( MutableArray(..)
  , newArray
  , sizeofMutableArray
  , writeArray
  )
import Data.Primitive.ByteArray (MutableByteArray(..))
import Data.Primitive.PrimArray
  ( MutablePrimArray(..)
  , getSizeofMutablePrimArray
  , newPrimArray
  , writePrimArray
  )
import Data.Primitive.Types (Prim)
import qualified Data.List as L
import qualified Data.Primitive.Sort as PSort
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector.Algorithms.Merge as Merge
import qualified Data.Vector.Algorithms.Tim as Tim
import qualified Data.Vector.Primitive.Mutable as MPV
import qualified Data.Vector.Mutable as MV

import Criterion.Main (Benchmark, defaultMain, bench, bgroup, perRunEnv)

import qualified Data.SamSort as Sam

main :: IO ()
main = defaultMain
  [ bgroup "Int" $ map (bgroupN id) sizes
  , bgroup "Maybe Int" $ map (bgroupN Just) sizes
  , bgroup "Int (Prim)" $ map bgroupPN sizes
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
    , bgroupIOA "Rand" (randIOA f n)
    , bgroupIOA "RandSmall" (randSmallIOA f n)
    , bgroupIOA "AscThenRand" (ascThenRandIOA f n)
    , bgroupIOA "RandThenAsc" (randThenAscIOA f n)
    ]

bgroupIOA :: Ord a => String -> IO (IOArray a) -> Benchmark
bgroupIOA name mkma = bgroup name
  [ bench "samsort sortArrayBy" $
    perRunEnv (fmap WHNF mkma) $ \(WHNF ma) -> samSort ma
  , bench "vector-algorithms Intro" $
    perRunEnv mkmv $ \(WHNF mv) -> Intro.sort mv
  , bench "vector-algorithms Merge" $
    perRunEnv mkmv $ \(WHNF mv) -> Merge.sort mv
  , bench "vector-algorithms Tim" $
    perRunEnv mkmv $ \(WHNF mv) -> Tim.sort mv
  ]
  where
    mkmv = fmap (\ma -> WHNF (MV.MVector 0 (sizeofMutableArray ma) ma)) mkma

bgroupPN :: Int -> Benchmark
bgroupPN n = bgroup (show n) $
  if n <= 1
  then
    [ bgroupIOPA "Same" (sameIOPA n)
    ]
  else
    [ bgroupIOPA "Asc" (ascIOPA n)
    , bgroupIOPA "Desc" (descIOPA n)
    , bgroupIOPA "UpDownAsc" (upDownAscIOPA n)
    , bgroupIOPA "UpDownDesc" (upDownDescIOPA n)
    , bgroupIOPA "Rand" (randIOPA n)
    , bgroupIOPA "RandSmall" (randSmallIOPA n)
    , bgroupIOPA "AscThenRand" (ascThenRandIOPA n)
    , bgroupIOPA "RandThenAsc" (randThenAscIOPA n)
    ]

bgroupIOPA :: String -> IO (IOPrimArray Int) -> Benchmark
bgroupIOPA name mkma = bgroup name
  [ bench "samsort sortIntArrayBy" $
    perRunEnv (fmap WHNF mkma) $ \(WHNF ma) -> samSortInts ma
  , bench "vector-algorithms Intro" $
    perRunEnv mkmv $ \(WHNF mv) -> Intro.sort mv
  , bench "vector-algorithms Merge" $
    perRunEnv mkmv $ \(WHNF mv) -> Merge.sort mv
  , bench "vector-algorithms Tim" $
    perRunEnv mkmv $ \(WHNF mv) -> Tim.sort mv
  , bench "primitive-sort" $
    perRunEnv (fmap WHNF mkma) $ \(WHNF ma) -> stToIO (PSort.sortMutable ma)
  ]
  where
    mkmv :: IO (WHNF (MPV.IOVector Int))
    mkmv = do
      ma@(MutablePrimArray ma#) <- mkma
      sz <- getSizeofMutablePrimArray ma
      pure (WHNF (MPV.MVector 0 sz (MutableByteArray ma#)))

samSort :: (PrimMonad m, Ord a) => MutableArray (PrimState m) a -> m ()
samSort ma@(MutableArray ma#) =
  stToPrim $ Sam.sortArrayBy compare ma# 0 (sizeofMutableArray ma)

samSortInts :: PrimMonad m => MutablePrimArray (PrimState m) Int -> m ()
samSortInts ma@(MutablePrimArray ma#) = do
  sz <- getSizeofMutablePrimArray ma
  stToPrim $ Sam.sortIntArrayBy compare ma# 0 sz

---------
-- Data
---------

same, asc, desc, upDownAsc, upDownDesc,
  rand, randSmall, ascThenRand, randThenAsc :: Int -> [Int]
same n = replicate n 0
asc n = [1..n]
desc n = [n,n-1..1]
upDownAsc n = map f [1..n]
  where
    f i = if odd i then i+1 else i-1
upDownDesc n = map f [1..n]  -- [2,1,4,3,6,5..]
  where
    f i = n - (if odd i then i+1 else i-1)
rand = randomInts
randSmall n = map (0xff .&.) (randomInts n)
ascThenRand n = [1..n2] ++ map (\x -> x `mod` n2 + 1) (randomInts (n-n2))
  where
    n2 = n `div` 2
randThenAsc n = map (\x -> x `mod` n2 + 1) (randomInts n2) ++ [1 .. n-n2]
  where
    n2 = n `div` 2

sameIOA, ascIOA, descIOA, upDownAscIOA, upDownDescIOA,
  randIOA, randSmallIOA, ascThenRandIOA, randThenAscIOA
    :: (Int -> a) -> Int -> IO (IOArray a)
sameIOA f n = mutArrayFromListN n (map f (same n))
ascIOA f n = mutArrayFromListN n (map f (asc n))
descIOA f n = mutArrayFromListN n (map f (desc n))
upDownAscIOA f n = mutArrayFromListN n (map f (upDownAsc n))
upDownDescIOA f n = mutArrayFromListN n (map f (upDownDesc n))
randIOA f n = mutArrayFromListN n (map f (rand n))
randSmallIOA f n = mutArrayFromListN n (map f (randSmall n))
ascThenRandIOA f n = mutArrayFromListN n (map f (ascThenRand n))
randThenAscIOA f n = mutArrayFromListN n (map f (randThenAsc n))

sameIOPA, ascIOPA, descIOPA, upDownAscIOPA, upDownDescIOPA,
  randIOPA, randSmallIOPA, ascThenRandIOPA, randThenAscIOPA
    :: Int -> IO (IOPrimArray Int)
sameIOPA n = mutPrimArrayFromListN n (same n)
ascIOPA n = mutPrimArrayFromListN n (asc n)
descIOPA n = mutPrimArrayFromListN n (desc n)
upDownAscIOPA n = mutPrimArrayFromListN n (upDownAsc n)
upDownDescIOPA n = mutPrimArrayFromListN n (upDownDesc n)
randIOPA n = mutPrimArrayFromListN n (rand n)
randSmallIOPA n = mutPrimArrayFromListN n (randSmall n)
ascThenRandIOPA n = mutPrimArrayFromListN n (ascThenRand n)
randThenAscIOPA n = mutPrimArrayFromListN n (randThenAsc n)

----------
-- Utils
----------

type IOArray = MutableArray RealWorld
type IOPrimArray = MutablePrimArray RealWorld

-- LCG
randomInts :: Int -> [Int]
randomInts n =
  take n $ L.iterate' (\i -> 0xffffffff .&. (i * 1103515245 + 12345)) n
{-# INLINE randomInts #-}

mutArrayFromListN
  :: PrimMonad m => Int -> [a] -> m (MutableArray (PrimState m) a)
mutArrayFromListN n xs = do
  ma <- newArray n (error "errorElement")
  let f x k !i = writeArray ma i x *> k (i+1)
  foldr f (\ !_ -> pure ()) xs 0
  pure ma
{-# INLINE mutArrayFromListN #-}

mutPrimArrayFromListN
  :: (PrimMonad m, Prim a) => Int -> [a] -> m (MutablePrimArray (PrimState m) a)
mutPrimArrayFromListN n xs = do
  ma <- newPrimArray n
  let f x k !i = writePrimArray ma i x *> k (i+1)
  foldr f (\ !_ -> pure ()) xs 0
  pure ma
{-# INLINE mutPrimArrayFromListN #-}

newtype WHNF a = WHNF a

instance NFData (WHNF a) where
  rnf = rwhnf

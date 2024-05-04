{-# OPTIONS_GHC -Wno-orphans #-} -- SPECIALIZE rules
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}

import Control.DeepSeq (NFData(..), rwhnf)
import Control.Monad.Primitive (PrimMonad(..), RealWorld, stToPrim)
import Control.Monad.ST (ST, stToIO)
import Data.Bits ((.&.))
import Data.Primitive.Array
  ( MutableArray(..)
  , newArray
  , sizeofMutableArray
  , writeArray
  )
import Data.Primitive.ByteArray (ByteArray(..), MutableByteArray(..))
import Data.Primitive.PrimArray
  ( MutablePrimArray(..)
  , PrimArray(..)
  , getSizeofMutablePrimArray
  , newPrimArray
  , writePrimArray
  )
import Data.Primitive.Types (Prim)
import qualified Data.List as L
import qualified Data.Primitive.Sort as PSort
import qualified Data.Vector.Algorithms.Heap as Heap
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector.Algorithms.Merge as Merge
import qualified Data.Vector.Algorithms.Tim as Tim
import qualified Data.Vector.Primitive.Mutable as MPV
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Primitive.Mutable as VPM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Base as VUB
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Generic as VG
import Data.Word (Word8)

import Criterion.Main (Benchmark, defaultMain, bench, bgroup, perRunEnv, whnf)

import qualified Data.SamSort as Sam

main :: IO ()
main = defaultMain
  [ bgroup "Int" $ map (bgroupN id) sizes
  , bgroup "Maybe Int" $ map (bgroupN Just) sizes
  , bgroup "Int (Prim)" $ map bgroupPN sizes
  , bgroup "Int*3 (Unboxed)" $ map bgroupVUN sizes
  , bgroup "Word8 (Unboxed)" $ map bgroupW8VUN sizes
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
    perRunEnv (fmap WHNF mkma) $ \(WHNF ma) -> samSortArray ma
  , bench "vector-algorithms Merge" $
    perRunEnv mkmv $ \(WHNF mv) -> Merge.sort mv
  , bench "vector-algorithms Intro" $
    perRunEnv mkmv $ \(WHNF mv) -> Intro.sort mv
  , bench "vector-algorithms Heap" $
    perRunEnv mkmv $ \(WHNF mv) -> Heap.sort mv
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
    perRunEnv (fmap WHNF mkma) $ \(WHNF ma) -> samSortIntArray ma
  , bench "vector-algorithms Merge" $
    perRunEnv mkmv $ \(WHNF mv) -> Merge.sort mv
  , bench "vector-algorithms Intro" $
    perRunEnv mkmv $ \(WHNF mv) -> Intro.sort mv
  , bench "vector-algorithms Heap" $
    perRunEnv mkmv $ \(WHNF mv) -> Heap.sort mv
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

bgroupVUN :: Int -> Benchmark
bgroupVUN n = bgroup (show n) $
  if n <= 1
  then
    [ bgroupVU "Same" (sameVU n)
    ]
  else
    [ bgroupVU "Asc" (ascVU n)
    , bgroupVU "Desc" (descVU n)
    , bgroupVU "UpDownAsc" (upDownAscVU n)
    , bgroupVU "UpDownDesc" (upDownDescVU n)
    , bgroupVU "Rand" (randVU n)
    , bgroupVU "RandSmall" (randSmallVU n)
    , bgroupVU "AscThenRand" (ascThenRandVU n)
    , bgroupVU "RandThenAsc" (randThenAscVU n)
    ]

bgroupVU :: String -> VU.Vector (Int, Int, Int) -> Benchmark
bgroupVU name !v = bgroup name
  [ bench "samsort sortIntArrayBy (by index)" $
    whnf samSortVecByIdx v
  , bench "vector-algorithms Merge" $
    whnf (sortVecUsing Merge.sort) v
  , bench "vector-algorithms Intro" $
    whnf (sortVecUsing Intro.sort) v
  , bench "vector-algorithms Heap" $
    whnf (sortVecUsing Heap.sort) v
  , bench "vector-algorithms Tim" $
    whnf (sortVecUsing Tim.sort) v
  , bench "vector-algorithms Merge (by index)" $
    whnf (sortVecByIdxUsing Merge.sortBy) v
  , bench "vector-algorithms Intro (by index)" $
    whnf (sortVecByIdxUsing Intro.sortBy) v
  , bench "vector-algorithms Heap (by index)" $
    whnf (sortVecByIdxUsing Heap.sortBy) v
  , bench "vector-algorithms Tim (by index)" $
    whnf (sortVecByIdxUsing Tim.sortBy) v
  ]

bgroupW8VUN :: Int -> Benchmark
bgroupW8VUN n = bgroup (show n) $
  if n <= 1
  then
    [ bgroupW8VU "Same" (sameW8VU n)
    ]
  else
    [ bgroupW8VU "RandSmall" (randSmallW8VU n)
    ]

bgroupW8VU :: String -> VU.Vector Word8 -> Benchmark
bgroupW8VU name !v = bgroup name
  [ bench "samsort sortIntArrayBy (by index)" $
    whnf samSortVecByIdx v
  , bench "vector-algorithms Merge" $
    whnf (sortVecUsing Merge.sort) v
  , bench "vector-algorithms Intro" $
    whnf (sortVecUsing Intro.sort) v
  , bench "vector-algorithms Heap" $
    whnf (sortVecUsing Heap.sort) v
  , bench "vector-algorithms Tim" $
    whnf (sortVecUsing Tim.sort) v
  , bench "vector-algorithms Merge (by index)" $
    whnf (sortVecByIdxUsing Merge.sortBy) v
  , bench "vector-algorithms Intro (by index)" $
    whnf (sortVecByIdxUsing Intro.sortBy) v
  , bench "vector-algorithms Heap (by index)" $
    whnf (sortVecByIdxUsing Heap.sortBy) v
  , bench "vector-algorithms Tim (by index)" $
    whnf (sortVecByIdxUsing Tim.sortBy) v
  , bench "primitive-sort" $
    whnf PSort.sort pv
  ]
  where
    !(VUB.V_Word8 (VP.Vector _off _len (ByteArray ba#))) = v
    -- Note: primitive-sort does not support sorting a slice! In this case we
    -- know that the vector is not a slice, so we ignore off and len.
    pv = PrimArray ba# :: PrimArray Word8

---------------
-- Sort utils
---------------

samSortArray :: (PrimMonad m, Ord a) => MutableArray (PrimState m) a -> m ()
samSortArray ma@(MutableArray ma#) =
  stToPrim $ Sam.sortArrayBy compare ma# 0 (sizeofMutableArray ma)

samSortIntArray :: PrimMonad m => MutablePrimArray (PrimState m) Int -> m ()
samSortIntArray ma@(MutablePrimArray ma#) = do
  sz <- getSizeofMutablePrimArray ma
  stToPrim $ Sam.sortIntArrayBy compare ma# 0 sz

samSortVecByIdx :: (Ord a, VG.Vector v a) => v a -> v a
samSortVecByIdx v = VG.generate n (VG.unsafeIndex v . VP.unsafeIndex ixa)
  where
    n = VG.length v
    ixa = VP.create $ do
      ixma <- VPM.generate n id
      let !(VPM.MVector off len (MutableByteArray ma#)) = ixma
          cmp i j = compare (VG.unsafeIndex v i) (VG.unsafeIndex v j)
      stToPrim $ Sam.sortIntArrayBy cmp ma# off len
      pure ixma

sortVecUsing
  :: (Ord a, VG.Vector v a)
  => (forall s. VG.Mutable v s a -> ST s ())
  -> v a
  -> v a
sortVecUsing sortf v = VG.create $ do
  vm <- VG.thaw v
  sortf vm
  pure vm
{-# INLINE sortVecUsing #-}

sortVecByIdxUsing
  :: (Ord a, VG.Vector v a)
  => (forall s. (Int -> Int -> Ordering) -> VP.MVector s Int -> ST s ())
  -> v a
  -> v a
sortVecByIdxUsing sortf v =
  VG.generate n (VG.unsafeIndex v . VP.unsafeIndex ixa)
  where
    n = VG.length v
    ixa = VP.create $ do
      ixma <- VPM.generate n id
      let cmp i j = compare (VG.unsafeIndex v i) (VG.unsafeIndex v j)
      sortf cmp ixma
      pure ixma
{-# INLINE sortVecByIdxUsing #-}

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

sameVU, ascVU, descVU, upDownAscVU, upDownDescVU,
  randVU, randSmallVU, ascThenRandVU, randThenAscVU
    :: Int -> VU.Vector (Int, Int, Int)
sameVU n = VU.fromList (map i3FromInt (same n))
ascVU n = VU.fromList (map i3FromInt (asc n))
descVU n = VU.fromList (map i3FromInt (desc n))
upDownAscVU n = VU.fromList (map i3FromInt (upDownAsc n))
upDownDescVU n = VU.fromList (map i3FromInt (upDownDesc n))
randVU n = VU.fromList (map i3FromInt (rand n))
randSmallVU n = VU.fromList (map i3FromInt (randSmall n))
ascThenRandVU n = VU.fromList (map i3FromInt (ascThenRand n))
randThenAscVU n = VU.fromList (map i3FromInt (randThenAsc n))

sameW8VU, randSmallW8VU :: Int -> VU.Vector Word8
sameW8VU n = VU.fromList (map fromIntegral (same n))
randSmallW8VU n = VU.fromList (map fromIntegral (randSmall n))

i3FromInt :: Int -> (Int, Int, Int)
i3FromInt x = let x' = fromIntegral x in (x',x',x')

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

--------------------
-- Specializations
--------------------

-- Needed because GHC fails to generate these
-- https://gitlab.haskell.org/ghc/ghc/-/issues/24765
{-# SPECIALIZE Heap.sort  :: VUM.MVector s (Int, Int, Int) -> ST s () #-}
{-# SPECIALIZE Intro.sort :: VUM.MVector s (Int, Int, Int) -> ST s () #-}
{-# SPECIALIZE Merge.sort :: VUM.MVector s (Int, Int, Int) -> ST s () #-}
{-# SPECIALIZE Tim.sort   :: VUM.MVector s (Int, Int, Int) -> ST s () #-}
{-# SPECIALIZE Heap.sort  :: VUM.MVector s Word8 -> ST s () #-}
{-# SPECIALIZE Intro.sort :: VUM.MVector s Word8 -> ST s () #-}
{-# SPECIALIZE Merge.sort :: VUM.MVector s Word8 -> ST s () #-}
{-# SPECIALIZE Tim.sort   :: VUM.MVector s Word8 -> ST s () #-}

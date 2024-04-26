{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
-- Copyright: (c) 2024 Soumik Sarkar
-- License: BSD-3-Clause
--
-- A stable adaptive mergesort implementation.
--
-- The merging strategy used is "2-merge" as described by
--
-- * Sam Buss, Alexander Korp,
--   /\"Strategies for Stable Merge Sorting\"/,
--   2018,
--   https://arxiv.org/abs/1801.04641
--
module Data.SamSort
  ( sortBy#
  ) where

import Control.Monad (when)
import Data.Bits (finiteBitSize, countLeadingZeros, shiftR)

import GHC.ST (ST(..))
import GHC.Exts
  ( Int#
  , Int(..)
  , MutableArray#
  , MutableByteArray#
  , State#
  , (*#)
  , copyMutableArray#
  , newArray#
  , newByteArray#
  , readArray#
  , readIntArray#
  , writeArray#
  , writeIntArray#
  )

-- | \(O(n \log n)\). Sort a slice of a @MutableArray#@ using a comparison
-- function.
--
-- @offset@ and @length@ must be valid, i.e.
--
-- * @0 <= offset < array size@ .
-- * @0 <= length@ .
-- * @offset + length <= array size@ .
--
sortBy#
  :: (a -> a -> Ordering)  -- ^ comparison
  -> MutableArray# s a
  -> Int#                  -- ^ offset
  -> Int#                  -- ^ length
  -> State# s
  -> State# s
sortBy# cmp ma# off# len# s =
  case sortByST cmp (MA ma#) (I# off#) (I# len#) of
    ST f -> case f s of (# s1, _ #) -> s1
{-# INLINE sortBy# #-}

sortByST
  :: (a -> a -> Ordering)
  -> MA s a
  -> Int
  -> Int
  -> ST s ()
sortByST _ !_ !_ len | len < 2 = pure ()
sortByST cmp ma off len = do
  !swp <- newA halfLen errorElement
  !stk <- newI (lg len)

  let -- Merge [i1,i2) and [i2,i3)
      -- Precondition: i1 < i2 < i3
      merge !i1 !i2 !i3
        | len1 <- i2-i1, len1 <= i3-i2 = do
          let loop !i !j !k = do
                x <- readA swp i
                y <- readA ma j
                if x `gt` y
                then do
                  writeA ma k y
                  if j+1 < i3
                  then loop i (j+1) (k+1)
                  else copyA swp i ma (k+1) (len1-i)
                else do
                  writeA ma k x
                  when (i+1 < len1) $
                    loop (i+1) j (k+1)
          copyA ma i1 swp 0 (i2-i1)
          loop 0 i2 i1
        | otherwise = do
          let loop !i !j !k = do
                x <- readA ma i
                y <- readA swp j
                if x `gt` y
                then do
                  writeA ma k x
                  if i > i1
                  then loop (i-1) j (k-1)
                  else copyA swp 0 ma i1 (j+1)
                else do
                  writeA ma k y
                  when (j > 0) $
                    loop i (j-1) (k-1)
          copyA ma i2 swp 0 (i3-i2)
          loop (i2-1) (i3-i2-1) (i3-1)

      -- [i1,i2) is the last run. Runs before it are on the stack.
      mergeRuns !top0 !i1 !i2
        | i2 >= end = finish top0 i1
        | otherwise = getRun i2 >>= popPush top0 i1 i2

      -- Maintain stack invariants
      popPush !top !i2 !i3 !i4
        | not (badYZ i2 i3 i4) = do
            writeI stk (top+1) i2
            mergeRuns (top+1) i3 i4
        | top < 0 = do
            merge i2 i3 i4
            mergeRuns top i2 i4
        | otherwise = do
            i1 <- readI stk top
            if mergeL i1 i2 i3 i4
            then do
              merge i1 i2 i3
              popPush (top-1) i1 i3 i4
            else do
              merge i2 i3 i4
              popPush (top-1) i1 i2 i4

      finish !top !_ | top < 0 = pure ()
      finish top i2 = do
        i1 <- readI stk top
        merge i1 i2 end
        finish (top-1) i1

  getRun off >>= mergeRuns (-1) off

  where
    gt x y = case cmp x y of GT -> True; _ -> False
    {-# INLINE gt #-}

    !end = off + len
    !halfLen = len `shiftR` 1

    runAsc i | i >= end = pure i
    runAsc i = do
      x <- readA ma (i-1)
      y <- readA ma i
      if x `gt` y
      then pure i
      else runAsc (i+1)

    runDesc i | i >= end = pure i
    runDesc i = do
      x <- readA ma (i-1)
      y <- readA ma i
      if x `gt` y
      then runDesc (i+1)
      else pure i

    -- Insertion sort [i2,i3) into [i1,i3)
    -- Precondition: i1 < i2, i1 < i3
    insLoop !_ i2 i3 | i2 >= i3 = pure i2
    insLoop i1 i2 i3 = do
      x <- readA ma (i2-1)
      y <- readA ma i2
      when (x `gt` y) $ do
        let ins j | j <= i1 = writeA ma j y
            ins j = do
              x' <- readA ma (j-1)
              if x' `gt` y
              then writeA ma j x' *> ins (j-1)
              else writeA ma j y
        writeA ma i2 x *> ins (i2-1)
      insLoop i1 (i2+1) i3

    getRun i | i >= end || i+1 >= end = pure end
    getRun i = do
      x <- readA ma i
      y <- readA ma (i+1)
      !j <- if x `gt` y
        then do
          !j <- runDesc (i+2)
          j <$ reverseA ma i (j-1)
        else runAsc (i+2)
      let k = i + minRunLen
          k' = if k <= 0 -- overflowed
               then end
               else min end k
      insLoop i j k'
{-# INLINE sortByST #-}

minRunLen :: Int
minRunLen = 8

badYZ :: Int -> Int -> Int -> Bool
badYZ i1 i2 i3 = (i2-i1) `shiftR` 1 < (i3-i2)
{-# INLINE badYZ #-}

mergeL :: Int -> Int -> Int -> Int -> Bool
mergeL i1 i2 i3 i4 = (i2-i1) < (i4-i3)
{-# INLINE mergeL #-}

reverseA
  :: MA s a
  -> Int     -- ^ Start
  -> Int     -- ^ End (inclusive)
  -> ST s ()
reverseA !ma = loop
  where
    loop i j | i >= j = pure ()
    loop i j = do
      x <- readA ma i
      readA ma j >>= writeA ma i
      writeA ma j x
      loop (i+1) (j-1)

lg :: Int -> Int
lg 0 = 0
lg i = finiteBitSize i - 1 - countLeadingZeros i
{-# INLINE lg #-}

errorElement :: a
errorElement = error "errorElement"

-- The boxed wrappers MA, MIA, and functions operating on then are for the
-- convenience of working in ST. The alternative is passing around state tokens.
-- All of it should get optimized away.

data MA s a = MA (MutableArray# s a)

newA :: Int -> a -> ST s (MA s a)
newA (I# n#) x = ST $ \s ->
  case newArray# n# x s of (# s1, ma# #) -> (# s1, MA ma# #)
{-# INLINE newA #-}

readA :: MA s a -> Int -> ST s a
readA (MA ma#) (I# i#) = ST $ readArray# ma# i#
{-# INLINE readA #-}

writeA :: MA s a -> Int -> a -> ST s ()
writeA (MA ma#) (I# i#) x = ST $ \s ->
  case writeArray# ma# i# x s of s1 -> (# s1, () #)
{-# INLINE writeA #-}

copyA :: MA s a -> Int -> MA s a -> Int -> Int -> ST s ()
copyA (MA src#) (I# srcOff#) (MA dst#) (I# dstOff#) (I# len#) = ST $ \s ->
  case copyMutableArray# src# srcOff# dst# dstOff# len# s of s1 -> (# s1, () #)
{-# INLINE copyA #-}

data MIA s = MIA (MutableByteArray# s)

newI :: Int -> ST s (MIA s)
newI (I# n#) = ST $ \s ->
  case newByteArray# (n# *# wsz#) s of (# s1, ma# #) -> (# s1, MIA ma# #)
  where
    !(I# wsz#) = finiteBitSize (0 :: Int) `shiftR` 3
{-# INLINE newI #-}

readI :: MIA s -> Int -> ST s Int
readI (MIA ma#) (I# i#) = ST $ \s ->
  case readIntArray# ma# i# s of (# s1, x# #) -> (# s1, I# x# #)
{-# INLINE readI #-}

writeI :: MIA s -> Int -> Int -> ST s ()
writeI (MIA ma#) (I# i#) (I# x#) = ST $ \s ->
  case writeIntArray# ma# i# x# s of s1 -> (# s1, () #)
{-# INLINE writeI #-}

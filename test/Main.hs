{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}

import qualified Data.Foldable as F
import qualified Data.List as L
import Data.Ord (comparing)
import Data.Primitive.Array
  ( MutableArray(..), arrayFromList, runArray, sizeofArray, thawArray )
import Data.Primitive.PrimArray
  ( MutablePrimArray(..)
  , primArrayFromList
  , primArrayToList
  , runPrimArray
  , sizeofPrimArray
  , thawPrimArray
  )
import GHC.ST (ST(..))
import GHC.Exts (Int(..), Int#)

import Test.Tasty (defaultMain, localOption, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests(..), Fun, applyFun, testProperty, (===))
import Test.QuickCheck.Poly (A, OrdA)

import Data.SamSort (sortArrayBy#, sortIntArrayBy#)

main :: IO ()
main = defaultMain $ localOption (QuickCheckTests 5000) $ testGroup "Tests"
  [ testProperty "sortArrayBy#" $ \xs ys zs ->
      sortViaMutableArray (comparing fst) (xs,ys,zs)
      ===
      ((xs :: [(OrdA, A)]) ++ L.sortBy (comparing fst) ys ++ zs)
  , testProperty "sortIntArrayBy#" $ \f xs ys zs ->
      sortInts (comparing (applyFun (f :: Fun Int OrdA))) (xs,ys,zs)
      ===
      (xs ++ L.sortBy (comparing (applyFun f)) ys ++ zs)
  ]

sortViaMutableArray
  :: (a -> a -> Ordering)
  -> ([a],[a],[a])
  -> [a]
sortViaMutableArray cmp (xs,ys,zs) = F.toList $ runArray $ do
  let a = arrayFromList (xs ++ ys ++ zs)
  ma@(MutableArray ma#) <- thawArray a 0 (sizeofArray a)
  ST $ \s -> case sortArrayBy# cmp ma# (len# xs) (len# ys) s of s1 -> (# s1, ma #)

sortInts
  :: (Int -> Int -> Ordering)
  -> ([Int],[Int],[Int])
  -> [Int]
sortInts cmp (xs,ys,zs) = primArrayToList $ runPrimArray $ do
  let a = primArrayFromList (xs ++ ys ++ zs)
  ma@(MutablePrimArray ma#) <- thawPrimArray a 0 (sizeofPrimArray a)
  ST $ \s ->
    case sortIntArrayBy#
           (\x# y# -> cmp (I# x#) (I# y#))
           ma#
           (len# xs)
           (len# ys)
           s of
      s1 -> (# s1, ma #)

len# :: [a] -> Int#
len# as = case length as of I# n# -> n#

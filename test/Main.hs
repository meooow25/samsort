{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}

import qualified Data.Foldable as F
import qualified Data.List as L
import Data.Ord (comparing)
import Data.Primitive.Array
  ( MutableArray(..), arrayFromList, runArray, sizeofArray, thawArray )
import GHC.ST (ST(..))
import GHC.Exts (Int(..), Int#, MutableArray#, State#)

import Test.Tasty (defaultMain, localOption, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests(..), testProperty, (===))
import Test.QuickCheck.Poly (A, OrdA)

import Data.SamSort (sortBy#)

main :: IO ()
main = defaultMain $ localOption (QuickCheckTests 5000) $ testGroup "Tests"
  [ testProperty "sortBy#" $ \xs ys zs ->
      withMutableArray
        (\ma# -> sortBy# (comparing fst) ma# (len# xs) (len# ys))
        (xs ++ ys ++ zs)
      ===
      (xs ++ L.sortBy (comparing fst) (ys :: [(OrdA, A)]) ++ zs)
  ]

len# :: [a] -> Int#
len# xs = case length xs of I# n# -> n#

withMutableArray
  :: (forall s. MutableArray# s a -> State# s -> State# s)
  -> [a]
  -> [a]
withMutableArray f xs = F.toList $ runArray $ do
  let a = arrayFromList xs
  ma@(MutableArray ma#) <- thawArray a 0 (sizeofArray a)
  ST $ \s -> case f ma# s of s1 -> (# s1, ma #)

{-|
Module      : Data.MedianStream
Description : Main module
Copyright   : (c) Joseph Canero, 2016
License     : BSD-3
Maintainer  : jmc41493@gmail.com
Stability   : experimental
Portability : POSIX
-}

module Data.MedianStream
(
  MedianStream
, (+>)
, (<+)
, empty
, insert
, median
, medianSame
, medianWith
, size
, fromList
, insertList
) where

import Control.Applicative ((<$>), (<*>))
import Data.Heap (MaxHeap, MinHeap, Heap)
import qualified Data.Heap as Heap
import Data.List (foldl')

type Left a  = MaxHeap a
type Right a = MinHeap a

-- | A MedianStream is a data type that can be inserted into and queried
-- to get a median of a stream of numeric values.
data MedianStream a = MedianStream !(Left a) !(Right a)
  deriving Eq
-- Invariants:
--
-- 1. Each element of the left heap is less than or equal to each
--    element of the right heap
--
-- 2. In MedianStream lh rh,
--    Heap.size rh <= Heap.size lh <= Heap.size rh + 1

-- | Infix wrapper around insert with the MedianStream on the left.
--
-- Complexity: \( O (\lg n) \)
(+>) :: Ord a => MedianStream a -> a -> MedianStream a
(+>) ms a = insert a ms

-- | Infix wrapper around insert with the MedianStream on the right.
-- Complexity: \( O (\lg n) \)
(<+) :: Ord a => a -> MedianStream a -> MedianStream a
(<+) = insert

-- | Create an empty MedianStream with no values.
--
-- Complexity: \( O(1) \)
empty :: MedianStream a
empty = MedianStream Heap.empty Heap.empty

-- | Insert a new numeric value into the median stream.
--
-- Complexity: \( O(\lg n) \)
insert :: Ord a => a -> MedianStream a -> MedianStream a
insert a ms@(MedianStream lh rh)
  -- \| Heap.size lh < Heap.size rh = error "boom"
  -- \| Heap.size lh > Heap.size rh + 1 = error "bang"
  | even $ size ms = oddMedianStream
  | otherwise      = evenMedianStream
  where
    -- Note: One might reasonably be concerned about using
    -- Heap.view when we may only need the greatest/least
    -- element. But Heap.view builds the remaining heap
    -- lazily, so we don't actually have to build it unless
    -- we need it. And everything inlines nicely, so we shouldn't
    -- even build the unnecessary thunks. Furthermore,
    -- viewHead itself is implemented using view, so using that
    -- surely won't help us.

    -- The result has an odd number of elements
    oddMedianStream
      | Just (mid, tl) <- Heap.view rh
      , a >= mid
      = MedianStream (Heap.insert mid lh) (Heap.insert a tl)
      | otherwise = MedianStream (Heap.insert a lh) rh

    -- The result has an even number of elements
    evenMedianStream
      | Just (mid, tl) <- Heap.view lh
      , a < mid
      = MedianStream (Heap.insert a tl) (Heap.insert mid rh)
      | otherwise = MedianStream lh (Heap.insert a rh)

-- | Query the 'MedianStream' for the median of the stream of numbers
-- inserted so far.
--
-- Complexity: \( O(1) \)
median :: (Real a, Fractional b) => MedianStream a -> Maybe b
median = medianWith
           (fromRational . toRational)
           (\ x l -> (/2 ) . fromRational $ toRational (x + l))

-- | A version of 'median' optimized for the case where the result
-- type is the same as the element type.
--
-- Complexity: \( O(1) \)
medianSame :: (Real a, Fractional a) => MedianStream a -> Maybe a
medianSame = medianWith id (\ x l -> (x + l)/2)

-- | Generalized median: @median f g ms@ returns @f x@ if there's a
-- unique contender, @g x y@ if there are two.  Use @median id const@
-- for a least element-biased call if you don't care too much about
-- the middle.
--
-- Complexity: \( O(1) \)
medianWith :: Ord a => (a -> b) -> (a -> a -> b) -> MedianStream a -> Maybe b
medianWith single double ms@(MedianStream lh rh)
  | even $ size ms = double <$> Heap.viewHead lh <*> Heap.viewHead rh
  | otherwise      = single <$> Heap.viewHead lh

-- | Returns the number of elements in the MedianStream.
--
-- Complexity: \( O(1) \)
size :: MedianStream a -> Int
size (MedianStream lh rh) = Heap.size lh + Heap.size rh

-- | Creates a MedianStream from a list of input elements.
--
-- Complexity: \( O(n \lg n) \)
fromList :: Ord a => [a] -> MedianStream a
fromList = insertList empty

-- | Adds a list of input elements to an existing MedianStream
--
-- Complexity: \( O(n \lg n) \)
insertList :: Ord a => MedianStream a -> [a] -> MedianStream a
insertList = foldl' (+>)

{-# LANGUAGE GADTs #-}

module Data.MedianStream
(
  MedianStream
, (+>)
, (<+)
, empty
, insert
, median
, size
) where

import Data.Heap (MaxHeap, MinHeap, Heap)
import qualified Data.Heap as Heap hiding (MaxHeap, MinHeap, Heap)
import Data.Maybe (fromJust)

type Left a  = MaxHeap a
type Right a = MinHeap a

data MedianStream a where
  MedianStream :: (Real a, Eq a) => Left a -> Right a -> MedianStream a

(+>) :: MedianStream a -> a -> MedianStream a
(+>) ms a = insert a ms

(<+) :: a -> MedianStream a -> MedianStream a
(<+) = insert

empty :: (Real a, Eq a) => MedianStream a
empty = MedianStream Heap.empty Heap.empty

insert :: a -> MedianStream a -> MedianStream a
insert a ms@(MedianStream lh rh)
  | even $ size ms = oddMedianStream
  | otherwise      = evenMedianStream
    where
      oddMedianStream
        | not (Heap.null rh) && a >= fromJust (Heap.viewHead rh) =
          uncurry MedianStream $ popAndSwap lh rh a
        | otherwise = MedianStream (Heap.insert a lh) rh
      evenMedianStream
        | a < fromJust (Heap.viewHead lh) =
          uncurry (flip MedianStream) $ popAndSwap rh lh a
        | otherwise = MedianStream lh (Heap.insert a rh)

median :: MedianStream a -> Maybe Double
median ms@(MedianStream lh rh)
  | even $ size ms = average <$> Heap.viewHead lh <*> Heap.viewHead rh
  | otherwise      = (fromRational . toRational) <$> Heap.viewHead lh

size :: MedianStream a -> Int
size (MedianStream lh rh) = Heap.size lh + Heap.size rh

popAndSwap :: (Heap.HeapItem pol1 a,
               Heap.HeapItem pol2 a)
           => Heap pol1 a
           -> Heap pol2 a
           -> a
           -> (Heap pol1 a, Heap pol2 a)
popAndSwap lh rh a =
  let
    ([top], srh) = Heap.splitAt 1 rh
    nlh          = Heap.insert top lh
    nrh          = Heap.insert a   srh
    in (nlh, nrh)

average :: (Real a) => a -> a -> Double
average x l = (/2 ) . fromRational $ toRational (x + l)

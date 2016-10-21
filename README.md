# median-stream
<a href="https://hackage.haskell.org/package/median-stream">Hackage</a>
[![Build Status](https://travis-ci.org/caneroj1/median-stream.svg?branch=master)](https://travis-ci.org/caneroj1/median-stream)
Haskell data structure for constant-time queries for the median of a stream of numeric data, where insertion into the stream occurs in O(nlgn). ```median-stream``` uses two heaps (a max-heap and a min-heap) to enable constant time access to the median. If there is an even number of elements in the stream, then the median is the average of the head of the two heaps. If there is an odd number, the median is the head of the max heap.

## Usage

```haskell
Data.MedianStream> let medianStream = empty +> 1 +> 3 +> 4 +> 2
Data.MedianStream> median medianStream
Just 2.5
Data.MedianStream> let medianStream2 = medianStream +> 0 +> (-1) +> 10
Data.MedianStream> median medianStream2
Just 2.0
```

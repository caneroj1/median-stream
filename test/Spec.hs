{-# LANGUAGE TemplateHaskell #-}

import Data.List hiding (insert)
import Data.MedianStream
import Test.QuickCheck

prop_medianStreamOnInts :: [Int] -> Bool
prop_medianStreamOnInts xs = medianOf xs == median (foldr insert empty xs)

prop_medianStreamOnDoubles :: [Double] -> Bool
prop_medianStreamOnDoubles xs = medianOf xs == median (foldr insert empty xs)

prop_medianStreamOnIntsV2 :: [Int] -> Bool
prop_medianStreamOnIntsV2 xs = medianOf xs == median (foldr (<+) empty xs)

prop_medianStreamOnDoublesV2 :: [Double] -> Bool
prop_medianStreamOnDoublesV2 xs = medianOf xs == median (foldr (<+) empty xs)

prop_medianStreamOnIntsV3 :: [Int] -> Bool
prop_medianStreamOnIntsV3 xs = medianOf xs == median (foldl' (+>) empty xs)

prop_medianStreamOnDoublesV3 :: [Double] -> Bool
prop_medianStreamOnDoublesV3 xs = medianOf xs == median (foldl' (+>) empty xs)

medianOf :: (Real a, Eq a) => [a] -> Maybe Double
medianOf []  = Nothing
medianOf [x] = Just . fromRational $ toRational x
medianOf xs  =
  let sorted = sort xs
      len    = length xs
    in case even len of
      True  -> let [x, y] = take 2 $ drop (len `div` 2 - 1) sorted
                in Just $ average x y
      False -> medianOf . take 1 $ drop (len `div` 2) sorted

average :: (Real a) => a -> a -> Double
average x l = (/2) . fromRational $ toRational (x + l)

return []
runTests = $quickCheckAll

main = runTests

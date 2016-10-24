{-# LANGUAGE TemplateHaskell #-}

import Data.List hiding (insert)
import Data.MedianStream
import Test.QuickCheck
import Utils

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

return []
runTests = $quickCheckAll

main = runTests

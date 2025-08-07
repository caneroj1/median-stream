{-# LANGUAGE TemplateHaskell #-}

import Data.List hiding (insert)
import Data.MedianStream
import Test.QuickCheck
import Utils
import System.Exit

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

prop_medianStreamOnIntsFromList :: [Int] -> Bool
prop_medianStreamOnIntsFromList xs = medianOf xs == median (fromList xs)

prop_medianStreamOnDoublesFromList :: [Double] -> Bool
prop_medianStreamOnDoublesFromList xs = medianOf xs == median (fromList xs)

prop_medianStreamOnIntsInsertList :: [Int] -> Bool
prop_medianStreamOnIntsInsertList xs =
  let (fh, sh) = halves xs
      ms       = fromList fh
    in medianOf fh         == median ms &&
       medianOf (fh ++ sh) == median (insertList ms sh)

prop_medianStreamOnDoublesInsertList :: [Double] -> Bool
prop_medianStreamOnDoublesInsertList xs =
  let (fh, sh) = halves xs
      ms       = fromList fh
    in medianOf fh         == median ms &&
       medianOf (fh ++ sh) == median (insertList ms sh)

halves :: [a] -> ([a], [a])
halves ls = splitAt mid ls
  where mid = length ls `div` 2

prop_medianStreamOnBoolsMedianWith :: [Bool] -> Bool
prop_medianStreamOnBoolsMedianWith xs =
  let l = length xs
      t = length (filter id xs)
      expected | l == 0 = Nothing
               | odd l = Just (Left (2*t > l))
               | 2*t < l = Just (Right (False,False))
               | 2*t > l = Just (Right (True,True))
               | otherwise = Just (Right (False,True))
  in medianWith Left ((Right .) . (,)) (fromList xs) == expected

return []

runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = do
  good <- runTests
  if good
    then exitSuccess
    else exitFailure

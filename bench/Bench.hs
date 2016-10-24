import Control.Monad
import Data.MedianStream
import Data.Time.Clock
import System.Random
import Utils

main :: IO ()
main = benchmarks

benchmarks :: IO ()
benchmarks =
  forM_ ns $ \n -> do
    randomList <- take n . randoms <$> newStdGen
    putStrLn $ "\nBENCHMARKING: " ++ show n ++ " elements."
    benchBoth randomList n
  where ns = [10, 100, 1000, 10000, 100000, 200000, 500000]

benchBoth :: [Int] -> Int -> IO ()
benchBoth ls n = do
  putStrLn "Naive Median"
  benchNaiveMedian ls n
  putStrLn "Median Stream"
  benchMedianStream ls n

benchMedianStream :: [Int] -> Int -> IO ()
benchMedianStream ls size = do
  putStrLn "------------"
  putStrLn $ "First " ++ show inc ++ " elements."
  let (ls1, ls2) = splitAt inc ls
  let ms1        = fromList ls1
  timeOpMS ms1
  putStrLn $ "Next " ++ show inc ++ " elements."
  let (ls3, ls4) = splitAt inc ls2
  let ms2        = insertList ms1 ls3
  timeOpMS ms2
  putStrLn $ "Third " ++ show inc ++ " elements."
  let (ls5, ls6) = splitAt inc ls4
  let ms3        = insertList ms2 ls5
  timeOpMS ms3
  putStrLn $ "Last  " ++ show inc ++ " elements."
  let (ls7, ls8) = splitAt inc ls6
  let ms4        = insertList ms3 ls7
  timeOpMS ms4
  putStrLn ""
  where
    inc = size `div` 4

benchNaiveMedian :: [Int] -> Int -> IO ()
benchNaiveMedian ls size = do
  putStrLn "------------"
  putStrLn $ "First " ++ show inc ++ " elements."
  let (ls1, ls2) = splitAt inc ls
  timeOp medianOf ls1
  putStrLn $ "Next " ++ show inc ++ " elements."
  let (ls3, ls4) = splitAt inc ls2
  let ns1        = ls1 ++ ls3
  timeOp medianOf ns1
  putStrLn $ "Third " ++ show inc ++ " elements."
  let (ls5, ls6) = splitAt inc ls4
  let ns2        = ns1 ++ ls5
  timeOp medianOf ns2
  putStrLn $ "Last  " ++ show inc ++ " elements."
  let (ls7, ls8) = splitAt inc ls6
  let ns3        = ns2 ++ ls7
  timeOp medianOf ns3
  where
    inc = size `div` 4

timeOpMS :: MedianStream a -> IO ()
timeOpMS ms = do
  start <- getCurrentTime
  logMedian $ median ms
  end   <- getCurrentTime
  putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
  where
    logMedian Nothing  = return ()
    logMedian (Just m) = putStrLn $ "Median: " ++ show m

timeOp :: ([Int] -> Maybe Double) -> [Int] -> IO ()
timeOp fn ls = do
  start <- getCurrentTime
  logMedian $ fn ls
  end   <- getCurrentTime
  putStrLn $ "Elapsed: " ++ show (diffUTCTime end start)
  where
    logMedian Nothing  = return ()
    logMedian (Just m) = putStrLn $ "Median: " ++ show m

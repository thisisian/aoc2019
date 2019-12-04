module Day1 where

import Common

import System.IO

parse :: String -> [Int]
parse = map read . lines

day1 :: IO Int
day1 = do
  wts <- parse <$> readFile "./inputs/1.txt"
  return $ sum $ map fuel wts

day1p2 :: IO Int
day1p2 = do
  wts <- parse <$> readFile "./inputs/1.txt"
  return $ sum $ map f wts

 where
  f :: Int -> Int
  f x
    | fl > 0  = fl + f fl
    | otherwise = 0
   where fl = fuel x

fuel x = (x `div` 3) - 2

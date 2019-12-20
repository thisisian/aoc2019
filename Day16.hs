module Day16 where

import Debug.Trace

day16 :: IO ()
day16 = do
  digits <- map (read . (:[])) . head . lines <$> readFile "./inputs/16.txt"
  print $ fft' 4 1 [1,2,3,4,5,6,7,8]
  print $ fft 99 [1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5]
  print $ "Part 1: " ++ concatMap show (fft 99 digits)

basePattern ::[Int]
basePattern = [0,1,0,-1]

fft :: Int -> [Int] -> [Int]
fft totIters xs = fft' totIters 0 xs


fft' :: Int -> Int -> [Int] -> [Int]
fft' totIters iter ys = if (totIters == iter) then outputDigits 0 ys else fft' totIters (iter+1) (outputDigits 0 ys)

outputDigits digitIter ys = if digitIter == length ys then [] else outputDigit digitIter ys : outputDigits (digitIter + 1) ys

outputDigit iter ys = (abs $ sum $ zipWith (*) ys (tail . concat . repeat $ cycleGroups (iter+1) (basePattern))) `mod` 10

f iter zs = (zipWith (*) zs (tail . concat .  repeat . cycleGroups iter $ basePattern))

cycleGroups :: Int -> [a] -> [a]
cycleGroups groupSize xs = cycleGroups' 0 xs
 where
  cycleGroups' iter (y:ys) =
    if iter == groupSize
    then cycleGroups' 0 ys
    else y : cycleGroups' (iter+1) (y:ys)
  cycleGroups' _ [] = []

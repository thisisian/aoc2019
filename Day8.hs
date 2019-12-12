module Day8 where

import Data.List
import Common
import Control.Monad

day8 = do
  img <- parse . head . lines <$> readFile "./inputs/8.txt"
  print $ pt1 img
  printImg $ pt2 img


parse  :: String -> [[Int]]
parse s = groupN (25*6) . map read . map ((:[])) $ s

pt1 ls = nDigits 1 * nDigits 2

  where
    nDigits d = length . filter (== d) $ leastZeros
    leastZeros = minimumBy (\a b ->(compare (length. filter (==0) $ a) (length.filter (==0) $ b))) ls


pt2 :: [[Int]] -> [Int]
pt2 img' = foldl f (head img) img
 where
   f a b = zipWith f' a b
   f' p1 p2 = if p2 == 2 then p1 else p2
   img = reverse img'


printImg :: [Int] -> IO ()
printImg img = do
  forM_ (groupN 25 img) (\line -> do forM_ line (\c -> putStr $ show c); putStr "\n")

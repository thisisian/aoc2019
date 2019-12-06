module Day4 where

import Data.List

day4 = day4' (138241, 674034)

day4' :: (Int, Int) -> Int
day4' (l, h) =
  length . filter pass . map show $ [138241..674034]

day4p2 = day4p2' (138241, 674034)

day4p2' :: (Int, Int) -> Int
day4p2' (l, h) =
  length . filter pass2 . map show $ [138241..674034]

pass xs = xs == sort xs && xs /= nub xs

pass2 xs = pass xs && (elem 2 . map length $ group xs)

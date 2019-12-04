module Day3 where

import qualified Data.Set as S
import Data.List

import Common

type Pt = (Int, Int)

day3 = do
  ps <- parse <$> readFile "./inputs/3.txt"
  day3' ps

day3' (l1, l2) =
  print $
    S.findMin
    . S.map (\(x, y) -> abs x + abs y)
    $ intersections (path l1) (path l2)

day3p2 = do
  ps <- parse <$> readFile "./inputs/3.txt"
  day3p2' ps

day3p2' (l1, l2) =
    print $
      S.findMin
      . S.map
         (\pt ->
            head (elemIndices pt p1) +
            head (elemIndices pt p2) + 2)
         $ intersections p1 p2
 where
  p1 = path l1
  p2 = path l2

parse :: String -> ([Pt], [Pt])
parse s = (l1, l2)
 where
  [l1, l2] =
    map (map parsePt . split ',') . lines $ s

parsePt :: String -> Pt
parsePt ('R':x) = (read x, 0)
parsePt ('L':x) = (-(read x), 0)
parsePt ('U':x) = (0, read x)
parsePt ('D':x) = (0, -(read x))

intersections :: [Pt] -> [Pt] -> S.Set Pt
intersections p1 p2 =
  S.intersection (S.fromList p1) (S.fromList p2)

path ps = reverse . fst $ foldl f ([], (0,0)) ps
 where
  f (ps, pi) dp = (steps ++ ps, head steps)
    where steps = reverse $ step pi dp

step (x1, y1) (0, dy) =
  [(x1, y) | y <- range y1 (y1+dy) ]
step (x1, y1) (dx, 0) =
  [(x, y1) | x <- range x1 (x1+dx) ]

range a b =
  if a > b
  then reverse [b .. a-1]
  else [a+1 .. b]

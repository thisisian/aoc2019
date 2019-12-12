{-# LANGUAGE ParallelListComp #-}
module Day10 where

import Data.List
import Data.Function
import Control.Monad.Writer
import Control.Monad.State
import qualified Data.Set as S

import Common

import Debug.Trace

day10 :: IO ()
day10 = do
  asteroids <- parse <$> readFile "./inputs/10.txt"
  runTests
  let
    (best, ct) = bestAsteroid asteroids
  --putStrLn $ show (maximumBy (compare `on` (\(pt, ct) -> ct)))
  putStrLn $ show $  zappedList asteroids best

  return ()

bestAsteroid :: [Pt Int] -> (Pt Int, Int)
bestAsteroid as = maximumBy (compare `on` snd) as'
 where
  as' = map ((\a -> (a, (length . seen as $ a)))) as


seen :: [Pt Int] -> Pt Int -> [Pt Int]
seen as a = filter (clearSight (S.fromList as) a) $ as
  where
    seenList = filter (clearSight (S.fromList as) a) as

clearSight asteroids a1 a2 =
  if a1 == a2 then False
  else
    if S.intersection asteroids pointsOnLine /= S.fromList [a1, a2]
    then False
    else True
 where pointsOnLine = pointsInCenter a1 a2

parse :: String -> [Pt Int]
parse s = foldr f [] $ charEnum
 where
  mapLines = lines s
  charEnum = zip [0..] . concat $ mapLines
  (width, height) = (length . head $ mapLines, length mapLines)

  f (i, c) acc = if c == '#'
    then ((x, y) : acc)
    else acc
   where
     x = i `mod` width
     y = i `div` width

type Pt a = (a, a)

-- Get the discrete points in a line between an initial and final point.
pointsInCenter :: (Show a, Integral a) => Pt a -> Pt a -> S.Set (Pt a)
pointsInCenter (xi, yi) (xf, yf) =
  if (xi, yi) == (xf, yf)
  then S.fromList []
  else S.fromList $ zipWith (,) (range xi dx xf) (range yi dy yf)
 where
  (dx', dy') = (xf - xi, yf - yi)
  gcd_dx_dy = gcd dx' dy'
  (dx, dy) = (dx' `div` gcd_dx_dy, dy' `div` gcd_dx_dy)

range :: Integral a => a -> a -> a -> [a]
range initial step final= unfoldr f initial
 where
  (maxV, minV) = (max initial final, min initial final)
  f x = if x >= minV && x <= maxV then Just (x, x + step) else Nothing

zappedList :: [Pt Int] -> Pt Int -> [(Int, Pt Int)]
zappedList as' base =
  fst $ runState (execWriterT (loop base)) (as', 1)
 where
   loop
     :: (MonadState ([Pt Int], Int) m, MonadWriter [(Int, Pt Int)] m)
     => Pt Int -> m ()
   loop base = do
     (as, i) <- get
     let
       seenAsteroids = sortBy (compare `on` (\x -> (-1) * angle base x)) $ seen as base
     traverse destroyAsteroid seenAsteroids

     (_, i') <- get
     if i' >= 200 || (length as <= 1)
       then return ()
       else loop base

   destroyAsteroid a = do
     (as, i) <- get
     let as' = delete a as
     tell [(i, a)]
     modify (\s -> (as', i+1))

angle :: Integral a => Pt a -> Pt a -> Float
angle (x1, y1) (x2, y2) =
  atan2 (fromIntegral (x2-x1)) (fromIntegral (y2-y1))

runTests :: IO ()
runTests = do
  let
    a3 = parse testMap3
    (best, ct) = bestAsteroid a3

  print best
  print (zappedList a3 (8,3))

 where
  testMap3 = ".#....#####...#..\n##...##.#####..##\n##...#...#.#####.\n..#.....#...###..\n..#.#.....#....##"

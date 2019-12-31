module Day19 where

import Common
import IntMachine
import Data.Maybe

day19 :: IO ()
day19 = do
  mach <- machineFromFile "./inputs/19.txt"
  putStrLn $ "Part 1: " ++ show (sum . tractorMap $ mach)
  putStrLn $ "Part 2: " ++ show (part2 mach)

findPt :: Machine -> Pt -> Int
findPt m (x, y) = head . evalMachine [x, y] $ m

tractorMap :: Machine -> [Int]
tractorMap m =
  [ findPt m (x, y) | x <- [0..49], y <- [0..49]]

part2 :: Machine -> Int
part2 m =
  ptToSoln . head . mapMaybe (fits m . findBeamHoriz m) $ [0..]
 where ptToSoln (x, y) = 10000 * x + y

-- Scan a vertical line for the tractor beam point with a minimum y
findBeamHoriz :: Machine -> Int -> Pt
findBeamHoriz m xpos = (xpos, ypos)
 where
   ypos = head . filter (\y -> findPt m (xpos, y) == 1) $ [0..]

fits :: Machine -> Pt -> Maybe Pt
fits m (x,y) =
  if findPt m (x-99, y+99) == 1
  then Just (x-99, y)
  else Nothing

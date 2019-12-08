module Day1 where

day1 :: IO ()
day1 = do
  wts <- parse <$> readFile "./inputs/1.txt"
  putStrLn $ "Part 1: " ++ show (sum . map fuel $ wts)
  putStrLn $ "Part 2: " ++ show (sum . map allFuel $ wts)

 where
  parse = map read . lines

allFuel :: Int -> Int
allFuel x
  | fl > 0  = fl + allFuel fl
  | otherwise = 0
 where fl = fuel x

fuel :: Int -> Int
fuel x = (x `div` 3) - 2

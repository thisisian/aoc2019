module Common where

import qualified Data.Text as T
import qualified Data.Map as M
import Data.List
import Data.Function
import Data.Graph

type Pt = (Int, Int)

split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
  ""  -> []
  s' -> s'' : split ',' rest
   where (s'', rest) = break (== c) s'

assertEq :: forall a. (Show a, Eq a) => a -> a -> IO Bool
assertEq expected actual = if expected == actual
  then do
    putStrLn "Passed"
    return True
  else do
    putStrLn $ "Failed. Got: " ++ show expected ++ " Expected: " ++ show actual
    return False

groupN :: Int -> [a] -> [[a]]
groupN _ [] = []
groupN n a = take n a : groupN n (drop n a)

class ShowChar a where
  displayChar :: a -> Char

showDisplay :: ShowChar a => a -> M.Map Pt a -> T.Text
showDisplay def dMap =
  T.unfoldr f ((minX, minY), False)
 where
  f (pt@(x, y), newLine) =
    if newLine then Just ('\n', ((x, y), False))
    else
      if y > maxY then Nothing
      else
        if x == maxX then Just (charAtPt pt, ((minX, y+1), True))
        else Just (charAtPt pt, ((x+1, y), False))

  charAtPt pt = case M.lookup pt $ dMap of
    Nothing -> displayChar def
    Just a -> displayChar a
  minX = fst . minimumBy (compare `on` fst) $ keys
  maxX = fst . maximumBy (compare `on` fst) $ keys
  minY = snd . minimumBy (compare `on` snd) $ keys
  maxY = snd . maximumBy (compare `on` snd) $ keys
  keys = M.keys dMap

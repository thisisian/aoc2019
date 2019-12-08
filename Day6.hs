module Day6 where

import qualified Data.Map as M
import Data.Tree
import Data.Tuple
import Data.Maybe
import qualified Text.Parsec as P

day6 :: IO ()
day6 = do
  edges <- parse <$> readFile "./inputs/6.txt"
  let root = findRoot edges
      tree = mkTree edges root
      path = fromJust $ findPath tree "YOU" "SAN"
  putStrLn $ "Part 1: " ++ show (ctOrbits tree)
  putStrLn $ "Part 2: " ++ show (length path)

parse :: String -> [(String, String)]
parse s = case P.parse pOrbits "" s of
  Left _   -> undefined
  Right ps -> ps
 where
   pOrbits = P.many1 pOrbit <* P.eof
   pOrbit  = do
     a <- P.many1 P.alphaNum <* P.char ')'
     b <- P.many1 P.alphaNum <* P.spaces
     return (a, b)

pathFromRoot :: Tree String -> String -> Maybe [String]
pathFromRoot t s = fmap reverse . findPath' [] $ t
  where
   findPath' ps (Node n ns) =
     if s == n
     then Just ps
     else listToMaybe . mapMaybe (findPath' (n:ps)) $ ns

findPath :: Tree String -> String -> String -> Maybe [String]
findPath t n1 n2 = do
  p1 <- pathFromRoot t n1
  p2 <- pathFromRoot t n2
  return $ diffLists p1 p2

 where
  diffLists (x:xs) (y:ys) =
    if x == y then diffLists xs ys
    else reverse (x:xs) ++ (y:ys)

ctOrbits :: Tree String -> Int
ctOrbits tree = sum $
  zipWith
    (\d ns -> d * length ns) [0..] (levels tree)

mkTree :: [(String, String)] -> String -> Tree String
mkTree edges root = unfoldTree (\b -> case M.lookup b m of
                                 Nothing -> (b, [])
                                 Just as -> (b, as)) root
 where
   m :: M.Map String [String]
   m = M.fromListWith (++) (map (\(a,b) -> (a, [b])) edges)

findRoot :: [(String, String)] -> String
findRoot es@((a, b):_) = loop (a, b)
 where
   m = M.fromList (map swap es)
   loop :: (String, String) -> String
   loop (a, b) = case M.lookup b m of
     Just e -> loop (b, e)
     Nothing -> b

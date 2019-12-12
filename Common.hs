module Common where

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

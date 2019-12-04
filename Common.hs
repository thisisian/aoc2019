module Common where

split :: Char -> String -> [String]
split c s = case dropWhile (== c) s of
  ""  -> []
  s' -> s'' : split ',' rest
   where (s'', rest) = break (== c) s'

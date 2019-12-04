module Day2 where

import qualified Data.Vector as V

import Common

day2 :: IO ()
day2 = do
  code <- V.fromList . map read . split ',' <$> readFile "./inputs/2.txt"

  print $ runCode 12 2 code
  print $ day2p2 code

 where
  day2p2 code = head [(n, v) | n <- [0..99],
                                v <- [0..99],
                                runCode n v code == 19690720 ]



  runCode :: Int -> Int -> V.Vector Int -> Int
  runCode n v code = runCode' 0 (code V.// [(1, n), (2,v)])


  runCode' :: Int -> V.Vector Int -> Int
  runCode' ix code = case code V.! ix of
    1 -> doOp (+) ix code
    2 -> doOp (*) ix code
    99 -> V.head code
    x -> error $ "Invalid program " ++ (show code)

  doOp :: (Int -> Int -> Int) -> Int -> V.Vector Int -> Int
  doOp binOp ix code =
    runCode' (ix+4) (code V.// [(dst, a `binOp` b)])
   where
    a = code V.! (code V.! (ix+1))
    b = code V.! (code V.! (ix+2))
    dst = code V.! (ix+3)

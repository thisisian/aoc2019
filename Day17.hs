module Day17 where

import IntMachine
import Data.Char
import Data.Maybe
import Data.List
import Common
import qualified Data.Set as S

day17 :: IO ()
day17 = do
  code <- map read . split ',' <$> readFile "./inputs/17.txt"
  let
    machine1 = newMachine code
    output = ascii (evalMachine [] machine1)
    charPts = charPtPairs output
    scflds = scaffolds charPts
    start = fromJust . lookup ('^') $  charPts
    pt1 = sum . fmap (\(x,y) -> x*y) . S.toList . intersections $ scflds

    machine2 = newMachine (2 : tail code)
    (pt2, _) = runMachine (mainRoutine ++ fA ++ fB ++ fC ++ (map ord "n\n")) machine2

  putStrLn $ "Part 1: " ++ show pt1
  putStrLn $ "Part 2: " ++ show (last pt2)

ascii :: [Int] -> String
ascii = map chr


data RawCommand = RawTurn Hand | RawFwd
  deriving (Show, Eq)

data Command = Turn Hand | Fwd Int

instance Show Command where
  show (Turn hand) = show hand
  show (Fwd x) = show x

data Hand = L | R
  deriving (Show, Eq)

ahead :: Dir -> Pt -> Pt
ahead dir (x,y) = case dir of
  North -> (x, y-1)
  South -> (x, y+1)
  East  -> (x+1, y)
  West  -> (x-1, y)

toCommand :: [RawCommand] -> [Command]
toCommand rc = map toCommand' (group rc)
 where
   toCommand' [RawTurn L] = Turn L
   toCommand' [RawTurn R] = Turn R
   toCommand' xs = Fwd (length xs)

mainRoutine, fA, fB, fC :: [Int]
mainRoutine = map ord "A,B,A,C,B,A,C,A,C,B\n"
fA = map ord "L,12,L,8,L,8\n"
fB = map ord "L,12,R,4,L,12,R,6\n"
fC = map ord "R,4,L,12,L,12,R,6\n"

commands :: Pt -> Dir -> S.Set Pt -> [RawCommand]
commands pt dir scaffold =
  if S.member (ahead dir pt) scaffold then
    RawFwd : commands (ahead dir pt) dir scaffold
    else if S.member (ahead (leftOf dir) pt) scaffold then
      (RawTurn L) : commands pt (leftOf dir) scaffold
      else if S.member (ahead (rightOf dir) pt) scaffold then
        (RawTurn R) : commands pt (rightOf dir) scaffold
        else []


intersections :: S.Set Pt -> S.Set Pt
intersections pts =
  S.filter (\pt -> (length . filter (\neigh -> S.member neigh pts) $ (neighbors pt) ) == 4) pts

scaffolds :: [(Char, Pt)] -> S.Set Pt
scaffolds ps = S.fromList $ map snd . filter (\(c, pt) -> c == '#') $ ps

charPtPairs :: [Char] -> [(Char, Pt)]
charPtPairs cs = charPt
 where
  charPt :: [(Char, Pt)]
  charPt = concatMap (\(line, y) -> map (\(c, x) -> (c, (x, y))) (zip line [0..])) linesYCoord
  linesYCoord :: [([Char], Int)]
  linesYCoord = zip (lines cs) [0..]

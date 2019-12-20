{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Day13 where

import Control.Monad.Writer
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import qualified Data.Map as M
import Data.List
import Data.Vector (Vector, (!), (//))
import Data.Foldable
import Data.Function
import Data.Maybe
import qualified Data.Vector as V
import Debug.Trace

import Common

data Mode = Position | Immediate | Relative
  deriving (Show, Eq)

data Op
  = Add Mode Mode Mode
  | Mul Mode Mode Mode
  | Input Mode
  | Output Mode
  | JumpT Mode Mode
  | JumpF Mode Mode
  | LessThan Mode Mode Mode
  | EqTo Mode Mode Mode
  | RbOffset Mode
  | Terminate
 deriving (Show)

day13 :: IO ()
day13 = do
  code <- map read . split ',' <$> readFile "./inputs/13.txt"

  let freePlay = 2 : tail code

  let out = interpTiles . evalMachine [] . newMachine $ code
  putStrLn $ "Part 1: " ++ show (length . filter (== Block) . map snd $ out)

  let arcade = newArcade (newMachine freePlay)
  putStrLn $ "Part 2: " ++ show (runArcade [] arcade)

height = 22
width = 39

data Arcade
  = Arcade
  { aMachine :: Machine
  , aScreen  :: (V.Vector Tile)
  , aScore   :: Int
  , aBallPos :: Int
  , aPaddlePos :: Int
  , aBlocksRemain :: Int}

newArcade machine = Arcade machine (V.replicate (width*height) Empty) 0 0 0 255

runArcade :: [Int] -> Arcade -> Int
runArcade input arcade = evalState (loop input) arcade
 where
  loop :: (MonadState Arcade m) => [Int] -> m Int
  loop input = do
    machine <- gets aMachine
    let (out, machine') = runMachine input machine
        tiles = interpTiles out
    traverse_ f tiles
    modify (\a@Arcade{..} -> a { aMachine = machine' } )
    blocksRemain <- gets aBlocksRemain
    score <- gets aScore
    if blocksRemain == 0
      then return score
      else do
      ballPos <- gets aBallPos
      paddlePos <- gets aPaddlePos
      case compare ballPos paddlePos of
        LT -> loop [-1]
        GT -> loop [1]
        EQ -> loop [0]

    where
     f ((x, y), t) = case t of
       (Score score) -> do
         score' <- gets aScore
         blocksRemain <- gets aBlocksRemain
         if score > score'
           then modify (\a@Arcade{..} -> a { aScore = score, aBlocksRemain = blocksRemain - 1})
           else return ()
       (Paddle) -> do
         modify (\a@Arcade{..} -> a { aPaddlePos = x })
       (Ball) -> do
         modify (\a@Arcade{..} -> a { aBallPos = x})
       _ -> return ()





instance Show Arcade where
  show (Arcade _ screen _ _ _ _)  = unlines . groupN width . concat . V.toList . V.map show $ screen

data Tile = Empty | Wall | Block | Paddle | Ball | Score Int
  deriving (Eq)

instance Show Tile where
  show Empty = " "
  show Wall = "#"
  show Block = "$"
  show Paddle = "_"
  show Ball = "o"
  show (Score score) = show score

interpTiles :: [Int] -> [(Pt, Tile)]
interpTiles (-1:0:score:xs) = ((-1, 0), Score score) : interpTiles xs
interpTiles (x:y:tid:xs) = ((x, y), toTile tid) : interpTiles xs
interpTiles [] = []
interpTiles _ = error "Invalid tile command"

toTile :: Int -> Tile
toTile 0 = Empty
toTile 1 = Wall
toTile 2 = Block
toTile 3 = Paddle
toTile 4 = Ball

type Pt = (Int, Int)

data Machine
  = Machine
  { mCode :: Vector Int, mPc :: Int, mHalt :: Bool, mRb :: Int }
  deriving Show

newMachine :: [Int] -> Machine
newMachine s = Machine (V.fromList s V.++ V.replicate 1000 0) 0 False 0

execMachine :: [Int] -> Machine -> Machine
execMachine input machine = snd $ runMachine input machine

evalMachine :: [Int] -> Machine -> [Int]
evalMachine input machine = fst $ runMachine input machine

runMachine :: [Int] -> Machine -> ([Int], Machine)
runMachine input' machine =
  runState (execWriterT (loop input')) machine
 where
  loop :: (MonadState Machine m, MonadWriter [Int] m)
    => [Int]
    -> m ()
  loop input = do
    Machine{..} <- get
    op <- getOp
    case op of
      (Add m1 m2 mw) -> do
        arithBinOp (+) m1 m2 mw
        stepPc op
        loop input
      (Mul m1 m2 mw) -> do
        arithBinOp (*) m1 m2 mw
        stepPc op
        loop input
      (Input m) -> do
        case input of
          [] -> return ()
          _ -> do
            writeMemNew m 1 (head input)
            stepPc op
            loop (tail input)
      (Output m) -> do
        v <- readMem m 1
        tell [v]
        stepPc op
        loop input
      (JumpT m1 m2) -> do
        v <- readMem m1 1
        case v of
          0 -> stepPc op
          _ -> do
            p <- readMem m2 2
            setPc p
        loop input
      (JumpF m1 m2) -> do
        v <- readMem m1 1
        case v of
          0 -> do
            p <- readMem m2 2
            setPc p
          _ -> stepPc op
        loop input
      (LessThan m1 m2 mw) -> do
        arithBinOp (\v1 v2 -> if v1 < v2 then 1 else 0) m1 m2 mw
        stepPc op
        loop input
      (EqTo m1 m2 mw) -> do
        arithBinOp (\v1 v2 -> if v1 == v2 then 1 else 0) m1 m2 mw
        stepPc op
        loop input
      (RbOffset m) -> do
        v <- readMem m 1
        modify (\m@Machine{..} -> m {mRb = mRb + v })
        stepPc op
        loop input
      Terminate -> do
        modify (\m@Machine{..} -> m {mHalt = True})
        return ()

   where
    arithBinOp f m1 m2 mw = do
     v1 <- readMem m1 1
     v2 <- readMem m2 2
     writeMemNew mw 3 (f v1 v2)

    getOp = do
      (Machine prog pc _ _) <- get
      return . parseOp $ prog ! pc

    writeMemNew mode offset v = do
      m@Machine{..} <- get
      p <- readMem Immediate offset
      case mode of
        Position ->
          put $ m{ mCode = mCode // [(p,v)]}
        Immediate ->
          put $ m{ mCode = mCode // [(p + mPc ,v)]}
        Relative ->
          put $ m{ mCode = mCode // [(p + mRb ,v)]}

    writeMem n p =
      modify (\m@Machine{..} -> m { mCode = mCode // [(p, n)] })

    readMem m offset = do
      (Machine prog pc _ rB) <- get
      let pVal = (prog ! (pc + offset))
      case m of
        Position  -> return $ prog ! pVal
        Immediate -> return $ pVal
        Relative -> return $ prog ! (pVal + rB)

    stepPc op =
      modify (\m@Machine{..} -> m { mPc = mPc + paramCt op + 1})

    setPc p =
      modify (\m -> m { mPc = p })

parseOp :: Int -> Op
parseOp o =
  case opCode o of
    1  -> Add (mode 1 o) (mode 2 o) (mode 3 o)
    2  -> Mul (mode 1 o) (mode 2 o) (mode 3 o)
    3  -> Input (mode 1 o)
    4  -> Output (mode 1 o)
    5  -> JumpT (mode 1 o) (mode 2 o)
    6  -> JumpF (mode 1 o) (mode 2 o)
    7  -> LessThan (mode 1 o) (mode 2 o) (mode 3 o)
    8  -> EqTo (mode 1 o) (mode 2 o) (mode 3 o)
    9  -> RbOffset (mode 1 o)
    99 -> Terminate
    e  -> error $  "Invalid opcode: " ++ show e

 where
  mode n x =
    case modeCode x n of
      0 -> Position
      1 -> Immediate
      2 -> Relative
      e -> error $ "Invalid mode: " ++ show e

  opCode x = x `mod` 100

  modeCode x n = (x `div` pow 10 (n+1)) `mod` 10
    where pow x n = product $ replicate n x

paramCt :: Op -> Int
paramCt (Add _ _ _)      = 3
paramCt (Mul _ _ _)      = 3
paramCt (Input _)        = 1
paramCt (Output _)       = 1
paramCt (JumpT _ _)      = 2
paramCt (JumpF _ _)      = 2
paramCt (LessThan _ _ _) = 3
paramCt (EqTo _ _ _)     = 3
paramCt (RbOffset _)     = 1
paramCt Terminate        = 0

--runTests = do
--  -- Running machine
--  assertEq (evalMachine eq0 [0]) [0]
--  assertEq (evalMachine eq0 [5]) [1]
--  assertEq (evalMachine eq8 [8]) [1]
--  assertEq (evalMachine eq8 [1]) [0]
--  assertEq (evalMachine lt8 [1]) [1]
--  assertEq (evalMachine lt8 [8]) [0]
--  assertEq (evalMachine eq8_2 [8]) [1]
--  assertEq (evalMachine eq8_2 [1]) [0]
--
--
--  let
--    rt1 = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
--    rt2 = [104,1125899906842624,99]
--  assertEq (evalMachine (newMachine rt1) []) rt1
--  assertEq (evalMachine (newMachine rt2) []) [1125899906842624]
--
--
--  return ()
--
-- where
--  eq8   = newMachine [3,9,8,9,10,9,4,9,99,-1,8]
--  lt8   = newMachine [3,9,7,9,10,9,4,9,99,-1,8]
--  eq8_2 = newMachine [3,3,1108,-1,8,3,4,3,99]
--  lt8_2 = newMachine [3,3,1107,-1,8,3,4,3,99]
--  eq0   = newMachine [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]
--  eq0_2 = newMachine [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]
--  test1 = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
--  test2 = [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]

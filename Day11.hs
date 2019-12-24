{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Day11 where

import Control.Monad.Writer
import Control.Monad.State.Strict
import qualified Data.Map as M
import Data.List
import Data.Vector (Vector, (!), (//))
import Data.Foldable
import Data.Function
import qualified Data.Vector as V

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

day11 :: IO ()
day11 = do
  code <- map read . split ',' <$> readFile "./inputs/11.txt"

--  putStrLn $ "Pt 1: " ++ show (M.size $ runRobot code M.empty)
  putStrLn $ "Pt 2: "
  putStr $ showHull (runRobot code (M.singleton (0,0) 1))

data Dir = U | D | L | R

data Robot
  = Rob
  { rMach :: Machine, rRobLoc :: Pt, rRobDir :: Dir, rColored :: M.Map Pt Int }

newHull :: [Int] -> Robot
newHull code = Rob { rMach = newMachine code, rRobLoc = (0,0), rRobDir = U, rColored = M.empty }

runRobot code hull =
  rColored $ execState loop ((newHull code){ rColored = hull })
 where
  loop :: (MonadState Robot m) => m ()
  loop = do
    rob@Rob{..} <- get
    case mHalt rMach of
      True -> return ()
      False -> do
        clr <- getColor
        let ([paint, turn], m') = runMachine rMach [clr]
        put rob { rColored = M.insert rRobLoc paint rColored, rMach = m' }
        doTurn turn
        moveForward
        loop

   where
    getColor = do
      rob@Rob{..} <- get
      case M.lookup rRobLoc rColored of
        Nothing -> return 0
        Just c  -> return c

    doTurn turnIx =
      modify (\rob@Rob{..} -> rob { rRobDir = turn turnIx rRobDir })
     where
      turn 0 U = L
      turn 0 D = R
      turn 0 L = D
      turn 0 R = U
      turn 1 U = R
      turn 1 D = L
      turn 1 L = U
      turn 1 R = D

    moveForward =
      modify (\rob@Rob{..} -> rob { rRobLoc = forward rRobLoc rRobDir })
     where
      forward (x, y) U = (x, y-1)
      forward (x, y) D = (x, y+1)
      forward (x, y) L = (x-1, y)
      forward (x, y) R = (x+1, y)

showHull :: M.Map Pt Int -> String
showHull hull = unlines . groupN width $ unfoldr f 0

 where
  f idx = res
   where
     res =
      let (x, y) = (idx `mod` width, idx `div` width)
      in
        if idx == width*height
        then Nothing
        else if elem (x, y) toDraw
             then (Just ('#', idx+1))
             else (Just (' ', idx+1))


  toDraw = ((map (\(x, y) -> (x-minX, (y-minY))) whiteOnly))
  whiteOnly = M.keys . M.filter (== 1) $ hull
  width = maxX - minX + 1
  height = maxY - minY + 1
  minX = fst . minimumBy (compare `on` fst) $ whiteOnly
  minY = snd . minimumBy (compare `on` snd) $ whiteOnly
  maxX = fst . maximumBy (compare `on` fst) $ whiteOnly
  maxY = snd . maximumBy (compare `on` snd) $ whiteOnly


data Machine
  = Machine
  { mCode :: Vector Int, mPc :: Int, mHalt :: Bool, mRb :: Int }
  deriving Show

newMachine :: [Int] -> Machine
newMachine s = Machine (V.fromList s V.++ V.replicate 1000 0) 0 False 0

execMachine :: Machine -> [Int] -> Machine
execMachine machine input = snd $ runMachine machine input

evalMachine :: Machine -> [Int] -> [Int]
evalMachine machine input = fst $ runMachine machine input

runMachine :: Machine -> [Int] -> ([Int], Machine)
runMachine machine input' =
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

runTests = do
  -- Running machine
  assertEq (evalMachine eq0 [0]) [0]
  assertEq (evalMachine eq0 [5]) [1]
  assertEq (evalMachine eq8 [8]) [1]
  assertEq (evalMachine eq8 [1]) [0]
  assertEq (evalMachine lt8 [1]) [1]
  assertEq (evalMachine lt8 [8]) [0]
  assertEq (evalMachine eq8_2 [8]) [1]
  assertEq (evalMachine eq8_2 [1]) [0]


  let
    rt1 = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
    rt2 = [104,1125899906842624,99]
  assertEq (evalMachine (newMachine rt1) []) rt1
  assertEq (evalMachine (newMachine rt2) []) [1125899906842624]


  return ()

 where
  eq8   = newMachine [3,9,8,9,10,9,4,9,99,-1,8]
  lt8   = newMachine [3,9,7,9,10,9,4,9,99,-1,8]
  eq8_2 = newMachine [3,3,1108,-1,8,3,4,3,99]
  lt8_2 = newMachine [3,3,1107,-1,8,3,4,3,99]
  eq0   = newMachine [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]
  eq0_2 = newMachine [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]
  test1 = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
  test2 = [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]

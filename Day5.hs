{-# LANGUAGE FlexibleContexts #-}

module Day5 where

import Control.Monad.Writer
import Control.Monad.State
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import Common

data Mode = Position | Immediate
  deriving (Show, Eq)

data Op
  = Add Mode Mode
  | Mul Mode Mode
  | JumpT Mode Mode
  | JumpF Mode Mode
  | LessThan Mode Mode
  | EqTo Mode Mode
  | Input
  | Output
  | Terminate
 deriving (Show)

day5 :: IO ()
day5 = do
  initialState <- newMachine . map read . split ',' <$> readFile "./inputs/5.txt"

  runTests

  putStrLn $ "Pt 1: " ++ (show $ machineOutput initialState 1)
  putStrLn $ "Pt 2: " ++ (show $ machineOutput initialState 5)

newMachine :: [Int] -> Machine
newMachine s = (Machine (V.fromList s) 0)

runTests = do
  assert (machineOutput eq0 0) [0]
  assert (machineOutput eq0 5) [1]
  assert (machineOutput eq8 8) [1]
  assert (machineOutput eq8 1) [0]
  assert (machineOutput lt8 1) [1]
  assert (machineOutput lt8 8) [0]
  assert (machineOutput eq8_2 8) [1]
  assert (machineOutput eq8_2 1) [0]

  return ()
 where
  eq8   = newMachine [3,9,8,9,10,9,4,9,99,-1,8]
  lt8   = newMachine [3,9,7,9,10,9,4,9,99,-1,8]
  eq8_2 = newMachine [3,3,1108,-1,8,3,4,3,99]
  lt8_2 = newMachine [3,3,1107,-1,8,3,4,3,99]
  eq0   = newMachine [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]
  eq0_2 = newMachine [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]

assert exp act = if exp == act
  then do
    putStrLn "Passed"
    return True
  else do
    putStrLn $ "Failed. Got: " ++ (show exp) ++ " Expected: " ++ (show act)
    return False

parseOp :: Int -> Op
parseOp x =
  case opCode x of
    1  -> Add (mode 1 x) (mode 2 x)
    2  -> Mul (mode 1 x) (mode 2 x)
    3  -> Input
    4  -> Output
    5  -> JumpT (mode 1 x) (mode 2 x)
    6  -> JumpF (mode 1 x) (mode 2 x)
    7  -> LessThan (mode 1 x) (mode 2 x)
    8  -> EqTo (mode 1 x) (mode 2 x)
    99 -> Terminate
    e  -> error $  "Invalid opcode: " ++ show e

 where
  mode n x =
    case modeCode x n of
      0 -> Position
      1 -> Immediate
      e -> error $ "Invalid mode: " ++ show e

  opCode x = x `mod` 100

modeCode x n = (x `div` (pow 10 (n+1))) `mod` 10
  where pow x n = product $ replicate n x

paramCt :: Op -> Int
paramCt (Add _ _)      = 3
paramCt (Mul _ _)      = 3
paramCt (Input)        = 1
paramCt (Output)       = 1
paramCt (JumpT _ _)    = 2
paramCt (JumpF _ _)    = 2
paramCt (LessThan _ _) = 3
paramCt (EqTo _ _)     = 3
paramCt (Terminate)    = 0

data Machine = Machine {mState :: (Vector Int), mPc :: Int}
  deriving Show

machineOutput :: Machine -> Int -> [Int]
machineOutput machine input =
  evalState (execWriterT (runMachine input)) machine

machineState :: Machine -> ([Int], Machine)
machineState machine = runState (execWriterT (runMachine 1)) machine

runMachine
  :: (MonadState Machine m, MonadWriter [Int] m)
  => Int
  -> m ()
runMachine input = do
  op <- getOp
  case op of
    (Add m1 m2) -> do
      arithBinOp (+) m1 m2
      stepPc op
      runMachine input
    (Mul m1 m2) -> do
      arithBinOp (*) m1 m2
      stepPc op
      runMachine input
    Input -> do
      p <- read Immediate 1
      write input p
      stepPc op
      runMachine input
    Output -> do
      p <- read Position 1
      tell [p]
      stepPc op
      runMachine input
    (JumpT m1 m2) -> do
      v <- read m1 1
      case v of
        0 -> stepPc op
        _ -> do
          p <- read m2 2
          setPc p
      runMachine input
    (JumpF m1 m2) -> do
      v <- read m1 1
      case v of
        0 -> do
          p <- read m2 2
          setPc p
        _ -> stepPc op
      runMachine input
    (LessThan m1 m2) -> do
      arithBinOp (\v1 v2 -> if v1 < v2 then 1 else 0) m1 m2
      stepPc op
      runMachine input
    (EqTo m1 m2) -> do
      arithBinOp (\v1 v2 -> if v1 == v2 then 1 else 0) m1 m2
      stepPc op
      runMachine input
    Terminate -> return ()

 where
  arithBinOp f m1 m2 = do
    v1 <- read m1 1
    v2 <- read m2 2
    p3 <- read Immediate 3
    write (f v1 v2) p3

  getOp = do
    (Machine state pc) <- get
    return . parseOp $ state ! pc

  write n p = do
    m@Machine{..} <- get
    put $ m { mState = mState // [(p, n)] }

  read m p = do
    (Machine state pc) <- get
    case m of
      Position  -> do
        return $ state ! (state ! (pc + p))
      Immediate ->
        return $ state ! (pc+p)

  stepPc op = do
    m@(Machine _ pc) <- get
    put $ m { mPc = (pc + paramCt op + 1) }

  setPc p =
    modify (\m -> m { mPc = p })

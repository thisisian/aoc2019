module IntMachine where

import Common
import Control.Monad.Writer
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.ST
import qualified Data.Map as M
import Data.List
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Data.Vector.Unboxed (Vector, (!), (//))
import Data.Foldable
import Data.Function
import Data.Maybe
import Data.STRef
import Debug.Trace

import Common

doTests = runTests

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

data Machine
  = Machine
  { mCode :: V.Vector Int, mPc :: Int, mRb :: Int }
  deriving Show

machineFromFile :: String -> IO (Machine)
machineFromFile f = newMachine . map read . split ',' <$> readFile f

newMachine :: [Int] -> Machine
newMachine s = Machine (V.fromList s V.++ V.replicate 9000 0) 0 0

data MutMachine s = MutMachine { mutCode :: VM.STVector s Int, mutPc :: STRef s Int, mutRelBase :: STRef s Int }

evalMachine :: [Int] -> Machine -> [Int]
evalMachine input machine = fst $ runMachine input machine

runMachine :: [Int] -> Machine -> ([Int], Machine)
runMachine input' Machine{..}  = runST $ do
  code <- V.thaw mCode
  pcRef <- newSTRef mPc
  rbRef <- newSTRef mRb
  outputRef <- newSTRef []
  loop (MutMachine code pcRef rbRef) input' outputRef

 where
  fromMutMachine :: MutMachine s -> ST s Machine
  fromMutMachine MutMachine{..} = do
    pc <- readSTRef mutPc
    relBase <- readSTRef mutRelBase
    code <- V.freeze mutCode
    return (Machine code pc relBase)

  loop mach@MutMachine{..} input outputRef = do
    pc <- readSTRef mutPc
    relBase <- readSTRef mutRelBase
    op <- getOp mutCode pc
    case op of
      (Add m1 m2 mw) -> do
        binOp mach (+) m1 m2 mw
        stepPc mach op
        loop mach input outputRef
      (Mul m1 m2 mw) -> do
        binOp mach (*) m1 m2 mw
        stepPc mach op
        loop mach input outputRef
      (Input mode) ->
        case input of
          [] -> do
            output <- readSTRef outputRef
            mach' <- fromMutMachine mach
            return (reverse output, mach')
          _ -> do
            store mach mode (pc+1) (head input)
            stepPc mach op
            loop mach (tail input) outputRef
      (Output mode) -> do
        v <- fetch mach mode (pc+1)
        modifySTRef outputRef (v:)
        stepPc mach op
        loop mach input outputRef
      (JumpT m1 m2) -> do
        v <- fetch mach m1 (pc+1)
        case v of
          0 -> stepPc mach op
          _ -> do
            p <- fetch mach m2 (pc+2)
            setPc mach p
        loop mach input outputRef
      (JumpF m1 m2) -> do
        v <- fetch mach m1 (pc+1)
        case v of
          0 -> do
            p <- fetch mach m2 (pc+2)
            setPc mach p
          _ -> stepPc mach op
        loop mach input outputRef
      (LessThan m1 m2 mw) -> do
        binOp mach (\v1 v2 -> if v1 < v2 then 1 else 0) m1 m2 mw
        stepPc mach op
        loop mach input outputRef
      (EqTo m1 m2 mw) -> do
        binOp mach (\v1 v2 -> if v1 == v2 then 1 else 0) m1 m2 mw
        stepPc mach op
        loop mach input outputRef
      (RbOffset mode) -> do
        v <- fetch mach mode (pc+1)
        modifySTRef mutRelBase (+v)
        stepPc mach op
        loop mach input outputRef
      Terminate -> do
        output <- readSTRef outputRef
        mach' <- fromMutMachine mach
        return (reverse output, mach')

  getOp code pc = parseOp <$> VM.read code pc

  stepPc MutMachine{..} op =
    modifySTRef mutPc (\pc -> pc + paramCt op + 1)

  setPc MutMachine{..} pos =
    modifySTRef mutPc (const pos)

  binOp mach@MutMachine{..} f m1 m2 mw = do
    pc <- readSTRef mutPc
    v1 <- fetch mach m1 (pc+1)
    v2 <- fetch mach m2 (pc+2)
    store mach mw (pc+3) (f v1 v2)

  fetch MutMachine{..} mode pos = do
    pVal <- VM.read mutCode pos
    case mode of
      Position -> VM.read mutCode pVal
      Immediate -> return pVal
      Relative -> do
        relBase <- readSTRef mutRelBase
        VM.read mutCode (pVal + relBase)

  store m@MutMachine{..} mode pos val = do
    p <- fetch m Immediate pos
    case mode of
      Position -> VM.modify mutCode (const val) p
      Immediate -> do
        pc <- readSTRef mutPc
        VM.modify mutCode (const val) (p+pc)
      Relative -> do
        rb <- readSTRef mutRelBase
        VM.modify mutCode (const val) (p+rb)

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
  assertEq (evalMachine [0] eq0) [0]
  assertEq (evalMachine [5] eq0) [1]
  assertEq (evalMachine [8] eq8) [1]
  assertEq (evalMachine [1] eq8) [0]
  assertEq (evalMachine [1] lt8) [1]
  assertEq (evalMachine [8] lt8) [0]
  assertEq (evalMachine [8] eq8_2) [1]
  assertEq (evalMachine [1] eq8_2) [0]


  let
    rt1 = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
    rt2 = [104,1125899906842624,99]
  assertEq (evalMachine [] (newMachine rt1)) rt1
  assertEq (evalMachine [] (newMachine rt2)) [1125899906842624]

  -- Day9 tests

  pt1 <- map read . split ',' <$> readFile "./inputs/9.txt"
  assertEq (evalMachine [1] (newMachine pt1)) [2399197539]
  assertEq (evalMachine [2] (newMachine pt1)) [35106]
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

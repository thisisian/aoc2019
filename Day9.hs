{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Day9 where

import Control.Monad.Writer
import Control.Monad.State
import qualified Data.IntMap as M
import Data.List
import Data.Vector (Vector, (!), (//))
import Data.Foldable
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

day9 :: IO ()
day9 = do
  program <- newMachine . map read . split ',' <$> readFile "./inputs/9.txt"
  runTests
  putStrLn $ "Pt 1: " ++ show (evalMachine program [1])
  putStrLn $ "Pt 2: " ++ show (evalMachine program [2])

phasesNoFeedback :: [[Int]]
phasesNoFeedback = permutations [0,1,2,3,4]

phasesFeedback :: [[Int]]
phasesFeedback = permutations [5,6,7,8,9]

maxAmpOutput :: [Int] -> [[Int]] -> Int
maxAmpOutput prgm phases =
  maximum $ map (head . evalAmps 0 . inputPhases (repeat initMachine)) phases
  where
    initMachine = newMachine prgm

inputPhases :: [Machine] -> [Int] -> [Machine]
inputPhases = zipWith (\m i -> execMachine m [i])

evalAmps :: Int -> [Machine] -> [Int]
evalAmps input ms =
  evalState (runAmps [input]) (M.fromList $ zip [0..] ms)

runAmps :: (MonadState (M.IntMap Machine) m) => [Int] -> m [Int]
runAmps input = do
  ms <- gets M.toList
  ret <- foldlM f input ms
  halted <- isHalt
  if halted
    then return ret
    else runAmps ret
 where
  f inp (idx, m) = do
    let (out, m') = runMachine m inp
    modify (M.adjust (const m') idx)
    return $ reverse out

  isHalt = do
    ms <- gets M.elems
    return $ all mHalt ms

data Machine = Machine { mCode :: Vector Int, mPc :: Int, mHalt :: Bool, mRb :: Int }
  deriving Show

newMachine :: [Int] -> Machine
newMachine s = Machine (V.fromList s V.++ V.replicate 4096 0) 0 False 0

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
  assert (evalMachine eq0 [0]) [0]
  assert (evalMachine eq0 [5]) [1]
  assert (evalMachine eq8 [8]) [1]
  assert (evalMachine eq8 [1]) [0]
  assert (evalMachine lt8 [1]) [1]
  assert (evalMachine lt8 [8]) [0]
  assert (evalMachine eq8_2 [8]) [1]
  assert (evalMachine eq8_2 [1]) [0]

  -- Amplifiers without feedback
  assert (maxAmpOutput test1 phasesNoFeedback) 43210
  assert (maxAmpOutput test2 phasesNoFeedback) 54321

  -- Amplifiers with feedback
  let
    ps1 = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
    ps2 = [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]

  assert (maxAmpOutput ps1 [[9,8,7,6,5]]) 139629729
  assert (maxAmpOutput ps2 [[9,7,8,5,6]]) 18216
  --assert (maxAmpOutput ps1 phasesFeedback) 139629729
  --assert (maxAmpOutput ps2 phasesFeedback) 18216

  -- Relative position instuctions
  let
    rt1 = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
    rt2 = [104,1125899906842624,99]
  assert (evalMachine (newMachine rt1) []) rt1
  assert (evalMachine (newMachine rt2) []) [1125899906842624]


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

assert :: forall a. (Show a, Eq a) => a -> a -> IO Bool
assert expected actual = if expected == actual
  then do
    putStrLn "Passed"
    return True
  else do
    putStrLn $ "Failed. Got: " ++ show expected ++ " Expected: " ++ show actual
    return False

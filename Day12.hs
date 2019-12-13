module Day12 where

import Control.Monad.State.Strict
import Control.Monad.ST
import Data.Foldable
import Data.STRef
import Debug.Trace

type Velocity = Int
type Position = Int

data Moon
  = Moon
  { mId :: Int
  , mX :: Position
  , mY :: Position
  , mZ :: Position
  , mVx :: Velocity
  , mVy :: Velocity
  , mVz :: Velocity }

inputMoons =
  [ Moon 0 (-1) (-4) 0 0 0 0
  , Moon 1   4 7 (-1) 0 0 0
  , Moon 2 (-14) (-10) 9 0 0 0
  , Moon 3   1 2 17  0 0 0 ]

testMoons1 =
  [ Moon 0 (-1) 0 2 0 0 0
  , Moon 1 2 (-10) (-7) 0 0 0
  , Moon 2 4 (-8) 8 0 0 0
  , Moon 3 3 5 (-1) 0 0 0 ]

instance Eq Moon where
  (==) m1 m2 = mId m1 == mId m2

instance Show Moon where
  show (Moon id x y z vx vy vz) = "Pos: " ++ show (x,y,z) ++ " Velocity: " ++ show (vx,vy,vz) ++ "\n"

day12 = do
  print $ totalEnergy $ runMoons inputMoons 1000
  print $ ctsToRepeat inputMoons

totalEnergy ::  [Moon] -> Int
totalEnergy ms =
  sum . map energy $ ms
 where
  energy (Moon _ x y z vx vy vz) =
    (abs x + abs y + abs z) * (abs vx + abs vy + abs vz)



runMoons :: [Moon] -> Int -> [Moon]
runMoons moons' stop = fst $ execState loop (moons', 0)
 where
  loop :: (MonadState ([Moon], Int) m) => m ()
  loop = do
    (moons, idx) <- get
    if idx >= stop
      then return ()
      else do
      let moons' = updateVelocities moons
          moons'' = updatePositions moons'
      put (moons'', idx+1)
      loop
   where
     updatePositions moons = map updatePosition moons
     updateVelocities moons =
       map (f moons) moons

      where
       f :: [Moon] -> Moon -> Moon
       f moons moon = runST $ do
             m <- newSTRef moon
             traverse_ (updateVelocityST m) moons
             m' <- readSTRef m
             return m'

showState :: ([Moon], Int) -> String
showState (ms, i) =
  "After " ++ show i ++ " steps:\n" ++
  concatMap show ms
 where


updateVelocityST :: STRef s Moon -> Moon -> ST s Moon
updateVelocityST moonRef moon2 = do
  modifySTRef moonRef (\m -> updateVelocity m moon2)
  m' <- readSTRef moonRef
  return m'

updateVelocity :: Moon -> Moon -> Moon
updateVelocity (Moon id x1 y1 z1 vx vy vz) (Moon _ x2 y2 z2 _ _ _) =
 (Moon id x1 y1 z1 (vx + vdiff x1 x2) (vy + vdiff y1 y2) (vz + vdiff z1 z2))

vdiff p1 p2 =
  if p1 > p2
  then (-1)
  else
    if p1 < p2
    then 1
    else 0

updatePosition :: Moon -> Moon
updatePosition (Moon id x y z vx vy vz) =
  (Moon id (x + vx) (y+vy) (z+vz) vx vy vz)

ctsToRepeat :: [Moon] -> Int
ctsToRepeat [(Moon _ x1 y1 z1 vx1 vy1 vz1), (Moon _ x2 y2 z2 vx2 vy2 vz2), (Moon _ x3 y3 z3 vx3 vy3 vz3), (Moon _ x4 y4 z4 vx4 vy4 vz4) ]
  = lcm ctsToRepeatX (lcm ctsToRepeatY ctsToRepeatZ)
 where
  initialX = (x1, x2, x3, x4, vx1, vx2, vx3, vx4)
  initialY = (y1, y2, y3, y4, vy1, vy2, vy3, vy4)
  initialZ = (z1, z2, z3, z4, vz1, vz2, vz3, vz4)
  ctsToRepeatX = ctsToRepeat' initialX initialX 0
  ctsToRepeatY = ctsToRepeat' initialY initialY 0
  ctsToRepeatZ = ctsToRepeat' initialZ initialZ 0

ctsToRepeat'
  :: (Position, Position, Position, Position, Velocity, Velocity, Velocity, Velocity)
  -> (Position, Position, Position, Position, Velocity, Velocity, Velocity, Velocity)
  -> Int
  -> Int
ctsToRepeat' initial (p1, p2, p3, p4, v1, v2, v3, v4) ct =
  let cur = ((p1', p2', p3', p4', v1', v2', v3', v4'))
  in
    if initial == cur
    then ct+1
    else
      ctsToRepeat' initial (p1', p2', p3', p4', v1', v2', v3', v4') (ct+1)

 where
  p1' = p1 + v1'
  v1' = v1 + vdiff p1 p2 + vdiff p1 p3 + vdiff p1 p4

  p2' = p2 + v2'
  v2' = v2 + vdiff p2 p1 + vdiff p2 p3 + vdiff p2 p4

  p3' = p3 + v3'
  v3' = v3 + vdiff p3 p1 + vdiff p3 p2 + vdiff p3 p4

  p4' = p4 + v4'
  v4' = v4 + vdiff p4 p1 + vdiff p4 p2 + vdiff p4 p3

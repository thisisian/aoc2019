{-# LANGUAGE FlexibleInstances #-}

module Day15 where

import Common
import IntMachine
import Control.Monad.IO.Class
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Text.IO as TIO
import Data.Tree
import qualified Data.Set as S
import Data.List
import Data.Maybe

day15 :: IO ()
day15 = do
  m <- newMachineFromFile "./inputs/15.txt"
  mazeMap <- doDroid m
  let oxygenPt = fst . fromJust . find (\x -> snd x == OxySys) . M.toList $ mazeMap
  print $ oxygenPt
  let mazeTreeOxy = mapToTree oxygenPt mazeMap
  let mazeTree = mapToTree (0,0) mazeMap
  print $ findIndex (elem OxySys) . levels $ mazeTree
  print $ length (levels mazeTreeOxy) - 1

  return ()

data MapObject = Unknown | Wall | Floor | CurrentLoc | OxySys | Start
  deriving (Show, Eq)

data Droid = Droid { dMach :: Machine, dPos :: Pt, dMap :: M.Map Pt [MapObject], dPointing :: Dir }


instance ShowChar [MapObject] where
  displayChar [] = ' '
  displayChar (Unknown:_) = ' '
  displayChar (Wall:_) = '#'
  displayChar (Floor:_) = '.'
  displayChar (CurrentLoc:_) = '@'
  displayChar (OxySys:_) = 'O'
  displayChar (Start:_) = 'S'

mapToTree :: Pt -> M.Map Pt MapObject -> Tree MapObject
mapToTree initialPt mazeMap = Node Start $ mkForest (S.singleton initialPt) (neighbors initialPt)
 where
  neighbors (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

  mkForest processed =
    mapMaybe (\pt ->
                let processed' = S.insert pt processed
                in if S.member pt processed then
                  Nothing
                  else
                    case M.lookup pt mazeMap of
                      Nothing -> Nothing
                      Just x -> Just $ Node x (mkForest (processed') (neighbors pt)))



doDroid :: Machine -> IO (M.Map Pt MapObject)
doDroid mach = do
  droidMap <- evalStateT (loop) initDroid
  return $
    M.filter (\x -> case x of
                      Floor -> True
                      OxySys -> True
                      _ -> False )
    . M.map last
    $ droidMap
 where
  initDroid
    = Droid
    { dMach = mach
    , dPos = (0,0)
    , dMap = (M.singleton (0,0) [CurrentLoc, Start])
    , dPointing = North }

  loop :: (MonadState Droid m, MonadIO m) => m (M.Map Pt [MapObject])
  loop = do
    mach <- gets dMach
    pointing <- gets dPointing
    pos <- gets dPos
    mazeMap <- gets dMap
    --liftIO . TIO.putStr . showDisplay [Unknown] $ mazeMap
    let
      ([output], mach') = runMachine [dirToInput (leftOf pointing)] mach
      pos' = ptFromDir pos (leftOf pointing)
    case output of
      0 -> do
        -- Wall
        let
          mazeMap' = M.insert pos' [Wall] mazeMap
          pointing' = rightOf pointing
        modify (\s -> s
                  { dMach = mach'
                  , dMap = mazeMap'
                  , dPointing = pointing' })
        loop

      1 ->
        -- Step
        if pos' == (0,0)
        then return mazeMap
        else do
          let mazeMap' = M.insert pos' [CurrentLoc, Floor] . M.adjust tail pos $ mazeMap
          modify (\s -> s
                    { dMach = mach'
                    , dPos = pos'
                    , dPointing = leftOf pointing
                    , dMap = mazeMap' })
          loop
      2 -> do
        -- Oxygen System
        let mazeMap' = M.insert pos' [CurrentLoc, OxySys] . M.adjust tail pos $ mazeMap :: M.Map Pt [MapObject]
        modify (\s -> s
                 { dMach = mach'
                 , dPos = pos'
                 , dMap = mazeMap' })
        loop
      _ -> error "Invalid output"

  dirToInput North = 1
  dirToInput South = 2
  dirToInput West  = 3
  dirToInput East  = 4

  ptFromDir (x, y) North = (x, y-1)
  ptFromDir (x, y) South = (x, y+1)
  ptFromDir (x, y) West  = (x-1, y)
  ptFromDir (x, y) East  = (x+1, y)

--  #.....#.......#.......#...#...#.......#.#
--  #.#.#.#####.#.#.#####.#.#.#.###.#.###.#.#
--  #.#.#...#...#.#.#.......#...#...#.#...#.#
--  #.#.###.#.###.#.#####.#####.#.###.###.#.#
--  #.#...#...# #.#.....#.#...#.#.#.#...#...#
--  #.###.####  #.#####.###. .###.#.###.###.#
--  #...#.....# #.....#.#...#...#.#...#.#...#
--   ########.#.#####.#.#.## ##.#.#.#.#.#. #
--  #.........#.........#...#.#.....#.#.#...#
--   .#########.###########.#.#######.#.###.#
--  #.#...#..@..#.........#.#.#.....#.#...#.#
--  #.#.#.#######.#######.#.#.#.###.## ##.#.#
--  #...#.#...#...#.....#.#.#.#...#...#...#.#
--  #.###.#.#.#.#####.#.#.#.#.###.###.#.## .#
--  #...#...#.#.......#.#.......#...#.#...#.#
--   ##.#####.#.#############.#####.#.#.#.##
--  #.#.#...#...#...........# #.....#.#.#...#
--  #.#.#.#######.#########.###.#####.#####.#
--  #.#.#.#.......#.#.....#...#.#...#.......#
--  #.#.#.#.#######.#.#. # ##.#. ##.#######.#
--  #.#.#.#...#.......#.#S#...#.#.....#.....#
--  #.#.#.###.###.#####.#.#.###.#.#####.## #
--  #.......#...#.....#.#...#...#.....#...#.#
--  #.#########.#####.#.#####.###.###.###.#.#
--  #.#.........#...#.#.......#...#.#.#...#.#
--  #.#.#########.###.#####.###.###.#.#.###.#
--  #.#...#.#.....#...#...#.#...#...#...#...#
--   ####.#.#.#. ##.###.###.#.#####. ### ##.#
--  #...#.#...#.#...#...#...#.....#.....#...#
--  #.#.#.#####.#.###.###.#######.###. .#.#.#
--  #.#...#...#.......#...#.....#...#.#...#.#
--  #.#####.#.#########.###.#### ##.#.##  #.#
--  #.#.....#.......#...#...#...#...#...#...#
--  #.#####.#######.#. ##.#.#.#.#.###.#.###.#
--  #.....#.......#.#.#...#.#.#...#...#...#.#
--   ####.#### ##.#.#.#.###.#.#####. ### .#.#
--  #.#...#...#...#...#.#.#.#.#.....#...#.#.#
--  #.#.###.#.#.#######.#.#.#.#######. .#.#.#
--  #.......#...#.........#...........#...#!#
--   ####### ### ######### ########### ### #

module Day14 where

import Text.Parsec
import Text.RawString.QQ
import qualified Data.Map as M
import Control.Monad.State.Strict
import Debug.Trace
import Data.Maybe (fromJust)

type Material = String

type Recipe =  (Material, (Int, [(Material, Int)]))

day14 = do
 print $ doThing . parseRecipes $ testInput2


data ChemState
  = CS
  { csNeedList :: M.Map Material Int
  , csHaveList :: M.Map Material Int
  , csOre      :: Int }

doThing :: [Recipe] -> Int
doThing recipes = evalState loop (CS initNeedList initHaveList 0)

 where
  recipeMap = M.fromList recipes
  initNeedList = M.fromList [("FUEL", 1)]
  initHaveList = M.empty

  loop :: (MonadState ChemState m) => m Int
  loop = do
    needList <- gets csNeedList
    traceShowM needList
    haveList <- gets csHaveList
    traceShowM haveList
    ore <- gets csOre
    if null needList
      then
        return ore
      else do
        f (head . M.toList $ needList)
        loop

   where
    f :: (MonadState ChemState m) => (Material, Int) -> m ()
    f (material, amtNeeded) = do
      needList <- gets csNeedList
      haveList <- gets csHaveList
      if material == "ORE"
        then
          modify
            (\s@CS{..} -> s { csNeedList = M.delete material needList, csOre = csOre + amtNeeded})
         else do
           let (amtProduced, matsNeeded) = fromJust $ M.lookup material recipeMap
           case M.lookup material haveList of
             Nothing -> do
               let numOfBatches = batches amtNeeded amtProduced
               let matsToAdd = M.map (*numOfBatches) . M.fromList $ matsNeeded
               let needList' = M.union matsToAdd . M.delete material $ needList
               let haveList' =
                     M.insert
                       material (amtProduced * numOfBatches - amtNeeded) haveList
               modify (\s -> s { csNeedList = needList', csHaveList = haveList'})

             Just have ->
               case compare have amtNeeded of
                 EQ -> do
                   let needList' = M.delete material needList
                   let haveList' = M.delete material haveList
                   modify
                     (\s -> s { csNeedList = needList', csHaveList = haveList'})
                 LT -> do
                   let numOfBatches = batches (amtNeeded - have) amtProduced
                   let matsToAdd = M.map (*numOfBatches) . M.fromList $ matsNeeded
                   let needList' = M.union matsToAdd . M.delete material $ needList
                   let haveList' =
                         M.insert
                           material (amtProduced * numOfBatches - (amtNeeded - have)) haveList
                   modify (\s -> s { csNeedList = needList', csHaveList = haveList'})


                 GT -> do
                   let needList' = M.delete material needList
                   let haveList' = M.adjust (amtNeeded -) material haveList
                   modify
                     (\s -> s { csNeedList = needList', csHaveList = haveList'})



batches :: Int -> Int -> Int
batches needed batchSize =
  case needed `div` batchSize of
    0 -> 1
    n ->
      if needed `mod` batchSize == 0
      then n
      else n+1

parseRecipes s = case parse pRecipes "" s of
  Left e   -> error $ show e
  Right ps -> ps

 where
  pRecipes = (pRecipe `sepBy` newline) <* eof
  pRecipe = do
    ingredients <- pIngredients
    string " => "
    ct <- read <$> many1 digit
    space
    endResult <- many1 upper
    return (endResult, (ct, ingredients))
  pIngredients = pIngredient `sepBy` (string ", ")
  pIngredient = do
    ct <- read <$> many1 digit
    space
    mat <- many1 upper
    return (mat, ct)



testInput0 = [r|10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FP
14 A, 1 FP => 15 F
7 A, 1 F => 1 G
7 A, 1 G => 1 FUEL|]

testInput1 = [r|9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL|]

testInput2 = [r|157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT|]


testInput3 = [r|2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
17 NVRVD, 3 JNWZP => 8 VPVL
53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
22 VJHF, 37 MNCFX => 5 FWMGM
139 ORE => 4 NVRVD
144 ORE => 7 JNWZP
5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
145 ORE => 6 MNCFX
1 NVRVD => 8 CXFTF
1 VJHF, 6 MNCFX => 4 RFSQX
176 ORE => 6 VJHF|]

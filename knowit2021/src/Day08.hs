{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day08 where

import Control.Lens
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.Semigroup (Max (Max), Min (Min))

parse xs = (fmap (read @(Int, Int)) coords, fmap (read @Int) order)
 where
  (coords, order) = break (\s -> not $ "(" `isPrefixOf` s) . lines $ xs

getCoords (coords, order) = (zip <*> tail) $ map (m Map.!) order
 where
  m = (Map.fromList $ zip [0 ..] coords)

makeRect (x1, y1) (x2, y2) = (,) <$> xs <*> ys
 where
  xs = if x1 < x2 then [x1 .. x2 -1] else [x1, x1 -1 .. x2 + 1]
  ys = if y1 < y2 then [y1 .. y2 -1] else [y1, y1 -1 .. y2 + 1]

solve :: ([(Int, Int)], [Int]) -> (Min Int, Min Int, Max Int, Max Int)
solve xs = ifoldMap (\i _ -> extremal i) $ Map.filter (== m) res
 where
  m = maximum res
  res = Map.unionsWith (+) . fmap (makeOnes . uncurry makeRect) . getCoords $ xs

makeOnes = Map.fromList . map (,1 :: Int)

extremal :: (Int, Int) -> (Min Int, Min Int, Max Int, Max Int)
extremal (x, y) = (Min x, Min y, Max x, Max y)

run :: IO ()
run = do
  inp <- readFile "inputs/8"
  let parsed = parse inp
  print $ solve parsed

--(Min {getMin = 550},Min {getMin = 418},Max {getMax = 551},Max {getMax = 421})

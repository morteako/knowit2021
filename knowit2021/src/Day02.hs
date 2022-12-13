{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Day02 where

import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Geo.Computations

parse :: String -> Map.Map [Char] Point
parse = Map.fromList . fmap (f . splitOn ",Point") . tail . lines
 where
  f [city, s] = (city, p)
   where
    [x, y] = fmap (read @Double) $ words $ tail $ init s
    p = pt y x Nothing Nothing
  f s = error $ show s

polpunkt :: Point
polpunkt = pt 90 0 Nothing Nothing

solve :: (Show a, Ord a) => Map.Map a Point -> Point -> [Distance]
solve m cur | null m = [distance cur polpunkt]
solve m cur
  | Just (k, p) <- Map.foldrWithKey (comp cur) Nothing m
    , let m' = Map.delete k m =
    distance cur p : solve m' p
solve m cur = error $ "ops " ++ show m

comp :: Point -> a -> Point -> Maybe (a, Point) -> Maybe (a, Point)
comp cur k p Nothing = Just (k, p)
comp cur k p (Just (k', p')) | distance cur p < distance cur p' = Just (k, p)
comp cur k p (Just (k', p')) = Just (k', p')

run :: IO ()
run = do
  inp <- readFile "inputs/2.csv"
  let parsed = parse inp
  print $ round $ sum $ solve parsed polpunkt
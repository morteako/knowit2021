{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Day03 where

import Control.Lens
import Data.Function
import Data.List
import Data.List.Extra (maximumOn)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe
import Data.Semigroup

parse = fmap f
 where
  f 'J' = 1
  f 'N' = negate 1
  f _ = error ""

sums = scanl1 (+)
solve xs = maximumOn fst $ catMaybes $ zipWith (\n -> fmap (,n)) [0 ..] $ foldMap (Just . Max) . (findIndices (== 0) . sums) <$> tails xs

-- go i [] [] = Nothing
-- go i (x:xs) (y:ys)
--   | x == y = go (i+1) xs ys
--   | Just q <- go (i+1) xs ys

run :: IO ()
run = do
  inp <- readFile "inputs/3"
  let parsed = parse inp
  let res = solve parsed

  print $ (Max{getMax = 29}, 1265) == res

-- mapM_ print $ fmap sums $ tails parsed

--   [1,2,3]
-- [1,2,3]
-- [3,2,1]
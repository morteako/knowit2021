{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Day04 where

import Control.Lens
import Linear (V2 (V2))

data Dir = N | E deriving (Show)

remo y num = not (mod num y == 0)

walk N (V2 x y) = let n = until (remo 5) (+ 3) (3 + y) in (V2 x n) : walk E (V2 x n)
walk E (V2 x y) = let n = until (remo 3) (+ 5) (5 + x) in (V2 n y) : walk N (V2 n y)

lim :: Integer
lim = 100000000000000000079

target = until e2 succ lim

e2 x = mod x 45 == 8

times = (target `div` 45)

mult x = (V2 (x * 30) (x * 15))

start :: V2 Integer
start = (V2 5 3)

adder = subtract (V2 9 0)

run :: IO ()
run = do
  mapM_ print $ take 51 $ filter (\(i, _, _) -> True || mod (i -2) 8 == 0) $ imap (\i (V2 x y) -> (i, x + y, V2 x y)) $ (V2 0 0) : walk N (V2 0 0)
  print $ adder $ start + (mult times)

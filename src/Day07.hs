{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Day07 where

import Control.Arrow
import Data.List (findIndex)
import Debug.Trace

step (!toAnt, !santa) = (1 + incTot * toAnt, santa + 20)
 where
  incTot = 1 + (20 / santa)

-- aTot = toAnt / santa

san n = (n + 1) * 20

ant 0 = 1
ant n = 1 + i * ant (n -1)
 where
  i = (n + 1) / n

stepp (!toAnt, !santa) = if toAnt >= santa then (toAnt, santa) else stepp (1 + incTot * toAnt, santa + 20)
 where
  incTot = 1 + (20 / santa)

run = do
  print 1

  print $ stepp (1, 20)

-- let q = iterate step (1, 20)
-- print $ q !! 272400599

-- print $ findIndex (\(ant, santa) -> ant >= santa) q

-- print $ take 3 $ drop (pred 272400599) q

-- mapM_ print $ take 10 $ iterate step (1, 20)

-- print $ map ((\x -> -1 + (x + 1) / x)) [1 .. 10]
-- mapM_ print $ map ((10 *) . ant &&& san) [1 .. 10]

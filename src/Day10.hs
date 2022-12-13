{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Day10 where

import Control.Lens
import Data.Foldable
import Data.List
import Data.List.Extra
import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Set as Set
import Debug.Trace
import Linear hiding (trace)

findPoss sizeLim = 1

allXY :: [V2 Integer]
allXY = (V2 <$> [0 .. 2] <*> [0 .. 2])

-- ns :: Map.Map (V2 Integer) ([V2 Integer], [V2 Integer])
ns = Map.fromList $ fmap (\v -> (v, neighs v)) allXY

traceLab s x = trace (s ++ ": " ++ show x) x

neighs xy = (filter isOK oneSteps, filter is2OK neighs)
  where
    is2OK (mid, t) = elem mid allXY && isOK t
    isOK = flip elem allXY
    oneSteps = delete xy allXY \\ fmap snd neighs
    neighs = drop 1 $ do
        x <- [0, (-1), 1]
        y <- [0, (-1), 1]
        pure $ (xy + (V2 x y), xy + (2 * (V2 x y)))

-- findPassList cur n m | trace ("! " ++ show cur ++ " - " ++ show m) False = undefined
findPassList cur 0 m = [[cur]]
findPassList cur n m | null m = []
findPassList cur n m = foldMap goOne vaidOnes <> foldMap (goOne . snd) vaidTwos
  where
    goOne t = map (cur :) $ findPassList t (n -1) (Set.delete t m)

    -- goSkip (mid, t) | Map.notMember mid m = goOne t
    -- goSkip (mid, t) = []

    vaidOnes = filter (flip Set.member m) ones

    vaidTwos = filter checkTwo twos

    checkTwo (mid, t) = Set.notMember mid m && Set.member t m

    (ones, twos) = ns Map.! cur

solve (traceLab "startCur" -> cur) lim = length $ f $ traceLab "startM" m
  where
    f = fmap (map pretty . checkUniq) . findPassList cur (lim -1)
    m = (Set.delete cur $ Set.fromList allXY)

checkUniq xs = if length xs == length (nubOrd xs) then xs else error $ show xs

pretty (V2 0 0) = 'A'
pretty (V2 1 0) = 'B'
pretty (V2 2 0) = 'C'
pretty (V2 0 1) = 'D'
pretty (V2 1 1) = 'E'
pretty (V2 2 1) = 'F'
pretty (V2 0 2) = 'G'
pretty (V2 1 2) = 'H'
pretty (V2 2 2) = 'I'
pretty _ = undefined

run = do
    -- print $ findPoss 3

    -- print $ (1 :: V2 Int)
    -- print $ ns

    -- print $ neighs 0

    -- print $ solve (V2 0 2) 3

    print $ map (flip solve 8) allXY
    -- print $ solve (V2 0 2) 8
    print $ ns

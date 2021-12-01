{-# LANGUAGE ViewPatterns #-}

module Day01 where

import Data.List (stripPrefix)

data Nr = Small Int | Large Int deriving (Show)

getNr :: Nr -> Int
getNr (Small i) = i
getNr (Large i) = i

makeNums :: [Char] -> (Nr, [Char])
makeNums (stripPrefix "ti" -> Just r) = (Large 10, r)
makeNums (stripPrefix "elleve" -> Just r) = (Large 11, r)
makeNums (stripPrefix "tolv" -> Just r) = (Large 12, r)
makeNums (stripPrefix "tretten" -> Just r) = (Large 13, r)
makeNums (stripPrefix "fjorten" -> Just r) = (Large 14, r)
makeNums (stripPrefix "femten" -> Just r) = (Large 15, r)
makeNums (stripPrefix "seksten" -> Just r) = (Large 16, r)
makeNums (stripPrefix "sytten" -> Just r) = (Large 17, r)
makeNums (stripPrefix "atten" -> Just r) = (Large 18, r)
makeNums (stripPrefix "nitten" -> Just r) = (Large 19, r)
makeNums (stripPrefix "tjue" -> Just r) = addIf 20 r
makeNums (stripPrefix "tretti" -> Just r) = addIf 30 r
makeNums (stripPrefix "førti" -> Just r) = addIf 40 r
makeNums (stripPrefix "femti" -> Just r) = addIf 50 r
makeNums (stripPrefix "en" -> Just r) = (Small 1, r)
makeNums (stripPrefix "to" -> Just r) = (Small 2, r)
makeNums (stripPrefix "tre" -> Just r) = (Small 3, r)
makeNums (stripPrefix "fire" -> Just r) = (Small 4, r)
makeNums (stripPrefix "fem" -> Just r) = (Small 5, r)
makeNums (stripPrefix "seks" -> Just r) = (Small 6, r)
makeNums (stripPrefix "syv" -> Just r) = (Small 7, r)
makeNums (stripPrefix "sju" -> Just r) = (Small 7, r)
makeNums (stripPrefix "åtte" -> Just r) = (Small 8, r)
makeNums (stripPrefix "ni" -> Just r) = (Small 9, r)
makeNums s = error s

addIf :: Int -> [Char] -> (Nr, [Char])
addIf num (makeNums -> (Small n, s)) = (Large (num + n), s)
addIf num s = (Large num, s)

iter :: [Char] -> [Int]
iter "" = []
iter [_] = []
iter xs = case makeNums xs of (n, s) -> getNr n : iter s

run :: IO ()
run = do
  inp <- readFile "inputs/1"
  print $ sum $ iter inp

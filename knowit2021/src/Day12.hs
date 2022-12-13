module Day12 where

import Control.Arrow
import Control.Lens
import Data.Maybe
import Data.Monoid
import Data.Tree

data GK = G | K deriving (Show, Read)

parse :: String -> [(Int, GK)]
parse = fmap ((length *** read . take 1) . break (/= '-')) . lines

makeTree :: Ord a1 => [(a1, a2)] -> [Tree a2]
makeTree [] = []
makeTree ((depth, x) : xs) = Node x children : rest
 where
  (children, rest) = over both makeTree $ span (\(y, _) -> depth < y) xs

removeNoGifts n@(Node G _) = Just n
removeNoGifts n@(Node K nested) =
  case mapMaybe removeNoGifts nested of
    [] -> Nothing
    xs -> Just $ Node K xs

count K{} = 1
count G{} = 0

solve :: [(Int, GK)] -> Sum Integer
solve = foldMap (foldMap (foldMap count)) . map removeNoGifts . makeTree

run = do
  inp <- readFile "inputs/12"
  print $ solve $ parse inp

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Day05 where

import Data.Char
import Debug.Trace
import Text.Parsec
import Text.Parsec.Combinator

data Tree2 = L String | Bin2 String Tree2 Tree2 deriving (Show)

s = "Aurora(T(Q Qq) X(x xx))"
ss = "Aurora(Toralv(Grinch(Kari Robinalv) Alvborg) Grinch(Alva(Alve-Berit Anna) Grete(Ola Hans)))"

parseInput :: String -> Tree2
parseInput = either (error . show) id . parse (tree <* eof) ""
 where
  tree = do
    name <- many1 (satisfy (\x -> isLetter x || x == '-'))
    -- parserTrace $ trace ("name is : " ++ name) $ "post name"
    try (double name) <|> pure (L name)

  double n = do
    char '('
    t1 <- tree
    char ' '
    t2 <- tree
    char ')'
    pure $ Bin2 n t1 t2

countDes (L "Grinch") = 0
countDes (L _) = 1
countDes (Bin2 "Grinch" l r) = max (countDes l) (countDes r)
countDes (Bin2 _ l r) = 1 + max (countDes l) (countDes r)

run = do
  inp <- readFile "inputs/5"
  -- print $ parseInput s
  -- print $ parseInput ss
  print $ subtract 1 $ countDes $ parseInput inp
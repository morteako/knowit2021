{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Day14 where

import Control.Lens
import Control.Lens.Regex.Text
import Data.List
import qualified Data.Text as T

parse = fmap T.pack . lines

isNisse = has [regex|^[^n].*n.{0,2}i.{0,2}s.{0,2}s.{0,2}e.{0,2}[^e]$|]
isTroll = has [regex|.*t.{1,5}r.{1,5}o.{1,5}l.{1,5}l.{1,5}.*|]

solve xs = length $ filter (\x -> isNisse x || isTroll x) xs
-- ^.*t.{1,5}r.{1,5}o.{1,5}l.{1,5}l.{1,5}.*

run = do
  inp <- readFile "inputs/14"
  -- let inp = "katastrofeopplevelsen\nantirasistens"
  -- print $ parse inp
  print $ solve $ parse inp

--15
--1403
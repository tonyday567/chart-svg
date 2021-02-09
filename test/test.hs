{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import NumHask.Prelude
import Test.DocTest

main :: IO ()
main = doctest
  [ "src/Chart.hs",
    "src/Data/FormatN.hs",
    "src/Data/Path.hs",
    "src/Chart/Types.hs",
    "src/Chart/Bar.hs",
    "src/Chart/Render.hs",
    "src/Chart/Surface.hs",
    "src/Chart/Examples.hs",
    "src/Data/Colour.hs",
    "src/Chart/Reanimate.hs"
  ]

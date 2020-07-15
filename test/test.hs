{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import NumHask.Prelude
import Test.DocTest

main :: IO ()
main = doctest
  [ "src/Chart.hs",
    "src/Chart/Types.hs"
  ]
  

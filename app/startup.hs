module Main where

import Chart
import NumHask.Prelude

main :: IO ()
main = writeFile "other/t1.svg" (renderChartsWith defaultSvgOptions [])

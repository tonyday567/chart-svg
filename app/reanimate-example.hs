#!/usr/bin/env stack
-- stack runghc --package reanimate

{- | reanimate example

To run this example:

stack runghc --package reanimate app/reanimate-example.hs

and wait for the browser to open ...

-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MonoLocalBinds #-}

module Main where

import Chart
import Chart.Examples
import Control.Lens hiding (transform)
import NumHask.Prelude hiding (fold)
import Chart.Reanimate
import Reanimate as Re

main :: IO ()
main =
  reanimate $
  foldl' seqA (pause 0) $ (applyE (overBeginning 1 fadeInE) . applyE (overEnding 1 fadeOutE)) . mapA pathify . (\cs -> animChartSvg defaultReanimateConfig (const cs)) . (#hudOptions %~ colourHudOptions light) <$> examples

examples :: [ChartSvg]
examples =
  [ unitExample,
    svgOptionsExample,
    hudOptionsExample,
    rectExample,
    textExample,
    glyphsExample,
    lineExample,
    barExample,
    waveExample,
    lglyphExample,
    glinesExample,
    compoundExample,
    textLocalExample,
    labelExample,
    legendExample,
    surfaceExample,
    arcExample,
    arcFlagsExample,
    ellipseExample,
    quadExample,
    cubicExample,
    pathExample,
    vennExample,
    arrowExample
  ]


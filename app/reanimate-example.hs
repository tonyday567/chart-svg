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

module Main where

import Chart
import Chart.Examples
import Control.Lens hiding (transform)
import Graphics.SvgTree.Types hiding (Point, Text)
import NumHask.Prelude hiding (fold)
import Reanimate hiding (scale)
-- import qualified Reanimate as Re
import Chart.Reanimate

main :: IO ()
main =
  reanimate .
  addStatic (mkBackgroundPixel (toPixelRGBA8 $ Colour 0.9 0.9 0.9 1)) .
  mkAnimation 5 $
  (\x ->
     chartSvgTree
     (expScaleChartData x hudOptionsExample))

-- (pan surf one (Rect 0.2 0.21 0.1 0.11))

surf :: Rect Double -> ChartSvg
surf r = surfacegExample "rosenbrock" (Point 100 100) (Point 20 20) r (bimap (-1.0 *) (-1.0 .*) . rosenbrock 1 10) & #hudOptions . #hudAxes %~ fmap (#axisTick . #tstyle .~ TickRound (FormatComma (Just 2)) 8 NoTickExtend)

pan :: (Rect Double -> ChartSvg) -> Rect Double -> Rect Double -> Double -> Tree
pan f start end x = chartSvgTree $ f (start + fmap (*x) (end - start))

expScaleChartData :: Double -> ChartSvg -> ChartSvg
expScaleChartData x cs =
     cs &
     #chartList %~ fmap (expScaleData (10 * (x - 0.5))) &
     #hudOptions . #hudAxes %~
     fmap (set (#axisTick . #tstyle)
           (TickRound (FormatComma (Just 2)) 8 NoTickExtend))

expScaleData :: Double -> Chart Double -> Chart Double
expScaleData s c = c & #xys %~ fmap (fmap ((10.0 ** s) *))


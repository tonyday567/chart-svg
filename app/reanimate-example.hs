#!/usr/bin/env stack
-- stack runghc --package reanimate

{- | reanimate example

To run this example:

stack runghc --package reanimate app/reanimate-example.hs

and wait for the browser to open ...

-}

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Chart
import Chart.Examples
import Control.Lens hiding (transform)
import Graphics.SvgTree.Types hiding (Point, Text)
import NumHask.Prelude hiding (fold)
import Reanimate hiding (scale)
import qualified Reanimate as Re
import Chart.Reanimate

main :: IO ()
main =
  reanimate $
  addStatic (mkBackgroundPixel (toPixelRGBA8 $ Colour 0.9 0.8 0.9 1)) $
  mkAnimation 20
  (\x ->
     cs &
     #chartList %~ fmap (expScaleData (10 * (x - 0.5))) &
     #hudOptions . #hudAxes %~
     fmap (set (#axisTick . #tstyle)
           (TickRound (FormatComma (Just 2)) 8 NoTickExtend)) &
     toTree)
  where
    cs = hudOptionsExample

expScaleData :: Double -> Chart Double -> Chart Double
expScaleData s c = c & #xys %~ fmap (fmap ((10.0 ** s) *))

toTree :: ChartSvg -> Tree
toTree cs =
  Re.scaleXY 5 (-5) $
  simplify $
  mkGroup $
  -- withViewBox
  chartSvgTree cs


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
import Chart.Reanimate
import Control.Lens hiding (transform)
import Graphics.SvgTree.Types hiding (Point, Text)
import NumHask.Prelude hiding (fold)
import Reanimate hiding (scale)
import qualified Reanimate as Re
-- import Reanimate.Animation
-- import qualified Graphics.SvgTree.Types as Svg
-- import Reanimate.Svg (unboxFit)

main :: IO ()
main =
  reanimate
    $ addStatic (mkBackgroundPixel (toPixelRGBA8 $ Colour 0.9 0.8 0.9 0.2))
    $ mkAnimation 20 (\x -> tree' [expScaleData (10 * (x - 0.5)) $ sinChart])

scaleData :: Double -> Chart Double -> Chart Double
scaleData s c = c & #xys %~ fmap (fmap (s *))

expScaleData :: Double -> Chart Double -> Chart Double
expScaleData s c = c & #xys %~ fmap (fmap ((10.0 ** s) *))

tree' :: [Chart Double] -> Tree
tree' cs =
  Re.scaleXY (5) (-5)
    $ simplify
    $ unbox
    $ fromHudOptionsChart
      (defaultSvgOptions & #svgHeight .~ 100 & #scaleCharts' .~ ScaleCharts)
      ( defaultHudOptions & #hudAxes
          %~ fmap
            ( #atick . #tstyle
                .~ TickRound (FormatComma (Just 2)) 8 NoTickExtend
            )
      )
      []
      cs

t1 :: Double -> Tree
t1 x =
  tree
    ( Chart
        (RectA defaultRectStyle)
        [RectXY $ (x *) <$> one]
    )

sinChart :: Chart Double
sinChart = Chart (GlyphA defaultGlyphStyle) (PointXY <$> gridP sin (Range 0 (2 * pi)) 30)

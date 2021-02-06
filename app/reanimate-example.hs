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
  -- reanimChartSvg defaultReanimateConfig (sOpac lineExample)
  reanimate $
  foldl' seqA (pause 0) $ animChartSvg defaultReanimateConfig . sOpac <$> examples

examples :: [ChartSvg]
examples =
  [ examplePalette defaultExamplePaletteConfig,
    unitExample,
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

sOpac :: ChartSvg -> Double -> ChartSvg
sOpac cs o =
  scaleOpacChartSvg (Range 0 1) o .
  (#hudOptions %~ colourHudOptions light) $
  cs

scaleOpacChartSvg :: Range Double -> Double -> ChartSvg -> ChartSvg
scaleOpacChartSvg r x cs =
  cs &
  #hudOptions .~ scaleOpacHudOptions (cs & view #hudOptions) (project (Range zero one) r x) &
  #chartList .~
  ((#annotation %~ scaleOpacAnn (project (Range zero one) r x)) <$>
    view #chartList cs)

-- palette visualisation
data ExamplePaletteConfig = ExamplePaletteConfig
  { epRange :: Range Double,
    epMax :: Int,
    epSize :: Double,
    epText :: Text,
    epBColor :: Colour,
    epUnit :: Double
  } deriving (Eq, Show, Generic)

defaultExamplePaletteConfig :: ExamplePaletteConfig
defaultExamplePaletteConfig = ExamplePaletteConfig (Range 0.5 1.0) 8 0.12 "chart-svg" transparent 1

examplePalette :: ExamplePaletteConfig -> ChartSvg
examplePalette cfg =
  setEPBase (view #epRange cfg) (view #epMax cfg) (view #epSize cfg) (view #epText cfg) (view #epBColor cfg) (view #epUnit cfg)

interpolate :: (Ring a) => Range a -> a -> a
interpolate (Range l u) x = l + x * (u-l)

setEPBase :: Range Double -> Int -> Double -> Text -> Colour -> Double -> ChartSvg
setEPBase r n s t bk o = mempty & #chartList .~ cs <> frame
  where
    frame = [Chart (scaleOpacAnn (interpolate r o) (RectA (defaultRectStyle & #color .~ bk))) (RectXY <$> maybeToList (styleBoxes cs))]
    cs =
      zipWith (\c x' -> Chart (TextA (defaultTextStyle &
                                    #size .~ s & #color .~ setOpac (interpolate r o) c)
                              [t]) [P 0 x'])
      p
      (grid OuterPos (one :: Range Double) (fromIntegral $ length p) :: [Double])
    p = reverse $ take n $ [grey, dark, light, black, white] <> palette1


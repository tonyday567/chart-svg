{-# LANGUAGE IncoherentInstances #-}
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

main :: IO ()
main =
  reanimChartSvg defaultReanimateConfig $ \o -> scaleOpacChartSvg defaultSetOpacConfig o lineExample
  -- reanimChartSvg defaultReanimateConfig $ \o -> scaleOpacChartSvg o (examplePalette defaultExamplePaletteConfig)

interpolate :: (Ring a) => Range a -> a -> a
interpolate (Range l u) x = l + x * (u-l)

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

-- fadeIn
-- fading in a ChartSvg
newtype SetOpacConfig = SetOpacConfig
  { soRange :: Range Double
  }
  deriving (Eq, Show, Generic)

defaultSetOpacConfig :: SetOpacConfig
defaultSetOpacConfig = SetOpacConfig (Range 0.5 1.0)

scaleOpacChartSvg :: SetOpacConfig -> Double -> ChartSvg -> ChartSvg
scaleOpacChartSvg cfg x cs =
  cs &
  #hudOptions .~ scaleOpacHudOptions (cs & view #hudOptions) (project (Range zero one) (view #soRange cfg) x) &
  #chartList .~
  ((#annotation %~ scaleOpacAnn (project (Range zero one) (view #soRange cfg) x)) <$>
    view #chartList cs)

-- helper for below
scaleOpac :: Double -> Colour -> Colour
scaleOpac x (Colour r g b o') = Colour r g b (o'*x)

-- dim (or brighten) the opacity of an Annotation by a scale
scaleOpacAnn :: Double -> Annotation -> Annotation
scaleOpacAnn x (RectA s) = RectA s'
  where
    s' = s & #color %~ scaleOpac x & #borderColor %~ scaleOpac x
scaleOpacAnn x (TextA s ts) = TextA s' ts
  where
    s' = s & #color %~ scaleOpac x
scaleOpacAnn x (LineA s) = LineA s'
  where
    s' = s & #color %~ scaleOpac x
scaleOpacAnn x (GlyphA s) = GlyphA s'
  where
    s' = s & #color %~ scaleOpac x & #borderColor %~ scaleOpac x
scaleOpacAnn x (PathA s pis) = PathA s' pis
  where
    s' = s & #color %~ scaleOpac x & #borderColor %~ scaleOpac x
scaleOpacAnn _ BlankA = BlankA

scaleOpacHudOptions :: HudOptions -> Double -> HudOptions
scaleOpacHudOptions ho o =
  ho &
  #hudCanvas %~ fmap (#color %~ scaleOpac o) &
  #hudTitles %~ fmap (#style . #color %~ scaleOpac o) &
  #hudAxes %~ fmap (#axisBar %~ fmap (#rstyle . #color %~ scaleOpac o)) &
  #hudAxes %~ fmap (#axisTick . #gtick %~ fmap (first ((#color %~ scaleOpac o) . (#borderColor %~ scaleOpac o)))) &
  #hudAxes %~ fmap (#axisTick . #ttick %~ fmap (first (#color %~ scaleOpac o))) &
  #hudAxes %~ fmap (#axisTick . #ltick %~ fmap (first (#color %~ scaleOpac o))) &
  #hudLegend %~ fmap (first (#ltext %~ (#color %~ scaleOpac o))) &
  #hudLegend %~ fmap (first (#legendFrame %~ fmap ((#color %~ scaleOpac o) . (#borderColor %~ scaleOpac o)))) &
  #hudLegend %~ fmap (second (fmap (first (scaleOpacAnn o))))

-- Circles example
circle :: Polar Double Double -> Double -> Colour -> Chart Double
circle pos s c = Chart (GlyphA $ defaultGlyphStyle & set #color c & set #size s) [PointXY $ coord pos]

data Circle = Circle { circlePos :: Polar Double Double, circleSize :: Double, circleColor :: Colour} deriving (Eq, Show, Generic)

circles :: [Circle]
circles = [
  Circle (Polar 1 (pi/4)) 0.05 (Colour 0.4 0.6 0.9 0.4)
          ]

-- legend
-- reanimChartSvg defaultReanimateConfig $ \o -> scaleOpacChartSvg defaultSetOpacConfig o (mempty & #svgOptions . #chartAspect .~ UnadjustedAspect & #chartList .~ (legendChart (zip (LineA <$> lopts) ["hockey","line","vertical"]) defaultLegendOptions & fmap (#annotation %~ scaleAnn 0.5) & projectXYs (Rect (-0.2) 0.2 (-0.2) 0.2)))

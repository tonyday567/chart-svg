# Seaborn Sine

Seaborn "whitegrid" + "notebook" aesthetic recreated in chart-svg.
Reference: [seaborn tutorial](https://seaborn.pydata.org/tutorial/aesthetics.html).

Output: `seaborn-sine.svg`

```haskell
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Chart hiding (palette)
import Optics.Core

-- | Seaborn "deep" palette
palette :: [Colour]
palette =
  [ Colour 0.298 0.447 0.690 1.0   -- #4C72B0 blue
  , Colour 0.867 0.518 0.322 1.0   -- #DD8452 orange
  , Colour 0.333 0.659 0.408 1.0   -- #55A868 green
  , Colour 0.769 0.306 0.322 1.0   -- #C44E52 red
  , Colour 0.506 0.447 0.702 1.0   -- #8172B3 purple
  , Colour 0.800 0.725 0.455 1.0   -- #CCB974 yellow
  ]

darkGray, lightGray, gridGray, white :: Colour
darkGray  = Colour 0.15 0.15 0.15 1.0
lightGray = Colour 0.80 0.80 0.80 1.0
gridGray  = Colour 0.75 0.75 0.75 0.5
white     = Colour 1.00 1.00 1.00 1.0
transparent = Colour 0.0 0.0 0.0 0.0

-- | Invisible anchor rect stretches domain so grid lines span full axis.
domain :: Chart
domain = BlankChart (defaultStyle & set #color transparent & set #borderSize 0)
                    [Rect (-1) 15 (-7) 7]

gridLine :: TickStyle
gridLine = TickStyle
  (defaultLineStyle & set #color gridGray & set #size 5.0e-3 & set #borderSize 0)
  CanvasSection 0

seabornYAxis :: AxisOptions
seabornYAxis =
  defaultYAxisOptions
    & set #place PlaceLeft
    & set #axisBar Nothing
    & set (#ticks % #glyphTick) Nothing
    & set (#ticks % #lineTick) (Just gridLine)

seabornXAxis :: AxisOptions
seabornXAxis =
  defaultXAxisOptions
    & set #place PlaceBottom
    & set #axisBar Nothing
    & set (#ticks % #glyphTick) Nothing
    & set (#ticks % #lineTick) (Just gridLine)

hud :: HudOptions
hud =
  defaultHudOptions
    & set #axes
        [ Priority 5 (seabornYAxis & set (#ticks % #tick)
                        (TickPlaced [(-6,"-6"),(-3,"-3"),(0,"0"),(3,"3"),(6,"6")]))
        , Priority 5 (seabornXAxis & set (#ticks % #tick)
                        (TickPlaced [(0,"0"),(2,"2"),(4,"4"),(6,"6"),(8,"8"),(10,"10"),(12,"12"),(14,"14")]))
        ]
    & set #legends []
    & set #frames [Priority 1 (defaultFrameOptions
                        & set #buffer 0
                        & set (#frame %? #borderSize) 5.0e-3
                        & set (#frame %? #borderColor) gridGray
                        & set (#frame %? #color) transparent
                        & set (#frame %? #size) 0)]

sea :: ChartOptions -> ChartOptions
sea co =
  co
    & set #hudOptions (hud <> view #hudOptions co)
    & set #chartTree (view #chartTree co <> unnamed [domain])
    & set (#markupOptions % #chartAspect) (FixedAspect 1.6)
    & set (#markupOptions % #cssOptions % #preferColorScheme) PreferHud

sinWaves :: Int -> [[(Double, Double)]]
sinWaves n =
  [ [ (x, sin (x + fromIntegral i * 0.5) * fromIntegral (n + 2 - i))
    | x <- [0, 0.1 .. 14] ]
  | i <- [1 .. n] ]

lineStyle :: Int -> Style
lineStyle i =
  defaultLineStyle
    & set #color (palette !! (i `mod` length palette))
    & set #size 0.007 & set #borderSize 0
    & set #lineCap (Just LineCapRound)

main :: IO ()
main =
  writeChartOptions "examples/seaborn-sine.svg" $
    sea (mempty
       & set #chartTree (named "sine" $
            zipWith (\i pts -> LineChart (lineStyle i) [uncurry Point <$> pts])
                    [0..] (sinWaves 6))
       & set (#hudOptions % #titles)
           [Priority 5 (defaultTitleOptions "Seaborn-style Sine Waves"
                          & set (#style % #size) 0.05)])
```

## Key techniques

**Domain anchor** — invisible `BlankChart` stretches the data bounding box so grid lines (`lineTick`) span the full axis range, working around [#55](https://github.com/tonyday567/chart-svg/issues/55).

**No spines** — `axisBar Nothing` on both axes, replacing them with `lineTick` grid lines + transparent frame border.

**TickPlaced** — exact tick positions for inner-only labels, matching seaborn's auto-tick choices.

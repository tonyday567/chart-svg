# Boxplot & Stacked Bar

Chart combinators extracted from [dataframe-load](https://github.com/tonyday567/dataframe-load).
Pure chart-svg — no DataFrame dependency.

Output: `boxplot.svg`, `stackedbar.svg`

```haskell
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Chart
import Data.List (scanl', sort)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Optics.Core

--------------------------------------------------------------------------------
-- quantiles

-- | Five-number summary: min, Q1, median, Q3, max.
-- Linear interpolation at non-integer positions.
quantiles :: [Double] -> [Double]
quantiles [] = [0, 0, 0, 0, 0]
quantiles xs = [q i | i <- [0 .. 4]]
  where
    s = sort xs
    n = length xs
    q i =
      let pos = fromIntegral (n - 1) * fromIntegral i / 4
          lo  = floor pos
          hi  = ceiling pos
          frac = pos - fromIntegral lo
       in if lo == hi
            then s !! lo
            else s !! lo + frac * (s !! hi - s !! lo)

--------------------------------------------------------------------------------
-- box plot

data BoxPlotOptions = BoxPlotOptions
  { q2Style :: Style
  , q3Style :: Style
  , q1Style :: Style
  , q4Style :: Style
  }
  deriving (Generic, Show, Eq)

defaultBoxPlotOptions :: BoxPlotOptions
defaultBoxPlotOptions =
  BoxPlotOptions defaultRectStyle defaultRectStyle defaultLineStyle defaultLineStyle

boxPlot :: BoxPlotOptions -> [Double] -> ChartOptions
boxPlot o v = c
  where
    qs = quantiles v
    l1 = LineChart (view #q1Style o) [[Point 0.5 (qs !! 0), Point 0.5 (qs !! 1)]]
    l4 = LineChart (view #q4Style o) [[Point 0.5 (qs !! 3), Point 0.5 (qs !! 4)]]
    r2 = RectChart (view #q2Style o) [Rect 0 1 (qs !! 1) (qs !! 2)]
    r3 = RectChart (view #q3Style o) [Rect 0 1 (qs !! 2) (qs !! 3)]
    c  =
      (mempty :: ChartOptions)
        & set (#markupOptions % #chartAspect) (FixedAspect 0.25)
        & set #hudOptions defaultHudOptions
        & over (#hudOptions % #axes) (drop 1)
        & set #chartTree (named "boxplot" [l1, r2, r3, l4])

--------------------------------------------------------------------------------
-- stacked bar

data StackedBarOptions = StackedBarOptions
  { itemStyles :: [(Style, Style)]
  , maxStacks  :: Int
  }
  deriving (Generic, Show, Eq)

defaultStackedBarOptions :: StackedBarOptions
defaultStackedBarOptions =
  StackedBarOptions
    ( fmap
        ( \c ->
            ( defaultRectStyle
                & set #borderSize zero
                & set #color c
                & set (#color % opac') 0.2
            , defaultTextStyle
                & set #size 0.06
                & set #color c
                & set (#color % opac') 0.6
                & over (#color % lightness') (* 0.7)
            )
        )
        (palette <$> [0 .. 19])
    )
    20

stackedBar :: StackedBarOptions -> Text -> [(Text, Double)] -> ChartOptions
stackedBar o t xs = co
  where
    ls  = fmap fst xs
    vs' = fmap snd xs
    vs  = (/ sum vs') <$> vs'
    bd  = BarData (fmap pure vs) [t] ls
    bc  =
      barChart
        ( defaultBarOptions
            & set #displayValues False
            & set #barStacked Stacked
            & set #barRectStyles (view #itemStyles o & fmap fst)
        )
        bd
    acc0 = scanl' (+) 0 vs
    mids = zipWith (\a0 a1 -> (a0 + a1) / 2) acc0 (drop 1 acc0)
    ct   =
      zipWith
        (\s (lbl, a) -> TextChart s [(lbl, Point zero (0.5 - a))])
        (view #itemStyles o & fmap snd)
        (zip ls mids)
    co =
      bc
        & set (#hudOptions % #axes % each % #item % #axisBar) Nothing
        & set (#hudOptions % #axes % each % #item % #ticks % #glyphTick) Nothing
        & set (#hudOptions % #axes % each % #item % #ticks % #textTick % _Just % #style % #size) 0.08
        & set (#hudOptions % #axes % each % #item % #adjustments) (Just $ Adjustments 0.08 0.16 0.2 True)
        & set (#markupOptions % #chartAspect) (FixedAspect 0.4)
        & set (#hudOptions % #legends) mempty
        & over #chartTree (<> named "labels" ct)

--------------------------------------------------------------------------------
-- demo

main :: IO ()
main = do
  -- Box plot: 1000 sin values
  writeChartOptions "examples/boxplot.svg" $
    boxPlot defaultBoxPlotOptions [sin (fromIntegral i * 0.01) * 50 | i <- [1 .. 1000 :: Int]]

  -- Stacked bar: categorical counts
  let items =
        [ ("person", 20.0)
        , ("woman", 23.1)
        , ("man", 31.0)
        , ("camera", 16.0)
        , ("tv", 10.0)
        ]
  writeChartOptions "examples/stackedbar.svg" $
    stackedBar defaultStackedBarOptions "inventory" items
```

## Usage

```haskell
boxPlot defaultBoxPlotOptions [1..1000]            -- box plot
stackedBar defaultStackedBarOptions "title" items  -- stacked bar
```

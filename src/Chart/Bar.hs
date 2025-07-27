{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Bar charts
module Chart.Bar
  ( BarOptions (..),
    defaultBarOptions,
    BarData (..),
    barRange,
    bars,
    barChart,
    barRects,
    barTextCharts,
  )
where

import Chart.Data
import Chart.Hud
import Chart.Markup
import Chart.Primitive
import Chart.Style
import Data.Bool
import Data.Colour
import Data.Data
import Data.Foldable
import Data.FormatN
import Data.List (transpose)
import Data.Maybe
import Data.Text (Text, pack)
import GHC.Generics
import Optics.Core
import Prelude hiding (abs)

-- $setup
--
-- >>> :set -XOverloadedLabels
-- >>> :set -XOverloadedStrings
-- >>> import Chart
-- >>> import Optics.Core
-- >>> import Data.Text (pack)

-- | Typical bar chart options.
--
-- The internal model for a bar chart (across the x-axis for a vertical bar chart) is:
--
-- - half the outerGap at the start and the end.
--
-- - each row collection of bars, including the outerGap and innerGaps has a value of 1.
--
-- - the entire x range of the chart isequal to the number of rows in the bar data.
--
-- - The value of inner and outer gaps are relative to this model.
--
-- >>> let barDataExample = BarData [[1, 2, 3, 5, 8, 0, -2, 11, 2, 1], [1 .. 10]] (("row " <>) . pack . show <$> [1 .. 11]) (("column " <>) . pack . show <$> [1 .. 2])
-- >>> let barExample = barChart defaultBarOptions barDataExample
--
-- > writeChartOptions "other/bar.svg" barExample
--
-- ![bar chart example](other/bar.svg)
data BarOptions = BarOptions
  { barRectStyles :: [Style],
    barTextStyles :: [Style],
    -- | gap between each bar collection row.
    outerGap :: Double,
    -- | gap between bars within a row collection. Negative numbers represent bar overlaps.
    innerGap :: Double,
    -- | gap between top of a bar and text representation of the bar value
    -- as a proportion of the highest absolute bar value
    textGap :: Double,
    -- | gap between top of a bar and text representation of the bar value,
    -- if the value is negative
    -- as a proportion of the highest absolute bar value
    textGapNegative :: Double,
    -- | A nudge to help text align for horizontal bar charts.
    textShiftVert :: Double,
    -- | Whether to display text values above bars.
    displayValues :: Bool,
    valueFormatN :: FormatN,
    barOrientation :: Orientation,
    barStacked :: Stacked,
    barLegendOptions :: LegendOptions
  }
  deriving (Eq, Show, Generic, Data)

-- | The official bar options.
defaultBarOptions :: BarOptions
defaultBarOptions =
  BarOptions
    gs
    ts
    0.1
    0
    0.03
    0.05
    (-0.008)
    True
    (FormatN FSCommaPrec (Just 2) 4 True True)
    Vert
    NonStacked
    defaultLegendOptions
  where
    gs = (\x -> defaultRectStyle & set #borderSize 0.005 & set #borderColor (palette x) & set #color (paletteO x 0.7)) <$> [1, 2, 6, 7, 5, 3, 4, 0]
    ts = (\x -> defaultTextStyle & set #color (palette x) & set #size 0.03) <$> [1, 2, 6, 7, 5, 3, 4, 0]

-- | Number of bars per row of data
cols :: Stacked -> [[Double]] -> Int
cols Stacked _ = 1
cols NonStacked xs = length xs

-- | Number of rows
rows :: [[Double]] -> Int
rows xs = maximum $ (0 :) $ length <$> xs

-- | Width of each bar
barWidth :: BarOptions -> [[Double]] -> Double
barWidth o xs = ((1 - outerGap o) / c) - (innerGap o * (c - 1))
  where
    c = fromIntegral $ cols (barStacked o) xs

-- | Placement for the ith row jth column bar (x axis for vertical bars)
barX0 :: BarOptions -> [[Double]] -> Int -> Int -> Double
barX0 o xs i j = outerGap o / 2 + fromIntegral i + fromIntegral j * (barWidth o xs + innerGap o)

-- | Make bars from the double list values, normalizing to one :: Rect.
--
-- >>> barRects defaultBarOptions [[1,2],[2,3]]
-- [[Rect (-0.5) (-0.26315789473684215) (-0.5) (-0.16666666666666669),Rect 2.631578947368418e-2 0.26315789473684204 (-0.5) 0.16666666666666663],[Rect (-0.26315789473684215) (-2.6315789473684292e-2) (-0.5) 0.16666666666666663,Rect 0.26315789473684204 0.4999999999999999 (-0.5) 0.5]]
--
-- >>> barRects defaultBarOptions [[]]
-- []
barRects :: BarOptions -> [[Double]] -> [[Rect Double]]
barRects o xs = rects'
  where
    rects' = fmap (fmap (projectOnR one sb)) rects
    rects = fmap (fmap (flipRect (barOrientation o))) $ accVals $ zip2With (\y x0 -> abs (Rect x0 (x0 + barWidth o xs') 0 y)) xs' (barX0s o xs')
    sb = fromMaybe one $ foldRect (mconcat rects)
    xs' = appendZeros xs
    accVals = bool id accRectYs (barStacked o == Stacked)
    accRectYs xss = foldr addLast [] xss
    addLast rs [] = [rs]
    addLast rs res@(l : _) = zipWith addW rs l : res
    addW (Rect x z y w) (Rect _ _ _ w') = Rect x z (y + w') (w + w')

zip2With :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zip2With f = zipWith (zipWith f)

-- outer product on functors
iter2 :: (Functor f, Functor g) => (a -> b -> c) -> f a -> g b -> f (g c)
iter2 f xs ys = f <$> xs <&> flip fmap ys -- or (\a -> f a <$> ys) <$> xs

-- | Placements for the bars (x axis for vertical bars)
barX0s :: BarOptions -> [[Double]] -> [[Double]]
barX0s o xs = transpose $ iter2 (barX0 o xs) [0 .. (rows xs - 1)] (bool (replicate (length xs) 0) [0 .. (length xs - 1)] (barStacked o == NonStacked))

flipRect :: Orientation -> Rect Double -> Rect Double
flipRect Vert r = r
flipRect Hori (Rect x z y w) = Rect y w x z

appendZeros :: [[Double]] -> [[Double]]
appendZeros xs =
  ( \x ->
      take
        (rows xs)
        (x <> repeat 0)
  )
    <$> xs

-- | A bar chart.
--
-- >>> emptyBar = barChart defaultBarOptions (BarData [] [] [])
-- >>> foldOf (#chartTree % charts') emptyBar
-- []
barChart :: BarOptions -> BarData -> ChartOptions
barChart bo bd =
  mempty
    & set #hudOptions (barHudOptions bo bd)
    & set
      #chartTree
      ( named
          "barchart"
          ( bars bo bd
              <> bool [] (barTextCharts bo bd) (view #displayValues bo)
          )
      )

barHudOptions :: BarOptions -> BarData -> HudOptions
barHudOptions bo bd =
  mempty
    & set #axes [Priority 1 axis1]
    & set #legends [Priority 10 (o & set #legendCharts (barLegendContent bo bd))]
  where
    o = view #barLegendOptions bo
    axis1 = bool defaultXAxisOptions defaultYAxisOptions (barOrientation bo == Hori) & set (#ticks % #lineTick) Nothing & set (#ticks % #tick) (barTicks bd)

-- | Two dimensional data, maybe with row and column labels.
data BarData = BarData
  { barData :: [[Double]],
    barRowLabels :: [Text],
    barColumnLabels :: [Text]
  }
  deriving (Eq, Show, Generic, Data)

-- | Calculate the Rect range of a bar data set.
--
-- >>> barRange [[1,2],[2,3]]
-- Rect 0.0 2.0 0.0 3.0
--
-- >>> barRange [[]]
-- Rect (-0.5) 0.5 (-0.5) 0.5
barRange ::
  [[Double]] -> Rect Double
barRange ys = padSingletons $ Rect 0 (fromIntegral $ rows ys) (min 0 l) u
  where
    (Range l u) = fromMaybe one $ space1 $ mconcat ys

-- | A bar chart without hud trimmings.
--
-- >>> bars defaultBarOptions (BarData [[1,2],[2,3]] [] [])
-- [Chart {chartStyle = Style {size = 6.0e-2, borderSize = 5.0e-3, color = Colour 0.02 0.29 0.48 0.70, borderColor = Colour 0.02 0.29 0.48 1.00, scaleP = NoScaleP, textAnchor = AnchorMiddle, rotation = Nothing, translate = Nothing, escapeText = EscapeText, frame = Nothing, lineCap = Nothing, lineJoin = Nothing, dasharray = Nothing, dashoffset = Nothing, hsize = 0.6, vsize = 1.1, vshift = -0.25, glyphShape = SquareGlyph}, chartData = RectData [Rect (-0.5) (-0.26315789473684215) (-0.5) (-0.16666666666666669),Rect 2.631578947368418e-2 0.26315789473684204 (-0.5) 0.16666666666666663]},Chart {chartStyle = Style {size = 6.0e-2, borderSize = 5.0e-3, color = Colour 0.66 0.07 0.55 0.70, borderColor = Colour 0.66 0.07 0.55 1.00, scaleP = NoScaleP, textAnchor = AnchorMiddle, rotation = Nothing, translate = Nothing, escapeText = EscapeText, frame = Nothing, lineCap = Nothing, lineJoin = Nothing, dasharray = Nothing, dashoffset = Nothing, hsize = 0.6, vsize = 1.1, vshift = -0.25, glyphShape = SquareGlyph}, chartData = RectData [Rect (-0.26315789473684215) (-2.6315789473684292e-2) (-0.5) 0.16666666666666663,Rect 0.26315789473684204 0.4999999999999999 (-0.5) 0.5]}]
--
-- >>> bars defaultBarOptions (BarData [[]] [] [])
-- []
bars :: BarOptions -> BarData -> [Chart]
bars bo bd = bool cs [] (null $ mconcat $ view #barData bd)
  where
    cs =
      zipWith
        (\o d -> RectChart o d)
        (view #barRectStyles bo <> repeat defaultRectStyle)
        (barRects bo (view #barData bd))

-- | Sensible ticks for a bar chart.
barTicks :: BarData -> Tick
barTicks bd
  | null (view #barData bd) = TickNone
  | null (view #barRowLabels bd) =
      TickLabels $ pack . show <$> [0 .. (rows (view #barData bd) - 1)]
  | otherwise =
      TickLabels $
        take (rows (view #barData bd)) $
          view #barRowLabels bd <> repeat ""

-- | A bar legend
barLegendContent :: BarOptions -> BarData -> [(Text, [Chart])]
barLegendContent bo bd
  | null (view #barData bd) = []
  | null (view #barColumnLabels bd) = []
  | otherwise =
      zip
        (view #barColumnLabels bd <> repeat "")
        ((\s -> [Chart s (RectData [one])]) <$> take (length (view #barData bd)) (view #barRectStyles bo))

barTexts :: BarOptions -> [[Double]] -> [[(Text, Point Double)]]
barTexts o xs = zip2With (\x r -> (formatN (valueFormatN o) x, gapt (barOrientation o) r x)) xs (barRects o xs)
  where
    gapt Vert (Rect x z y w) x' = Point ((x + z) / 2) (bool (w + textGap o) (y - textGapNegative o) (x' < 0))
    gapt Hori (Rect x z y w) x' = Point (bool (z + textGap o) (x - textGapNegative o) (x' < 0)) ((y + w) / 2 + textShiftVert o)

-- | Placed text, hold the bars.
barTextCharts :: BarOptions -> BarData -> [Chart]
barTextCharts bo bd =
  zipWith TextChart (view #barTextStyles bo <> repeat defaultTextStyle & set (each % #scaleP) ScalePArea) (barTexts bo (view #barData bd))

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
  )
where

import Chart.Data
import Chart.Hud
import Chart.Markup
import Chart.Primitive
import Chart.Style
import Data.Bool
import Data.Colour
import Data.Foldable
import Data.FormatN
import Data.List (scanl', transpose)
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
  { barRectStyles :: [RectStyle],
    barTextStyles :: [TextStyle],
    -- | gap between each bar collection row.
    outerGap :: Double,
    -- | gap between bars within a row collection, negative overlaps
    innerGap :: Double,
    -- | gap between top of a bar and text representation of the bar value
    -- as a proportion of the highest absolute bar value
    textGap :: Double,
    -- | gap between top of a bar and text representation of the bar value,
    -- if the value is negative
    -- as a proportion of the highest absolute bar value
    textGapNegative :: Double,
    displayValues :: Bool,
    valueFormatN :: FormatN,
    barOrientation :: Orientation,
    barStacked :: Stacked,
    barLegendOptions :: LegendOptions
  }
  deriving (Show, Eq, Generic)

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

-- | Make bars from the double list values.
--
-- >>> barRects defaultBarOptions [[1,2],[2,3]]
-- [[Rect 5.0e-2 0.5 0.0 1.0,Rect 1.05 1.5 0.0 2.0],[Rect 0.5 0.95 0.0 2.0,Rect 1.5 1.95 0.0 3.0]]
--
-- >>> barRects defaultBarOptions [[]]
-- []
barRects :: BarOptions -> [[Double]] -> [[Rect Double]]
barRects o xs = fmap (flipRect (barOrientation o)) <$> zip2With (\y x0 -> abs (Rect x0 (x0 + barWidth o xs) 0 y)) xs' (barX0s o xs')
  where
    xs' = bool id accRows (barStacked o == Stacked) (appendZeros xs)

zip2With :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zip2With f = zipWith (zipWith f)

-- outer product on functors
iter2 :: (Functor f, Functor g) => (a -> b -> c) -> f a -> g b -> f (g c)
iter2 f xs ys = f <$> xs <&> flip fmap ys -- or (\a -> f a <$> ys) <$> xs

-- | Placements for the bars (x axis for vertical bars)
barX0s :: BarOptions -> [[Double]] -> [[Double]]
barX0s o xs = transpose $ iter2 (barX0 o xs) [0 .. (rows xs - 1)] [0 .. cols (barStacked o) xs - 1]

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

accRows :: [[Double]] -> [[Double]]
accRows xs = transpose $ drop 1 . scanl' (+) 0 <$> transpose (fmap toList $ toList xs)

-- | A bar chart.
--
-- >>> emptyBar = barChart defaultBarOptions (BarData [] [] [])
-- >>> foldOf (#charts % charts') emptyBar
-- []
barChart :: BarOptions -> BarData -> ChartOptions
barChart bo bd =
  mempty
    & set #hudOptions (barHudOptions bo bd)
    & set
      #charts
      ( named
          "barchart"
          ( bars bo bd
              <> bool [] (barTextCharts bo bd) (view #displayValues bo)
          )
      )

barHudOptions :: BarOptions -> BarData -> HudOptions
barHudOptions bo bd =
  mempty
    & #axes
      .~ [ (1, axis1)
         ]
    & #legends
      .~ [ (10, o & #legendCharts .~ barLegendContent bo bd)
         ]
  where
    o = view #barLegendOptions bo
    axis1 = bool id flipAxis (barOrientation bo == Hori) (defaultAxisOptions & #ticks % #ltick .~ Nothing & #ticks % #style .~ barTicks bd)

-- | The official bar options.
defaultBarOptions :: BarOptions
defaultBarOptions =
  BarOptions
    gs
    ts
    0.1
    0
    0.04
    0.1
    True
    (FormatN FSCommaPrec (Just 2) 4 True True)
    Vert
    NonStacked
    defaultLegendOptions
  where
    gs = (\x -> RectStyle 0.005 (palette1 x) (palette1a x 0.7)) <$> [1, 2, 6, 7, 5, 3, 4, 0]
    ts = (\x -> defaultTextStyle & #color .~ palette1 x & #size .~ 0.24) <$> [1, 2, 6, 7, 5, 3, 4, 0]

-- | Two dimensional data, maybe with row and column labels.
data BarData = BarData
  { barData :: [[Double]],
    barRowLabels :: [Text],
    barColumnLabels :: [Text]
  }
  deriving (Show, Eq, Generic)

-- | Calculate the Rect range of a bar data set.
--
-- >>> barRange [[1,2],[2,3]]
-- Rect 0.0 2.0 0.0 3.0
--
-- >>> barRange [[]]
-- Rect -0.5 0.5 -0.5 0.5
barRange ::
  [[Double]] -> Rect Double
barRange ys = singletonGuard $ Just $ Rect 0 (fromIntegral $ rows ys) (min 0 l) u
  where
    (Range l u) = fromMaybe one $ space1 $ mconcat ys

-- | A bar chart without hud trimmings.
--
-- >>> bars defaultBarOptions (BarData [[1,2],[2,3]] [] [])
-- [RectChart (RectStyle {borderSize = 5.0e-3, borderColor = Colour 0.02 0.29 0.48 1.00, color = Colour 0.02 0.29 0.48 0.70}) [Rect 5.0e-2 0.5 0.0 1.0,Rect 1.05 1.5 0.0 2.0],RectChart (RectStyle {borderSize = 5.0e-3, borderColor = Colour 0.66 0.07 0.55 1.00, color = Colour 0.66 0.07 0.55 0.70}) [Rect 0.5 0.95 0.0 2.0,Rect 1.5 1.95 0.0 3.0],BlankChart [Rect 0.0 2.0 0.0 3.0]]
--
-- >>> bars defaultBarOptions (BarData [[]] [] [])
-- []
bars :: BarOptions -> BarData -> [Chart]
bars bo bd = bool cs [] (null $ mconcat $ view #barData bd)
  where
    cs =
      zipWith
        (\o d -> RectChart o d)
        (bo ^. #barRectStyles <> repeat defaultRectStyle)
        (barRects bo (bd ^. #barData))
        <> [BlankChart [barRange (bd ^. #barData)]]

-- | Sensible ticks for a bar chart.
barTicks :: BarData -> TickStyle
barTicks bd
  | null (bd ^. #barData) = TickNone
  | null (bd ^. #barRowLabels) =
      TickLabels $ pack . show <$> [0 .. (rows (bd ^. #barData) - 1)]
  | otherwise =
      TickLabels $
        take (rows (bd ^. #barData)) $
          (bd ^. #barRowLabels) <> repeat ""

-- | A bar legend
barLegendContent :: BarOptions -> BarData -> [(Text, [Chart])]
barLegendContent bo bd
  | null (bd ^. #barData) = []
  | null (bd ^. #barColumnLabels) = []
  | otherwise =
      zip
        (view #barColumnLabels bd <> repeat "")
        ((\s -> [RectChart s [one]]) <$> take (length (view #barData bd)) (bo ^. #barRectStyles))

flipPoint :: Orientation -> Point a -> Point a
flipPoint Vert p = p
flipPoint Hori (Point x y) = Point y x

maxAbsValue :: [[Double]] -> Double
maxAbsValue xs = maximum $ fmap abs (0 : mconcat xs)

barTexts :: BarOptions -> [[Double]] -> [[(Text, Point Double)]]
barTexts o xs = zip2With (\t (Rect x z y w) -> (t, flipPoint (barOrientation o) (Point ((x + z) / 2) (bool (w + gap) (y - gapn) (y < 0))))) (fmap (formatN (valueFormatN o)) <$> xs) (barRects o xs)
  where
    gap = textGap o * maxAbsValue xs
    gapn = textGapNegative o * maxAbsValue xs

-- | Placed text, hold the bars.
barTextCharts :: BarOptions -> BarData -> [Chart]
barTextCharts bo bd =
  zipWith TextChart (bo ^. #barTextStyles <> repeat defaultTextStyle) (barTexts bo (bd ^. #barData))

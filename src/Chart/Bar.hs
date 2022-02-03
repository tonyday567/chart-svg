{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

-- | bar charts
module Chart.Bar
  ( BarOptions (..),
    defaultBarOptions,
    BarData (..),
    barDataLowerUpper,
    barRange,
    bars,
    barChart,
    barRects,
  )
where

import Chart.Svg
import Chart.Primitive
import Chart.Style
import Chart.Hud
import Optics.Core
import Data.Bool
import Data.Colour
import Data.FormatN
import Data.List (scanl', transpose)
import Data.Maybe
import Data.Text (Text, pack)
import GHC.Generics
import Prelude hiding (abs)
import Chart.Data
import Data.Foldable

-- $setup
--
-- >>> :set -XOverloadedLabels
-- >>> :set -XOverloadedStrings
-- >>> import Chart
-- >>> import Optics.Core
-- >>> import Data.Text (pack)

-- | Typical bar chart options.
--
-- >>> let barDataExample = BarData [[1, 2, 3, 5, 8, 0, -2, 11, 2, 1], [1 .. 10]] (("row " <>) . pack . show <$> [1 .. 11]) (("column " <>) . pack . show <$> [1 .. 2])
-- >>> let barExample = barChart defaultBarOptions barDataExample
--
-- > writeChartSvg "other/bar.svg" barExample
--
-- ![bar chart example](other/bar.svg)
data BarOptions = BarOptions
  { barRectStyles :: [RectStyle],
    barTextStyles :: [TextStyle],
    outerGap :: Double,
    innerGap :: Double,
    textGap :: Double,
    textGapNegative :: Double,
    displayValues :: Bool,
    valueFormatN :: FormatN,
    accumulateValues :: Bool,
    barOrientation :: Orientation,
    barLegendOptions :: LegendOptions
  }
  deriving (Show, Eq, Generic)

-- | A bar chart.
--
-- >>> emptyBar = barChart defaultBarOptions (BarData [] [] [])
-- >>> foldOf (#charts % charts') emptyBar
-- []
barChart :: BarOptions -> BarData -> ChartSvg
barChart bo bd =
  mempty
    & set #hudOptions (barHudOptions bo bd)
    & set #charts
      (named "barchart"
             (bars bo bd <>
              bool [] (barTextCharts bo bd) (view #displayValues bo)))

barHudOptions :: BarOptions -> BarData -> HudOptions
barHudOptions bo bd =
    mempty &
    #axes .~
      [ (1, axis1),
        (1, axis2)
      ] &
    #legends .~
      [ (10, o & #content .~ barLegendContent bo bd)
      ]
      where
        o = view #barLegendOptions bo
        axis1 = bool id flipAxis (barOrientation bo == Vert) (defaultAxisOptions & #ticks % #ltick .~ Nothing & #ticks % #style .~ barTicks bd)
        axis2 = bool id flipAxis (barOrientation bo == Hori) defaultAxisOptions

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
    (FormatN FSCommaPrec (Just 2) True)
    False
    Hori
    defaultLegendOptions
  where
    gs = (\x -> RectStyle 0.002 (palette1 x) (palette1 x)) <$> [0..9]
    ts = (\x -> defaultTextStyle & #color .~ palette1 x & #size .~ 0.04) <$> [0..9]

-- | imagine a dataframe you get in other languages:
--
-- - some [[Double]]
--
-- - maybe some row names
--
-- - maybe some column names
data BarData = BarData
  { barData :: [[Double]],
    barRowLabels :: [Text],
    barColumnLabels :: [Text]
  }
  deriving (Show, Eq, Generic)

-- | Convert BarData to rectangles
--
-- >>> barRects defaultBarOptions [[1,2],[2,3]]
-- [[Rect 5.0e-2 0.45 0.0 1.0,Rect 1.05 1.4500000000000002 0.0 2.0],[Rect 0.45 0.8500000000000001 0.0 2.0,Rect 1.4500000000000002 1.85 0.0 3.0]]
--
-- >>> barRects defaultBarOptions [[]]
-- [[]]
barRects ::
  BarOptions ->
  [[Double]] ->
  [[Rect Double]]
barRects (BarOptions _ _ ogap igap _ _ _ _ add orient _) bs = rects'' orient
  where
    bs' = bool bs (appendZero bs) add
    rects'' Hori = rects'
    rects'' Vert = fmap (\(Rect x z y w) -> Rect y w x z) <$> rects'
    rects' = zipWith batSet [0 ..] (barDataLowerUpper add bs')
    batSet z ys =
      zipWith
        ( \x (yl, yh) ->
            abs
              ( Rect
                  (x + (ogap / 2) + z * bstep)
                  (x + (ogap / 2) + z * bstep + bstep - igap')
                  yl
                  yh
              )
        )
        [0 ..]
        ys
    n = fromIntegral (length bs')
    bstep = (1 - (1 + 1) * ogap + (n - 1) * igap') / n
    igap' = igap * (1 - (1 + 1) * ogap)

-- | convert data to a range assuming a zero bound
-- a very common but implicit assumption in a lot of bar charts
--
-- >>> barDataLowerUpper False [[1,2],[2,3]]
-- [[(0.0,1.0),(0.0,2.0)],[(0.0,2.0),(0.0,3.0)]]
barDataLowerUpper :: Bool -> [[Double]] -> [[(Double, Double)]]
barDataLowerUpper add bs =
  case add of
    False -> fmap (fmap (0,)) bs
    True -> fmap (fmap (0,)) (accRows bs)

-- | calculate the Rect range of a bar data set.
--
-- >>> barRange [[1,2],[2,3]]
-- Rect 0.0 2.0 0.0 3.0
--
-- >>> barRange [[]]
-- Rect -0.5 0.5 -0.5 0.5
barRange ::
  [[Double]] -> Rect Double
barRange ys = singletonGuard $ Just $ Rect 0 (fromIntegral $ maximum (length <$> ys)) (min 0 l) u
  where
    (Range l u) = fromMaybe one $ space1 $ mconcat ys

-- | A bar chart without hud trimmings.
--
-- >>> bars defaultBarOptions (BarData [[1,2],[2,3]] [] [])
-- [RectChart (RectStyle {borderSize = 2.0e-3, borderColor = Colour 0.69 0.35 0.16 1.00, color = Colour 0.69 0.35 0.16 1.00}) [Rect 5.0e-2 0.45 0.0 1.0,Rect 1.05 1.4500000000000002 0.0 2.0],RectChart (RectStyle {borderSize = 2.0e-3, borderColor = Colour 0.65 0.81 0.89 1.00, color = Colour 0.65 0.81 0.89 1.00}) [Rect 0.45 0.8500000000000001 0.0 2.0,Rect 1.4500000000000002 1.85 0.0 3.0],BlankChart [Rect -5.0e-2 1.9500000000000002 0.0 3.0]]
--
-- >>> bars defaultBarOptions (BarData [[]] [] [])
-- []
bars :: BarOptions -> BarData -> [Chart]
bars bo bd = bool cs [] (null $ mconcat $ view #barData bd)
  where
    cs = zipWith (\o d -> RectChart o d) (bo ^. #barRectStyles <> repeat defaultRectStyle) (barRects bo (bd ^. #barData)) <> [BlankChart [Rect (x - (bo ^. #outerGap)) (z + (bo ^. #outerGap)) y w]]
    (Rect x z y w) = fromMaybe one $ foldRect $ mconcat $ barRects bo (bd ^. #barData)

maxRows :: [[Double]] -> Int
maxRows xs = maximum $ length <$> xs

appendZero :: [[Double]] -> [[Double]]
appendZero xs =
  (\x -> take (maxRows xs)
    (x <> repeat 0)) <$> xs

accRows :: [[Double]] -> [[Double]]
accRows xs = transpose $ drop 1 . scanl' (+) 0 <$> transpose (fmap toList $ toList xs)

-- | sensible ticks
barTicks :: BarData -> TickStyle
barTicks bd
  | null (bd ^. #barData) = TickNone
  | null (bd ^. #barRowLabels) =
    TickLabels $ pack . show <$> [0 .. (maxRows (bd ^. #barData) - 1)]
  | otherwise =
    TickLabels $
      take (maxRows (bd ^. #barData)) $
        (bd ^. #barRowLabels) <> repeat ""

-- | bar legend
barLegendContent :: BarOptions -> BarData -> [(Text, Chart)]
barLegendContent bo bd
  | null (bd ^. #barData) = []
  | null (bd ^. #barColumnLabels) = []
  | otherwise =
    zip
    (view #barColumnLabels bd <> repeat "")
    ((\s -> RectChart s [one]) <$> take (length (view #barData bd)) (bo ^. #barRectStyles))

barDataTP :: Bool -> FormatN -> Double -> Double -> [[Double]] -> [[(Text, Double)]]
barDataTP add fn d negd bs =
  zipWith (zipWith (\x y' -> (formatN fn x, drop' y'))) bs' (bool bs' (accRows bs') add)
  where
    drop' x = bool (x - (negd * (w - y))) (x + (d * (w - y))) (x >= 0)
    bs' = appendZero bs
    (Rect _ _ y w) = barRange bs'

-- | Convert BarData to text
barTexts ::
  BarOptions ->
  [[Double]] ->
  [[(Text, Point Double)]]
barTexts (BarOptions _ _ ogap igap tgap tgapneg _ fn add orient _) bs =
  zipWith zip (fmap fst <$> barDataTP add fn tgap tgapneg bs') (txs'' orient)
  where
    bs' = bool bs (appendZero bs) add
    txs'' Hori = txs'
    txs'' Vert = fmap (\(Point x y) -> Point y x) <$> txs'
    txs' = zipWith addX [0 ..] (fmap snd <$> barDataTP add fn tgap tgapneg bs')
    addX z y =
      zipWith
        ( \x y' ->
            Point
              (x + (ogap / 2) + z * bstep + bstep / 2 - igap' / 2)
              y'
        )
        [0 ..]
        y
    n = fromIntegral (length bs')
    bstep = (1 - (1 + 1) * ogap + (n - 1) * igap') / n
    igap' = igap * (1 - (1 + 1) * ogap)

-- | text, hold the bars
barTextCharts :: BarOptions -> BarData -> [Chart]
barTextCharts bo bd =
  zipWith TextChart (bo ^. #barTextStyles <> repeat defaultTextStyle) (barTexts bo (bd ^. #barData))

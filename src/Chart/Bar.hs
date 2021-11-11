{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
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
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty(..), fromList)
import Data.Maybe
import Data.Text (Text, pack)
import GHC.Generics
import Prelude hiding (abs)
import Chart.Data
import Data.Semigroup
import Data.Foldable

-- $setup
--

-- | Typical bar chart options.
--
-- >>> let barDataExample = BarData [[1, 2, 3, 5, 8, 0, -2, 11, 2, 1], [1 .. 10]] (Just (("row " <>) . pack . show <$> [1 .. 11])) (Just (("column " <>) . pack . show <$> [1 .. 2]))
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
barChart :: BarOptions -> BarData -> ChartSvg
barChart bo bd =
  mempty
    & set #hudOptions (barHudOptions bo bd)
    & set #charts
      (named "barchart"
             (toList (bars bo bd) <>
              bool [] (toList $ barTextCharts bo bd) (view #displayValues bo)))

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
    (FormatComma (Just 2))
    False
    Hori
    defaultLegendOptions
  where
    gs = (\x -> RectStyle 0.002 (palette1 x) (palette1 x)) <$> [0..9]
    ts = (\x -> defaultTextStyle & #color .~ palette1 x & #size .~ 0.04) <$> [0..9]

-- | imagine a dataframe you get in other languages:
--
-- - definitely some [[Double]]
--
-- - maybe some row names
--
-- - maybe some column names
data BarData = BarData
  { barData :: NonEmpty (NonEmpty Double),
    barRowLabels :: [Text],
    barColumnLabels :: [Text]
  }
  deriving (Show, Eq, Generic)

-- | Convert BarData to rectangles
--
-- >>> barRects defaultBarOptions [[1,2],[2,3]]
-- [[Rect 5.0e-2 0.45 0.0 1.0,Rect 1.05 1.4500000000000002 0.0 2.0],[Rect 0.45 0.8500000000000001 0.0 2.0,Rect 1.4500000000000002 1.85 0.0 3.0]]
barRects ::
  BarOptions ->
  NonEmpty (NonEmpty Double) ->
  NonEmpty (NonEmpty (Rect Double))
barRects (BarOptions _ _ ogap igap _ _ _ _ add orient _) bs = rects'' orient
  where
    bs' = bool bs (appendZero bs) add
    rects'' Hori = rects'
    rects'' Vert = fmap (\(Rect x z y w) -> Rect y w x z) <$> rects'
    rects' = NonEmpty.zipWith batSet [0 ..] (barDataLowerUpper add bs')
    batSet z ys =
      NonEmpty.zipWith
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
barDataLowerUpper :: Bool -> NonEmpty (NonEmpty Double) -> NonEmpty (NonEmpty (Double, Double))
barDataLowerUpper add bs =
  case add of
    False -> fmap (0,) <$> bs
    True -> fmap (0,) <$> accRows bs

-- | calculate the Rect range of a bar data set.
--
-- >>> barRange [[1,2],[2,3]]
-- Rect 0.0 2.0 0.0 3.0
barRange ::
  NonEmpty (NonEmpty Double) -> Rect Double
barRange ys'@(y :| ys) = Rect 0 (fromIntegral $ maximum (length <$> ys')) (min 0 l) u
  where
    (Range l u) = sconcat $ space1 <$> (y :| ys)

-- | A bar chart without hud trimmings.
--
-- >>> bars defaultBarOptions (BarData [[1,2],[2,3]] Nothing Nothing)
-- [Chart {annotation = RectA (RectStyle {borderSize = 2.0e-3, borderColor = Colour 0.69 0.35 0.16 1.00, color = Colour 0.69 0.35 0.16 1.00}), xys = [R 5.0e-2 0.45 0.0 1.0,R 1.05 1.4500000000000002 0.0 2.0]},Chart {annotation = RectA (RectStyle {borderSize = 2.0e-3, borderColor = Colour 0.65 0.81 0.89 1.00, color = Colour 0.65 0.81 0.89 1.00}), xys = [R 0.45 0.8500000000000001 0.0 2.0,R 1.4500000000000002 1.85 0.0 3.0]},Chart {annotation = BlankA, xys = [R -5.0e-2 1.9500000000000002 0.0 3.0]}]
bars :: BarOptions -> BarData -> NonEmpty Chart
bars bo bd =
  NonEmpty.zipWith (\o d -> RectChart o d) (fromList (bo ^. #barRectStyles <> repeat defaultRectStyle)) (barRects bo (bd ^. #barData)) <> [BlankChart [Rect (x - (bo ^. #outerGap)) (z + (bo ^. #outerGap)) y w]]
  where
    (Rect x z y w) = foldRectUnsafe $ foldRectUnsafe <$> barRects bo (bd ^. #barData)

maxRows :: NonEmpty (NonEmpty Double) -> Int
maxRows xs = maximum $ length <$> xs

appendZero :: NonEmpty (NonEmpty Double) -> NonEmpty (NonEmpty Double)
appendZero xs =
  (\x -> fromList $ NonEmpty.take (maxRows xs)
    (x <> NonEmpty.repeat 0)) <$> xs

accRows :: NonEmpty (NonEmpty Double) -> NonEmpty (NonEmpty Double)
accRows xs = fmap fromList $ fromList $ transpose $ drop 1 . scanl' (+) 0 <$> transpose (fmap toList $ toList xs)

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

barDataTP :: Bool -> FormatN -> Double -> Double -> NonEmpty (NonEmpty Double) -> NonEmpty (NonEmpty (Text, Double))
barDataTP add fn d negd bs =
  NonEmpty.zipWith (NonEmpty.zipWith (\x y' -> (formatN fn x, drop' y'))) bs' (bool bs' (accRows bs') add)
  where
    drop' x = bool (x - (negd * (w - y))) (x + (d * (w - y))) (x >= 0)
    bs' = appendZero bs
    (Rect _ _ y w) = barRange bs'

-- | Convert BarData to text
barTexts ::
  BarOptions ->
  NonEmpty (NonEmpty Double) ->
  NonEmpty (NonEmpty (Text, Point Double))
barTexts (BarOptions _ _ ogap igap tgap tgapneg _ fn add orient _) bs = NonEmpty.zipWith NonEmpty.zip (fmap fst <$> barDataTP add fn tgap tgapneg bs') (txs'' orient)
  where
    bs' = bool bs (appendZero bs) add
    txs'' Hori = txs'
    txs'' Vert = fmap (\(Point x y) -> Point y x) <$> txs'
    txs' = NonEmpty.zipWith addX [0 ..] (fmap snd <$> barDataTP add fn tgap tgapneg bs')
    addX z y =
      NonEmpty.zipWith
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
barTextCharts :: BarOptions -> BarData -> NonEmpty Chart
barTextCharts bo bd =
  NonEmpty.zipWith TextChart (fromList $ bo ^. #barTextStyles <> repeat defaultTextStyle) (barTexts bo (bd ^. #barData))

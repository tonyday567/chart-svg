{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

-- | Bar charts
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
-- >>> let barDataExample = BarData [[1, 2, 3, 5, 8, 0, -2, 11, 2, 1], [1 .. 10]] (("row " <>) . pack . show <$> [1 .. 11]) (("column " <>) . pack . show <$> [1 .. 2])
-- >>> let barExample = barChart defaultBarOptions barDataExample
--
-- > writeChartOptions "other/bar.svg" barExample
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
    barOrientation :: Orientation,
    barStacked :: Stacked,
    barLegendOptions :: LegendOptions
  }
  deriving (Show, Eq, Generic)

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
      .~ [ (1, axis1),
           (1, axis2)
         ]
    & #legends
      .~ [ (10, o & #content .~ barLegendContent bo bd)
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

-- | Convert BarData to Rects
--
-- >>> barRects defaultBarOptions [[1,2],[2,3]]
-- [[Rect 5.0e-2 0.5 0.0 1.0,Rect 1.05 1.5 0.0 2.0],[Rect 0.5 0.95 0.0 2.0,Rect 1.5 1.95 0.0 3.0]]
--
-- >>> barRects defaultBarOptions [[]]
-- [[]]
barRects ::
  BarOptions ->
  [[Double]] ->
  [[Rect Double]]
barRects (BarOptions _ _ ogap igap _ _ _ _ orient stacked _) bs = rects'' orient
  where
    bs' = appendZero bs
    rects'' Vert = rects'
    rects'' Hori = fmap (\(Rect x z y w) -> Rect y w x z) <$> rects'
    rects' = zipWith batSet (bool [0 ..] (repeat 0) (stacked == Stacked)) (barDataLowerUpper stacked bs')
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
    bstep = case stacked of
      NonStacked -> (1 - ogap + (n - 1) * igap') / n
      Stacked -> 1 - ogap
    igap' = case stacked of
      NonStacked -> igap * (1 - ogap)
      Stacked -> 0

-- | Convert data to a range assuming a zero bound (a very common but implicit assumption in a lot of bar charts)
--
-- >>> barDataLowerUpper NonStacked [[1,2],[2,3]]
-- [[(0.0,1.0),(0.0,2.0)],[(0.0,2.0),(0.0,3.0)]]
barDataLowerUpper :: Stacked -> [[Double]] -> [[(Double, Double)]]
barDataLowerUpper stacked bs =
  case stacked of
    NonStacked -> fmap (fmap (0,)) bs
    Stacked -> drop 1 $ scanl' (\acc xs -> zip (fmap snd acc) xs) (repeat (0, 0)) (accRows bs)

-- | Calculate the Rect range of a bar data set.
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
-- [RectChart (RectStyle {borderSize = 5.0e-3, borderColor = Colour 0.02 0.29 0.48 1.00, color = Colour 0.02 0.29 0.48 0.70}) [Rect 5.0e-2 0.5 0.0 1.0,Rect 1.05 1.5 0.0 2.0],RectChart (RectStyle {borderSize = 5.0e-3, borderColor = Colour 0.66 0.07 0.55 1.00, color = Colour 0.66 0.07 0.55 0.70}) [Rect 0.5 0.95 0.0 2.0,Rect 1.5 1.95 0.0 3.0],BlankChart [Rect -5.0e-2 2.05 0.0 3.0]]
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
  ( \x ->
      take
        (maxRows xs)
        (x <> repeat 0)
  )
    <$> xs

accRows :: [[Double]] -> [[Double]]
accRows xs = transpose $ drop 1 . scanl' (+) 0 <$> transpose (fmap toList $ toList xs)

-- | Sensible ticks for a bar chart.
barTicks :: BarData -> TickStyle
barTicks bd
  | null (bd ^. #barData) = TickNone
  | null (bd ^. #barRowLabels) =
      TickLabels $ pack . show <$> [0 .. (maxRows (bd ^. #barData) - 1)]
  | otherwise =
      TickLabels $
        take (maxRows (bd ^. #barData)) $
          (bd ^. #barRowLabels) <> repeat ""

-- | A bar legend
barLegendContent :: BarOptions -> BarData -> [(Text, Chart)]
barLegendContent bo bd
  | null (bd ^. #barData) = []
  | null (bd ^. #barColumnLabels) = []
  | otherwise =
      zip
        (view #barColumnLabels bd <> repeat "")
        ((\s -> RectChart s [one]) <$> take (length (view #barData bd)) (bo ^. #barRectStyles))

barDataTP :: Stacked -> FormatN -> Double -> Double -> [[Double]] -> [[(Text, Double)]]
barDataTP stacked fn d negd bs =
  zipWith (zipWith (\x y' -> (formatN fn x, drop' y'))) bs' (bool bs' (accRows bs') (stacked == Stacked))
  where
    drop' x = bool (x - (negd * (w - y))) (x + (d * (w - y))) (x >= 0)
    bs' = appendZero bs
    (Rect _ _ y w) = barRange bs'

-- | Convert BarData to text placed above (or below) the bars.
barTexts ::
  BarOptions ->
  [[Double]] ->
  [[(Text, Point Double)]]
barTexts (BarOptions _ _ ogap igap tgap tgapneg _ fn orient stacked _) bs =
  zipWith zip (fmap fst <$> barDataTP stacked fn tgap tgapneg bs') (txs'' orient)
  where
    bs' = bool bs (appendZero bs) (stacked == Stacked)
    txs'' Vert = txs'
    txs'' Hori = fmap (\(Point x y) -> Point y x) <$> txs'
    txs' = zipWith addX [0 ..] (fmap snd <$> barDataTP stacked fn tgap tgapneg bs')
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

-- | Placed text, hold the bars.
barTextCharts :: BarOptions -> BarData -> [Chart]
barTextCharts bo bd =
  zipWith TextChart (bo ^. #barTextStyles <> repeat defaultTextStyle) (barTexts bo (bd ^. #barData))

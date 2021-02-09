{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
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
  )
where

import Chart.Types
import Chart.Render
import Control.Lens
import Data.Colour
import Data.FormatN
import Data.Generics.Labels ()
import qualified Data.List.NonEmpty as NonEmpty
import NumHask.Prelude
import NumHask.Space

-- $setup
--
-- >>> :set -XOverloadedLabels
-- >>> :set -XOverloadedStrings
-- >>> :set -XNoImplicitPrelude
-- >>> import NumHask.Prelude
-- >>> import Chart
-- >>> import Control.Lens

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
    barHudOptions :: HudOptions
  }
  deriving (Show, Eq, Generic)

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
    ( defaultHudOptions
        & #hudAxes
          .~ [ defaultAxisOptions
                 & #axisTick . #ltick .~ Nothing,
               defaultAxisOptions & #place .~ PlaceLeft
             ]
        & #hudTitles .~ []
        & #hudLegend
          ?~ ( defaultLegendOptions
                 & #lplace .~ PlaceRight
                 & #lsize .~ 0.12
                 & #vgap .~ 0.4
                 & #hgap .~ 0.14
                 & #ltext . #size .~ 0.12
                 & #lscale .~ 0.4,
               []
             )
    )
  where
    gs = (\x -> RectStyle 0.002 x x) <$> palette1_
    ts = (\x -> defaultTextStyle & #color .~ x & #size .~ 0.04) <$> palette1_

-- | imagine a dataframe you get in other languages:
--
-- - definitely some [[Double]]
--
-- - maybe some row names
--
-- - maybe some column names
data BarData = BarData
  { barData :: [[Double]],
    barRowLabels :: Maybe [Text],
    barColumnLabels :: Maybe [Text]
  }
  deriving (Show, Eq, Generic)

-- | Convert BarData to rectangles
--
-- >>> barRects defaultBarOptions [[1,2],[2,3]]
-- [[Rect 5.0e-2 0.45 0.0 1.0,Rect 1.05 1.4500000000000002 0.0 2.0],[Rect 0.45 0.8500000000000001 0.0 2.0,Rect 1.4500000000000002 1.85 0.0 3.0]]
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
    False -> fmap (0,) <$> bs
    True -> fmap (0,) <$> accRows bs

-- | calculate the Rect range of a bar data set.
--
-- >>> barRange [[1,2],[2,3]]
-- Rect 0.0 2.0 0.0 3.0
barRange ::
  [[Double]] -> Rect Double
barRange [] = Rect 0 0 0 0
barRange ys'@(y : ys) = Rect 0 (fromIntegral $ maximum (length <$> ys')) (min 0 l) u
  where
    (Range l u) = sconcat $ space1 <$> (y NonEmpty.:| ys)

-- | A bar chart without hud trimmings.
--
-- >>> bars defaultBarOptions (BarData [[1,2],[2,3]] Nothing Nothing)
-- [Chart {annotation = RectA (RectStyle {borderSize = 2.0e-3, borderColor = Colour 0.69 0.35 0.16 1.00, color = Colour 0.69 0.35 0.16 1.00}), xys = [R 5.0e-2 0.45 0.0 1.0,R 1.05 1.4500000000000002 0.0 2.0]},Chart {annotation = RectA (RectStyle {borderSize = 2.0e-3, borderColor = Colour 0.65 0.81 0.89 1.00, color = Colour 0.65 0.81 0.89 1.00}), xys = [R 0.45 0.8500000000000001 0.0 2.0,R 1.4500000000000002 1.85 0.0 3.0]},Chart {annotation = BlankA, xys = [R -5.0e-2 1.9500000000000002 0.0 3.0]}]
bars :: BarOptions -> BarData -> [Chart Double]
bars bo bd =
  zipWith (\o d -> Chart (RectA o) d) (bo ^. #barRectStyles) (fmap RectXY <$> barRects bo (bd ^. #barData)) <> [Chart BlankA [RectXY (Rect (x - (bo ^. #outerGap)) (z + (bo ^. #outerGap)) y w)]]
  where
    (Rect x z y w) = fromMaybe one $ foldRect $ catMaybes $ foldRect <$> barRects bo (bd ^. #barData)

maxRows :: [[Double]] -> Int
maxRows [] = 0
maxRows xs = maximum $ length <$> xs

appendZero :: [[Double]] -> [[Double]]
appendZero xs = (\x -> take (maxRows xs) (x <> repeat 0)) <$> xs

accRows :: [[Double]] -> [[Double]]
accRows xs = transpose $ drop 1 . scanl' (+) 0 <$> transpose xs

-- | sensible ticks
barTicks :: BarData -> TickStyle
barTicks bd
  | null (bd ^. #barData) = TickNone
  | isNothing (bd ^. #barRowLabels) =
    TickLabels $ pack . show <$> [0 .. (maxRows (bd ^. #barData) - 1)]
  | otherwise =
    TickLabels $
      take (maxRows (bd ^. #barData)) $
        fromMaybe [] (bd ^. #barRowLabels) <> repeat ""

tickFirstAxis :: BarData -> [AxisOptions] -> [AxisOptions]
tickFirstAxis _ [] = []
tickFirstAxis bd (x : xs) = (x & #axisTick . #tstyle .~ barTicks bd) : xs

-- | bar legend
barLegend :: BarData -> BarOptions -> [(Annotation, Text)]
barLegend bd bo
  | null (bd ^. #barData) = []
  | isNothing (bd ^. #barColumnLabels) = []
  | otherwise = zip (RectA <$> bo ^. #barRectStyles) $ take (length (bd ^. #barData)) $ fromMaybe [] (bd ^. #barColumnLabels) <> repeat ""

-- | A bar chart.
--
-- By convention only, the first axis (if any) is the bar axis.
barChart :: BarOptions -> BarData -> ChartSvg
barChart bo bd =
  mempty &
  #hudOptions .~ bo ^. #barHudOptions &
  #hudOptions . #hudLegend %~ fmap (second (const (barLegend bd bo))) &
  #hudOptions . #hudAxes %~ tickFirstAxis bd . flipAllAxes (barOrientation bo) &
  #chartList .~ bars bo bd <> bool [] (barTextCharts bo bd) (bo ^. #displayValues)

flipAllAxes :: Orientation -> [AxisOptions] -> [AxisOptions]
flipAllAxes o = fmap (bool id flipAxis (o == Vert))

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
barTexts (BarOptions _ _ ogap igap tgap tgapneg _ fn add orient _) bs = zipWith zip (fmap fst <$> barDataTP add fn tgap tgapneg bs') (txs'' orient)
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
barTextCharts :: BarOptions -> BarData -> [Chart Double]
barTextCharts bo bd =
  zipWith (\o d -> Chart (TextA o (fst <$> d)) (PointXY . snd <$> d)) (bo ^. #barTextStyles) (barTexts bo (bd ^. #barData))

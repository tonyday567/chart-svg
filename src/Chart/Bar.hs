{-# LANGUAGE DataKinds #-}
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
  )
where


import Chart.Hud
import Chart.Types
import Control.Lens
import Data.Generics.Labels ()
import qualified Data.List.NonEmpty as NonEmpty
import NumHask.Prelude
import NumHask.Space
import qualified Prelude as P

-- | the usual bar chart eye-candy
data BarOptions
  = BarOptions
      { barRectStyles :: [RectStyle],
        barTextStyles :: [TextStyle],
        outerGap :: Double,
        innerGap :: Double,
        textGap :: Double,
        textGapNegative :: Double,
        displayValues :: Bool,
        valueFormatN :: FormatN,
        accumulateValues :: Bool,
        orientation :: Orientation,
        barHudOptions :: HudOptions
      }
  deriving (Show, Eq, Generic)

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
    (FormatFixed 0)
    False
    Hori
    ( defaultHudOptions
        & #hudAxes
          .~ [ defaultAxisOptions
                 & #atick . #ltick .~ Nothing,
               defaultAxisOptions & #place .~ PlaceLeft
             ]
        & #hudTitles .~ [defaultTitle "Default Bar Chart"]
        & #hudLegend
          .~ Just
            ( defaultLegendOptions
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
    gs = (\x -> RectStyle 0.002 x x) <$> palette1
    ts = (\x -> defaultTextStyle & #color .~ x & #size .~ 0.04) <$> palette1

-- | imagine a data frame ...
data BarData
  = BarData
      { barData :: [[Double]],
        barRowLabels :: Maybe [Text],
        barColumnLabels :: Maybe [Text]
      }
  deriving (Show, Eq, Generic)

-- | Convert BarData to rectangles
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
            P.abs
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
barDataLowerUpper :: Bool -> [[Double]] -> [[(Double, Double)]]
barDataLowerUpper add bs =
  case add of
    False -> fmap (0,) <$> bs
    True -> fmap (0,) <$> accRows bs

-- | calculate the Rect range of a bar data set.
barRange ::
  [[Double]] -> Rect Double
barRange [] = Rect 0 0 0 0
barRange ys'@(y : ys) = Rect 0 (fromIntegral $ maximum (length <$> ys')) (min 0 l) u
  where
    (Range l u) = sconcat $ space1 <$> (y NonEmpty.:| ys)

-- | A bar chart without hud trimmings.
bars :: BarOptions -> BarData -> [Chart Double]
bars bo bd =
  zipWith (\o d -> Chart (RectA o) d) (bo ^. #barRectStyles) (fmap SpotRect <$> barRects bo (bd ^. #barData)) <> [Chart BlankA [SpotRect (Rect (x - (bo ^. #outerGap)) (z + (bo ^. #outerGap)) y w)]]
  where
    (Rect x z y w) = fromMaybe unitRect $ foldRect $ catMaybes $ foldRect <$> barRects bo (bd ^. #barData)

maxRows :: [[Double]] -> Int
maxRows [] = 0
maxRows xs = maximum $ length <$> xs

appendZero :: [[Double]] -> [[Double]]
appendZero xs = (\x -> take (maxRows xs) (x <> repeat 0)) <$> xs

accRows :: [[Double]] -> [[Double]]
accRows xs = transpose $ drop 1 . scanl' (+) 0 <$> transpose xs

barTicks :: BarData -> TickStyle
barTicks bd
  | bd ^. #barData == [] = TickNone
  | isNothing (bd ^. #barRowLabels) =
    TickLabels $ pack . show <$> [1 .. maxRows (bd ^. #barData)]
  | otherwise =
    TickLabels $ take (maxRows (bd ^. #barData)) $
      fromMaybe [] (bd ^. #barRowLabels) <> repeat ""

flipAllAxes :: Orientation -> [AxisOptions] -> [AxisOptions]
flipAllAxes o = fmap (bool id flipAxis (o == Vert))

tickFirstAxis :: BarData -> [AxisOptions] -> [AxisOptions]
tickFirstAxis _ [] = []
tickFirstAxis bd (x : xs) = (x & #atick . #tstyle .~ barTicks bd) : xs

barLegend :: BarData -> BarOptions -> [(Annotation, Text)]
barLegend bd bo
  | bd ^. #barData == [] = []
  | isNothing (bd ^. #barColumnLabels) = []
  | otherwise = zip (RectA <$> bo ^. #barRectStyles) $ take (length (bd ^. #barData)) $ fromMaybe [] (bd ^. #barColumnLabels) <> repeat ""

-- | A bar chart with hud trimmings.
--
-- By convention only, the first axis (if any) is the bar axis.
barChart :: BarOptions -> BarData -> (HudOptions, [Chart Double])
barChart bo bd =
  ( bo ^. #barHudOptions & #hudLegend %~ fmap (second (const (barLegend bd bo))) & #hudAxes %~ tickFirstAxis bd . flipAllAxes (bo ^. #orientation),
    bars bo bd <> bool [] (barTextCharts bo bd) (bo ^. #displayValues)
  )

-- | convert data to a text and Point
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

barTextCharts :: BarOptions -> BarData -> [Chart Double]
barTextCharts bo bd =
  zipWith (\o d -> Chart (TextA o (fst <$> d)) (SpotPoint . snd <$> d)) (bo ^. #barTextStyles) (barTexts bo (bd ^. #barData))

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

-- | bar charts
module Chart.Bar
  ( BarOptions(..)
  , defaultBarOptions
  , BarData(..)
  , barDataLowerUpper
  , barRange
  , bars
  , barChart
  ) where

import Chart.Core
import Chart.Hud
import Chart.Types
import Control.Lens
import Data.Bifunctor
import Data.Bool
import Data.Generics.Labels ()
import Data.List (transpose, scanl')
import Data.Maybe
import Data.Text (Text, pack)
import GHC.Exts
import GHC.Generics
import NumHask.Space
import Prelude
import qualified Data.List.NonEmpty as NonEmpty
import Data.Semigroup

-- | the usual bar chart eye-candy
data BarOptions = BarOptions
  { barRectStyles :: [RectStyle]
  , barTextStyles :: [TextStyle]
  , outerGap :: Double
  , innerGap :: Double
  , textGap :: Double
  , displayValues :: Bool
  , valueFormatN :: FormatN
  , accumulateValues :: Bool
  , orientation :: Orientation
  , barHudConfig :: HudConfig
  } deriving (Show, Eq, Generic)

defaultBarOptions :: [Text] -> BarOptions
defaultBarOptions legendLabels =
    BarOptions
      gs
      ts
      0.1
      0
      0.04
      True
      (FormatFixed 0)
      False
      Hori
      (defaultHudConfig & #hudAxes .~ [(defaultAxisConfig :: AxisConfig Double) & (#atick . #ltick .~ Nothing :: AxisConfig Double -> AxisConfig Double), defaultAxisConfig & #place .~ PlaceLeft] & #hudTitles .~ [defaultTitle "Default Bar Chart"] & #hudLegend .~ Just (LegendManual (zip (RectA <$> gs) legendLabels), (defaultLegendOptions :: LegendOptions Double) & #lplace .~ PlaceRight & (#lsize .~ 0.12  :: LegendOptions Double -> LegendOptions Double) & #vgap .~ (0.16 :: Double) & #hgap .~ (0.14 :: Double) & #ltext . #size .~ (0.16 :: Double) & #scale .~ 0.33))
  where
    gs = (\x -> RectStyle 0.002 grey 1 x 0.5) <$> d3Palette1
    ts = (\x -> defaultTextStyle & #color .~ x & #size .~ 0.04 & #opacity .~ 0.5) <$> d3Palette1

-- | imagine a data frame ...
data BarData = BarData
  { barData :: [[Double]]
  , barRowLabels :: Maybe [Text]
  , barColumnLabels :: Maybe [Text]
  } deriving (Show, Eq, Generic)

-- | Convert BarData to rectangles
barRects ::
    BarOptions
  -> [[Double]]
  -> [[Rect Double]]
barRects (BarOptions _ _ ogap igap _ _ _ add orient _) bs = rects'' orient
  where
    bs' = bool bs (appendZero bs) add
    rects'' Hori = rects'
    rects'' Vert = fmap (\(Rect x z y w) -> Rect y w x z) <$> rects'
    rects' = zipWith batSet [0 ..] (barDataLowerUpper add bs')
    batSet z ys =
      zipWith
        (\x (yl, yh) ->
           abs
             (Rect
                (x + (ogap/2) + z * bstep)
                (x + (ogap/2) + z * bstep + bstep - igap')
                yl
                yh))
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
barRange ys'@(y:ys) = Rect 0 (fromIntegral $ maximum (length <$> ys')) (min 0 l) u
  where
    (Range l u) = sconcat $ space1 <$> (y NonEmpty.:| ys)

-- | A bar chart without hud trimmings.
--
bars :: BarOptions -> BarData -> [Chart Double]
bars bo bd =
  zipWith (\o d -> Chart (RectA o) d) (bo ^. #barRectStyles) (fmap SpotRect <$> barRects bo (bd ^. #barData)) <> [Chart BlankA [SR (x - (bo ^. #outerGap)) (z + (bo ^. #outerGap)) y w]]
  where
    (Rect x z y w) = fromMaybe unitRect $ foldRect $ catMaybes $ foldRect <$> barRects bo (bd ^. #barData) 

maxRows :: [[Double]] -> Int
maxRows [] = 0
maxRows xs = maximum $ length <$> xs

appendZero :: [[Double]] -> [[Double]]
appendZero xs = (\x -> take (maxRows xs) (x <> repeat 0)) <$> xs

accRows :: [[Double]] -> [[Double]]
accRows xs =  transpose $ drop 1 . scanl' (+) 0 <$> transpose xs

barTicks :: BarData -> TickStyle Double
barTicks bd
  | bd ^. #barData == [] = TickNone
  | isNothing (bd ^. #barRowLabels) =
      TickLabels $ pack . show <$> [1..maxRows (bd ^. #barData)]
  | otherwise = TickLabels $ take (maxRows (bd ^. #barData)) $
    fromMaybe [] (bd ^. #barRowLabels) <> repeat ""

flipAllAxes :: Orientation -> [AxisConfig Double] -> [AxisConfig Double]
flipAllAxes o = fmap (bool id flipAxis (o==Vert))

tickFirstAxis :: BarData -> [AxisConfig Double] -> [AxisConfig Double]
tickFirstAxis _ [] = []
tickFirstAxis bd (x:xs) = (x & #atick . #tstyle .~ barTicks bd):xs

barLegend :: BarData -> BarOptions -> LegendRows
barLegend bd bo
  | bd ^. #barData == [] = LegendManual []
  | isNothing (bd ^. #barColumnLabels) = LegendManual []
  | otherwise = LegendManual $ zip (RectA <$> bo ^. #barRectStyles) $ take (length (bd ^. #barData)) $ fromMaybe [] (bd ^. #barColumnLabels) <> repeat ""

-- | A bar chart with hud trimmings.
--
-- By convetion only, the first axis (if any) is the bar axis.
barChart :: Rect Double -> BarOptions -> BarData -> ChartSvg Double
barChart asp bo bd =
  hud
  (bo ^. #barHudConfig & #hudLegend %~ fmap (first (const (barLegend bd bo))) & #hudAxes %~ tickFirstAxis bd . flipAllAxes (bo ^. #orientation))
  asp
  (bars bo bd <> bool [] (barTextCharts bo bd) (bo ^. #displayValues))

-- | convert data to a text and Point
barDataTP :: Bool -> FormatN -> Double -> [[Double]] -> [[(Text, Double)]]
barDataTP add fn d bs =
  zipWith (zipWith (\x y' -> (formatN fn x, drop' y'))) bs' (bool bs' (accRows bs') add)
  where
    drop' x = bool (x-(d * (w-y))) (x+(d * (w-y))) (x>=0)
    bs' = appendZero bs
    (Rect _ _ y w) = barRange bs'

-- | Convert BarData to text
barTexts ::
    BarOptions
  -> [[Double]]
  -> [[(Text, Point Double)]]
barTexts (BarOptions _ _ ogap igap tgap _ fn add orient _) bs = zipWith zip (fmap fst <$> barDataTP add fn tgap bs') (txs'' orient)
  where
    bs' = bool bs (appendZero bs) add
    txs'' Hori = txs'
    txs'' Vert = fmap (\(Point x y) -> Point y x) <$> txs'
    txs' = zipWith addX [0..] (fmap snd <$> barDataTP add fn tgap bs')
    addX z y =
      zipWith
        (\x y' ->
             Point
                (x + (ogap/2) + z * bstep + bstep/2 - igap'/2)
                y')
        [0..]
        y
    n = fromIntegral (length bs')
    bstep = (1 - (1 + 1) * ogap + (n - 1) * igap') / n
    igap' = igap * (1 - (1 + 1) * ogap)

-- | A bar chart without hud trimmings.
--
barTextCharts :: BarOptions -> BarData -> [Chart Double]
barTextCharts bo bd =
  zipWith (\o d -> Chart (TextA o (fst <$> d)) (SpotPoint . snd <$> d)) (bo ^. #barTextStyles) (barTexts bo (bd ^. #barData))


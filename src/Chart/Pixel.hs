{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

-- | pixel charts
--
-- Opting for a Point or a Rect as concrete data elements that make up an individual chart leaves us with a bit more work to construct a Pixel chart, where colors represent detailed data
module Chart.Pixel
  ( PixelOptions (..),
    defaultPixelOptions,
    pixels,
    pixelate,
    pixelf,
    pixelfl,
    pixelLegendChart,
    PixelLegendOptions (..),
    defaultPixelLegendOptions,
    isHori,
    makePixelTick,
  )
where

import Chart.FormatN
import Chart.Types
import Control.Lens
import Data.Generics.Labels ()
import NumHask.Prelude
import NumHask.Space

data PixelOptions
  = PixelOptions
      { poStyle :: PixelStyle,
        poGrain :: Point Int,
        poRange :: Rect Double
      }
  deriving (Show, Eq, Generic)

defaultPixelOptions :: PixelOptions
defaultPixelOptions =
  PixelOptions defaultPixelStyle (Point 10 10) one

-- | pixel elements
data PixelData
  = PixelData
      { pixelRect :: Rect Double,
        pixelColor :: Colour
      }
  deriving (Show, Eq, Generic)

-- | pixel chart without any hud trimmings
pixels :: RectStyle -> [PixelData] -> [Chart Double]
pixels rs ps =
  ( \(PixelData r c) ->
      Chart
        (RectA (rs & #color .~ c))
        [RectXY r]
  )
    <$> ps

-- | create pixel data from a function on a Point
pixelate ::
  (Point Double -> Double) ->
  Rect Double ->
  Grid (Rect Double) ->
  Colour ->
  Colour ->
  ([PixelData], Range Double)
pixelate f r g c0 c1 = ((\(x, y) -> let c = blend y c0 c1 in PixelData x c) <$> ps', space1 rs)
  where
    ps = gridF f r g
    rs = realToFrac . snd <$> ps
    rs' = project (space1 rs :: Range Double) (Range 0 1) <$> rs
    ps' = zip (fst <$> ps) rs'

-- | create a pixel chart from a function
pixelf :: (Point Double -> Double) -> PixelOptions -> ([Chart Double], Range Double)
pixelf f cfg =
  first (pixels (cfg ^. #poStyle . #pixelRectStyle)) $
    pixelate
      f
      (cfg ^. #poRange)
      (cfg ^. #poGrain)
      (cfg ^. #poStyle . #pixelColorMin)
      (cfg ^. #poStyle . #pixelColorMax)

pixelfl :: (Point Double -> Double) -> PixelOptions -> PixelLegendOptions -> ([Chart Double], [Hud Double])
pixelfl f po plo = (cs, [legendHud (plo ^. #ploLegendOptions) (pixelLegendChart dr plo)])
  where
    (cs, dr) = pixelf f po

data PixelLegendOptions
  = PixelLegendOptions
      {ploStyle :: PixelStyle, ploTitle :: Text, ploWidth :: Double, ploAxisOptions :: AxisOptions, ploLegendOptions :: LegendOptions}
  deriving (Eq, Show, Generic)

pixelAxisOptions :: AxisOptions
pixelAxisOptions =
  AxisOptions
    Nothing
    Nothing
    ( Tick
        (TickRound (FormatPrec (Just 3)) 4 NoTickExtend)
        (Just (defaultGlyphTick & #color .~ black & #shape .~ VLineGlyph 0.005, 0.01))
        (Just (defaultTextTick, 0.03))
        Nothing
    )
    PlaceRight

defaultPixelLegendOptions :: Text -> PixelLegendOptions
defaultPixelLegendOptions t =
  PixelLegendOptions defaultPixelStyle t 0.05 pixelAxisOptions pixelLegendOptions

pixelLegendOptions :: LegendOptions
pixelLegendOptions =
  defaultLegendOptions
    & #lplace .~ PlaceRight
    & #lscale .~ 0.7
    & #lsize .~ 0.5
    & #vgap .~ 0.05
    & #hgap .~ 0.01
    & #innerPad .~ 0.05
    & #outerPad .~ 0.02
    & #ltext . #hsize .~ 0.5

pixelLegendChart :: Range Double -> PixelLegendOptions -> [Chart Double]
pixelLegendChart dataRange l =
  padChart (l ^. #ploLegendOptions . #outerPad)
    . maybe id (\x -> frameChart x (l ^. #ploLegendOptions . #innerPad)) (l ^. #ploLegendOptions . #legendFrame)
    $ hs
  where
    (Range x0 x1) = dataRange
    a = makePixelTick l pchart
    pchart
      | l ^. #ploLegendOptions . #lplace == PlaceBottom
          || l ^. #ploLegendOptions . #lplace == PlaceTop =
        Chart (PixelA (l ^. #ploStyle & #pixelGradient .~ 0)) [R x0 x1 0 (l ^. #ploWidth)]
      | otherwise =
        Chart (PixelA (l ^. #ploStyle & #pixelGradient .~ (pi / 2))) [R 0 (l ^. #ploWidth) x0 x1]
    t = Chart (TextA (l ^. #ploLegendOptions . #ltext & #anchor .~ AnchorStart) [l ^. #ploTitle]) [zero]
    hs = vert (l ^. #ploLegendOptions . #vgap) [a, [t]]

isHori :: PixelLegendOptions -> Bool
isHori l =
  l ^. #ploLegendOptions . #lplace == PlaceBottom
    || l ^. #ploLegendOptions . #lplace == PlaceTop

makePixelTick :: PixelLegendOptions -> Chart Double -> [Chart Double]
makePixelTick l pchart = phud
  where
    r = fromMaybe one (styleBox pchart)
    r' = bool (Rect 0 (l ^. #ploWidth) 0 (l ^. #ploLegendOptions . #lsize)) (Rect 0 (l ^. #ploLegendOptions . #lsize) 0 (l ^. #ploWidth)) (isHori l)
    (hs, _) =
      makeHud
        r
        ( mempty & #hudAxes
            .~ [ l ^. #ploAxisOptions
                   & #place .~ bool PlaceRight PlaceBottom (isHori l)
               ]
        )
    phud = runHudWith r' r hs [pchart]

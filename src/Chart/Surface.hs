{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

-- | Surface chart combinators.
--
-- A common chart is to present a set of rectangles on the XY plane with colour representing values of the underlying data; a surface chart (often called a heatmap).
--
-- 'SurfaceData', the rectangle and the color value, is a different shape to the usual data elements of a chart, so there is a bit more wrangling to do compared with other chart types.
module Chart.Surface
  ( SurfaceData (..),
    SurfaceOptions (..),
    defaultSurfaceOptions,
    SurfaceStyle (..),
    defaultSurfaceStyle,
    mkSurfaceData,
    surfaces,
    surfacef,
    surfacefl,
    SurfaceLegendOptions (..),
    defaultSurfaceLegendOptions,
  )
where

import Chart.Types
import Control.Lens
import Data.Colour
import Data.FormatN
import Data.Generics.Labels ()
import NumHask.Prelude
import NumHask.Space

-- | Options for a Surface chart.
--
-- >>> defaultSurfaceOptions
-- SurfaceOptions {soStyle = SurfaceStyle {surfaceColors = [RGBA 0.65 0.81 0.89 1.00,RGBA 0.12 0.47 0.71 1.00], surfaceRectStyle = RectStyle {borderSize = 0.0, borderColor = RGBA 0.00 0.00 0.00 0.00, color = RGBA 0.00 0.00 0.00 1.00}}, soGrain = Point 10 10, soRange = Rect -0.5 0.5 -0.5 0.5}
data SurfaceOptions
  = SurfaceOptions
      { -- | surface style
        soStyle :: SurfaceStyle,
        -- | The grain or granularity of the chart
        soGrain :: Point Int,
        -- | Chart range
        soRange :: Rect Double
      }
  deriving (Show, Eq, Generic)

-- | official style
defaultSurfaceOptions :: SurfaceOptions
defaultSurfaceOptions =
  SurfaceOptions defaultSurfaceStyle (Point 10 10) one

-- | A surface chart is a specialization of a 'RectA' chart
--
-- >>> defaultSurfaceStyle
-- SurfaceStyle {surfaceColors = [RGBA 0.65 0.81 0.89 1.00,RGBA 0.12 0.47 0.71 1.00], surfaceRectStyle = RectStyle {borderSize = 0.0, borderColor = RGBA 0.00 0.00 0.00 0.00, color = RGBA 0.00 0.00 0.00 1.00}}
--
-- ![surface example](other/surface.svg)
data SurfaceStyle
  = SurfaceStyle
      { -- | list of colours to interpolate between.
        surfaceColors :: [Colour],
        surfaceRectStyle :: RectStyle
      }
  deriving (Show, Eq, Generic)

-- | The official surface style.
defaultSurfaceStyle :: SurfaceStyle
defaultSurfaceStyle =
  SurfaceStyle (take 2 palette1) (blob dark)

-- | Main surface data elements
data SurfaceData
  = SurfaceData
      { -- | XY Coordinates of surface.
        surfaceRect :: Rect Double,
        -- | Surface colour.
        surfaceColor :: Colour
      }
  deriving (Show, Eq, Generic)

-- | surface chart without any hud trimmings
surfaces :: RectStyle -> [SurfaceData] -> [Chart Double]
surfaces rs ps =
  ( \(SurfaceData r c) ->
      Chart
        (RectA (rs & #color .~ c))
        [RectXY r]
  )
    <$> ps

-- | create surface data from a function on a Point
mkSurfaceData ::
  (Point Double -> Double) ->
  Rect Double ->
  Grid (Rect Double) ->
  [Colour] ->
  ([SurfaceData], Range Double)
mkSurfaceData f r g cs = ((\(x, y) -> SurfaceData x (blends y cs)) <$> ps', space1 rs)
  where
    ps = gridF f r g
    rs = snd <$> ps
    rs' = project (space1 rs :: Range Double) (Range 0 1) <$> rs
    ps' = zip (fst <$> ps) rs'

-- | create a surface chart from a function.
surfacef :: (Point Double -> Double) -> SurfaceOptions -> ([Chart Double], Range Double)
surfacef f cfg =
  first (surfaces (cfg ^. #soStyle . #surfaceRectStyle)) $
    mkSurfaceData
      f
      (cfg ^. #soRange)
      (cfg ^. #soGrain)
      (cfg ^. #soStyle . #surfaceColors)

-- | Create a surface chart and accompanying legend from a function.
surfacefl :: (Point Double -> Double) -> SurfaceOptions -> SurfaceLegendOptions -> ([Chart Double], [Hud Double])
surfacefl f po slo = (cs, [legendHud (slo ^. #sloLegendOptions) (surfaceLegendChart dr slo)])
  where
    (cs, dr) = surfacef f po

-- | Legend specialization for a surface chart.
--
-- >>> defaultSurfaceLegendOptions ""
-- SurfaceLegendOptions {sloStyle = SurfaceStyle {surfaceColors = [RGBA 0.65 0.81 0.89 1.00,RGBA 0.12 0.47 0.71 1.00], surfaceRectStyle = RectStyle {borderSize = 0.0, borderColor = RGBA 0.00 0.00 0.00 0.00, color = RGBA 0.00 0.00 0.00 1.00}}, sloTitle = "", sloWidth = 5.0e-2, sloResolution = 100, sloAxisOptions = AxisOptions {axisBar = Nothing, adjust = Nothing, axisTick = Tick {tstyle = TickRound (FormatPrec (Just 3)) 4 NoTickExtend, gtick = Just (GlyphStyle {size = 3.0e-2, color = RGBA 0.00 0.00 0.00 1.00, borderColor = RGBA 0.50 0.50 0.50 1.00, borderSize = 5.0e-3, shape = VLineGlyph 5.0e-3, rotation = Nothing, translate = Nothing},1.0e-2), ttick = Just (TextStyle {size = 5.0e-2, color = RGBA 0.50 0.50 0.50 1.00, anchor = AnchorMiddle, hsize = 0.5, vsize = 1.45, nudge1 = -0.2, rotation = Nothing, translate = Nothing},3.0e-2), ltick = Nothing}, place = PlaceRight}, sloLegendOptions = LegendOptions {lsize = 0.5, vgap = 5.0e-2, hgap = 1.0e-2, ltext = TextStyle {size = 8.0e-2, color = RGBA 0.20 0.20 0.20 1.00, anchor = AnchorMiddle, hsize = 0.5, vsize = 1.45, nudge1 = -0.2, rotation = Nothing, translate = Nothing}, lmax = 10, innerPad = 5.0e-2, outerPad = 2.0e-2, legendFrame = Nothing, lplace = PlaceRight, lscale = 0.7}}
data SurfaceLegendOptions
  = SurfaceLegendOptions
      { sloStyle :: SurfaceStyle,
        sloTitle :: Text,
        -- | Width of the legend glyph
        sloWidth :: Double,
        -- | Resolution of the legend glyph
        sloResolution :: Int,
        sloAxisOptions :: AxisOptions,
        sloLegendOptions :: LegendOptions
      }
  deriving (Eq, Show, Generic)

surfaceAxisOptions :: AxisOptions
surfaceAxisOptions =
  AxisOptions
    Nothing
    Nothing
    ( Tick
        (TickRound (FormatPrec (Just 3)) 4 NoTickExtend)
        (Just (defaultGlyphTick & #color .~ dark & #shape .~ VLineGlyph 0.005, 0.01))
        (Just (defaultTextTick, 0.03))
        Nothing
    )
    PlaceRight

-- | official surface legend options
defaultSurfaceLegendOptions :: Text -> SurfaceLegendOptions
defaultSurfaceLegendOptions t =
  SurfaceLegendOptions defaultSurfaceStyle t 0.05 100 surfaceAxisOptions surfaceLegendOptions

surfaceLegendOptions :: LegendOptions
surfaceLegendOptions =
  defaultLegendOptions
    & #lplace .~ PlaceRight
    & #lscale .~ 0.7
    & #lsize .~ 0.5
    & #vgap .~ 0.05
    & #hgap .~ 0.01
    & #innerPad .~ 0.05
    & #outerPad .~ 0.02
    & #ltext . #hsize .~ 0.5
    & #legendFrame .~ Nothing

-- | Creation of the classical heatmap glyph within a legend context.
--
surfaceLegendChart :: Range Double -> SurfaceLegendOptions -> [Chart Double]
surfaceLegendChart dataRange l =
  padChart (l ^. #sloLegendOptions . #outerPad)
    . maybe id (\x -> frameChart x (l ^. #sloLegendOptions . #innerPad)) (l ^. #sloLegendOptions . #legendFrame)
    $ hs
  where
    a = makeSurfaceTick l pchart
    pchart
      | l ^. #sloLegendOptions . #lplace == PlaceBottom
          || l ^. #sloLegendOptions . #lplace == PlaceTop = vertGlyph
      | otherwise = horiGlyph
    t = Chart (TextA (l ^. #sloLegendOptions . #ltext & #anchor .~ AnchorStart) [l ^. #sloTitle]) [zero]
    hs = vert (l ^. #sloLegendOptions . #vgap) [a, [t]]
    vertGlyph :: [Chart Double]
    vertGlyph =
      zipWith
      (\r c -> Chart (RectA $ blob c) [RectXY r])
      ((\xr -> Ranges xr (Range 0 (l ^. #sloWidth))) <$> gridSpace dataRange
       (l ^. #sloResolution))
      ((\x -> blends x (l ^. #sloStyle . #surfaceColors)) <$>
       grid MidPos (Range 0 1) (l ^. #sloResolution))
    horiGlyph :: [Chart Double]
    horiGlyph =
      zipWith
      (\r c -> Chart (RectA $ blob c) [RectXY r])
      ((\yr -> Ranges (Range 0 (l ^. #sloWidth)) yr) <$> gridSpace dataRange
       (l ^. #sloResolution))
      ((\x -> blends x (l ^. #sloStyle . #surfaceColors)) <$>
       grid MidPos (Range 0 1) (l ^. #sloResolution))

isHori :: SurfaceLegendOptions -> Bool
isHori l =
  l ^. #sloLegendOptions . #lplace == PlaceBottom
    || l ^. #sloLegendOptions . #lplace == PlaceTop

makeSurfaceTick :: SurfaceLegendOptions -> [Chart Double] -> [Chart Double]
makeSurfaceTick l pchart = phud
  where
    r = fromMaybe one (styleBoxes pchart)
    r' = bool (Rect 0 (l ^. #sloWidth) 0 (l ^. #sloLegendOptions . #lsize)) (Rect 0 (l ^. #sloLegendOptions . #lsize) 0 (l ^. #sloWidth)) (isHori l)
    (hs, _) =
      makeHud
        r
        ( mempty & #hudAxes
            .~ [ l ^. #sloAxisOptions
                   & #place .~ bool PlaceRight PlaceBottom (isHori l)
               ]
        )
    phud = runHudWith r' r hs pchart


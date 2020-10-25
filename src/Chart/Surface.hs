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
-- SurfaceOptions {poStyle = SurfaceStyle {surfaceColors = [RGBA 0.65 0.81 0.89 1.00,RGBA 0.12 0.47 0.71 1.00], surfaceRectStyle = RectStyle {borderSize = 0.0, borderColor = RGBA 0.00 0.00 0.00 0.00, color = RGBA 0.00 0.00 0.00 1.00}}, poGrain = Point 10 10, poRange = Rect -0.5 0.5 -0.5 0.5}
data SurfaceOptions
  = SurfaceOptions
      { -- | surface style
        poStyle :: SurfaceStyle,
        -- | The grain or granularity of the chart
        poGrain :: Point Int,
        -- | Chart range
        poRange :: Rect Double
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
  SurfaceStyle (take 2 palette1) (blob black)

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
    rs = realToFrac . snd <$> ps
    rs' = project (space1 rs :: Range Double) (Range 0 1) <$> rs
    ps' = zip (fst <$> ps) rs'

-- | create a surface chart from a function.
surfacef :: (Point Double -> Double) -> SurfaceOptions -> ([Chart Double], Range Double)
surfacef f cfg =
  first (surfaces (cfg ^. #poStyle . #surfaceRectStyle)) $
    mkSurfaceData
      f
      (cfg ^. #poRange)
      (cfg ^. #poGrain)
      (cfg ^. #poStyle . #surfaceColors)

-- | Create a surface chart and accompanying legend from a function.
surfacefl :: (Point Double -> Double) -> SurfaceOptions -> SurfaceLegendOptions -> ([Chart Double], [Hud Double])
surfacefl f po plo = (cs, [legendHud (plo ^. #ploLegendOptions) (surfaceLegendChart dr plo)])
  where
    (cs, dr) = surfacef f po

-- | Legend specialization for a surface chart.
--
-- >>> defaultSurfaceLegendOptions ""
-- SurfaceLegendOptions {ploStyle = SurfaceStyle {surfaceColors = [RGBA 0.65 0.81 0.89 1.00,RGBA 0.12 0.47 0.71 1.00], surfaceRectStyle = RectStyle {borderSize = 0.0, borderColor = RGBA 0.00 0.00 0.00 0.00, color = RGBA 0.00 0.00 0.00 1.00}}, ploTitle = "", ploWidth = 5.0e-2, ploAxisOptions = AxisOptions {abar = Nothing, adjust = Nothing, atick = Tick {tstyle = TickRound (FormatPrec (Just 3)) 4 NoTickExtend, gtick = Just (GlyphStyle {size = 3.0e-2, color = RGBA 0.00 0.00 0.00 1.00, borderColor = RGBA 0.50 0.50 0.50 1.00, borderSize = 5.0e-3, shape = VLineGlyph 5.0e-3, rotation = Nothing, translate = Nothing},1.0e-2), ttick = Just (TextStyle {size = 5.0e-2, color = RGBA 0.50 0.50 0.50 1.00, anchor = AnchorMiddle, hsize = 0.5, vsize = 1.45, nudge1 = -0.2, rotation = Nothing, translate = Nothing},3.0e-2), ltick = Nothing}, place = PlaceRight}, ploLegendOptions = LegendOptions {lsize = 0.5, vgap = 5.0e-2, hgap = 1.0e-2, ltext = TextStyle {size = 8.0e-2, color = RGBA 0.20 0.20 0.20 1.00, anchor = AnchorMiddle, hsize = 0.5, vsize = 1.45, nudge1 = -0.2, rotation = Nothing, translate = Nothing}, lmax = 10, innerPad = 5.0e-2, outerPad = 2.0e-2, legendFrame = Just (RectStyle {borderSize = 2.0e-2, borderColor = RGBA 0.50 0.50 0.50 1.00, color = RGBA 1.00 1.00 1.00 1.00}), lplace = PlaceRight, lscale = 0.7}}
data SurfaceLegendOptions
  = SurfaceLegendOptions
      {ploStyle :: SurfaceStyle, ploTitle :: Text, ploWidth :: Double, ploAxisOptions :: AxisOptions, ploLegendOptions :: LegendOptions}
  deriving (Eq, Show, Generic)

surfaceAxisOptions :: AxisOptions
surfaceAxisOptions =
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

-- | official surface legend options
defaultSurfaceLegendOptions :: Text -> SurfaceLegendOptions
defaultSurfaceLegendOptions t =
  SurfaceLegendOptions defaultSurfaceStyle t 0.05 surfaceAxisOptions surfaceLegendOptions

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

-- | FIXME:
--
-- Input how many little Rects to include
-- Work out orientation of little Rects
surfaceLegendChart :: Range Double -> SurfaceLegendOptions -> [Chart Double]
surfaceLegendChart dataRange l =
  padChart (l ^. #ploLegendOptions . #outerPad)
    . maybe id (\x -> frameChart x (l ^. #ploLegendOptions . #innerPad)) (l ^. #ploLegendOptions . #legendFrame)
    $ hs
  where
    (Range x0 x1) = dataRange
    a = makeSurfaceTick l pchart
    pchart
      | l ^. #ploLegendOptions . #lplace == PlaceBottom
          || l ^. #ploLegendOptions . #lplace == PlaceTop =
        Chart (RectA defaultRectStyle) [R x0 x1 0 (l ^. #ploWidth)]
      | otherwise =
        Chart (RectA defaultRectStyle) [R 0 (l ^. #ploWidth) x0 x1]
    t = Chart (TextA (l ^. #ploLegendOptions . #ltext & #anchor .~ AnchorStart) [l ^. #ploTitle]) [zero]
    hs = vert (l ^. #ploLegendOptions . #vgap) [a, [t]]

isHori :: SurfaceLegendOptions -> Bool
isHori l =
  l ^. #ploLegendOptions . #lplace == PlaceBottom
    || l ^. #ploLegendOptions . #lplace == PlaceTop

makeSurfaceTick :: SurfaceLegendOptions -> Chart Double -> [Chart Double]
makeSurfaceTick l pchart = phud
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

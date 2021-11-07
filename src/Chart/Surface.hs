{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
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
    surfaceAxisOptions,
  )
where

import Chart.Primitive
import Chart.Style
import Chart.Hud
import Optics.Core
import Data.Bifunctor
import Data.Colour
import Data.FormatN
import Data.Text (Text)
import GHC.Generics
import Prelude
import Data.List.NonEmpty (NonEmpty(..))
import Chart.Data
import Data.Bool
import Data.Foldable
import Data.Tree

-- | Options for a Surface chart.
data SurfaceOptions = SurfaceOptions
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

-- | A surface chart is a specialization of a 'RectChart'
--
-- >>> defaultSurfaceStyle
-- SurfaceStyle {surfaceColors = [Colour 0.69 0.35 0.16 1.00,Colour 0.65 0.81 0.89 1.00], surfaceRectStyle = RectStyle {borderSize = 0.0, borderColor = Colour 0.00 0.00 0.00 0.00, color = Colour 0.05 0.05 0.05 1.00}}
--
-- ![surface example](other/surface.svg)
data SurfaceStyle = SurfaceStyle
  { -- | list of colours to interpolate between.
    surfaceColors :: NonEmpty Colour,
    surfaceRectStyle :: RectStyle
  }
  deriving (Show, Eq, Generic)

-- | The official surface style.
defaultSurfaceStyle :: SurfaceStyle
defaultSurfaceStyle =
  SurfaceStyle (palette1 <$> [0..1]) (blob dark)

-- | Main surface data elements
data SurfaceData = SurfaceData
  { -- | XY Coordinates of surface.
    surfaceRect :: Rect Double,
    -- | Surface colour.
    surfaceColor :: Colour
  }
  deriving (Show, Eq, Generic)

-- | surface chart without any hud trimmings
surfaces :: RectStyle -> [SurfaceData] -> [Chart]
surfaces rs ps =
  ( \(SurfaceData r c) ->
      RectChart
        (rs & #color .~ c)
        [r]
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
surfacef :: (Point Double -> Double) -> SurfaceOptions -> ([Chart], Range Double)
surfacef f cfg =
  first (surfaces (cfg ^. #soStyle % #surfaceRectStyle)) $
    mkSurfaceData
      f
      (cfg ^. #soRange)
      (cfg ^. #soGrain)
      (toList $ cfg ^. #soStyle % #surfaceColors)

-- | Create a surface chart and accompanying legend from a function.
surfacefl :: (Point Double -> Double) -> SurfaceOptions -> SurfaceLegendOptions -> ([Chart], [Hud])
surfacefl f po slo =
  (cs,
   [Hud 10 (legendHud (slo ^. #sloLegendOptions) (surfaceLegendChart dr slo))])
  where
    (cs, dr) = surfacef f po

-- | Legend specialization for a surface chart.
data SurfaceLegendOptions = SurfaceLegendOptions
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

surfaceAxisOptions :: Colour -> AxisOptions
surfaceAxisOptions c =
  AxisOptions
    Nothing
    Nothing
    ( Ticks
        (TickRound (FormatPrec (Just 3)) 4 NoTickExtend)
        (Just (defaultGlyphTick & #borderColor .~ c & #color .~ c & #shape .~ VLineGlyph, 0.01))
        (Just (defaultTextTick & #color .~ c, 0.03))
        Nothing
    )
    PlaceRight

-- | official surface legend options
defaultSurfaceLegendOptions :: Colour -> Text -> SurfaceLegendOptions
defaultSurfaceLegendOptions c t =
  SurfaceLegendOptions defaultSurfaceStyle t 0.05 100 (surfaceAxisOptions c) surfaceLegendOptions

surfaceLegendOptions :: LegendOptions
surfaceLegendOptions =
  defaultLegendOptions
    & #place .~ PlaceRight
    & #overallScale .~ 0.7
    & #size .~ 0.5
    & #vgap .~ 0.05
    & #hgap .~ 0.01
    & #innerPad .~ 0.05
    & #outerPad .~ 0.02
    & #style % #hsize .~ 0.5
    & #frame .~ Nothing

-- | Creation of the classical heatmap glyph within a legend context.
surfaceLegendChart :: Range Double -> SurfaceLegendOptions -> Tree ChartNode
surfaceLegendChart dataRange l =
  legendFrame (view #sloLegendOptions l) hs
  where
    a = makeSurfaceTick l (toTree (Just "pchart") pchart)
    pchart
      | l ^. #sloLegendOptions % #place == PlaceBottom
          || l ^. #sloLegendOptions % #place == PlaceTop =
        vertGlyph
      | otherwise = horiGlyph
    t = TextChart (l ^. #sloLegendOptions % #style & #anchor .~ AnchorStart) [(l ^. #sloTitle, zero)]
    hs = vert (l ^. #sloLegendOptions % #vgap) [a, toTree Nothing [t]]
    vertGlyph :: [Chart]
    vertGlyph =
      zipWith
        (\r c -> RectChart (blob c) [r])
        ( (\xr -> Ranges xr (Range 0 (l ^. #sloWidth)))
            <$> gridSpace
              dataRange
              (l ^. #sloResolution)
        )
        ( (\x -> blends x (toList $ l ^. #sloStyle % #surfaceColors))
            <$> grid MidPos (Range 0 1) (l ^. #sloResolution)
        )
    horiGlyph :: [Chart]
    horiGlyph =
      zipWith
        (\r c -> RectChart (blob c) [r])
        ( (\yr -> Ranges (Range 0 (l ^. #sloWidth)) yr)
            <$> gridSpace
              dataRange
              (l ^. #sloResolution)
        )
        ( (\x -> blends x (toList $ l ^. #sloStyle % #surfaceColors))
            <$> grid MidPos (Range 0 1) (l ^. #sloResolution)
        )

isHori :: SurfaceLegendOptions -> Bool
isHori l =
  l ^. #sloLegendOptions % #place == PlaceBottom
    || l ^. #sloLegendOptions % #place == PlaceTop

makeSurfaceTick :: SurfaceLegendOptions -> Tree ChartNode -> Tree ChartNode
makeSurfaceTick l pchart = phud
  where
    r = styleBoxes (view charts' pchart)
    r' = bool (Rect 0 (l ^. #sloWidth) 0 (l ^. #sloLegendOptions % #size)) (Rect 0 (l ^. #sloLegendOptions % #size) 0 (l ^. #sloWidth)) (isHori l)
    (hs, db) = toHuds (mempty & set #axes [(9, l ^. #sloAxisOptions & #place .~ bool PlaceRight PlaceBottom (isHori l))]) r
    phud = runHudWith r' db hs pchart

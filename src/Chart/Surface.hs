{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Surface chart combinators.
--
-- A common chart is to present a set of rectangles on the XY plane with colour representing values of the underlying data. This library uses the term /surface/ chart but it is often referred to as a heatmap.
module Chart.Surface
  ( SurfaceData (..),
    SurfaceOptions (..),
    defaultSurfaceOptions,
    SurfaceStyle (..),
    defaultSurfaceStyle,
    mkSurfaceData,
    surfaces,
    surfacef,
    SurfaceLegendOptions (..),
    defaultSurfaceLegendOptions,
    surfaceLegendAxisOptions,
    gridReferenceChart,
    addSurfaceLegend,
  )
where

import Chart.Data
import Chart.Hud
import Chart.Primitive
import Chart.Style
import Data.Bifunctor
import Data.Bool
import Data.Colour
import Data.Foldable
import Data.FormatN
import Data.Maybe
import GHC.Generics
import Optics.Core
import Prelude

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
-- SurfaceStyle {surfaceColors = [Colour 0.02 0.73 0.80 1.00,Colour 0.02 0.29 0.48 1.00], surfaceRectStyle = Style {size = 6.0e-2, borderSize = 0.0, color = Colour 0.05 0.05 0.05 1.00, borderColor = Colour 0.00 0.00 0.00 0.00, scaleP = NoScaleP, anchor = AnchorMiddle, rotation = Nothing, translate = Nothing, escapeText = EscapeText, frame = Nothing, lineCap = Nothing, lineJoin = Nothing, dasharray = Nothing, dashoffset = Nothing, hsize = 0.6, vsize = 1.1, vshift = -0.25, glyphShape = SquareGlyph}}
--
-- ![surface example](other/surface.svg)
data SurfaceStyle = SurfaceStyle
  { -- | list of colours to interpolate between.
    surfaceColors :: [Colour],
    surfaceRectStyle :: Style
  }
  deriving (Show, Eq, Generic)

-- | The official surface style.
defaultSurfaceStyle :: SurfaceStyle
defaultSurfaceStyle =
  SurfaceStyle (palette <$> [0 .. 1]) (blob dark)

-- | Main surface data elements
data SurfaceData = SurfaceData
  { -- | XY Coordinates of surface.
    surfaceRect :: Rect Double,
    -- | Surface colour.
    surfaceColor :: Colour
  }
  deriving (Show, Eq, Generic)

-- | surface chart without any hud trimmings
surfaces :: Style -> [SurfaceData] -> [Chart]
surfaces rs ps =
  ( \(SurfaceData r c) ->
      Chart
        (rs & set #color c)
        (RectData [r])
  )
    <$> ps

-- | Create surface data from a function on a Point
mkSurfaceData ::
  (Point Double -> Double) ->
  Rect Double ->
  Grid (Rect Double) ->
  [Colour] ->
  ([SurfaceData], Range Double)
mkSurfaceData f r g cs = (zipWith SurfaceData rects (flip mixes cs <$> proj), rx)
  where
    ps = gridF f r g
    rects = fst <$> ps
    vs = snd <$> ps
    rx = unsafeSpace1 vs :: Range Double
    proj = project rx (Range 0 1) <$> vs

-- | Create a surface chart from a function.
surfacef :: (Point Double -> Double) -> SurfaceOptions -> ([Chart], Range Double)
surfacef f cfg =
  first (surfaces (view (#soStyle % #surfaceRectStyle) cfg)) $
    mkSurfaceData
      f
      (view #soRange cfg)
      (view #soGrain cfg)
      (toList $ view (#soStyle % #surfaceColors) cfg)

-- | Legend specialization for a surface chart.
data SurfaceLegendOptions = SurfaceLegendOptions
  { sloAxisOptions :: AxisOptions,
    -- | Width of the legend glyph
    sloWidth :: Double,
    -- | Resolution of the legend glyph
    sloResolution :: Int,
    sloDataRange :: Range Double,
    -- | Placement of the legend versus normalised chart placement
    sloRect :: Rect Double,
    sloSurfaceStyle :: SurfaceStyle
  }
  deriving (Eq, Show, Generic)

-- | 'AxisOptions' for a surface chart legend.
surfaceLegendAxisOptions :: AxisOptions
surfaceLegendAxisOptions =
  AxisOptions
    Nothing
    Nothing
    ( Ticks
        (TickRound (FormatN FSPrec (Just 3) 4 True True) 4 NoTickExtend)
        (Just defaultGlyphTickStyleY)
        (Just (defaultTextTick & set #buffer 0.05))
        Nothing
    )
    PlaceRight

-- | official surface legend options
defaultSurfaceLegendOptions :: SurfaceLegendOptions
defaultSurfaceLegendOptions =
  SurfaceLegendOptions surfaceLegendAxisOptions 0.2 100 one (Rect 0.7 0.9 0 0.5) defaultSurfaceStyle

gridReferenceChart :: SurfaceLegendOptions -> ChartTree
gridReferenceChart slo =
  named "grid reference" $
    zipWith
      (\r c -> Chart (blob c) (RectData [r]))
      (gridf <$> spaceGrid)
      colorGrid
  where
    spaceGrid = gridSpace (view #sloDataRange slo) (view #sloResolution slo)
    gridf =
      bool
        (\yr -> Ranges (Range 0 (view #sloWidth slo)) yr)
        (\xr -> Ranges xr (Range 0 (view #sloWidth slo)))
        (isHori slo)
    colorGrid =
      (\x -> mixes x (toList $ view (#sloSurfaceStyle % #surfaceColors) slo))
        <$> grid MidPos (Range 0 1) (view #sloResolution slo)

isHori :: SurfaceLegendOptions -> Bool
isHori slo =
  view (#sloAxisOptions % #place) slo == PlaceBottom
    || view (#sloAxisOptions % #place) slo == PlaceTop

addSurfaceLegend :: SurfaceLegendOptions -> ChartTree -> ChartTree
addSurfaceLegend slo ct = ctBoth
  where
    grc = gridReferenceChart slo
    hoLegend = (mempty :: HudOptions) & set #axes [Priority 1 (view #sloAxisOptions slo)]
    grcLegend = addHud (FixedAspect (view #sloWidth slo)) hoLegend grc
    ctbox = fromMaybe one (view styleBox' ct)
    legbox = projectOnR ctbox one (view #sloRect slo)
    ctBoth = mconcat [projectChartTree legbox grcLegend, ct]

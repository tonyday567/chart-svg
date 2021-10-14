{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Stylistic elements
module Chart.Style
  ( -- * Styles
    RectStyle (..),
    defaultRectStyle,
    blob,
    clear,
    border,
    TextStyle (..),
    defaultTextStyle,
    GlyphStyle (..),
    defaultGlyphStyle,
    GlyphShape (..),
    glyphText,
    LineStyle (..),
    defaultLineStyle,
    LineCap (..),
    fromLineCap,
    toLineCap,
    LineJoin (..),
    fromLineJoin,
    toLineJoin,
    fromDashArray,
    Anchor (..),
    fromAnchor,
    toAnchor,
    PathStyle (..),
    -- toPathChart,
    defaultPathStyle,

    ChartAspect (..),
    toChartAspect,
    fromChartAspect,
    Orientation (..),
    fromOrientation,
    toOrientation,

    -- * SVG primitives
    CssShapeRendering (..),
    CssPreferColorScheme (..),
    CssOptions (..),
    defaultCssOptions,
    SvgOptions (..),
    defaultSvgOptions,
    defaultSvgFrame,

  )
where

import Control.Lens
import Control.Monad.State.Lazy
import Data.Bifunctor
import Data.Bool
import Data.Colour
import Data.Foldable hiding (sum)
import Data.FormatN
import Data.Generics.Labels ()
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Data.Path
import Data.String
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Data.Time
import GHC.Generics
import GHC.OverloadedLabels
import NumHask.Prelude
import NumHask.Space as NH hiding (Element, singleton)
import Text.HTML.TagSoup hiding (Attribute)

-- $setup
--
-- >>> :set -XOverloadedLabels
-- >>> import Control.Lens
-- >>> import Chart
-- >>> import Chart.Render
-- >>> import Data.Colour

-- | Rectangle styling
--
-- >>> defaultRectStyle
-- RectStyle {borderSize = 1.0e-2, borderColor = Colour 0.65 0.81 0.89 1.00, color = Colour 0.12 0.47 0.71 1.00}
--
-- ![unit example](other/unit.svg)
data RectStyle = RectStyle
  { borderSize :: Double,
    borderColor :: Colour,
    color :: Colour
  }
  deriving (Show, Eq, Generic)

-- | the style
defaultRectStyle :: RectStyle
defaultRectStyle = RectStyle 0.01 (palette1 1) (palette1 2)

-- | solid rectangle, no border
--
-- >>> blob black
-- RectStyle {borderSize = 0.0, borderColor = Colour 0.00 0.00 0.00 0.00, color = Colour 0.00 0.00 0.00 1.00}
blob :: Colour -> RectStyle
blob = RectStyle 0 transparent

-- | transparent rect
--
-- >>> clear
-- RectStyle {borderSize = 0.0, borderColor = Colour 0.00 0.00 0.00 0.00, color = Colour 0.00 0.00 0.00 0.00}
clear :: RectStyle
clear = RectStyle 0 transparent transparent

-- | transparent rectangle, with border
--
-- >>> border 0.01 transparent
-- RectStyle {borderSize = 1.0e-2, borderColor = Colour 0.00 0.00 0.00 0.00, color = Colour 0.00 0.00 0.00 0.00}
border :: Double -> Colour -> RectStyle
border s c = RectStyle s c transparent

-- | Text styling
--
-- >>> defaultTextStyle
-- TextStyle {size = 8.0e-2, color = Colour 0.05 0.05 0.05 1.00, anchor = AnchorMiddle, hsize = 0.5, vsize = 1.45, nudge1 = -0.2, rotation = Nothing}
--
-- >>> import qualified Data.Text as Text
-- >>> let t = zipWith (\x y -> Chart (TextA (defaultTextStyle & (#size .~ (0.05 :: Double))) [x]) [PointXY y]) (fmap Text.singleton ['a' .. 'y']) [Point (sin (x * 0.1)) x | x <- [0 .. 25]]
--
-- ![text example](other/text.svg)
data TextStyle = TextStyle
  { size :: Double,
    color :: Colour,
    anchor :: Anchor,
    hsize :: Double,
    vsize :: Double,
    nudge1 :: Double,
    rotation :: Maybe Double
  }
  deriving (Show, Eq, Generic)

-- | position anchor
data Anchor = AnchorMiddle | AnchorStart | AnchorEnd deriving (Eq, Show, Generic)

-- | text
fromAnchor :: (IsString s) => Anchor -> s
fromAnchor AnchorMiddle = "Middle"
fromAnchor AnchorStart = "Start"
fromAnchor AnchorEnd = "End"

-- | from text
toAnchor :: (Eq s, IsString s) => s -> Anchor
toAnchor "Middle" = AnchorMiddle
toAnchor "Start" = AnchorStart
toAnchor "End" = AnchorEnd
toAnchor _ = AnchorMiddle

-- | the offical text style
defaultTextStyle :: TextStyle
defaultTextStyle =
  TextStyle 0.08 dark AnchorMiddle 0.5 1.45 -0.2 Nothing

-- | Glyph styling
--
-- >>> defaultGlyphStyle
-- GlyphStyle {size = 3.0e-2, color = Colour 0.65 0.81 0.89 1.00, borderColor = Colour 0.12 0.47 0.71 1.00, borderSize = 3.0e-3, shape = SquareGlyph, rotation = Nothing, translate = Nothing}
--
-- ![glyph example](other/glyphs.svg)
data GlyphStyle = GlyphStyle
  { -- | glyph radius
    size :: Double,
    -- | fill color
    color :: Colour,
    -- | stroke color
    borderColor :: Colour,
    -- | stroke width (adds a bit to the bounding box)
    borderSize :: Double,
    shape :: GlyphShape,
    rotation :: Maybe Double,
    translate :: Maybe (Point Double)
  }
  deriving (Show, Eq, Generic)

-- | the offical glyph style
defaultGlyphStyle :: GlyphStyle
defaultGlyphStyle =
  GlyphStyle
    0.03
    (palette1 1)
    (palette1 2)
    0.003
    SquareGlyph
    Nothing
    Nothing

-- | glyph shapes
data GlyphShape
  = CircleGlyph
  | SquareGlyph
  | EllipseGlyph Double
  | RectSharpGlyph Double
  | RectRoundedGlyph Double Double Double
  | TriangleGlyph (Point Double) (Point Double) (Point Double)
  | VLineGlyph Double
  | HLineGlyph Double
  | PathGlyph Text
  deriving (Show, Eq, Generic)

-- | textifier
glyphText :: GlyphShape -> Text
glyphText sh =
  case sh of
    CircleGlyph -> "Circle"
    SquareGlyph -> "Square"
    TriangleGlyph {} -> "Triangle"
    EllipseGlyph _ -> "Ellipse"
    RectSharpGlyph _ -> "RectSharp"
    RectRoundedGlyph {} -> "RectRounded"
    VLineGlyph _ -> "VLine"
    HLineGlyph _ -> "HLine"
    PathGlyph _ -> "Path"

-- | line cap style
data LineCap = LineCapButt | LineCapRound | LineCapSquare deriving (Eq, Show, Generic)

-- | textifier
fromLineCap :: (IsString s) => LineCap -> s
fromLineCap LineCapButt = "butt"
fromLineCap LineCapRound = "round"
fromLineCap LineCapSquare = "square"

-- | readifier
toLineCap :: (Eq s, IsString s) => s -> LineCap
toLineCap "butt" = LineCapButt
toLineCap "round" = LineCapRound
toLineCap "square" = LineCapSquare
toLineCap _ = LineCapButt

-- | line cap style
data LineJoin = LineJoinMiter | LineJoinBevel | LineJoinRound deriving (Eq, Show, Generic)

-- | textifier
fromLineJoin :: (IsString s) => LineJoin -> s
fromLineJoin LineJoinMiter = "miter"
fromLineJoin LineJoinBevel = "bevel"
fromLineJoin LineJoinRound = "round"

-- | readifier
toLineJoin :: (Eq s, IsString s) => s -> LineJoin
toLineJoin "miter" = LineJoinMiter
toLineJoin "bevel" = LineJoinBevel
toLineJoin "round" = LineJoinRound
toLineJoin _ = LineJoinMiter

-- | Convert a dash representation from a list to text
fromDashArray :: [Double] -> Text
fromDashArray xs = Text.intercalate " " $ pack . show <$> xs

-- | line style
--
-- >>> defaultLineStyle
-- LineStyle {width = 1.2e-2, color = Colour 0.05 0.05 0.05 1.00, linecap = Nothing, linejoin = Nothing, dasharray = Nothing, dashoffset = Nothing}
--
-- ![line example](other/line.svg)
--
-- See also <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute>
data LineStyle = LineStyle
  { width :: Double,
    color :: Colour,
    linecap :: Maybe LineCap,
    linejoin :: Maybe LineJoin,
    dasharray :: Maybe [Double],
    dashoffset :: Maybe Double
  }
  deriving (Show, Eq, Generic)

-- | the official default line style
defaultLineStyle :: LineStyle
defaultLineStyle = LineStyle 0.012 dark Nothing Nothing Nothing Nothing

-- | Path styling
--
-- >>> defaultPathStyle
-- PathStyle {borderSize = 1.0e-2, borderColor = Colour 0.65 0.81 0.89 1.00, color = Colour 0.12 0.47 0.71 1.00}
data PathStyle = PathStyle
  { borderSize :: Double,
    borderColor :: Colour,
    color :: Colour
  }
  deriving (Show, Eq, Generic)

-- | the style
defaultPathStyle :: PathStyle
defaultPathStyle =
  PathStyle 0.01 (palette1 1) (palette1 2)

{-
-- | Convert from a path command list to a PathA chart
toPathChart :: PathStyle -> [(PathInfo Double, Point Double)] -> Chart Double
toPathChart ps xs = Chart (PathA ps (fst <$> xs)) (singleton (PointXY . snd <$> xs))

-}

-- | Verticle or Horizontal
data Orientation = Vert | Hori deriving (Eq, Show, Generic)

-- | textifier
fromOrientation :: (IsString s) => Orientation -> s
fromOrientation Hori = "Hori"
fromOrientation Vert = "Vert"

-- | readifier
toOrientation :: (Eq s, IsString s) => s -> Orientation
toOrientation "Hori" = Hori
toOrientation "Vert" = Vert
toOrientation _ = Hori

data CssShapeRendering = UseGeometricPrecision | UseCssCrisp | NoShapeRendering deriving (Show, Eq, Generic)

data CssPreferColorScheme = PreferDark | PreferLight | PreferNormal deriving (Show, Eq, Generic)

-- | css options
-- >>> defaultCssOptions
data CssOptions = CssOptions { shapeRendering :: CssShapeRendering, preferColorScheme :: CssPreferColorScheme} deriving (Show, Eq, Generic)

-- | No special shape rendering and no reponse to OS color scheme preferences.
defaultCssOptions :: CssOptions
defaultCssOptions = CssOptions NoShapeRendering PreferLight

-- | The basis for the x-y ratio of the final chart
--
-- Default style features tend towards assuming that the usual height of the overall svg image is around 1, and ChartAspect is based on this assumption, so that a ChartAspect of "FixedAspect 1.5", say, means a height of 1 and a width of 1.5.
data ChartAspect
  = -- | Rescale charts to a fixed x-y ratio, inclusive of hud and style features
    FixedAspect Double
  | -- | Rescale charts to an overall height of 1, preserving the x-y ratio of the data canvas.
    CanvasAspect Double
  | -- | Rescale charts to a height of 1, preserving the existing x-y ratio of the underlying charts, inclusive of hud and style.
    ChartAspect
  | -- | Do not rescale.
    UnadjustedAspect
  deriving (Show, Eq, Generic)

-- | textifier
fromChartAspect :: (IsString s) => ChartAspect -> s
fromChartAspect (FixedAspect _) = "FixedAspect"
fromChartAspect (CanvasAspect _) = "CanvasAspect"
fromChartAspect ChartAspect = "ChartAspect"
fromChartAspect UnadjustedAspect = "UnadjustedAspect"

-- | readifier
toChartAspect :: (Eq s, IsString s) => s -> Double -> ChartAspect
toChartAspect "FixedAspect" a = FixedAspect a
toChartAspect "CanvasAspect" a = CanvasAspect a
toChartAspect "ChartAspect" _ = ChartAspect
toChartAspect "UnadjustedAspect" _ = UnadjustedAspect
toChartAspect _ _ = ChartAspect

-- | SVG tag options.
--
-- >>> defaultSvgOptions
-- SvgOptions {svgHeight = 300.0, outerPad = Just 2.0e-2, innerPad = Nothing, chartFrame = Nothing, cssOptions = defaultCssOptions, chartAspect = FixedAspect 1.5, background = Nothing}
--
--
-- ![svgoptions example](other/svgoptions.svg)
data SvgOptions = SvgOptions
  { svgHeight :: Double,
    outerPad :: Maybe Double,
    innerPad :: Maybe Double,
    chartFrame :: Maybe RectStyle,
    cssOptions :: CssOptions,
    chartAspect :: ChartAspect,
    background :: Maybe Colour
  }
  deriving (Eq, Show, Generic)

-- | The official svg options
defaultSvgOptions :: SvgOptions
defaultSvgOptions = SvgOptions 300 (Just 0.02) Nothing Nothing defaultCssOptions (FixedAspect 1.5) Nothing

-- | frame style
defaultSvgFrame :: RectStyle
defaultSvgFrame = border 0.01 dark

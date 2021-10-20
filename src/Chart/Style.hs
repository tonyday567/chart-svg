{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Stylistic elements
module Chart.Style
  ( -- * Styles
    Styles(..),
    scaleStyle_,
    scaleOpacStyle_,
    colourStyle_,

    -- * Specific Styles
    RectStyle (..),
    defaultRectStyle,
    blob,
    clear,
    border,
    TextStyle (..),
    defaultTextStyle,
    styleBoxText,
    GlyphStyle (..),
    defaultGlyphStyle,
    styleBoxGlyph,
    ScaleBorder(..),
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
    defaultPathStyle,

    -- * SVG Options
    SvgOptions (..),
    defaultSvgOptions,
    defaultSvgFrame,
    ChartAspect (..),
    toChartAspect,
    fromChartAspect,
    Orientation (..),
    fromOrientation,
    toOrientation,

    -- * SVG Style primitives
    CssOptions (..),
    defaultCssOptions,
    CssShapeRendering (..),
    CssPreferColorScheme (..),
  )
where

import Data.Colour
import Data.Maybe
import Data.String
import Data.Text (Text, pack)
import qualified Data.Text as Text
import GHC.Generics
import Prelude
import Control.Lens
import Text.HTML.TagSoup (maybeTagText, parseTags)
import Data.Path
import Chart.Data as CD

-- $setup
--
-- >>> :set -XOverloadedLabels
-- >>> import Control.Lens
-- >>> import Chart
-- >>> import Chart.Render
-- >>> import Data.Colour

data Styles
  = RectA RectStyle
  | TextA TextStyle
  | GlyphA GlyphStyle
  | LineA LineStyle
  | PathA PathStyle
  deriving (Eq, Show, Generic)

-- | Generically scale an Annotation.
scaleStyle_ :: Double -> Styles -> Styles
scaleStyle_ x (LineA a) = LineA $ a & #width %~ (* x)
scaleStyle_ x (RectA a) = RectA $ a & #borderSize %~ (* x)
scaleStyle_ x (TextA a) = TextA (a & #size %~ (* x))
scaleStyle_ x (GlyphA a) = GlyphA (a & #size %~ (* x))
scaleStyle_ x (PathA a) = PathA (a & #borderSize %~ (* x))

-- | dim (or brighten) the opacity of an Annotation by a scale
scaleOpacStyle_ :: Double -> Styles -> Styles
scaleOpacStyle_ x (RectA s) = RectA s'
  where
    s' = s & #color %~ scaleOpac x & #borderColor %~ scaleOpac x
scaleOpacStyle_ x (TextA s) = TextA s'
  where
    s' = s & #color %~ scaleOpac x
scaleOpacStyle_ x (LineA s) = LineA s'
  where
    s' = s & #color %~ scaleOpac x
scaleOpacStyle_ x (GlyphA s) = GlyphA s'
  where
    s' = s & #color %~ scaleOpac x & #borderColor %~ scaleOpac x
scaleOpacStyle_ x (PathA s) = PathA s'
  where
    s' = s & #color %~ scaleOpac x & #borderColor %~ scaleOpac x

-- | select a main colour
colourStyle_ :: Colour -> Styles -> Styles
colourStyle_ c (RectA s) = RectA s'
  where
    s' = s & #color %~ mix c & #borderColor %~ mix c
colourStyle_ c (TextA s) = TextA s'
  where
    s' = s & #color %~ mix c
colourStyle_ c (LineA s) = LineA s'
  where
    s' = s & #color %~ mix c
colourStyle_ c (GlyphA s) = GlyphA s'
  where
    s' = s & #color %~ mix c & #borderColor %~ mix c
colourStyle_ c (PathA s) = PathA s'
  where
    s' = s & #color %~ mix c & #borderColor %~ mix c

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
  TextStyle 0.08 dark AnchorMiddle 0.5 1.45 (-0.2) Nothing

-- | the extra area from text styling
styleBoxText ::
  TextStyle ->
  Text ->
  Point Double ->
  Rect Double
styleBoxText o t p = move p $ maybe flat (`rotationBound` flat) (o ^. #rotation)
  where
    flat = Rect ((-x' / 2.0) + x' * a') (x' / 2 + x' * a') ((-y' / 2) - n1') (y' / 2 - n1')
    s = o ^. #size
    h = o ^. #hsize
    v = o ^. #vsize
    n1 = o ^. #nudge1
    x' = s * h * fromIntegral (sum $ maybe 0 Text.length . maybeTagText <$> parseTags t)
    y' = s * v
    n1' = s * n1
    a' = case o ^. #anchor of
      AnchorStart -> 0.5
      AnchorEnd -> -0.5
      AnchorMiddle -> 0.0

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

data ScaleBorder = ScaleBorder | NoScaleBorder deriving (Show, Eq, Generic)

-- | glyph shapes
data GlyphShape
  = CircleGlyph
  | SquareGlyph
  | EllipseGlyph Double
  | RectSharpGlyph Double
  | RectRoundedGlyph Double Double Double
  | TriangleGlyph (Point Double) (Point Double) (Point Double)
  -- ^ line width is determined by borderSize
  | VLineGlyph
  | HLineGlyph
  | PathGlyph Text ScaleBorder
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
    VLineGlyph -> "VLine"
    HLineGlyph -> "HLine"
    PathGlyph _ _ -> "Path"

-- | the extra area from glyph styling
styleBoxGlyph :: GlyphStyle -> Rect Double
styleBoxGlyph s = move p' $
  sw $ case sh of
    CircleGlyph -> (sz *) <$> one
    SquareGlyph -> (sz *) <$> one
    EllipseGlyph a -> CD.scale (Point sz (a * sz)) one
    RectSharpGlyph a -> CD.scale (Point sz (a * sz)) one
    RectRoundedGlyph a _ _ -> CD.scale (Point sz (a * sz)) one
    VLineGlyph -> CD.scale (Point ((s ^. #borderSize) * sz) sz) one
    HLineGlyph -> CD.scale (Point sz ((s ^. #borderSize) * sz)) one
    TriangleGlyph a b c -> (sz *) <$> space1 [a,b,c]
    PathGlyph path' _ -> (sz *) <$> (pathBoxes . fmap pathInfoToSvgCoords . toPathXYs . either error id . parsePath $ path')
  where
    sh = s ^. #shape
    sz = s ^. #size
    sw = padRect (0.5 * s ^. #borderSize)
    p' = fromMaybe (Point 0.0 0.0) (s ^. #translate)

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

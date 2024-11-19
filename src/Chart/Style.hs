{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Definition of the syntactical manifestation of chart elements.
module Chart.Style
  ( Style (..),
    defaultStyle,
    scaleStyle,

    -- * RectStyle
    defaultRectStyle,
    blob,
    clear,
    border,

    -- * TextStyle
    defaultTextStyle,
    styleBoxText,
    EscapeText (..),

    -- * GlyphStyle
    defaultGlyphStyle,
    styleBoxGlyph,
    gpalette,
    GlyphShape (..),

    -- * LineStyle
    defaultLineStyle,
    LineCap (..),
    fromLineCap,
    toLineCap,
    LineJoin (..),
    fromLineJoin,
    toLineJoin,

    -- * Stack Styling
    TextAnchor (..),
    fromTextAnchor,
    fromAnchoring,
    Align (..),

    -- * PathStyle
    defaultPathStyle,

    -- * Style scaling
    ScaleP (..),
    scaleRatio,
  )
where

import Chart.Data
import Data.Bool
import Data.ByteString (ByteString)
import Data.Colour
import Data.List qualified as List
import Data.Maybe
import Data.Path
import Data.Path.Parser
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics
import NumHask.Space
import Optics.Core
import Prelude

-- $setup
--
-- >>> :set -XOverloadedLabels
-- >>> :set -XOverloadedStrings
-- >>> import Chart
-- >>> import Optics.Core

-- | Stylistic content of chart elements, involving how chart data is represented in the physical chart.
--
-- >>> defaultStyle
-- Style {size = 6.0e-2, borderSize = 1.0e-2, color = Colour 0.02 0.73 0.80 0.10, borderColor = Colour 0.02 0.29 0.48 1.00, scaleP = NoScaleP, textAnchor = AnchorMiddle, rotation = Nothing, translate = Nothing, escapeText = EscapeText, frame = Nothing, lineCap = Nothing, lineJoin = Nothing, dasharray = Nothing, dashoffset = Nothing, hsize = 0.6, vsize = 1.1, vshift = -0.25, glyphShape = SquareGlyph}
data Style = Style
  { -- | The size of the element in relation to the canvas domain.
    size :: Double,
    -- | stroke-width
    borderSize :: Double,
    -- | fill & fill-opacity
    color :: Colour,
    -- | stroke & stroke-opacity
    borderColor :: Colour,
    -- | How to treat scale projections.
    scaleP :: ScaleP,
    -- | text-anchor
    textAnchor :: TextAnchor,
    -- | element rotation is radians
    rotation :: Maybe Double,
    -- | element translation
    translate :: Maybe (Point Double),
    -- | whether to html-like escape text
    escapeText :: EscapeText,
    -- | rectangular frame around an element.
    frame :: Maybe Style,
    -- | stroke-linecap
    lineCap :: Maybe LineCap,
    -- | stroke-linejoin
    lineJoin :: Maybe LineJoin,
    -- | stroke-dasharray
    dasharray :: Maybe [Double],
    -- | stroke-dashoffset
    dashoffset :: Maybe Double,
    -- | horizontal scaling modifier for text
    hsize :: Double,
    -- | vertical scaling modifier for text
    vsize :: Double,
    -- | horizontal shift for text alignment
    vshift :: Double,
    -- | shape for glyph chart elements
    glyphShape :: GlyphShape
  }
  deriving (Eq, Show, Generic)

-- | The official default style
--
-- >>> defaultStyle
-- Style {size = 6.0e-2, borderSize = 1.0e-2, color = Colour 0.02 0.73 0.80 0.10, borderColor = Colour 0.02 0.29 0.48 1.00, scaleP = NoScaleP, textAnchor = AnchorMiddle, rotation = Nothing, translate = Nothing, escapeText = EscapeText, frame = Nothing, lineCap = Nothing, lineJoin = Nothing, dasharray = Nothing, dashoffset = Nothing, hsize = 0.6, vsize = 1.1, vshift = -0.25, glyphShape = SquareGlyph}
defaultStyle :: Style
defaultStyle = Style 0.06 0.01 (paletteO 0 0.1) (paletteO 1 1) NoScaleP AnchorMiddle Nothing Nothing EscapeText Nothing Nothing Nothing Nothing Nothing 0.6 1.1 (-0.25) SquareGlyph

-- | The official style for rectangles.
--
-- >>> defaultRectStyle
-- Style {size = 6.0e-2, borderSize = 1.0e-2, color = Colour 0.02 0.73 0.80 0.10, borderColor = Colour 0.02 0.29 0.48 1.00, scaleP = NoScaleP, textAnchor = AnchorMiddle, rotation = Nothing, translate = Nothing, escapeText = EscapeText, frame = Nothing, lineCap = Nothing, lineJoin = Nothing, dasharray = Nothing, dashoffset = Nothing, hsize = 0.6, vsize = 1.1, vshift = -0.25, glyphShape = SquareGlyph}
defaultRectStyle :: Style
defaultRectStyle = defaultStyle

-- | The official style for text elements.
--
-- >>> defaultTextStyle
-- Style {size = 6.0e-2, borderSize = 1.0e-2, color = Colour 0.05 0.05 0.05 1.00, borderColor = Colour 0.02 0.29 0.48 1.00, scaleP = NoScaleP, textAnchor = AnchorMiddle, rotation = Nothing, translate = Nothing, escapeText = EscapeText, frame = Nothing, lineCap = Nothing, lineJoin = Nothing, dasharray = Nothing, dashoffset = Nothing, hsize = 0.6, vsize = 1.1, vshift = -0.25, glyphShape = SquareGlyph}
defaultTextStyle :: Style
defaultTextStyle = defaultStyle & set #size 0.06 & set #color dark

-- | The official style for glyphs.
--
-- >>> defaultGlyphStyle
-- Style {size = 3.0e-2, borderSize = 3.0e-3, color = Colour 0.02 0.73 0.80 0.20, borderColor = Colour 0.02 0.29 0.48 1.00, scaleP = NoScaleP, textAnchor = AnchorMiddle, rotation = Nothing, translate = Nothing, escapeText = EscapeText, frame = Nothing, lineCap = Nothing, lineJoin = Nothing, dasharray = Nothing, dashoffset = Nothing, hsize = 0.6, vsize = 1.1, vshift = -0.25, glyphShape = SquareGlyph}
defaultGlyphStyle :: Style
defaultGlyphStyle = defaultStyle & set #size 0.03 & set #color (paletteO 0 0.2) & set #borderColor (set lightness' 0.4 $ paletteO 1 1) & set #borderSize 0.003

-- | The official style for lines.
--
-- >>> defaultLineStyle
-- Style {size = 1.2e-2, borderSize = 1.0e-2, color = Colour 0.05 0.05 0.05 1.00, borderColor = Colour 0.02 0.29 0.48 1.00, scaleP = NoScaleP, textAnchor = AnchorMiddle, rotation = Nothing, translate = Nothing, escapeText = EscapeText, frame = Nothing, lineCap = Nothing, lineJoin = Nothing, dasharray = Nothing, dashoffset = Nothing, hsize = 0.6, vsize = 1.1, vshift = -0.25, glyphShape = SquareGlyph}
defaultLineStyle :: Style
defaultLineStyle = defaultStyle & set #size 0.012 & set #color dark

-- | The official style for paths.
--
-- >>> defaultPathStyle
-- Style {size = 6.0e-2, borderSize = 1.0e-2, color = Colour 0.66 0.07 0.55 1.00, borderColor = Colour 0.02 0.29 0.48 1.00, scaleP = NoScaleP, textAnchor = AnchorMiddle, rotation = Nothing, translate = Nothing, escapeText = EscapeText, frame = Nothing, lineCap = Nothing, lineJoin = Nothing, dasharray = Nothing, dashoffset = Nothing, hsize = 0.6, vsize = 1.1, vshift = -0.25, glyphShape = SquareGlyph}
defaultPathStyle :: Style
defaultPathStyle = defaultStyle & set #color (palette 2) & set #borderColor (palette 1)

-- | Scale the size, borderSize and any translations of a 'Style'.
scaleStyle :: Double -> Style -> Style
scaleStyle x s =
  s
    & over #size (x *)
    & over #borderSize (x *)
    & over #translate (fmap (fmap (x *)))

-- | solid rectangle, no border
--
-- >>> blob black
-- Style {size = 6.0e-2, borderSize = 0.0, color = Colour 0.00 0.00 0.00 1.00, borderColor = Colour 0.00 0.00 0.00 0.00, scaleP = NoScaleP, textAnchor = AnchorMiddle, rotation = Nothing, translate = Nothing, escapeText = EscapeText, frame = Nothing, lineCap = Nothing, lineJoin = Nothing, dasharray = Nothing, dashoffset = Nothing, hsize = 0.6, vsize = 1.1, vshift = -0.25, glyphShape = SquareGlyph}
blob :: Colour -> Style
blob c = defaultRectStyle & set #borderSize 0 & set #borderColor transparent & set #color c

-- | transparent rect
--
-- >>> clear
-- Style {size = 6.0e-2, borderSize = 0.0, color = Colour 0.00 0.00 0.00 0.00, borderColor = Colour 0.00 0.00 0.00 0.00, scaleP = NoScaleP, textAnchor = AnchorMiddle, rotation = Nothing, translate = Nothing, escapeText = EscapeText, frame = Nothing, lineCap = Nothing, lineJoin = Nothing, dasharray = Nothing, dashoffset = Nothing, hsize = 0.6, vsize = 1.1, vshift = -0.25, glyphShape = SquareGlyph}
clear :: Style
clear = defaultRectStyle & set #borderSize 0 & set #borderColor transparent & set #color transparent

-- | transparent rectangle, with border
--
-- >>> border 0.01 transparent
-- Style {size = 6.0e-2, borderSize = 1.0e-2, color = Colour 0.00 0.00 0.00 0.00, borderColor = Colour 0.00 0.00 0.00 0.00, scaleP = NoScaleP, textAnchor = AnchorMiddle, rotation = Nothing, translate = Nothing, escapeText = EscapeText, frame = Nothing, lineCap = Nothing, lineJoin = Nothing, dasharray = Nothing, dashoffset = Nothing, hsize = 0.6, vsize = 1.1, vshift = -0.25, glyphShape = SquareGlyph}
border :: Double -> Colour -> Style
border s c = defaultRectStyle & set #borderSize s & set #borderColor c & set #color transparent

-- | Whether to escape the common XML escaped characters.
data EscapeText = EscapeText | NoEscapeText deriving (Eq, Show, Generic)

-- | the extra area from text styling
styleBoxText ::
  Style ->
  Text ->
  Point Double ->
  Rect Double
styleBoxText o t p = mpad $ move p $ maybe flat (`rotationBound` flat) (view #rotation o)
  where
    flat = Rect ((-(x' / 2.0)) + x' * a') (x' / 2 + x' * a') (-(y' / 2) + n1') (y' / 2 + n1')
    s = view #size o
    h = view #hsize o
    v = view #vsize o
    n1 = view #vshift o
    x' = s * h * fromIntegral (Text.length t)
    y' = s * v
    n1' = (-s) * n1
    a' = case view #textAnchor o of
      AnchorStart -> 0.5
      AnchorEnd -> -0.5
      AnchorMiddle -> 0.0
    mpad = case view #frame o of
      Nothing -> id
      Just f -> padRect (0.5 * view #borderSize f * view #size o)

-- | glyph shapes
data GlyphShape
  = CircleGlyph
  | SquareGlyph
  | EllipseGlyph Double
  | RectSharpGlyph Double
  | RectRoundedGlyph Double Double Double
  | -- | line width is determined by borderSize
    TriangleGlyph (Point Double) (Point Double) (Point Double)
  | VLineGlyph
  | HLineGlyph
  | PathGlyph ByteString
  deriving (Show, Eq, Generic)

-- | the extra area from glyph styling
styleBoxGlyph :: Style -> Rect Double
styleBoxGlyph s = move p' $
  rot' $
    sw $ case view #glyphShape s of
      CircleGlyph -> (sz *) <$> one
      SquareGlyph -> (sz *) <$> one
      EllipseGlyph a -> scale (Point sz (a * sz)) one
      RectSharpGlyph a -> scale (Point sz (a * sz)) one
      RectRoundedGlyph a _ _ -> scale (Point sz (a * sz)) one
      VLineGlyph -> scale (Point (view #borderSize s) sz) one
      HLineGlyph -> scale (Point sz (view #borderSize s)) one
      TriangleGlyph a b c -> (sz *) <$> unsafeSpace1 ([a, b, c] :: [Point Double])
      PathGlyph path' -> maybe zero (fmap (sz *)) (pathBoxes . svgToPathData $ path')
  where
    sz = view #size s
    sw = padRect (0.5 * view #borderSize s)
    p' = fromMaybe (Point 0.0 0.0) (view #translate s)
    rot' = maybe id rotationBound (view #rotation s)

-- | Infinite list of glyph shapes
--
-- >>> gpalette 0
-- CircleGlyph
gpalette :: Int -> GlyphShape
gpalette x = cycle gpalette1_ List.!! x

-- | finite list of glyphs
gpalette1_ :: [GlyphShape]
gpalette1_ =
  [ CircleGlyph,
    SquareGlyph,
    RectSharpGlyph 0.75,
    RectRoundedGlyph 0.75 0.01 0.01,
    EllipseGlyph 0.75,
    VLineGlyph,
    HLineGlyph,
    TriangleGlyph (Point 0.0 0.0) (Point 1 1) (Point 1 0),
    PathGlyph "M0.05,-0.03660254037844387 A0.1 0.1 0.0 0 1 0.0,0.05 0.1 0.1 0.0 0 1 -0.05,-0.03660254037844387 0.1 0.1 0.0 0 1 0.05,-0.03660254037844387 Z"
  ]

-- | line cap style
data LineCap = LineCapButt | LineCapRound | LineCapSquare deriving (Eq, Show, Generic)

-- | svg textifier
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

-- | svg textifier
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

-- | Text Anchor
data TextAnchor = AnchorMiddle | AnchorStart | AnchorEnd deriving (Eq, Show, Generic)

fromTextAnchor :: TextAnchor -> ByteString
fromTextAnchor AnchorMiddle = "middle"
fromTextAnchor AnchorStart = "start"
fromTextAnchor AnchorEnd = "end"

fromAnchoring :: Double -> TextAnchor
fromAnchoring x = case compare x zero of
  EQ -> AnchorMiddle
  GT -> AnchorEnd
  LT -> AnchorStart

-- | Aligning stacked things.
data Align = NoAlign | AlignRight | AlignLeft | AlignMid deriving (Eq, Show, Generic)

-- | Scale Projection options
data ScaleP
  = -- | Do not scale under projection.
    NoScaleP
  | -- | Scale based on the X axis ratio of a projection
    ScalePX
  | -- | Scale based on the Y axis ratio of a projection
    ScalePY
  | -- | Scale based on minimum of (X axis, Y axis) ratio
    ScalePMinDim
  | -- | Scale based on the area ratio of a projection
    ScalePArea
  deriving (Generic, Eq, Show)

-- | given a ScaleP and two Rects, what is the scaling factor for a projection
--
-- Guards against scaling to zero or infinity
scaleRatio :: ScaleP -> Rect Double -> Rect Double -> Double
scaleRatio NoScaleP _ _ = 1
scaleRatio ScalePX new old = bool 1 (width nx / width ox) (width ox > 0 && width nx > 0)
  where
    (Ranges nx _) = new
    (Ranges ox _) = old
scaleRatio ScalePY new old = bool 1 (width ny / width oy) (width oy > 0 && width ny > 0)
  where
    (Ranges _ ny) = new
    (Ranges _ oy) = old
scaleRatio ScalePArea new old = bool 1 (sqrt (an / ao)) (an > 0 && ao > 0)
  where
    (Ranges nx ny) = new
    (Ranges ox oy) = old
    an = width nx * width ny
    ao = width ox * width oy
scaleRatio ScalePMinDim new old = closestToOne
  where
    x' = scaleRatio ScalePX new old
    y' = scaleRatio ScalePY new old
    closestToOne
      | x' >= 1 && y' >= 1 = bool x' y' (x' > y')
      | x' >= 1 && y' < 1 = bool x' y' (x' > (1 / y'))
      | x' < 1 && y' >= 1 = bool x' y' ((1 / x') > y')
      | otherwise = bool x' y' ((1 / x') > (1 / y'))

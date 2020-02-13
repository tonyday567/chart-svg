{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module Chart.Types
  ( Chart (..),
    Chartable,
    Annotation (..),
    annotationText,
    Tree (..),
    DrawAttributes (..),
    RectStyle (RectStyle),
    defaultRectStyle,
    blob,
    clear,
    border,
    TextStyle (..),
    defaultTextStyle,
    Anchor (..),
    fromAnchor,
    toAnchor,
    FormatN (..),
    fromFormatN,
    toFormatN,
    GlyphStyle (..),
    defaultGlyphStyle,
    GlyphShape (..),
    glyphText,
    LineStyle (..),
    defaultLineStyle,
    ChartSvg (..),
    Orientation(..),
    fromOrientation,
    toOrientation,
    blue,
    grey,
    black,
    white,
    red,
    toColour,
    fromColour,
    d3Palette1,
    ChartException (..),
    ChartSvgStyle (..),
    defaultChartSvgStyle,
    defaultSvgFrame,
    Spot (..),
    toRect,
    toPoint,
    pattern SR,
    pattern SP,
  )
where

import Codec.Picture.Types
import Control.Exception
import Data.Generics.Labels ()
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Exts
import GHC.Generics
import Graphics.Svg (DrawAttributes (..), Tree (..))
import NumHask.Space
import Prelude
import qualified Data.Colour.RGBSpace as C
import qualified Data.Colour.SRGB.Linear as C
import qualified Data.Colour.Palette.ColorSet as C

data ChartException = NotYetImplementedException deriving (Show)

instance Exception ChartException

-- * Chart

-- | A `Chart` consists of
-- - a list of spots on the xy-plane, and
-- - specific style of representation for each spot (an Annotation)
data Chart a
  = Chart
      { annotation :: Annotation,
        spots :: [Spot a]
      }
  deriving (Eq, Show, Generic)

-- | the aspects a number needs to be to form the data for a chart
type Chartable a =
  (Real a, Fractional a, RealFrac a, RealFloat a, Floating a)

-- | a piece of chart structure
-- | The use of #rowName with these Annotation collection doesn't seem to mesh well with polymorphism, so a switch to concrete types (which fit it with svg-tree methods) occurs at this layer, and the underlying data structure is a lot of Doubles
data Annotation
  = RectA RectStyle
  | TextA TextStyle [Text.Text]
  | GlyphA GlyphStyle
  | LineA LineStyle
  | BlankA
  deriving (Eq, Show, Generic)

annotationText :: Annotation -> Text
annotationText (RectA _) = "RectA"
annotationText TextA {} = "TextA"
annotationText (GlyphA _) = "GlyphA"
annotationText (LineA _) = "LineA"
annotationText BlankA = "BlankA"

-- | Rectangle styling
data RectStyle
  = RectStyle
      { borderSize :: Double,
        borderColor :: PixelRGB8,
        borderOpacity :: Double,
        color :: PixelRGB8,
        opacity :: Double
      }
  deriving (Show, Eq, Generic)

-- | the official style
defaultRectStyle :: RectStyle
defaultRectStyle = RectStyle 0.005 grey 0.5 red 0.5

-- | solid rectangle, no border
blob :: PixelRGB8 -> Double -> RectStyle
blob = RectStyle 0 black 0

-- | clear and utrans rect
clear :: RectStyle
clear = RectStyle 0 black 0 black 0

-- | transparent rectangle, with border
border :: Double -> PixelRGB8 -> Double -> RectStyle
border s c o = RectStyle s c o black 0

-- | Text styling
data TextStyle
  = TextStyle
      { size :: Double,
        color :: PixelRGB8,
        opacity :: Double,
        anchor :: Anchor,
        hsize :: Double,
        vsize :: Double,
        nudge1 :: Double,
        rotation :: Maybe Double,
        translate :: Maybe (Point Double),
        hasMathjax :: Bool
      }
  deriving (Show, Eq, Generic)

data Anchor = AnchorMiddle | AnchorStart | AnchorEnd deriving (Eq, Show, Generic)

fromAnchor :: (IsString s) => Anchor -> s
fromAnchor AnchorMiddle = "Middle"
fromAnchor AnchorStart = "Start"
fromAnchor AnchorEnd = "End"

toAnchor :: (Eq s, IsString s) => s -> Anchor
toAnchor "Middle" = AnchorMiddle
toAnchor "Start" = AnchorStart
toAnchor "End" = AnchorEnd
toAnchor _ = AnchorMiddle

data FormatN = FormatFixed Int | FormatComma Int | FormatNone deriving (Eq, Show, Generic)

fromFormatN :: (IsString s) => FormatN -> s
fromFormatN (FormatFixed _) = "Fixed"
fromFormatN (FormatComma _) = "Comma"
fromFormatN FormatNone = "None"

toFormatN :: (Eq s, IsString s) => s -> Int -> FormatN
toFormatN "Fixed" n = FormatFixed n
toFormatN "Comma" n = FormatComma n
toFormatN "None" _ = FormatNone
toFormatN _ _ = FormatNone

-- | the offical text style
defaultTextStyle :: TextStyle
defaultTextStyle =
  TextStyle 0.08 grey 1.0 AnchorMiddle 0.5 1.45 (-0.2) Nothing Nothing False

-- | Glyph styling
data GlyphStyle
  = GlyphStyle
      { -- | glyph radius
        size :: Double,
        -- | fill color
        color :: PixelRGB8,
        opacity :: Double,
        -- | stroke color
        borderColor :: PixelRGB8,
        borderOpacity :: Double,
        -- | stroke width (adds a bit to the bounding box)
        borderSize :: Double,
        shape :: GlyphShape,
        rotation :: Maybe Double,
        translate :: Maybe (Point Double)
      }
  deriving (Show, Eq, Generic)

-- | the offical circle style
defaultGlyphStyle :: GlyphStyle
defaultGlyphStyle =
  GlyphStyle
    0.03
    (PixelRGB8 217 151 33)
    0.8
    (PixelRGB8 44 66 157)
    0.4
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
  | SmileyGlyph
  deriving (Show, Eq, Generic)

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
    SmileyGlyph -> "Smiley"

-- | line style
data LineStyle
  = LineStyle
      { width :: Double,
        color :: PixelRGB8,
        opacity :: Double
      }
  deriving (Show, Eq, Generic)

-- | the official default line style
defaultLineStyle :: LineStyle
defaultLineStyle = LineStyle 0.02 blue 0.5

-- | Svg of a Chart consists of
-- An Svg `Tree` list and a Rect
data ChartSvg a
  = ChartSvg
      { vbox :: Rect a,
        chartTrees :: [Tree]
      }
  deriving (Eq, Show)

instance (Ord a) => Semigroup (ChartSvg a) where
  (ChartSvg a b) <> (ChartSvg a' b') = ChartSvg (a <> a') (b <> b')

instance (Chartable a) => Monoid (ChartSvg a) where
  mempty = ChartSvg unitRect mempty

-- | Verticle or Horizontal
data Orientation = Vert | Hori deriving (Eq, Show, Generic)

fromOrientation :: (IsString s) => Orientation -> s
fromOrientation Hori = "Hori"
fromOrientation Vert = "Vert"

toOrientation :: (Eq s, IsString s) => s -> Orientation
toOrientation "Hori" = Hori
toOrientation "Vert" = Vert
toOrientation _ = Hori

-- * color

-- | the official chart-unit blue
blue :: PixelRGB8
blue = PixelRGB8 93 165 218

-- | the official chart-unit grey
grey :: PixelRGB8
grey = PixelRGB8 102 102 102

-- | black
black :: PixelRGB8
black = PixelRGB8 0 0 0

-- | white
white :: PixelRGB8
white = PixelRGB8 255 255 255

-- | red
red :: PixelRGB8
red = PixelRGB8 255 0 0

-- | convert a 'PixelRGB8' to a 'Colour' representation.
toColour :: PixelRGB8 -> C.Colour Double
toColour (PixelRGB8 r g b) =
  C.rgb (fromIntegral r / 256.0) (fromIntegral g / 256.0) (fromIntegral b / 256.0)

-- | convert a 'Colour' to a 'PixelRGB8' representation.
fromColour :: C.Colour Double -> PixelRGB8
fromColour (C.toRGB -> C.RGB r g b) =
  PixelRGB8 (floor (256 * r)) (floor (256 * g)) (floor (256 * b))

-- | the d3 palette
d3Palette1 :: [PixelRGB8]
d3Palette1 = fromColour . C.d3Colors1 <$> [0..9]

-- | Top-level SVG options.
data ChartSvgStyle
  = ChartSvgStyle
      { sizex :: Double,
        sizey :: Double,
        chartAspect :: Double,
        outerPad :: Maybe Double,
        innerPad :: Maybe Double,
        chartFrame :: Maybe RectStyle,
        orig :: Maybe GlyphStyle,
        escapeText :: Bool
      }
  deriving (Eq, Show, Generic)

defaultChartSvgStyle :: ChartSvgStyle
defaultChartSvgStyle = ChartSvgStyle 800 600 1.33 (Just 1.02) Nothing Nothing Nothing True

defaultSvgFrame :: RectStyle
defaultSvgFrame = border 0.01 blue 1.0

-- * primitive Chart elements

-- | unification of a point and rect on the plane
data Spot a
  = SpotPoint (Point a)
  | SpotRect (Rect a)
  deriving (Eq, Show, Functor)

instance (Ord a, Num a, Fractional a) => Num (Spot a) where

  SpotPoint (Point x y) + SpotPoint (Point x' y') = SpotPoint (Point (x + x') (y + y'))
  SpotPoint (Point x' y') + SpotRect (Rect x z y w) = SpotRect $ Rect (x + x') (z + x') (y + y') (w + y')
  SpotRect (Rect x z y w) + SpotPoint (Point x' y') = SpotRect $ Rect (x + x') (z + x') (y + y') (w + y')
  SpotRect (Rect x z y w) + SpotRect (Rect x' z' y' w') =
    SpotRect $ Rect (x + x') (z + z') (y + y') (w + w')

  x * y = SpotRect $ toRect x `multRect` toRect y

  abs x = SpotPoint $ abs <$> toPoint x

  signum x = SpotPoint $ signum <$> toPoint x

  negate (SpotPoint (Point x y)) = SpotPoint (Point (- x) (- y))
  negate (SpotRect (Rect x z y w)) = SpotRect (Rect (- x) (- z) (- y) (- w))

  fromInteger x = SP (fromInteger x) (fromInteger x)

-- | pattern for SP x y
pattern SP :: a -> a -> Spot a
pattern SP a b = SpotPoint (Point a b)

{-# COMPLETE SP #-}

-- | pattern for SA lowerx upperx lowery uppery
pattern SR :: a -> a -> a -> a -> Spot a
pattern SR a b c d = SpotRect (Rect a b c d)

{-# COMPLETE SR #-}

-- | Convert a spot to an Rect
toRect :: Spot a -> Rect a
toRect (SP x y) = Rect x x y y
toRect (SpotRect a) = a

-- | Convert a spot to a Point
toPoint :: (Ord a, Fractional a) => Spot a -> Point a
toPoint (SP x y) = Point x y
toPoint (SpotRect (Ranges x y)) = Point (mid x) (mid y)

instance (Ord a) => Semigroup (Spot a) where
  (<>) a b = SpotRect (toRect a `union` toRect b)

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
{-# OPTIONS_GHC -Wall #-}

module Chart.Types
  ( Chart (..),
    Chartable,
    Annotation (..),
    annotationText,
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
    GlyphStyle (..),
    defaultGlyphStyle,
    GlyphShape (..),
    glyphText,
    LineStyle (..),
    defaultLineStyle,
    PixelStyle (..),
    defaultPixelStyle,
    Orientation (..),
    fromOrientation,
    toOrientation,
    Spot (..),
    toRect,
    toPoint,
    pattern SR,
    pattern SP,
    padRect,
    SvgAspect (..),
    toSvgAspect,
    fromSvgAspect,
    EscapeText (..),
    CssOptions (..),
    ScaleCharts (..),
    SvgOptions (..),
    defaultSvgOptions,
    defaultSvgFrame,
    ChartDims (..),
    HudT (..),
    Hud,
    HudOptions (..),
    defaultHudOptions,
    defaultCanvas,
    AxisOptions (..),
    defaultAxisOptions,
    Place (..),
    placeText,
    Bar (..),
    defaultBar,
    Title (..),
    defaultTitle,
    Tick (..),
    defaultGlyphTick,
    defaultTextTick,
    defaultLineTick,
    defaultTick,
    TickStyle (..),
    defaultTickStyle,
    tickStyleText,
    TickExtend (..),
    Adjustments (..),
    defaultAdjustments,
    LegendOptions (..),
    defaultLegendOptions,
    FormatN (..),
    defaultFormatN,
  )
where

import Chart.Color
import Codec.Picture.Types
import Control.Lens
import Data.Generics.Labels ()
import Data.List ((!!))
import qualified Data.Text as Text
import GHC.Exts
import GHC.Generics
import NumHask.Space hiding (Element)
import Protolude

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
-- | The use of #rowName with Annotation doesn't seem to mesh well with polymorphism, so a switch to concrete types (which fit it with svg-tree methods) occurs at this layer, and the underlying ADTs use a lot of Doubles
data Annotation
  = RectA RectStyle
  | TextA TextStyle [Text.Text]
  | GlyphA GlyphStyle
  | LineA LineStyle
  | BlankA
  | PixelA PixelStyle
  deriving (Eq, Show, Generic)

annotationText :: Annotation -> Text
annotationText (RectA _) = "RectA"
annotationText TextA {} = "TextA"
annotationText (GlyphA _) = "GlyphA"
annotationText (LineA _) = "LineA"
annotationText BlankA = "BlankA"
annotationText (PixelA _) = "PixelA"

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
defaultRectStyle = RectStyle 0.02 (chartPalette !! 0) 0.5 (chartPalette !! 3) 0.5

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
  | VLineGlyph
  | HLineGlyph
  | PathGlyph Text
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
    VLineGlyph -> "VLine"
    HLineGlyph -> "HLine"
    PathGlyph _ -> "Path"

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

data PixelStyle
  = PixelStyle
      { pixelColorMin :: PixelRGB8,
        pixelOpacityMin :: Double,
        pixelColorMax :: PixelRGB8,
        pixelOpacityMax :: Double,
        -- | expressed in directional terms
        -- 0 for horizontal
        -- pi/2 for vertical
        pixelGradient :: Double,
        pixelRectStyle :: RectStyle,
        pixelTextureId :: Text
      }
  deriving (Show, Eq, Generic)

defaultPixelStyle :: PixelStyle
defaultPixelStyle =
  PixelStyle grey 1 blue 1 (pi / 2) (RectStyle 0 black 0 black 1) "pixel"

-- | Verticle or Horizontal
data Orientation = Vert | Hori deriving (Eq, Show, Generic)

fromOrientation :: (IsString s) => Orientation -> s
fromOrientation Hori = "Hori"
fromOrientation Vert = "Vert"

toOrientation :: (Eq s, IsString s) => s -> Orientation
toOrientation "Hori" = Hori
toOrientation "Vert" = Vert
toOrientation _ = Hori

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

-- | additive padding
padRect :: (Num a) => a -> Rect a -> Rect a
padRect p (Rect x z y w) = Rect (x - p) (z + p) (y - p) (w + p)

data EscapeText = EscapeText | NoEscapeText deriving (Show, Eq, Generic)

data CssOptions = UseCssCrisp | NoCssOptions deriving (Show, Eq, Generic)

data ScaleCharts = ScaleCharts | NoScaleCharts deriving (Show, Eq, Generic)

data SvgAspect = ManualAspect Double | ChartAspect deriving (Show, Eq, Generic)

fromSvgAspect :: (IsString s) => SvgAspect -> s
fromSvgAspect (ManualAspect _) = "ManualAspect"
fromSvgAspect ChartAspect = "ChartAspect"

toSvgAspect :: (Eq s, IsString s) => s -> Double -> SvgAspect
toSvgAspect "ManualAspect" a = ManualAspect a
toSvgAspect "ChartAspect" _ = ChartAspect
toSvgAspect _ _ = ChartAspect

-- | Top-level SVG options.
data SvgOptions
  = SvgOptions
      { svgHeight :: Double,
        outerPad :: Maybe Double,
        innerPad :: Maybe Double,
        chartFrame :: Maybe RectStyle,
        escapeText :: EscapeText,
        useCssCrisp :: CssOptions,
        scaleCharts' :: ScaleCharts,
        svgAspect :: SvgAspect
      }
  deriving (Eq, Show, Generic)

defaultSvgOptions :: SvgOptions
defaultSvgOptions = SvgOptions 300 (Just 0.02) Nothing Nothing NoEscapeText NoCssOptions ScaleCharts (ManualAspect 1.5)

defaultSvgFrame :: RectStyle
defaultSvgFrame = border 0.01 blue 1.0

-- | In order to create huds, there are three main pieces of state that need to be kept track of:
--
-- - chartDim: the rectangular dimension of the physical representation of a chart on the screen so that new hud elements can be appended. Adding a hud piece tends to expand the chart dimension.
--
-- - canvasDim: the rectangular dimension of the canvas on which data will be represented. At times appending a hud element will cause the canvas dimension to shift.
--
-- - dataDim: the rectangular dimension of the data being represented. Adding hud elements can cause this to change.
data ChartDims a
  = ChartDims
      { chartDim :: Rect a,
        canvasDim :: Rect a,
        dataDim :: Rect a
      }
  deriving (Eq, Show, Generic)

newtype HudT m a = Hud {unhud :: [Chart a] -> StateT (ChartDims a) m [Chart a]}

type Hud = HudT Identity

instance (Monad m) => Semigroup (HudT m a) where
  (<>) (Hud h1) (Hud h2) = Hud $ h1 >=> h2

instance (Monad m) => Monoid (HudT m a) where
  mempty = Hud pure

-- | Practically, the configuration of a Hud is going to be in decimals, typed into config files and the like, and so we concrete at the configuration level, and settle on doubles for specifying the geomtry of hud elements.
data HudOptions
  = HudOptions
      { hudCanvas :: Maybe RectStyle,
        hudTitles :: [Title],
        hudAxes :: [AxisOptions],
        hudLegend :: Maybe (LegendOptions, [(Annotation, Text)])
      }
  deriving (Eq, Show, Generic)

instance Semigroup HudOptions where
  (<>) (HudOptions c t a l) (HudOptions c' t' a' l') =
    HudOptions (listToMaybe $ catMaybes [c, c']) (t <> t') (a <> a') (listToMaybe $ catMaybes [l, l'])

instance Monoid HudOptions where
  mempty = HudOptions Nothing [] [] Nothing

defaultHudOptions :: HudOptions
defaultHudOptions =
  HudOptions
    (Just defaultCanvas)
    []
    [ defaultAxisOptions,
      defaultAxisOptions & #place .~ PlaceLeft
    ]
    Nothing

defaultCanvas :: RectStyle
defaultCanvas = blob grey 0.03

-- | Placement of elements around (what is implicity but maybe shouldn't just be) a rectangular canvas
data Place
  = PlaceLeft
  | PlaceRight
  | PlaceTop
  | PlaceBottom
  | PlaceAbsolute (Point Double)
  deriving (Show, Eq, Generic)

placeText :: Place -> Text
placeText p =
  case p of
    PlaceTop -> "Top"
    PlaceBottom -> "Bottom"
    PlaceLeft -> "Left"
    PlaceRight -> "Right"
    PlaceAbsolute _ -> "Absolute"

data AxisOptions
  = AxisOptions
      { abar :: Maybe Bar,
        adjust :: Maybe Adjustments,
        atick :: Tick,
        place :: Place
      }
  deriving (Eq, Show, Generic)

defaultAxisOptions :: AxisOptions
defaultAxisOptions = AxisOptions (Just defaultBar) (Just defaultAdjustments) defaultTick PlaceBottom

data Bar
  = Bar
      { rstyle :: RectStyle,
        wid :: Double,
        buff :: Double
      }
  deriving (Show, Eq, Generic)

defaultBar :: Bar
defaultBar = Bar (RectStyle 0 grey 0 (PixelRGB8 95 3 145) 0.5) 0.005 0.01

-- | Options for titles.  Defaults to center aligned, and placed at Top of the hud
data Title
  = Title
      { text :: Text,
        style :: TextStyle,
        place :: Place,
        anchor :: Anchor,
        buff :: Double
      }
  deriving (Show, Eq, Generic)

defaultTitle :: Text -> Title
defaultTitle txt =
  Title
    txt
    ( (#size .~ 0.12)
        . (#color .~ PixelRGB8 0 0 0)
        $ defaultTextStyle
    )
    PlaceTop
    AnchorMiddle
    0.04

data Tick
  = Tick
      { tstyle :: TickStyle,
        gtick :: Maybe (GlyphStyle, Double),
        ttick :: Maybe (TextStyle, Double),
        ltick :: Maybe (LineStyle, Double)
      }
  deriving (Show, Eq, Generic)

defaultGlyphTick :: GlyphStyle
defaultGlyphTick =
  defaultGlyphStyle
    & #borderSize .~ 0.005
    & #color .~ PixelRGB8 95 3 145
    & #opacity .~ 1
    & #shape .~ VLineGlyph

defaultTextTick :: TextStyle
defaultTextTick =
  defaultTextStyle & #size .~ 0.05

defaultLineTick :: LineStyle
defaultLineTick =
  defaultLineStyle
    & #color .~ PixelRGB8 168 229 238
    & #width .~ 5.0e-3
    & #opacity .~ 0.3

defaultTick :: Tick
defaultTick =
  Tick
    defaultTickStyle
    (Just (defaultGlyphTick, 0.01))
    (Just (defaultTextTick, 0.015))
    (Just (defaultLineTick, 0.005))

-- | Style of tick marks on an axis.
data TickStyle
  = -- | no ticks on axis
    TickNone
  | -- | specific labels (equidistant placement)
    TickLabels [Text]
  | -- | sensibly rounded ticks, a guide to how many, and whether to extend beyond the data bounding box
    TickRound FormatN Int TickExtend
  | -- | exactly n equally spaced ticks
    TickExact FormatN Int
  | -- | specific labels and placement
    TickPlaced [(Double, Text)]
  deriving (Show, Eq, Generic)

defaultTickStyle :: TickStyle
defaultTickStyle = TickRound (FormatComma 0) 8 TickExtend

tickStyleText :: TickStyle -> Text
tickStyleText TickNone = "TickNone"
tickStyleText TickLabels {} = "TickLabels"
tickStyleText TickRound {} = "TickRound"
tickStyleText TickExact {} = "TickExact"
tickStyleText TickPlaced {} = "TickPlaced"

data TickExtend = TickExtend | NoTickExtend deriving (Eq, Show, Generic)

-- | options for prettifying axis decorations
data Adjustments
  = Adjustments
      { maxXRatio :: Double,
        maxYRatio :: Double,
        angledRatio :: Double,
        allowDiagonal :: Bool
      }
  deriving (Show, Eq, Generic)

defaultAdjustments :: Adjustments
defaultAdjustments = Adjustments 0.08 0.06 0.12 True

-- You're all Legends!

-- | Legend options
data LegendOptions
  = LegendOptions
      { lsize :: Double,
        vgap :: Double,
        hgap :: Double,
        ltext :: TextStyle,
        lmax :: Int,
        innerPad :: Double,
        outerPad :: Double,
        legendFrame :: Maybe RectStyle,
        lplace :: Place,
        lscale :: Double
      }
  deriving (Show, Eq, Generic)

defaultLegendOptions :: LegendOptions
defaultLegendOptions =
  LegendOptions
    0.1
    0.2
    0.1
    ( defaultTextStyle
        & #size .~ 0.08
        & #color .~ grey
    )
    10
    0.1
    0.1
    (Just (RectStyle 0.02 (PixelRGB8 55 100 160) 0.5 (PixelRGB8 255 255 255) 1))
    PlaceBottom
    0.2

data FormatN
  = FormatFixed Int
  | FormatComma Int
  | FormatExpt Int
  | FormatDollar
  | FormatPercent Int
  | FormatNone
  deriving (Eq, Show, Generic)

defaultFormatN :: FormatN
defaultFormatN = FormatComma 2

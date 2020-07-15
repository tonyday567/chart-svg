{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Chart.Types
  ( Chart (..),
    Annotation (..),
    annotationText,
    RectStyle (..),
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
    Direction (..),
    fromDirection,
    toDirection,
    Spot (..),
    toRect,
    toPoint,
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

    -- $color
    Colour,
    pattern Colour,
    opac,
    setOpac,
    fromRGB,
    hex,
    palette,
    palette1,
    blend,
    toHex,
    fromHex,
    unsafeFromHex,

    grayscale,
    colorText,
    transparent,
    black,
    white,

    -- * re-exports
    module Graphics.Color.Model,

    -- $formats
    FormatN(..),
    defaultFormatN,
    fromFormatN,
    toFormatN,
    fixed,
    comma,
    expt,
    dollar,
    formatN,
    precision,
    formatNs,
  )
where


import Control.Lens
import qualified Data.Attoparsec.Text as A
import Data.Generics.Labels ()
import Data.List ((!!))
import Graphics.Color.Model
import NumHask.Prelude
import NumHask.Space hiding (Element)
import qualified Prelude as P
import qualified Data.Text as Text
import Data.List (nub)
import Data.Scientific

-- * Chart

-- | A `Chart` consists of
-- - a list of spots on the xy-plane, and
-- - specific style of representation for each spot.
data Chart a
  = Chart
      { annotation :: Annotation,
        spots :: [Spot a]
      }
  deriving (Eq, Show, Generic)

-- | Manifestation of the data on a screen.
data Annotation
  = RectA RectStyle
  | TextA TextStyle [Text]
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
--
-- >>> defaultRectStyle
-- RectStyle {borderSize = 1.0e-2, borderColor = RGBA 0.12 0.47 0.71 0.80, color = RGBA 0.12 0.47 0.71 0.30}
--
data RectStyle
  = RectStyle
      { borderSize :: Double,
        borderColor :: Colour,
        color :: Colour
      }
  deriving (Show, Eq, Generic)

-- | the style
defaultRectStyle :: RectStyle
defaultRectStyle = RectStyle 0.01 (fromRGB (palette !! 1) 0.8) (fromRGB (palette !! 1) 0.3)

-- | solid rectangle, no border
--
-- >>> blob black
-- RectStyle {borderSize = 0.0, borderColor = RGBA 0.00 0.00 0.00 0.00, color = RGBA 0.00 0.00 0.00 1.00}
--
blob :: Colour -> RectStyle
blob = RectStyle 0 transparent

-- | transparent rect
--
-- >>> clear
-- RectStyle {borderSize = 0.0, borderColor = RGBA 0.00 0.00 0.00 0.00, color = RGBA 0.00 0.00 0.00 0.00}
--
clear :: RectStyle
clear = RectStyle 0 transparent transparent

-- | transparent rectangle, with border
--
-- >>> border 0.01 transparent
-- RectStyle {borderSize = 1.0e-2, borderColor = RGBA 0.00 0.00 0.00 0.00, color = RGBA 0.00 0.00 0.00 0.00}
--
border :: Double -> Colour -> RectStyle
border s c = RectStyle s c transparent

-- | Text styling
--
-- >>> defaultTextStyle
-- TextStyle {size = 8.0e-2, color = RGBA 0.20 0.20 0.20 1.00, anchor = AnchorMiddle, hsize = 0.5, vsize = 1.45, nudge1 = -0.2, rotation = Nothing, translate = Nothing, hasMathjax = False}
--
data TextStyle
  = TextStyle
      { size :: Double,
        color :: Colour,
        anchor :: Anchor,
        hsize :: Double,
        vsize :: Double,
        nudge1 :: Double,
        rotation :: Maybe Double,
        translate :: Maybe (Point Double),
        hasMathjax :: Bool
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
  TextStyle 0.08 colorText AnchorMiddle 0.5 1.45 (-0.2) Nothing Nothing False

-- | Glyph styling
--
-- >>> defaultGlyphStyle
-- GlyphStyle {size = 3.0e-2, color = RGBA 0.65 0.81 0.89 0.30, borderColor = RGBA 0.12 0.47 0.71 0.80, borderSize = 3.0e-3, shape = SquareGlyph, rotation = Nothing, translate = Nothing}
--
data GlyphStyle
  = GlyphStyle
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
    (fromRGB (palette !! 0) 0.3)
    (fromRGB (palette !! 1) 0.8)
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

-- | line style
--
-- >>> defaultLineStyle
-- LineStyle {width = 1.2e-2, color = RGBA 0.65 0.81 0.89 0.30}
--
data LineStyle
  = LineStyle
      { width :: Double,
        color :: Colour
      }
  deriving (Show, Eq, Generic)

-- | the official default line style
defaultLineStyle :: LineStyle
defaultLineStyle = LineStyle 0.012 (fromRGB (palette !! 0) 0.3)

-- | A pixel chart is a specialization of a 'RectA' chart
--
-- >>> defaultPixelStyle
-- PixelStyle {pixelColorMin = RGBA 0.65 0.81 0.89 1.00, pixelColorMax = RGBA 0.12 0.47 0.71 1.00, pixelGradient = 1.5707963267948966, pixelRectStyle = RectStyle {borderSize = 0.0, borderColor = RGBA 0.00 0.00 0.00 0.00, color = RGBA 0.00 0.00 0.00 1.00}, pixelTextureId = "pixel"}
--
data PixelStyle
  = PixelStyle
      { pixelColorMin :: Colour,
        pixelColorMax :: Colour,
        -- | expressed in directional terms
        -- 0 for horizontal
        -- pi/2 for vertical
        pixelGradient :: Double,
        pixelRectStyle :: RectStyle,
        pixelTextureId :: Text
      }
  deriving (Show, Eq, Generic)

-- | The official pixel style.
defaultPixelStyle :: PixelStyle
defaultPixelStyle =
  PixelStyle (fromRGB (palette !! 0) 1) (fromRGB (palette !! 1) 1) (pi / 2) (blob black) "pixel"

-- | Verticle or Horizontal
data Direction = Vert | Hori deriving (Eq, Show, Generic)

-- | textifier
fromDirection :: (IsString s) => Direction -> s
fromDirection Hori = "Hori"
fromDirection Vert = "Vert"

-- | readifier
toDirection :: (Eq s, IsString s) => s -> Direction
toDirection "Hori" = Hori
toDirection "Vert" = Vert
toDirection _ = Hori

-- | unification of a point and rect on the plane
data Spot a
  = SpotPoint (Point a)
  | SpotRect (Rect a)
  deriving (Eq, Show, Functor)

instance (Ord a, Num a, Fractional a) => Num (Spot a) where
  SpotPoint (Point x y) + SpotPoint (Point x' y') = SpotPoint (Point (x P.+ x') (y P.+ y'))
  SpotPoint (Point x' y') + SpotRect (Rect x z y w) = SpotRect $ Rect (x P.+ x') (z P.+ x') (y P.+ y') (w P.+ y')
  SpotRect (Rect x z y w) + SpotPoint (Point x' y') = SpotRect $ Rect (x P.+ x') (z P.+ x') (y P.+ y') (w P.+ y')
  SpotRect (Rect x z y w) + SpotRect (Rect x' z' y' w') =
    SpotRect $ Rect (x P.+ x') (z P.+ z') (y P.+ y') (w P.+ w')

  x * y = SpotRect $ toRect x `multRect` toRect y

  abs x = SpotPoint $ P.abs <$> toPoint x

  signum x = SpotPoint $ signum <$> toPoint x

  negate (SpotPoint (Point x y)) = SpotPoint (Point (P.negate x) (P.negate y))
  negate (SpotRect (Rect x z y w)) = SpotRect (Rect (P.negate x) (P.negate z) (P.negate y) (P.negate w))

  fromInteger x = SpotPoint (Point (P.fromInteger x) (P.fromInteger x))

-- | Convert a spot to an Rect
toRect :: Spot a -> Rect a
toRect (SpotPoint (Point x y)) = Rect x x y y
toRect (SpotRect a) = a

-- | Convert a spot to a Point
toPoint :: (Ord a, Fractional a) => Spot a -> Point a
toPoint (SpotPoint (Point x y)) = Point x y
toPoint (SpotRect (Ranges x y)) = Point (mid x) (mid y)

instance (Ord a) => Semigroup (Spot a) where
  (<>) a b = SpotRect (toRect a `union` toRect b)

-- | additive padding
padRect :: (Num a) => a -> Rect a -> Rect a
padRect p (Rect x z y w) = Rect (x P.- p) (z P.+ p) (y P.- p) (w P.+ p)

-- | or html
data EscapeText = EscapeText | NoEscapeText deriving (Show, Eq, Generic)

-- | pixel chart helper
data CssOptions = UseCssCrisp | NoCssOptions deriving (Show, Eq, Generic)

-- | turns off scaling.  Usually not what you want.
data ScaleCharts = ScaleCharts | NoScaleCharts deriving (Show, Eq, Generic)

-- | The x-y ratio of the viewing box
data SvgAspect = ManualAspect Double | ChartAspect deriving (Show, Eq, Generic)

-- | textifier
fromSvgAspect :: (IsString s) => SvgAspect -> s
fromSvgAspect (ManualAspect _) = "ManualAspect"
fromSvgAspect ChartAspect = "ChartAspect"

-- | readifier
toSvgAspect :: (Eq s, IsString s) => s -> Double -> SvgAspect
toSvgAspect "ManualAspect" a = ManualAspect a
toSvgAspect "ChartAspect" _ = ChartAspect
toSvgAspect _ _ = ChartAspect

-- | Top-level SVG options.
--
-- >>> defaultSvgOptions
-- SvgOptions {svgHeight = 300.0, outerPad = Just 2.0e-2, innerPad = Nothing, chartFrame = Nothing, escapeText = NoEscapeText, useCssCrisp = NoCssOptions, scaleCharts' = ScaleCharts, svgAspect = ManualAspect 1.5}
--
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

-- | The official svg options
defaultSvgOptions :: SvgOptions
defaultSvgOptions = SvgOptions 300 (Just 0.02) Nothing Nothing NoEscapeText NoCssOptions ScaleCharts (ManualAspect 1.5)

-- | frame style
defaultSvgFrame :: RectStyle
defaultSvgFrame = border 0.01 (fromRGB (grayscale 0.7) 0.5)

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

-- | Hud monad transformer
newtype HudT m a = Hud {unhud :: [Chart a] -> StateT (ChartDims a) m [Chart a]}

type Hud = HudT Identity

instance (Monad m) => Semigroup (HudT m a) where
  (<>) (Hud h1) (Hud h2) = Hud $ h1 >=> h2

instance (Monad m) => Monoid (HudT m a) where
  mempty = Hud pure

-- | Practically, the configuration of a Hud is going to be in decimals, typed into config files and the like, and so we concrete at the configuration level, and settle on doubles for specifying the geomtry of hud elements.
--
-- >>> defaultHudOptions
-- HudOptions {hudCanvas = Just (RectStyle {borderSize = 0.0, borderColor = RGBA 0.00 0.00 0.00 0.00, color = RGBA 0.50 0.50 0.50 0.02}), hudTitles = [], hudAxes = [AxisOptions {abar = Just (Bar {rstyle = RectStyle {borderSize = 0.0, borderColor = RGBA 0.50 0.50 0.50 1.00, color = RGBA 0.50 0.50 0.50 1.00}, wid = 5.0e-3, buff = 1.0e-2}), adjust = Just (Adjustments {maxXRatio = 8.0e-2, maxYRatio = 6.0e-2, angledRatio = 0.12, allowDiagonal = True}), atick = Tick {tstyle = TickRound (FormatComma 0) 8 TickExtend, gtick = Just (GlyphStyle {size = 3.0e-2, color = RGBA 0.50 0.50 0.50 1.00, borderColor = RGBA 0.50 0.50 0.50 1.00, borderSize = 5.0e-3, shape = VLineGlyph 5.0e-3, rotation = Nothing, translate = Nothing},1.25e-2), ttick = Just (TextStyle {size = 5.0e-2, color = RGBA 0.50 0.50 0.50 1.00, anchor = AnchorMiddle, hsize = 0.5, vsize = 1.45, nudge1 = -0.2, rotation = Nothing, translate = Nothing, hasMathjax = False},1.5e-2), ltick = Just (LineStyle {width = 5.0e-3, color = RGBA 0.50 0.50 0.50 0.05},0.0)}, place = PlaceBottom},AxisOptions {abar = Just (Bar {rstyle = RectStyle {borderSize = 0.0, borderColor = RGBA 0.50 0.50 0.50 1.00, color = RGBA 0.50 0.50 0.50 1.00}, wid = 5.0e-3, buff = 1.0e-2}), adjust = Just (Adjustments {maxXRatio = 8.0e-2, maxYRatio = 6.0e-2, angledRatio = 0.12, allowDiagonal = True}), atick = Tick {tstyle = TickRound (FormatComma 0) 8 TickExtend, gtick = Just (GlyphStyle {size = 3.0e-2, color = RGBA 0.50 0.50 0.50 1.00, borderColor = RGBA 0.50 0.50 0.50 1.00, borderSize = 5.0e-3, shape = VLineGlyph 5.0e-3, rotation = Nothing, translate = Nothing},1.25e-2), ttick = Just (TextStyle {size = 5.0e-2, color = RGBA 0.50 0.50 0.50 1.00, anchor = AnchorMiddle, hsize = 0.5, vsize = 1.45, nudge1 = -0.2, rotation = Nothing, translate = Nothing, hasMathjax = False},1.5e-2), ltick = Just (LineStyle {width = 5.0e-3, color = RGBA 0.50 0.50 0.50 0.05},0.0)}, place = PlaceLeft}], hudLegend = Nothing}
--
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

-- | The official hud options.
defaultHudOptions :: HudOptions
defaultHudOptions =
  HudOptions
    (Just defaultCanvas)
    []
    [ defaultAxisOptions,
      defaultAxisOptions & #place .~ PlaceLeft
    ]
    Nothing

-- | The official hud canvas
defaultCanvas :: RectStyle
defaultCanvas = blob (fromRGB (grayscale 0.5) 0.025)

-- | Placement of elements around (what is implicity but maybe shouldn't just be) a rectangular canvas
data Place
  = PlaceLeft
  | PlaceRight
  | PlaceTop
  | PlaceBottom
  | PlaceAbsolute (Point Double)
  deriving (Show, Eq, Generic)

-- | textifier
placeText :: Place -> Text
placeText p =
  case p of
    PlaceTop -> "Top"
    PlaceBottom -> "Bottom"
    PlaceLeft -> "Left"
    PlaceRight -> "Right"
    PlaceAbsolute _ -> "Absolute"

-- | axis options
--
-- >>> defaultAxisOptions
-- AxisOptions {abar = Just (Bar {rstyle = RectStyle {borderSize = 0.0, borderColor = RGBA 0.50 0.50 0.50 1.00, color = RGBA 0.50 0.50 0.50 1.00}, wid = 5.0e-3, buff = 1.0e-2}), adjust = Just (Adjustments {maxXRatio = 8.0e-2, maxYRatio = 6.0e-2, angledRatio = 0.12, allowDiagonal = True}), atick = Tick {tstyle = TickRound (FormatComma 0) 8 TickExtend, gtick = Just (GlyphStyle {size = 3.0e-2, color = RGBA 0.50 0.50 0.50 1.00, borderColor = RGBA 0.50 0.50 0.50 1.00, borderSize = 5.0e-3, shape = VLineGlyph 5.0e-3, rotation = Nothing, translate = Nothing},1.25e-2), ttick = Just (TextStyle {size = 5.0e-2, color = RGBA 0.50 0.50 0.50 1.00, anchor = AnchorMiddle, hsize = 0.5, vsize = 1.45, nudge1 = -0.2, rotation = Nothing, translate = Nothing, hasMathjax = False},1.5e-2), ltick = Just (LineStyle {width = 5.0e-3, color = RGBA 0.50 0.50 0.50 0.05},0.0)}, place = PlaceBottom}
--
data AxisOptions
  = AxisOptions
      { abar :: Maybe Bar,
        adjust :: Maybe Adjustments,
        atick :: Tick,
        place :: Place
      }
  deriving (Eq, Show, Generic)

-- | The official axis
defaultAxisOptions :: AxisOptions
defaultAxisOptions = AxisOptions (Just defaultBar) (Just defaultAdjustments) defaultTick PlaceBottom

-- | The bar on an axis representing the x or y plane.
--
-- >>> defaultBar
-- Bar {rstyle = RectStyle {borderSize = 0.0, borderColor = RGBA 0.50 0.50 0.50 1.00, color = RGBA 0.50 0.50 0.50 1.00}, wid = 5.0e-3, buff = 1.0e-2}
--
data Bar
  = Bar
      { rstyle :: RectStyle,
        wid :: Double,
        buff :: Double
      }
  deriving (Show, Eq, Generic)

-- | The official axis bar
defaultBar :: Bar
defaultBar = Bar (RectStyle 0 (fromRGB (grayscale 0.5) 1) (fromRGB (grayscale 0.5) 1)) 0.005 0.01

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

-- | The official hud title
defaultTitle :: Text -> Title
defaultTitle txt =
  Title
    txt
    ( (#size .~ 0.12)
        . (#color .~ colorText)
        $ defaultTextStyle
    )
    PlaceTop
    AnchorMiddle
    0.04

-- | xy coordinate markings
--
-- >>> defaultTick
-- Tick {tstyle = TickRound (FormatComma 0) 8 TickExtend, gtick = Just (GlyphStyle {size = 3.0e-2, color = RGBA 0.50 0.50 0.50 1.00, borderColor = RGBA 0.50 0.50 0.50 1.00, borderSize = 5.0e-3, shape = VLineGlyph 5.0e-3, rotation = Nothing, translate = Nothing},1.25e-2), ttick = Just (TextStyle {size = 5.0e-2, color = RGBA 0.50 0.50 0.50 1.00, anchor = AnchorMiddle, hsize = 0.5, vsize = 1.45, nudge1 = -0.2, rotation = Nothing, translate = Nothing, hasMathjax = False},1.5e-2), ltick = Just (LineStyle {width = 5.0e-3, color = RGBA 0.50 0.50 0.50 0.05},0.0)}
--
data Tick
  = Tick
      { tstyle :: TickStyle,
        gtick :: Maybe (GlyphStyle, Double),
        ttick :: Maybe (TextStyle, Double),
        ltick :: Maybe (LineStyle, Double)
      }
  deriving (Show, Eq, Generic)

-- | The official glyph tick
defaultGlyphTick :: GlyphStyle
defaultGlyphTick =
  defaultGlyphStyle
    & #borderSize .~ 0.005
    & #borderColor .~ fromRGB (grayscale 0.5) 1
    & #color .~ fromRGB (grayscale 0.5) 1
    & #shape .~ VLineGlyph 0.005

-- | The official text tick
defaultTextTick :: TextStyle
defaultTextTick =
  defaultTextStyle & #size .~ 0.05 & #color .~ fromRGB (grayscale 0.5) 1

-- | The official line tick
defaultLineTick :: LineStyle
defaultLineTick =
  defaultLineStyle
    & #color .~ fromRGB (grayscale 0.5) 0.05
    & #width .~ 5.0e-3

-- | The official tick
defaultTick :: Tick
defaultTick =
  Tick
    defaultTickStyle
    (Just (defaultGlyphTick, 0.0125))
    (Just (defaultTextTick, 0.015))
    (Just (defaultLineTick, 0))

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

-- | The official tick style
defaultTickStyle :: TickStyle
defaultTickStyle = TickRound (FormatComma 0) 8 TickExtend

-- | textifier
tickStyleText :: TickStyle -> Text
tickStyleText TickNone = "TickNone"
tickStyleText TickLabels {} = "TickLabels"
tickStyleText TickRound {} = "TickRound"
tickStyleText TickExact {} = "TickExact"
tickStyleText TickPlaced {} = "TickPlaced"

-- | Whether Ticks are allowed to extend the data range
data TickExtend = TickExtend | NoTickExtend deriving (Eq, Show, Generic)

-- | options for prettifying axis decorations
--
-- >>> defaultAdjustments
-- Adjustments {maxXRatio = 8.0e-2, maxYRatio = 6.0e-2, angledRatio = 0.12, allowDiagonal = True}
--
data Adjustments
  = Adjustments
      { maxXRatio :: Double,
        maxYRatio :: Double,
        angledRatio :: Double,
        allowDiagonal :: Bool
      }
  deriving (Show, Eq, Generic)

-- | The official hud adjustments.
defaultAdjustments :: Adjustments
defaultAdjustments = Adjustments 0.08 0.06 0.12 True

-- You're all Legends!

-- | Legend options
--
-- >>> defaultLegendOptions
-- LegendOptions {lsize = 0.1, vgap = 0.2, hgap = 0.1, ltext = TextStyle {size = 8.0e-2, color = RGBA 0.20 0.20 0.20 1.00, anchor = AnchorMiddle, hsize = 0.5, vsize = 1.45, nudge1 = -0.2, rotation = Nothing, translate = Nothing, hasMathjax = False}, lmax = 10, innerPad = 0.1, outerPad = 0.1, legendFrame = Just (RectStyle {borderSize = 2.0e-2, borderColor = RGBA 0.50 0.50 0.50 1.00, color = RGBA 1.00 1.00 1.00 1.00}), lplace = PlaceBottom, lscale = 0.2}
--
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

-- | The official legend options
defaultLegendOptions :: LegendOptions
defaultLegendOptions =
  LegendOptions
    0.1
    0.2
    0.1
    ( defaultTextStyle
        & #size .~ 0.08
    )
    10
    0.1
    0.1
    (Just (RectStyle 0.02 (fromRGB (grayscale 0.5) 1) white))
    PlaceBottom
    0.2

-- | snatching Colour as the library color representation.
newtype Colour = Colour' { color' :: Color (Alpha RGB) Double } deriving (Eq, Generic)

-- | Constructor.
pattern Colour :: Double -> Double -> Double -> Double -> Colour
pattern Colour r g b a = Colour' (ColorRGBA r g b a)
{-# COMPLETE Colour #-}

instance Show Colour where
  show (Colour r g b a) =
    Text.unpack $ "RGBA " <>
    fixed 2 r <> " " <>
    fixed 2 g <> " " <> 
    fixed 2 b <> " " <> 
    fixed 2 a

-- | get opacity
opac :: Colour -> Double
opac c = getAlpha (color' c)

-- | set opacity
setOpac :: Double -> Colour -> Colour
setOpac o (Colour r g b _) = Colour r g b o

-- |
fromRGB :: Color RGB Double -> Double -> Colour
fromRGB (ColorRGB r b g) o = Colour' $ ColorRGBA r b g o

-- |
hex :: Colour -> Text
hex c = toHex c

-- | interpolate between 2 colors
blend :: Double -> Colour -> Colour -> Colour
blend c (Colour r g b a) (Colour r' g' b' a') = Colour r'' g'' b'' a''
  where
    r'' = r + c * (r' - r)
    g'' = g + c * (g' - g)
    b'' = b + c * (b' - b)
    a'' = a + c * (a' - a)

-- |
parseHex :: A.Parser (Color RGB Double)
parseHex =
  (fmap toDouble
    . ( \((r, g), b) ->
            ColorRGB (fromIntegral r) (fromIntegral g) (fromIntegral b) :: Color RGB Word8
        )
    . (\(f, b) -> (f `divMod` (256 :: Int), b))
    . (`divMod` 256)
    <$> (A.string "#" *> A.hexadecimal))

-- |
fromHex :: Text -> Either Text (Color RGB Double)
fromHex = first pack . A.parseOnly parseHex

-- |
unsafeFromHex :: Text -> Color RGB Double
unsafeFromHex t = either (const (ColorRGB 0 0 0)) id $ A.parseOnly parseHex t

-- | convert from 'Colour' to #xxxxxx
toHex :: Colour -> Text
toHex c =
  "#"
    <> Text.justifyRight 2 '0' (hex' r)
    <> Text.justifyRight 2 '0' (hex' g)
    <> Text.justifyRight 2 '0' (hex' b)
  where
    (ColorRGBA r g b _) = toWord8 <$> color' c

-- |
hex' :: (FromInteger a, ToIntegral a Integer, Integral a, Ord a, Subtractive a) => a -> Text
hex' i
  | i < 0 = "-" <> go (- i)
  | otherwise = go i
  where
    go n
      | n < 16 = hexDigit n
      | otherwise = go (n `quot` 16) <> hexDigit (n `rem` 16)

-- |
hexDigit :: (Ord a, FromInteger a, ToIntegral a Integer) => a -> Text
hexDigit n
  | n <= 9 = Text.singleton P.$! i2d (fromIntegral n)
  | otherwise = Text.singleton P.$! toEnum (fromIntegral n + 87)

-- |
i2d :: Int -> Char
i2d i = (chr (ord '0' + i))

-- | some RGB colors to work with
palette :: [Color RGB Double]
palette = unsafeFromHex <$> ["#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#6a3d9a","#ffff99","#b15928"]

-- | some RGBA colors
palette1 :: [Colour]
palette1 = (\c -> fromRGB c 1) <$> palette 

-- | gray with 1 opacity
grayscale :: Double -> Color RGB Double
grayscale n = ColorRGB n n n

-- | standard text color
colorText :: Colour
colorText = fromRGB (grayscale 0.2) 1

-- |
black :: Colour
black = fromRGB (grayscale 0) 1

-- |
white :: Colour
white = fromRGB (grayscale 1) 1

-- |
transparent :: Colour
transparent = Colour 0 0 0 0

-- | Number formatting options.
--
-- >>> defaultFormatN
-- FormatComma 2
--
data FormatN
  = FormatFixed Int
  | FormatComma Int
  | FormatExpt Int
  | FormatDollar
  | FormatPercent Int
  | FormatNone
  deriving (Eq, Show, Generic)

-- | The official format
defaultFormatN :: FormatN
defaultFormatN = FormatComma 2

-- | textifier
fromFormatN :: (IsString s) => FormatN -> s
fromFormatN (FormatFixed _) = "Fixed"
fromFormatN (FormatComma _) = "Comma"
fromFormatN (FormatExpt _) = "Expt"
fromFormatN FormatDollar = "Dollar"
fromFormatN (FormatPercent _) = "Percent"
fromFormatN FormatNone = "None"

-- | readifier
toFormatN :: (Eq s, IsString s) => s -> Int -> FormatN
toFormatN "Fixed" n = FormatFixed n
toFormatN "Comma" n = FormatComma n
toFormatN "Expt" n = FormatExpt n
toFormatN "Dollar" _ = FormatDollar
toFormatN "Percent" n = FormatPercent n
toFormatN "None" _ = FormatNone
toFormatN _ _ = FormatNone

-- | to x decimal places
fixed :: Int -> Double -> Text
fixed x n = pack $ formatScientific Fixed (Just x) (fromFloatDigits n)

-- | comma format
comma :: Int -> Double -> Text
comma n a
  | a < 1000 = fixed n a
  | otherwise = go (fromInteger $ floor a) ""
  where
    go :: Int -> Text -> Text
    go x t
      | x < 0 = "-" <> go (- x) ""
      | x < 1000 = pack (show x) <> t
      | otherwise =
        let (d, m) = divMod x 1000
         in go d ("," <> pack (show' m))
      where
        show' n' = let x' = show n' in (replicate (3 - length x') '0' <> x')

-- | scientific exponential
expt :: Int -> Double -> Text
expt x n = pack $ formatScientific Exponent (Just x) (fromFloatDigits n)

-- | dollars and cents
dollar :: Double -> Text
dollar = ("$" <>) . comma 2

-- | fixed percent
percent :: Int -> Double -> Text
percent x n = (<> "%") $ fixed x (100 * n)

-- | make text
formatN :: FormatN -> Double -> Text
formatN (FormatFixed n) x = fixed n x
formatN (FormatComma n) x = comma n x
formatN (FormatExpt n) x = expt n x
formatN FormatDollar x = dollar x
formatN (FormatPercent n) x = percent n x
formatN FormatNone x = pack (show x)

-- | Provide formatted text for a list of numbers so that they are just distinguished.  'precision commas 2 ticks' means give the tick labels as much precision as is needed for them to be distinguished, but with at least 2 significant figures, and format Integers with commas.
precision :: (Int -> Double -> Text) -> Int -> [Double] -> [Text]
precision f n0 xs =
  precLoop f (fromIntegral n0) xs
  where
    precLoop f' n xs' =
      let s = f' n <$> xs'
       in if s == nub s || n > 4
            then s
            else precLoop f' (n + 1) xs'

-- | textifier
formatNs :: FormatN -> [Double] -> [Text]
formatNs (FormatFixed n) xs = precision fixed n xs
formatNs (FormatComma n) xs = precision comma n xs
formatNs (FormatExpt n) xs = precision expt n xs
formatNs FormatDollar xs = precision (const dollar) 2 xs
formatNs (FormatPercent n) xs = precision percent n xs
formatNs FormatNone xs = pack . show <$> xs

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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Chart.Types
  ( Chart (..),
    Annotation (..),
    annotationText,
    blank,
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
    FormatN (..),
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
    -- $core
    projectTo,
    projectSpots,
    projectSpotsWith,
    dataBox,
    toAspect,
    scaleAnn,
    defRect,
    defRectS,
    moveChart,
    -- $hud
    runHudWith,
    runHud,
    makeHud,
    freezeTicks,
    flipAxis,
    canvas,
    title,
    tick,
    adjustTick,
    makeTickDates,
    makeTickDatesContinuous,
    legendHud,
    legendEntry,
    legendChart,
    legendFromChart,
    -- $svg
    svg,
    svgt,
    chartDef,
    chartDefs,
    styleBox,
    styleBoxes,
    noStyleBoxes,
    styleBoxText,
    styleBoxGlyph,
    padChart,
    frameChart,
    hori,
    vert,
    stack,
    addChartBox,
    addChartBoxes,
  )
where

import qualified Control.Foldl as L
import Control.Lens
import qualified Data.Attoparsec.Text as A
import Data.Generics.Labels ()
import Data.List ((!!), nub)
import Data.Scientific
import qualified Data.Text as Text
import Data.Time
import Graphics.Color.Model
import qualified Lucid
import Lucid (class_, height_, id_, term, toHtmlRaw, width_, with)
import Lucid.Base (makeXmlElementNoEnd)
import qualified Lucid.Base as Lucid
import NumHask.Prelude
import NumHask.Space as NH hiding (Element)
import Text.HTML.TagSoup hiding (Attribute)
import qualified Prelude as P

-- $setup
--
-- >>> :set -XOverloadedLabels
-- >>> :set -XNoImplicitPrelude
-- >>> -- import NumHask.Prelude
-- >>> import Control.Lens
-- >>> import Chart.Render

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

blank :: [Chart Double]
blank = [Chart BlankA []]

-- | Rectangle styling
--
-- >>> defaultRectStyle
-- RectStyle {borderSize = 1.0e-2, borderColor = RGBA 0.12 0.47 0.71 0.80, color = RGBA 0.12 0.47 0.71 0.30}
--
-- >writeCharts "other/unit.svg" [Chart (RectA defaultRectStyle) [SpotRect (unitRect::Rect Double)]]
--
-- ![unit example](other/unit.svg)
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
blob :: Colour -> RectStyle
blob = RectStyle 0 transparent

-- | transparent rect
--
-- >>> clear
-- RectStyle {borderSize = 0.0, borderColor = RGBA 0.00 0.00 0.00 0.00, color = RGBA 0.00 0.00 0.00 0.00}
clear :: RectStyle
clear = RectStyle 0 transparent transparent

-- | transparent rectangle, with border
--
-- >>> border 0.01 transparent
-- RectStyle {borderSize = 1.0e-2, borderColor = RGBA 0.00 0.00 0.00 0.00, color = RGBA 0.00 0.00 0.00 0.00}
border :: Double -> Colour -> RectStyle
border s c = RectStyle s c transparent

-- | Text styling
--
-- >>> defaultTextStyle
-- TextStyle {size = 8.0e-2, color = RGBA 0.20 0.20 0.20 1.00, anchor = AnchorMiddle, hsize = 0.5, vsize = 1.45, nudge1 = -0.2, rotation = Nothing, translate = Nothing, hasMathjax = False}
--
-- >>> let t = zipWith (\x y -> Chart (TextA (defaultTextStyle & (#size .~ (0.05 :: Double))) [x]) [SpotPoint y]) (fmap Text.singleton ['a' .. 'y']) [Point (sin (x * 0.1)) x | x <- [0 .. 25]]
--
-- > writeCharts "other/text.svg" t
--
-- ![text example](other/text.svg)
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
-- ![glyph example](other/glyph.svg)
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
-- ![pixel example](other/pixel.svg)
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
-- > writeChartsWith "other/svgoptions.svg" (defaultSvgOptions & #svgAspect .~ ManualAspect 0.7) lines
--
-- ![svgoptions example](other/svgoptions.svg)
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
-- > writeHudOptionsChart "other/hud.svg" defaultSvgOptions defaultHudOptions [] []
--
-- ![hud example](other/hud.svg)
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
-- ![legend example](other/legend.svg)
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
newtype Colour = Colour' {color' :: Color (Alpha RGB) Double} deriving (Eq, Generic)

-- | Constructor.
pattern Colour :: Double -> Double -> Double -> Double -> Colour
pattern Colour r g b a = Colour' (ColorRGBA r g b a)

{-# COMPLETE Colour #-}

instance Show Colour where
  show (Colour r g b a) =
    Text.unpack $
      "RGBA "
        <> fixed 2 r
        <> " "
        <> fixed 2 g
        <> " "
        <> fixed 2 b
        <> " "
        <> fixed 2 a

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
  fmap toDouble
      . ( \((r, g), b) ->
            ColorRGB (fromIntegral r) (fromIntegral g) (fromIntegral b) :: Color RGB Word8
        )
      . (\(f, b) -> (f `divMod` (256 :: Int), b))
      . (`divMod` 256)
      <$> (A.string "#" *> A.hexadecimal)

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
i2d i = chr (ord '0' + i)

-- | some RGB colors to work with
palette :: [Color RGB Double]
palette = unsafeFromHex <$> ["#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#ffff99", "#b15928"]

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

-- | project a Spot from one Rect to another, preserving relative position.
--
-- >>> projectOn unitRect (Rect 0 1 0 1) (SpotPoint $ Point 0 0)
-- SpotPoint Point -0.5 -0.5
projectOn :: (Ord a, Fractional a) => Rect a -> Rect a -> Spot a -> Spot a
projectOn new old@(Rect x z y w) po@(SpotPoint (Point px py))
  | x == z && y == w = po
  | x == z = SpotPoint (Point px py')
  | y == w = SpotPoint (Point px' py)
  | otherwise = SpotPoint (Point px' py')
  where
    (Point px' py') = project old new (toPoint po)
projectOn new old@(Rect x z y w) ao@(SpotRect (Rect ox oz oy ow))
  | x == z && y == w = ao
  | x == z = SpotRect (Rect ox oz ny nw)
  | y == w = SpotRect (Rect nx nz oy ow)
  | otherwise = SpotRect a
  where
    a@(Rect nx nz ny nw) = projectRect old new (toRect ao)

-- | project a [Spot a] from it's folded space to the given area
--
-- >>> projectTo unitRect (SpotPoint <$> zipWith Point [0..2] [0..2])
-- [SpotPoint Point -0.5 -0.5,SpotPoint Point 0.0 0.0,SpotPoint Point 0.5 0.5]
projectTo :: (Ord a, Fractional a) => Rect a -> [Spot a] -> [Spot a]
projectTo _ [] = []
projectTo vb (x : xs) = projectOn vb (toRect $ sconcat (x :| xs)) <$> (x : xs)

defRect :: (Fractional a) => Maybe (Rect a) -> Rect a
defRect = fromMaybe unitRect

defRectS :: (Subtractive a, Eq a, FromRational a, Fractional a) => Maybe (Rect a) -> Rect a
defRectS r = maybe unitRect singletonUnit r
  where
    singletonUnit :: (Subtractive a, Eq a, FromRational a) => Rect a -> Rect a
    singletonUnit (Rect x z y w)
      | x == z && y == w = Rect (x - 0.5) (x + 0.5) (y - 0.5) (y + 0.5)
      | x == z = Rect (x - 0.5) (x + 0.5) y w
      | y == w = Rect x z (y - 0.5) (y + 0.5)
      | otherwise = Rect x z y w

projectSpots :: (Ord a, Fractional a) => Rect a -> [Chart a] -> [Chart a]
projectSpots a cs = cs'
  where
    xss = projectTo2 a (spots <$> cs)
    ss = annotation <$> cs
    cs' = zipWith Chart ss xss
    projectTo2 vb xss =
      fmap
        ( maybe
            id
            (projectOn vb)
            (fold $ foldRect . fmap toRect <$> xss)
        )
        <$> xss

projectSpotsWith :: (Ord a, Fractional a) => Rect a -> Rect a -> [Chart a] -> [Chart a]
projectSpotsWith new old cs = cs'
  where
    xss = fmap (projectOn new old) . spots <$> cs
    ss = annotation <$> cs
    cs' = zipWith Chart ss xss

toAspect :: (Divisive a, Subtractive a) => Rect a -> a
toAspect (Rect x z y w) = (z - x) / (w - y)

-- |
dataBox :: (Ord a) => [Chart a] -> Maybe (Rect a)
dataBox cs = foldRect . mconcat $ fmap toRect <$> (spots <$> cs)

scaleAnn :: Double -> Annotation -> Annotation
scaleAnn x (LineA a) = LineA $ a & #width %~ (* x)
scaleAnn x (RectA a) = RectA $ a & #borderSize %~ (* x)
scaleAnn x (TextA a txs) = TextA (a & #size %~ (* x)) txs
scaleAnn x (GlyphA a) = GlyphA (a & #size %~ (* x))
scaleAnn x (PixelA a) = PixelA $ a & #pixelRectStyle . #borderSize %~ (* x)
scaleAnn _ BlankA = BlankA

moveChart :: (Ord a, Fractional a) => Spot a -> [Chart a] -> [Chart a]
moveChart sp cs = fmap (#spots %~ fmap (sp P.+)) cs

-- | pattern for SP x y
pattern SP' :: a -> a -> Spot a
pattern SP' a b = SpotPoint (Point a b)

{-# COMPLETE SP' #-}

-- | pattern for SA lowerx upperx lowery uppery
pattern SR' :: a -> a -> a -> a -> Spot a
pattern SR' a b c d = SpotRect (Rect a b c d)

{-# COMPLETE SR' #-}

-- | combine huds and charts to form a new Chart using the supplied
-- initial canvas and data dimensions.
-- Note that chart data is transformed by this computation.
-- used once in makePixelTick
runHudWith ::
  -- | initial canvas dimension
  Rect Double ->
  -- | initial data dimension
  Rect Double ->
  -- | huds to add
  [Hud Double] ->
  -- | underlying chart
  [Chart Double] ->
  -- | chart list
  [Chart Double]
runHudWith ca xs hs cs =
  flip evalState (ChartDims ca' da' xs) $
    (unhud $ mconcat hs) cs'
  where
    da' = defRect $ dataBox cs'
    ca' = defRect $ styleBoxes cs'
    cs' = projectSpotsWith ca xs cs

-- | Combine huds and charts to form a new [Chart] using the supplied canvas and the actual data dimension.
-- Note that the original chart data are transformed and irrevocably lost by this computation.
-- used once in renderHudChart
runHud :: Rect Double -> [Hud Double] -> [Chart Double] -> [Chart Double]
runHud ca hs cs = runHudWith ca (defRectS $ dataBox cs) hs cs

-- | Make huds from a HudOptions
-- Some huds, such as the creation of tick values, can extend the data dimension of a chart, so we also return a blank chart with the new data dimension.
-- The complexity internally is due to the creation of ticks and, specifically, gridSensible, which is not idempotent. As a result, a tick calculation that does extends the data area, can then lead to new tick values when applying TickRound etc.
makeHud :: Rect Double -> HudOptions -> ([Hud Double], [Chart Double])
makeHud xs cfg =
  (haxes <> [can] <> titles <> [l], [xsext])
  where
    can = maybe mempty (\x -> canvas x) (cfg ^. #hudCanvas)
    titles = title <$> (cfg ^. #hudTitles)
    newticks = (\a -> freezeTicks (a ^. #place) xs (a ^. #atick . #tstyle)) <$> (cfg ^. #hudAxes)
    axes' = zipWith (\c t -> c & #atick . #tstyle .~ fst t) (cfg ^. #hudAxes) newticks
    xsext = Chart BlankA (SpotRect <$> catMaybes (snd <$> newticks))
    haxes = (\x -> maybe mempty (\a -> bar (x ^. #place) a) (x ^. #abar) <> adjustedTickHud x) <$> axes'
    l = maybe mempty (\(lo, ats) -> legendHud lo (legendChart ats lo)) (cfg ^. #hudLegend)

-- convert TickRound to TickPlaced
freezeTicks :: Place -> Rect Double -> TickStyle -> (TickStyle, Maybe (Rect Double))
freezeTicks pl xs' ts@TickRound {} = maybe (ts, Nothing) (\x -> (TickPlaced (zip ps ls), Just x)) ((\x -> replaceRange pl x xs') <$> ext)
  where
    (TickComponents ps ls ext) = makeTicks ts (placeRange pl xs')
    replaceRange :: Place -> Range Double -> Rect Double -> Rect Double
    replaceRange pl' (Range a0 a1) (Rect x z y w) = case pl' of
      PlaceRight -> Rect x z a0 a1
      PlaceLeft -> Rect x z a0 a1
      _ -> Rect a0 a1 y w
freezeTicks _ _ ts = (ts, Nothing)

-- |
flipAxis :: AxisOptions -> AxisOptions
flipAxis ac = case ac ^. #place of
  PlaceBottom -> ac & #place .~ PlaceLeft
  PlaceTop -> ac & #place .~ PlaceRight
  PlaceLeft -> ac & #place .~ PlaceBottom
  PlaceRight -> ac & #place .~ PlaceTop
  PlaceAbsolute _ -> ac

addToRect :: (Ord a) => Rect a -> Maybe (Rect a) -> Rect a
addToRect r r' = sconcat $ r :| maybeToList r'

canvas :: (Monad m) => RectStyle -> HudT m Double
canvas s = Hud $ \cs -> do
  a <- use #canvasDim
  let c = Chart (RectA s) [SpotRect a]
  #canvasDim .= addToRect a (styleBox c)
  pure $ c : cs

bar_ :: Place -> Bar -> Rect Double -> Rect Double -> Chart Double
bar_ pl b (Rect x z y w) (Rect x' z' y' w') =
  case pl of
    PlaceTop ->
      Chart
        (RectA (rstyle b))
        [ SR'
            x
            z
            (w' + b ^. #buff)
            (w' + b ^. #buff + b ^. #wid)
        ]
    PlaceBottom ->
      Chart
        (RectA (rstyle b))
        [ SR'
            x
            z
            (y' - b ^. #wid - b ^. #buff)
            (y' - b ^. #buff)
        ]
    PlaceLeft ->
      Chart
        (RectA (rstyle b))
        [ SR'
            (x' - b ^. #wid - b ^. #buff)
            (x' - b ^. #buff)
            y
            w
        ]
    PlaceRight ->
      Chart
        (RectA (rstyle b))
        [ SR'
            (z' + (b ^. #buff))
            (z' + (b ^. #buff) + (b ^. #wid))
            y
            w
        ]
    PlaceAbsolute (Point x'' _) ->
      Chart
        (RectA (rstyle b))
        [ SR'
            (x'' + (b ^. #buff))
            (x'' + (b ^. #buff) + (b ^. #wid))
            y
            w
        ]

bar :: (Monad m) => Place -> Bar -> HudT m Double
bar pl b = Hud $ \cs -> do
  da <- use #chartDim
  ca <- use #canvasDim
  let c = bar_ pl b ca da
  #chartDim .= addChartBox c da
  pure $ c : cs

title_ :: Title -> Rect Double -> Chart Double
title_ t a =
  Chart
    ( TextA
        ( style'
            & #translate ?~ (realToFrac <$> (placePos' a P.+ alignPos a))
            & #rotation ?~ rot
        )
        [t ^. #text]
    )
    [SP' 0 0]
  where
    style'
      | t ^. #anchor == AnchorStart =
        #anchor .~ AnchorStart $ t ^. #style
      | t ^. #anchor == AnchorEnd =
        #anchor .~ AnchorEnd $ t ^. #style
      | otherwise = t ^. #style
    rot
      | t ^. #place == PlaceRight = 90.0
      | t ^. #place == PlaceLeft = -90.0
      | otherwise = 0
    placePos' (Rect x z y w) = case t ^. #place of
      PlaceTop -> Point ((x + z) / 2.0) (w + (t ^. #buff))
      PlaceBottom ->
        Point
          ((x + z) / 2.0)
          ( y - (t ^. #buff)
              - 0.5
              * realToFrac (t ^. #style . #vsize)
              * realToFrac (t ^. #style . #size)
          )
      PlaceLeft -> Point (x - (t ^. #buff)) ((y + w) / 2.0)
      PlaceRight -> Point (z + (t ^. #buff)) ((y + w) / 2.0)
      PlaceAbsolute p -> p
    alignPos (Rect x z y w)
      | t ^. #anchor == AnchorStart
          && t ^. #place `elem` [PlaceTop, PlaceBottom] =
        Point ((x - z) / 2.0) 0.0
      | t ^. #anchor == AnchorStart
          && t ^. #place == PlaceLeft =
        Point 0.0 ((y - w) / 2.0)
      | t ^. #anchor == AnchorStart
          && t ^. #place == PlaceRight =
        Point 0.0 ((w - y) / 2.0)
      | t ^. #anchor == AnchorEnd
          && t ^. #place `elem` [PlaceTop, PlaceBottom] =
        Point ((- x + z) / 2.0) 0.0
      | t ^. #anchor == AnchorEnd
          && t ^. #place == PlaceLeft =
        Point 0.0 ((- y + w) / 2.0)
      | t ^. #anchor == AnchorEnd
          && t ^. #place == PlaceRight =
        Point 0.0 ((y - w) / 2.0)
      | otherwise = Point 0.0 0.0

-- | Add a title to a chart. The logic used to work out placement is flawed due to being able to freely specify text rotation.  It works for specific rotations (Top, Bottom at 0, Left at 90, Right @ 270)
title :: (Monad m) => Title -> HudT m Double
title t = Hud $ \cs -> do
  ca <- use #chartDim
  let c = title_ t ca
  #chartDim .= addChartBox c ca
  pure $ c : cs

placePos :: Place -> Double -> Rect Double -> Point Double
placePos pl b (Rect x z y w) = case pl of
  PlaceTop -> Point 0 (w + b)
  PlaceBottom -> Point 0 (y - b)
  PlaceLeft -> Point (x - b) 0
  PlaceRight -> Point (z + b) 0
  PlaceAbsolute p -> p

placeRot :: Place -> Maybe Double
placeRot pl = case pl of
  PlaceRight -> Just (-90.0)
  PlaceLeft -> Just (-90.0)
  _ -> Nothing

textPos :: Place -> TextStyle -> Double -> Point Double
textPos pl tt b = case pl of
  PlaceTop -> Point 0 b
  PlaceBottom -> Point 0 (- b - 0.5 * realToFrac (tt ^. #vsize) * realToFrac (tt ^. #size))
  PlaceLeft ->
    Point
      (- b)
      (realToFrac (tt ^. #nudge1) * realToFrac (tt ^. #vsize) * realToFrac (tt ^. #size))
  PlaceRight ->
    Point
      b
      (realToFrac (tt ^. #nudge1) * realToFrac (tt ^. #vsize) * realToFrac (tt ^. #size))
  PlaceAbsolute p -> p

placeRange :: Place -> Rect Double -> Range Double
placeRange pl (Rect x z y w) = case pl of
  PlaceRight -> Range y w
  PlaceLeft -> Range y w
  _ -> Range x z

placeOrigin :: Place -> Double -> Point Double
placeOrigin pl x
  | pl `elem` [PlaceTop, PlaceBottom] = Point x 0
  | otherwise = Point 0 x

placeTextAnchor :: Place -> (TextStyle -> TextStyle)
placeTextAnchor pl
  | pl == PlaceLeft = #anchor .~ AnchorEnd
  | pl == PlaceRight = #anchor .~ AnchorStart
  | otherwise = id

placeGridLines :: Place -> Rect Double -> Double -> Double -> [Spot Double]
placeGridLines pl (Rect x z y w) a b
  | pl `elem` [PlaceTop, PlaceBottom] = [SP' a (y - b), SP' a (w + b)]
  | otherwise = [SP' (x - b) a, SP' (z + b) a]

-- | compute tick values and labels given options, ranges and formatting
ticksR :: TickStyle -> Range Double -> Range Double -> [(Double, Text)]
ticksR s d r =
  case s of
    TickNone -> []
    TickRound f n e -> zip (project r d <$> ticks0) (formatNs f ticks0)
      where
        ticks0 = gridSensible OuterPos (e == NoTickExtend) r (fromIntegral n :: Integer)
    TickExact f n -> zip (project r d <$> ticks0) (formatNs f ticks0)
      where
        ticks0 = grid OuterPos r n
    TickLabels ls ->
      zip
        ( project (Range 0 (fromIntegral $ length ls)) d
            <$> ((\x -> x - 0.5) . fromIntegral <$> [1 .. length ls])
        )
        ls
    TickPlaced xs -> zip (project r d . fst <$> xs) (snd <$> xs)

data TickComponents
  = TickComponents
      { positions :: [Double],
        labels :: [Text],
        extension :: Maybe (Range Double)
      }
  deriving (Eq, Show, Generic)

-- | compute tick components given style, ranges and formatting
makeTicks :: TickStyle -> Range Double -> TickComponents
makeTicks s r =
  case s of
    TickNone -> TickComponents [] [] Nothing
    TickRound f n e ->
      TickComponents
        ticks0
        (formatNs f ticks0)
        (bool (Just $ space1 ticks0) Nothing (e == NoTickExtend))
      where
        ticks0 = gridSensible OuterPos (e == NoTickExtend) r (fromIntegral n :: Integer)
    TickExact f n -> TickComponents ticks0 (formatNs f ticks0) Nothing
      where
        ticks0 = grid OuterPos r n
    TickLabels ls ->
      TickComponents
        ( project (Range 0 (fromIntegral $ length ls)) r
            <$> ((\x -> x - 0.5) . fromIntegral <$> [1 .. length ls])
        )
        ls
        Nothing
    TickPlaced xs -> TickComponents (fst <$> xs) (snd <$> xs) Nothing

-- | compute tick values given placement, canvas dimension & data range
ticksPlaced :: TickStyle -> Place -> Rect Double -> Rect Double -> TickComponents
ticksPlaced ts pl d xs = TickComponents (project (placeRange pl xs) (placeRange pl d) <$> ps) ls ext
  where
    (TickComponents ps ls ext) = makeTicks ts (placeRange pl xs)

tickGlyph_ :: Place -> (GlyphStyle, Double) -> TickStyle -> Rect Double -> Rect Double -> Rect Double -> Chart Double
tickGlyph_ pl (g, b) ts ca da xs =
  Chart
    (GlyphA (g & #rotation .~ (realToFrac <$> placeRot pl)))
    ( SpotPoint . (placePos pl b ca P.+) . placeOrigin pl
        <$> positions
          (ticksPlaced ts pl da xs)
    )

-- | aka marks
tickGlyph ::
  (Monad m) =>
  Place ->
  (GlyphStyle, Double) ->
  TickStyle ->
  HudT m Double
tickGlyph pl (g, b) ts = Hud $ \cs -> do
  a <- use #chartDim
  d <- use #canvasDim
  xs <- use #dataDim
  let c = tickGlyph_ pl (g, b) ts a d xs
  #chartDim .= addToRect a (styleBox c)
  pure $ c : cs

tickText_ ::
  Place ->
  (TextStyle, Double) ->
  TickStyle ->
  Rect Double ->
  Rect Double ->
  Rect Double ->
  [Chart Double]
tickText_ pl (txts, b) ts ca da xs =
  zipWith
    ( \txt sp ->
        Chart
          ( TextA
              (placeTextAnchor pl txts)
              [txt]
          )
          [SpotPoint sp]
    )
    (labels $ ticksPlaced ts pl da xs)
    ( (placePos pl b ca P.+ textPos pl txts b P.+) . placeOrigin pl
        <$> positions (ticksPlaced ts pl da xs)
    )

-- | aka tick labels
tickText ::
  (Monad m) =>
  Place ->
  (TextStyle, Double) ->
  TickStyle ->
  HudT m Double
tickText pl (txts, b) ts = Hud $ \cs -> do
  ca <- use #chartDim
  da <- use #canvasDim
  xs <- use #dataDim
  let c = tickText_ pl (txts, b) ts ca da xs
  #chartDim .= addChartBoxes c ca
  pure $ c <> cs

-- | aka grid lines
tickLine ::
  (Monad m) =>
  Place ->
  (LineStyle, Double) ->
  TickStyle ->
  HudT m Double
tickLine pl (ls, b) ts = Hud $ \cs -> do
  da <- use #canvasDim
  xs <- use #dataDim
  let c =
        Chart (LineA ls) . (\x -> placeGridLines pl da x b)
          <$> positions (ticksPlaced ts pl da xs)
  #chartDim %= addChartBoxes c
  pure $ c <> cs

-- | Create tick glyphs (marks), lines (grid) and text (labels)
tick ::
  (Monad m) =>
  Place ->
  Tick ->
  HudT m Double
tick pl t =
  maybe mempty (\x -> tickGlyph pl x (t ^. #tstyle)) (t ^. #gtick)
    <> maybe mempty (\x -> tickText pl x (t ^. #tstyle)) (t ^. #ttick)
    <> maybe mempty (\x -> tickLine pl x (t ^. #tstyle)) (t ^. #ltick)
    <> extendData pl t

-- | compute an extension to the Range if a tick went over the data bounding box
computeTickExtension :: TickStyle -> Range Double -> Maybe (Range Double)
computeTickExtension s r =
  case s of
    TickNone -> Nothing
    TickRound _ n e -> bool Nothing (Just (space1 ticks0 <> r)) (e == TickExtend)
      where
        ticks0 = gridSensible OuterPos (e == NoTickExtend) r (fromIntegral n :: Integer)
    TickExact _ _ -> Nothing
    TickLabels _ -> Nothing
    TickPlaced xs -> Just $ r <> space1 (fst <$> xs)

-- | Create a style extension for the data, if ticks extend beyond the existing range
tickExtended ::
  Place ->
  Tick ->
  Rect Double ->
  Rect Double
tickExtended pl t xs =
  maybe
    xs
    (\x -> rangeext xs x)
    (computeTickExtension (t ^. #tstyle) (ranged xs))
  where
    ranged xs' = case pl of
      PlaceTop -> rangex xs'
      PlaceBottom -> rangex xs'
      PlaceLeft -> rangey xs'
      PlaceRight -> rangey xs'
      PlaceAbsolute _ -> rangex xs'
    rangex (Rect x z _ _) = Range x z
    rangey (Rect _ _ y w) = Range y w
    rangeext (Rect x z y w) (Range a0 a1) = case pl of
      PlaceTop -> Rect a0 a1 y w
      PlaceBottom -> Rect a0 a1 y w
      PlaceLeft -> Rect x z a0 a1
      PlaceRight -> Rect x z a0 a1
      PlaceAbsolute _ -> Rect a0 a1 y w

extendData :: (Monad m) => Place -> Tick -> HudT m Double
extendData pl t = Hud $ \cs -> do
  #dataDim %= tickExtended pl t
  pure cs

-- | adjust Tick for sane font sizes etc
adjustTick ::
  Adjustments ->
  Rect Double ->
  Rect Double ->
  Place ->
  Tick ->
  Tick
adjustTick (Adjustments mrx ma mry ad) vb cs pl t
  | pl `elem` [PlaceBottom, PlaceTop] = case ad of
    False -> t & #ttick . _Just . _1 . #size %~ (/ adjustSizeX)
    True ->
      case adjustSizeX > 1 of
        True ->
          ( case pl of
              PlaceBottom -> #ttick . _Just . _1 . #anchor .~ AnchorEnd
              PlaceTop -> #ttick . _Just . _1 . #anchor .~ AnchorStart
              _ -> #ttick . _Just . _1 . #anchor .~ AnchorEnd
          )
            . (#ttick . _Just . _1 . #size %~ (/ adjustSizeA))
            $ (#ttick . _Just . _1 . #rotation ?~ (-45)) t
        False -> (#ttick . _Just . _1 . #size %~ (/ adjustSizeA)) t
  | otherwise = -- pl `elem` [PlaceLeft, PlaceRight]
    (#ttick . _Just . _1 . #size %~ (/ adjustSizeY)) t
  where
    max' [] = 1
    max' xs = maximum xs
    ra (Rect x z y w)
      | pl `elem` [PlaceTop, PlaceBottom] = Range x z
      | otherwise = Range y w
    asp = ra vb
    r = ra cs
    tickl = snd <$> ticksR (t ^. #tstyle) asp r
    maxWidth :: Double
    maxWidth =
      maybe
        1
        ( \tt ->
            max' $
              (\(Rect x z _ _) -> z - x)
                . (\x -> styleBoxText (fst tt) x (Point 0 0)) <$> tickl
        )
        (t ^. #ttick)
    maxHeight =
      maybe
        1
        ( \tt ->
            max' $
              (\(Rect _ _ y w) -> w - y)
                . (\x -> styleBoxText (fst tt) x (Point 0 0)) <$> tickl
        )
        (t ^. #ttick)
    adjustSizeX :: Double
    adjustSizeX = max' [(maxWidth / realToFrac (upper asp - lower asp)) / mrx, 1]
    adjustSizeY = max' [(maxHeight / realToFrac (upper asp - lower asp)) / mry, 1]
    adjustSizeA = max' [(maxHeight / realToFrac (upper asp - lower asp)) / ma, 1]

adjustedTickHud :: (Monad m) => AxisOptions -> HudT m Double
adjustedTickHud c = Hud $ \cs -> do
  vb <- use #chartDim
  xs <- use #dataDim
  let adjTick =
        maybe
          (c ^. #atick)
          (\x -> adjustTick x vb xs (c ^. #place) (c ^. #atick))
          (c ^. #adjust)
  unhud (tick (c ^. #place) adjTick) cs

-- | Convert a UTCTime list into sensible ticks, placed exactly
makeTickDates :: PosDiscontinuous -> Maybe Text -> Int -> [UTCTime] -> [(Int, Text)]
makeTickDates pc fmt n dates =
  lastOnes (\(_, x0) (_, x1) -> x0 == x1) . fst $ placedTimeLabelDiscontinuous pc fmt n dates
  where
    lastOnes :: (a -> a -> Bool) -> [a] -> [a]
    lastOnes _ [] = []
    lastOnes _ [x] = [x]
    lastOnes f (x : xs) = L.fold (L.Fold step (x, []) (\(x0, x1) -> reverse $ x0 : x1)) xs
      where
        step (a0, rs) a1 = if f a0 a1 then (a1, rs) else (a1, a0 : rs)

-- | Convert a UTCTime list into sensible ticks, placed on the (0,1) space
makeTickDatesContinuous :: PosDiscontinuous -> Maybe Text -> Int -> [UTCTime] -> [(Double, Text)]
makeTickDatesContinuous pc fmt n dates = placedTimeLabelContinuous pc fmt n (l, u)
  where
    l = minimum dates
    u = maximum dates

legendHud :: LegendOptions -> [Chart Double] -> Hud Double
legendHud l lcs = Hud $ \cs -> do
  ca <- use #chartDim
  let cs' = cs <> movedleg ca scaledleg
  #chartDim .= defRect (styleBoxes cs')
  pure cs'
  where
    scaledleg =
      (#annotation %~ scaleAnn (realToFrac $ l ^. #lscale))
        . (#spots %~ fmap (fmap (* l ^. #lscale)))
        <$> lcs
    movedleg ca' leg =
      maybe id (moveChart . SpotPoint . placel (l ^. #lplace) ca') (styleBoxes leg) leg
    placel pl (Rect x z y w) (Rect x' z' y' w') =
      case pl of
        PlaceTop -> Point ((x + z) / 2.0) (w + (w' - y') / 2.0)
        PlaceBottom -> Point ((x + z) / 2.0) (y - (w' - y' / 2.0))
        PlaceLeft -> Point (x - (z' - x') / 2.0) ((y + w) / 2.0)
        PlaceRight -> Point (z + (z' - x') / 2.0) ((y + w) / 2.0)
        PlaceAbsolute p -> p

legendEntry ::
  LegendOptions ->
  Annotation ->
  Text ->
  (Chart Double, Chart Double)
legendEntry l a t =
  ( Chart ann sps,
    Chart (TextA (l ^. #ltext & #anchor .~ AnchorStart) [t]) [SP' 0 0]
  )
  where
    (ann, sps) = case a of
      RectA rs ->
        ( RectA rs,
          [SR' 0 (l ^. #lsize) 0 (l ^. #lsize)]
        )
      PixelA ps ->
        ( PixelA ps,
          [SR' 0 (l ^. #lsize) 0 (l ^. #lsize)]
        )
      TextA ts txts ->
        ( TextA (ts & #size .~ realToFrac (l ^. #lsize)) (take 1 txts),
          [SP' 0 0]
        )
      GlyphA gs ->
        ( GlyphA (gs & #size .~ realToFrac (l ^. #lsize)),
          [SP' (0.5 * l ^. #lsize) (0.33 * l ^. #lsize)]
        )
      LineA ls ->
        ( LineA (ls & #width %~ (/ (realToFrac $ l ^. #lscale))),
          [SP' 0 (0.33 * l ^. #lsize), SP' (2 * l ^. #lsize) (0.33 * l ^. #lsize)]
        )
      BlankA ->
        ( BlankA,
          [SP' 0 0]
        )

legendChart :: [(Annotation, Text)] -> LegendOptions -> [Chart Double]
legendChart lrs l =
  padChart (l ^. #outerPad)
    . maybe id (\x -> frameChart x (l ^. #innerPad)) (l ^. #legendFrame)
    . vert (l ^. #hgap)
    $ (\(a, t) -> hori ((l ^. #vgap) + twidth - gapwidth t) [[t], [a]])
      <$> es
  where
    es = reverse $ uncurry (legendEntry l) <$> lrs
    twidth = maybe 0 (\(Rect _ z _ _) -> z) . foldRect $ catMaybes (styleBox . snd <$> es)
    gapwidth t = maybe 0 (\(Rect _ z _ _) -> z) (styleBox t)

legendFromChart :: [Text] -> [Chart Double] -> [(Annotation, Text)]
legendFromChart = zipWith (\t c -> (c ^. #annotation, t))

terms :: Text -> [Lucid.Attribute] -> Lucid.Html ()
terms t = with $ makeXmlElementNoEnd t

-- | the extra area from text styling
styleBoxText ::
  TextStyle ->
  Text ->
  Point Double ->
  Rect Double
styleBoxText o t p = move (p P.+ p') $ maybe flat (`rotateRect` flat) (o ^. #rotation)
  where
    flat = Rect ((- x' / 2.0) + x' * a') (x' / 2 + x' * a') ((- y' / 2) - n1') (y' / 2 - n1')
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
    p' = fromMaybe (Point 0.0 0.0) (o ^. #translate)

-- | the extra area from glyph styling
styleBoxGlyph :: GlyphStyle -> Rect Double
styleBoxGlyph s = move p' $ sw $ case sh of
  EllipseGlyph a -> NH.scale (Point sz (a * sz)) unitRect
  RectSharpGlyph a -> NH.scale (Point sz (a * sz)) unitRect
  RectRoundedGlyph a _ _ -> NH.scale (Point sz (a * sz)) unitRect
  VLineGlyph _ -> NH.scale (Point ((s ^. #borderSize) * sz) sz) unitRect
  HLineGlyph _ -> NH.scale (Point sz ((s ^. #borderSize) * sz)) unitRect
  TriangleGlyph a b c -> (sz *) <$> sconcat (toRect . SpotPoint <$> (a :| [b, c]) :: NonEmpty (Rect Double))
  _ -> (sz *) <$> unitRect
  where
    sh = s ^. #shape
    sz = s ^. #size
    sw = padRect (0.5 * s ^. #borderSize)
    p' = fromMaybe (Point 0.0 0.0) (s ^. #translate)

-- | the geometric dimensions of a Chart inclusive of style geometry
styleBox :: Chart Double -> Maybe (Rect Double)
styleBox (Chart (TextA s ts) xs) = foldRect $ zipWith (\t x -> styleBoxText s t (toPoint x)) ts xs
styleBox (Chart (GlyphA s) xs) = foldRect $ (\x -> move (toPoint x) (styleBoxGlyph s)) <$> xs
styleBox (Chart (RectA s) xs) = foldRect (padRect (0.5 * s ^. #borderSize) . toRect <$> xs)
styleBox (Chart (LineA s) xs) = foldRect (padRect (0.5 * s ^. #width) . toRect <$> xs)
styleBox (Chart BlankA xs) = foldRect (toRect <$> xs)
styleBox (Chart (PixelA s) xs) = foldRect (padRect (0.5 * s ^. #pixelRectStyle . #borderSize) . toRect <$> xs)

-- | the extra geometric dimensions of a [Chart]
styleBoxes :: [Chart Double] -> Maybe (Rect Double)
styleBoxes xss = foldRect $ catMaybes (styleBox <$> xss)

-- | geometric dimensions of a [Chart] not including style
noStyleBoxes :: [Chart Double] -> Maybe (Rect Double)
noStyleBoxes cs = foldRect $ toRect <$> mconcat (view #spots <$> cs)

-- | calculate the linear gradient to shove in defs
-- FIXME: Only works for #pixelGradient = 0 or pi//2. Can do much better with something like https://stackoverflow.com/questions/9025678/how-to-get-a-rotated-linear-gradient-svg-for-use-as-a-background-image
lgPixel :: PixelStyle -> Lucid.Html ()
lgPixel o =
  term
    "linearGradient"
    [ Lucid.id_ (o ^. #pixelTextureId),
      Lucid.makeAttribute "x1" (show x0),
      Lucid.makeAttribute "y1" (show y0),
      Lucid.makeAttribute "x2" (show x1),
      Lucid.makeAttribute "y2" (show y1)
    ]
    ( mconcat
        [ terms
            "stop"
            [ Lucid.makeAttribute "stop-opacity" (show $ opac $ o ^. #pixelColorMin),
              Lucid.makeAttribute "stop-color" (toHex (o ^. #pixelColorMin)),
              Lucid.makeAttribute "offset" "0"
            ],
          terms
            "stop"
            [ Lucid.makeAttribute "stop-opacity" (show $ opac $ o ^. #pixelColorMax),
              Lucid.makeAttribute "stop-color" (toHex (o ^. #pixelColorMax)),
              Lucid.makeAttribute "offset" "1"
            ]
        ]
    )
  where
    x0 = min 0 (cos (o ^. #pixelGradient))
    x1 = max 0 (cos (o ^. #pixelGradient))
    y0 = max 0 (sin (o ^. #pixelGradient))
    y1 = min 0 (sin (o ^. #pixelGradient))

-- | get chart definitions
chartDefs :: [Chart a] -> Lucid.Html ()
chartDefs cs = bool (term "defs" (mconcat ds)) mempty (0 == length ds)
  where
    ds = mconcat $ chartDef <$> cs

chartDef :: Chart a -> [Lucid.Html ()]
chartDef c = case c of
  (Chart (PixelA s) _) -> [lgPixel s]
  _ -> []

-- | Rectangle svg
svgRect :: Rect Double -> Lucid.Html ()
svgRect (Rect x z y w) =
  terms
    "rect"
    [ width_ (show $ z - x),
      height_ (show $ w - y),
      term "x" (show x),
      term "y" (show $ - w)
    ]

-- | Text svg
svgText :: TextStyle -> Text -> Point Double -> Lucid.Html ()
svgText s t p@(Point x y) =
  bool id (term "g" [class_ "hasmathjax"]) (s ^. #hasMathjax) $
    term
      "text"
      ( [ term "x" (show x),
          term "y" (show $ - y)
        ]
          <> maybe [] (\x' -> [term "transform" (toRotateText x' p)]) (s ^. #rotation)
      )
      (toHtmlRaw t)

-- | line svg
svgLine :: [Point Double] -> Lucid.Html ()
svgLine [] = mempty
svgLine xs = terms "polyline" [term "points" (toPointsText xs)]
  where
    toPointsText xs' = Text.intercalate "\n" $ (\(Point x y) -> show x <> "," <> show (- y)) <$> xs'

-- | GlyphShape to svg Tree
svgShape :: GlyphShape -> Double -> Point Double -> Lucid.Html ()
svgShape CircleGlyph s (Point x y) =
  terms
    "circle"
    [ term "cx" (show x),
      term "cy" (show $ - y),
      term "r" (show $ 0.5 * s)
    ]
svgShape SquareGlyph s p =
  svgRect (move p ((s *) <$> unitRect))
svgShape (RectSharpGlyph x') s p =
  svgRect (move p (NH.scale (Point s (x' * s)) unitRect))
svgShape (RectRoundedGlyph x' rx ry) s p =
  terms
    "rect"
    [ term "width" (show $ z - x),
      term "height" (show $ w - y),
      term "x" (show x),
      term "y" (show $ - w),
      term "rx" (show rx),
      term "ry" (show ry)
    ]
  where
    (Rect x z y w) = move p (NH.scale (Point s (x' * s)) unitRect)
svgShape (TriangleGlyph (Point xa ya) (Point xb yb) (Point xc yc)) s p =
  terms
    "polygon"
    [ term "transform" (toTranslateText p),
      term "points" (show (s * xa) <> "," <> show (- (s * ya)) <> " " <> show (s * xb) <> "," <> show (- (s * yb)) <> " " <> show (s * xc) <> "," <> show (- (s * yc)))
    ]
svgShape (EllipseGlyph x') s (Point x y) =
  terms
    "ellipse"
    [ term "cx" (show x),
      term "cy" (show $ - y),
      term "rx" (show $ 0.5 * s),
      term "ry" (show $ 0.5 * s * x')
    ]
svgShape (VLineGlyph _) s (Point x y) =
  terms "polyline" [term "points" (show x <> "," <> show (- (y - s / 2)) <> "\n" <> show x <> "," <> show (- (y + s / 2)))]
svgShape (HLineGlyph _) s (Point x y) =
  terms "polyline" [term "points" (show (x - s / 2) <> "," <> show (- y) <> "\n" <> show (x + s / 2) <> "," <> show (- y))]
svgShape (PathGlyph path) _ p =
  terms "path" [term "d" path, term "transform" (toTranslateText p)]

-- | GlyphStyle to svg Tree
svgGlyph :: GlyphStyle -> Point Double -> Lucid.Html ()
svgGlyph s p =
  svgShape (s ^. #shape) (s ^. #size) (realToFrac <$> p)
    & maybe id (\r -> term "g" [term "transform" (toRotateText r p)]) (s ^. #rotation)

-- | convert a Chart to svg
svg :: Chart Double -> Lucid.Html ()
svg (Chart (TextA s ts) xs) =
  term "g" (attsText s) (mconcat $ zipWith (\t p -> svgText s t (toPoint p)) ts xs)
svg (Chart (GlyphA s) xs) =
  term "g" (attsGlyph s) (mconcat $ svgGlyph s . toPoint <$> xs)
svg (Chart (LineA s) xs) =
  term "g" (attsLine s) (svgLine $ toPoint <$> xs)
svg (Chart (RectA s) xs) =
  term "g" (attsRect s) (mconcat $ svgRect . toRect <$> xs)
svg (Chart (PixelA s) xs) =
  term "g" (attsPixel s) (mconcat $ svgRect . toRect <$> xs)
svg (Chart BlankA _) = mempty

-- | add a tooltip to a chart
svgt :: Chart Double -> (TextStyle, Text) -> Lucid.Html ()
svgt (Chart (TextA s ts) xs) (s', ts') =
  term "g" (attsText s) (Lucid.title_ (attsText s') (Lucid.toHtml ts') <> mconcat (zipWith (\t p -> svgText s t (toPoint p)) ts xs))
svgt (Chart (GlyphA s) xs) (s', ts') =
  term "g" (attsGlyph s) (Lucid.title_ (attsText s') (Lucid.toHtml ts') <> mconcat (svgGlyph s . toPoint <$> xs))
svgt (Chart (LineA s) xs) (s', ts') =
  term "g" (attsLine s) (Lucid.title_ (attsText s') (Lucid.toHtml ts') <> svgLine (toPoint <$> xs))
svgt (Chart (RectA s) xs) (s', ts') =
  term "g" (attsRect s) (Lucid.title_ (attsText s') (Lucid.toHtml ts') <> mconcat (svgRect . toRect <$> xs))
svgt (Chart (PixelA s) xs) (s', ts') =
  term "g" (attsPixel s) (Lucid.title_ (attsText s') (Lucid.toHtml ts') <> mconcat (svgRect . toRect <$> xs))
svgt (Chart BlankA _) _ = mempty

-- * Style to Attributes

attsRect :: RectStyle -> [Lucid.Attribute]
attsRect o =
  [ term "stroke-width" (show $ o ^. #borderSize),
    term "stroke" (hex $ o ^. #borderColor),
    term "stroke-opacity" (show $ opac $ o ^. #borderColor),
    term "fill" (hex $ o ^. #color),
    term "fill-opacity" (show $ opac $ o ^. #color)
  ]

attsPixel :: PixelStyle -> [Lucid.Attribute]
attsPixel o =
  [ term "stroke-width" (show $ o ^. #pixelRectStyle . #borderSize),
    term "stroke" (toHex $ o ^. #pixelRectStyle . #borderColor),
    term "stroke-opacity" (show $ opac $ o ^. #pixelRectStyle . #borderColor),
    term "fill" ("url(#" <> (o ^. #pixelTextureId) <> ")")
  ]

attsText :: TextStyle -> [Lucid.Attribute]
attsText o =
  [ term "stroke-width" "0.0",
    term "stroke" "none",
    term "fill" (toHex $ o ^. #color),
    term "fill-opacity" (show $ opac $ o ^. #color),
    term "font-size" (show $ o ^. #size),
    term "text-anchor" (toTextAnchor $ o ^. #anchor)
  ]
    <> maybe [] ((: []) . term "transform" . toTranslateText) (o ^. #translate)
  where
    toTextAnchor :: Anchor -> Text
    toTextAnchor AnchorMiddle = "middle"
    toTextAnchor AnchorStart = "start"
    toTextAnchor AnchorEnd = "end"

attsGlyph :: GlyphStyle -> [Lucid.Attribute]
attsGlyph o =
  [ term "stroke-width" (show $ o ^. #borderSize),
    term "stroke" (toHex $ o ^. #borderColor),
    term "stroke-opacity" (show $ opac $ o ^. #borderColor),
    term "fill" (toHex $ o ^. #color),
    term "fill-opacity" (show $ opac $ o ^. #color)
  ]
    <> maybe [] ((: []) . term "transform" . toTranslateText) (o ^. #translate)

attsLine :: LineStyle -> [Lucid.Attribute]
attsLine o =
  [ term "stroke-width" (show $ o ^. #width),
    term "stroke" (toHex $ o ^. #color),
    term "stroke-opacity" (show $ opac $ o ^. #color),
    term "fill" "none"
  ]

toTranslateText :: Point Double -> Text
toTranslateText (Point x y) =
  "translate(" <> show x <> ", " <> show (- y) <> ")"

toRotateText :: Double -> Point Double -> Text
toRotateText r (Point x y) =
  "rotate(" <> show r <> ", " <> show x <> ", " <> show (- y) <> ")"

-- | additively pad a [Chart]
--
-- >>> padChart 0.1 [Chart (RectA defaultRectStyle) [SpotRect unitRect]]
-- [Chart {annotation = RectA (RectStyle {borderSize = 1.0e-2, borderColor = RGBA 0.12 0.47 0.71 0.80, color = RGBA 0.12 0.47 0.71 0.30}), spots = [SpotRect Rect -0.5 0.5 -0.5 0.5]},Chart {annotation = BlankA, spots = [SpotRect Rect -0.605 0.605 -0.605 0.605]}]
padChart :: Double -> [Chart Double] -> [Chart Double]
padChart p cs = cs <> [Chart BlankA (maybeToList (SpotRect . padRect p <$> styleBoxes cs))]

-- | overlay a frame on some charts with some additive padding between
--
-- >>> frameChart defaultRectStyle 0.1 blank
-- [Chart {annotation = RectA (RectStyle {borderSize = 1.0e-2, borderColor = RGBA 0.12 0.47 0.71 0.80, color = RGBA 0.12 0.47 0.71 0.30}), spots = []},Chart {annotation = BlankA, spots = []}]
frameChart :: RectStyle -> Double -> [Chart Double] -> [Chart Double]
frameChart rs p cs = [Chart (RectA rs) (maybeToList (SpotRect . padRect p <$> styleBoxes cs))] <> cs

-- horizontally stack a list of list of charts (proceeding to the right) with a gap between
hori :: Double -> [[Chart Double]] -> [Chart Double]
hori _ [] = []
hori gap cs = foldl step [] cs
  where
    step x a = x <> (a & fmap (#spots %~ fmap (\s -> SpotPoint (Point (z x) 0) P.- SpotPoint (Point (origx x) 0) P.+ s)))
    z xs = maybe 0 (\(Rect _ z' _ _) -> z' + gap) (styleBoxes xs)
    origx xs = maybe 0 (\(Rect x' _ _ _) -> x') (styleBoxes xs)

-- vertically stack a list of charts (proceeding upwards), aligning them to the left
vert :: Double -> [[Chart Double]] -> [Chart Double]
vert _ [] = []
vert gap cs = foldl step [] cs
  where
    step x a = x <> (a & fmap (#spots %~ fmap (\s -> SpotPoint (Point (origx x - origx a) (w x)) P.+ s)))
    w xs = maybe 0 (\(Rect _ _ _ w') -> w' + gap) (styleBoxes xs)
    origx xs = maybe 0 (\(Rect x' _ _ _) -> x') (styleBoxes xs)

-- stack a list of charts horizontally, then vertically
stack :: Int -> Double -> [[Chart Double]] -> [Chart Double]
stack _ _ [] = []
stack n gap cs = vert gap (hori gap <$> group' cs [])
  where
    group' [] acc = reverse acc
    group' x acc = group' (drop n x) (take n x : acc)

addChartBox :: Chart Double -> Rect Double -> Rect Double
addChartBox c r = sconcat (r :| maybeToList (styleBox c))

addChartBoxes :: [Chart Double] -> Rect Double -> Rect Double
addChartBoxes c r = sconcat (r :| maybeToList (styleBoxes c))

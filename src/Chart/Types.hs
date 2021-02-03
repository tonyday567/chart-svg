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

-- | Chart API
module Chart.Types
  ( -- * Chart
    Chart (..),
    moveChart,
    projectXYs,
    projectXYsWith,
    projectArcPosition,

    -- * Annotation
    Annotation (..),
    annotationText,
    scaleAnn,
    padRect,

    -- * Styles
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
    toPathChart,
    defaultPathStyle,

    -- * Hud types
    ChartDims (..),
    HudT (..),
    Hud,
    simulHud,
    HudOptions (..),
    defaultHudOptions,
    defaultCanvas,
    runHudWith,
    runHud,
    makeHud,
    ChartAspect (..),
    toChartAspect,
    fromChartAspect,
    initialCanvas,
    chartAspectHud,
    canvas,
    title,
    tick,

    -- * Hud primitives
    AxisOptions (..),
    defaultAxisOptions,
    flipAxis,
    Place (..),
    placeText,
    AxisBar (..),
    defaultAxisBar,
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
    adjustTick,
    makeTickDates,
    makeTickDatesContinuous,
    Adjustments (..),
    defaultAdjustments,
    LegendOptions (..),
    defaultLegendOptions,
    legendHud,
    legendChart,
    legendEntry,
    Orientation (..),
    fromOrientation,
    toOrientation,

    -- * SVG primitives
    CssOptions (..),
    SvgOptions (..),
    defaultSvgOptions,
    defaultSvgFrame,

    -- * Chart manipulation
    padChart,
    frameChart,
    hori,
    vert,
    stack,

    -- * Bounding box calculation
    padBox,
    dataBox,
    dataBoxes,
    dataBoxesS,
    styleBox,
    styleBoxes,
    styleBoxesS,
    styleBoxText,
    styleBoxGlyph,
  )
where

import Control.Lens
import Data.Colour
import Data.FormatN
import Data.Generics.Labels ()
import qualified Data.List as List
import Data.Path
import qualified Data.Text as Text
import Data.Time
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

-- | A `Chart` is annotated xy-data.
data Chart a = Chart
  { -- | annotation style for the data
    annotation :: Annotation,
    -- | list of data elements, either points or rectangles.
    xys :: [XY a]
  }
  deriving (Eq, Show, Generic)

-- | How data will be represented onscreen.
--
-- The definition of what might be an Annotation type is opinionated.
--
-- More complex combinations across Annotations can be constructed from combining charts.  See 'Chart.Example.glinesExample', 'Chart.Examples.lglyphExample' and "Chart.Bar" for examples.
--
-- There may be exceptions, but the approximate magnitude of annotation values are in reference to the size of the screen.  For example, a size of 0.01 (say), will means about 1% of the height and/or width of the screen height or width.
data Annotation
  = RectA RectStyle
  | TextA TextStyle [Text]
  | GlyphA GlyphStyle
  | LineA LineStyle
  | PathA PathStyle [PathInfo Double]
  | BlankA
  deriving (Eq, Show, Generic)

-- | textifier
annotationText :: Annotation -> Text
annotationText (RectA _) = "RectA"
annotationText TextA {} = "TextA"
annotationText (GlyphA _) = "GlyphA"
annotationText (LineA _) = "LineA"
annotationText PathA {} = "PathA"
annotationText BlankA = "BlankA"

-- | Rectangle styling
--
-- >>> defaultRectStyle
-- RectStyle {borderSize = 1.0e-2, borderColor = RGBA 0.12 0.47 0.71 0.80, color = RGBA 0.12 0.47 0.71 0.30}
--
-- > writeChartSvgDefault "other/unit.svg" [Chart (RectA defaultRectStyle) [one]]
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
defaultRectStyle = RectStyle 0.01 (palette1 List.!! 1) (palette1 List.!! 2)

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
-- TextStyle {size = 8.0e-2, color = RGBA 0.20 0.20 0.20 1.00, anchor = AnchorMiddle, hsize = 0.5, vsize = 1.45, nudge1 = -0.2, rotation = Nothing, translate = Nothing}
--
-- >>> let t = zipWith (\x y -> Chart (TextA (defaultTextStyle & (#size .~ (0.05 :: Double))) [x]) [PointXY y]) (fmap Text.singleton ['a' .. 'y']) [Point (sin (x * 0.1)) x | x <- [0 .. 25]]
--
-- > writeChartSvgDefault "other/text.svg" t
--
-- ![text example](other/text.svg)
data TextStyle = TextStyle
  { size :: Double,
    color :: Colour,
    anchor :: Anchor,
    hsize :: Double,
    vsize :: Double,
    nudge1 :: Double,
    rotation :: Maybe Double,
    translate :: Maybe (Point Double)
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
  TextStyle 0.08 light AnchorMiddle 0.5 1.45 -0.2 Nothing Nothing

-- | Glyph styling
--
-- >>> defaultGlyphStyle
-- GlyphStyle {size = 3.0e-2, color = RGBA 0.65 0.81 0.89 0.30, borderColor = RGBA 0.12 0.47 0.71 0.80, borderSize = 3.0e-3, shape = SquareGlyph, rotation = Nothing, translate = Nothing}
--
-- See 'Chart.Examples.glyphsExample'.
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
    (palette1 List.!! 1)
    (palette1 List.!! 2)
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
fromDashArray xs = Text.intercalate " " $ show <$> xs

-- | line style
--
-- >>> defaultLineStyle
-- LineStyle {width = 1.2e-2, color = RGBA 0.65 0.81 0.89 0.30, linecap = Nothing, linejoin = Nothing, dasharray = Nothing, dashoffset = Nothing}
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
defaultLineStyle = LineStyle 0.012 light Nothing Nothing Nothing Nothing

-- | Path styling
--
-- >>> defaultPathStyle
-- PathStyle {borderSize = 1.0e-2, borderColor = RGBA 0.12 0.47 0.71 0.80, color = RGBA 0.12 0.47 0.71 0.30}
data PathStyle = PathStyle
  { borderSize :: Double,
    borderColor :: Colour,
    color :: Colour
  }
  deriving (Show, Eq, Generic)

-- | the style
defaultPathStyle :: PathStyle
defaultPathStyle =
  PathStyle 0.01 (palette1 List.!! 1) (palette1 List.!! 2)

-- | Convert from a path command list to a PathA chart
toPathChart :: PathStyle -> [(PathInfo Double, Point Double)] -> Chart Double
toPathChart ps xs = Chart (PathA ps (fst <$> xs)) (PointXY . snd <$> xs)

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

-- | additive padding
padRect :: (Num a) => a -> Rect a -> Rect a
padRect p (Rect x z y w) = Rect (x P.- p) (z P.+ p) (y P.- p) (w P.+ p)

-- |
data CssOptions = UseGeometricPrecision | UseCssCrisp | NoCssOptions deriving (Show, Eq, Generic)

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

-- | calculation of the canvas given the 'ChartAspect'
initialCanvas :: ChartAspect -> [Chart Double] -> Rect Double
initialCanvas (FixedAspect a) _ = aspect a
initialCanvas (CanvasAspect a) _ = aspect a
initialCanvas ChartAspect cs = aspect $ ratio $ styleBoxesS cs
initialCanvas UnadjustedAspect cs = dataBoxesS cs

-- | SVG tag options.
--
-- >>> defaultSvgOptions
-- SvgOptions {svgHeight = 300.0, outerPad = Just 2.0e-2, innerPad = Nothing, chartFrame = Nothing, cssOptions = NoCssOptions, chartAspect = FixedAspect 1.5}
--
-- > writeChartSvg "other/svgoptions.svg" (SvgChart (defaultSvgOptions & #chartAspect .~ FixedAspect 0.7) mempty [] lines)
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
defaultSvgOptions = SvgOptions 300 (Just 0.02) Nothing Nothing NoCssOptions (FixedAspect 1.5) Nothing

-- | frame style
defaultSvgFrame :: RectStyle
defaultSvgFrame = border 0.01 light

-- | Dimensions that are tracked in the 'HudT':
--
-- - chartDim: the rectangular dimension of the physical representation of a chart on the screen so that new hud elements can be appended. Adding a hud piece tends to expand the chart dimension.
--
-- - canvasDim: the rectangular dimension of the canvas on which data will be represented. At times appending a hud element will cause the canvas dimension to shift.
--
-- - dataDim: the rectangular dimension of the data being represented. Adding hud elements can cause this to change.
data ChartDims a = ChartDims
  { chartDim :: Rect a,
    canvasDim :: Rect a,
    dataDim :: Rect a
  }
  deriving (Eq, Show, Generic)

-- | Hud monad transformer
newtype HudT m a = Hud {unhud :: [Chart a] -> StateT (ChartDims a) m [Chart a]}

-- | Heads-Up-Display for a 'Chart'
type Hud = HudT Identity

instance (Monad m) => Semigroup (HudT m a) where
  (<>) (Hud h1) (Hud h2) = Hud $ h1 >=> h2

instance (Monad m) => Monoid (HudT m a) where
  mempty = Hud pure

-- | run two hud's simultaneously (using the same original ChartDims state) rather than sequentially (which is the <> operation).
simulHud :: (Ord a, Monad m) => HudT m a -> HudT m a -> HudT m a
simulHud (Hud fa) (Hud fb) = Hud $ \cs -> do
  s <- get
  (cs', ChartDims ch ca d) <- lift $ runStateT (fa cs) s
  (cs'', ChartDims ch' ca' d') <- lift $ runStateT (fb cs') s
  put (ChartDims (ch <> ch') (ca <> ca') (d <> d'))
  pure cs''

-- | Project the chart data given the ChartAspect
chartAspectHud :: (Monad m) => ChartAspect -> HudT m Double
chartAspectHud fa = Hud $ \cs -> do
  canvasd <- use #canvasDim
  chartd <- use #chartDim
  case fa of
    FixedAspect a -> pure $ projectXYs (aspect a) cs
    CanvasAspect a ->
      pure $
        projectXYs (aspect (a * ratio canvasd / ratio chartd)) cs
    ChartAspect -> pure $ projectXYs (aspect $ ratio chartd) cs
    UnadjustedAspect -> pure cs

-- | Combine huds and charts to form a new Chart using the supplied initial canvas and data dimensions. Note that chart data is transformed by this computation (and the use of a linear type is an open question).
runHudWith ::
  -- | initial canvas dimension
  Rect Double ->
  -- | initial data dimension
  Rect Double ->
  -- | huds to add
  [Hud Double] ->
  -- | underlying chart
  [Chart Double] ->
  -- | integrated chart list
  [Chart Double]
runHudWith ca xs hs cs =
  evalState
    ((unhud $ mconcat hs) cs')
    (ChartDims ca' da' xs)
  where
    da' = fromMaybe one $ dataBoxes cs'
    ca' = fromMaybe one $ styleBoxes cs'
    cs' = projectXYsWith ca xs cs

-- | Combine huds and charts to form a new [Chart] using the supplied canvas and the actual data dimension.
--
-- Note that the original chart data are transformed and irrevocably lost by this computation.
runHud ::
  -- | initial canvas dimension
  Rect Double ->
  -- | huds
  [Hud Double] ->
  -- | underlying charts
  [Chart Double] ->
  -- | integrated chart list
  [Chart Double]
runHud ca hs cs = runHudWith ca (padBox $ dataBoxes cs) hs cs

-- | Typical configurable hud elements. Anything else can be hand-coded as a 'HudT'.
--
-- ![hud example](other/hudoptions.svg)
data HudOptions = HudOptions
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
defaultCanvas = blob (setOpac 0.05 light)

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
data AxisOptions = AxisOptions
  { axisBar :: Maybe AxisBar,
    adjust :: Maybe Adjustments,
    axisTick :: Tick,
    place :: Place
  }
  deriving (Eq, Show, Generic)

-- | The official axis
defaultAxisOptions :: AxisOptions
defaultAxisOptions = AxisOptions (Just defaultAxisBar) (Just defaultAdjustments) defaultTick PlaceBottom

-- | The bar on an axis representing the x or y plane.
--
-- >>> defaultAxisBar
-- AxisBar {rstyle = RectStyle {borderSize = 0.0, borderColor = RGBA 0.50 0.50 0.50 1.00, color = RGBA 0.50 0.50 0.50 1.00}, wid = 5.0e-3, buff = 1.0e-2}
data AxisBar = AxisBar
  { rstyle :: RectStyle,
    wid :: Double,
    buff :: Double
  }
  deriving (Show, Eq, Generic)

-- | The official axis bar
defaultAxisBar :: AxisBar
defaultAxisBar = AxisBar (RectStyle 0 transparent (setOpac 0.4 light)) 0.004 0.01

-- | Options for titles.  Defaults to center aligned, and placed at Top of the hud
--
-- >>> defaultTitle "title"
-- Title {text = "title", style = TextStyle {size = 0.12, color = RGBA 0.20 0.20 0.20 1.00, anchor = AnchorMiddle, hsize = 0.5, vsize = 1.45, nudge1 = -0.2, rotation = Nothing, translate = Nothing}, place = PlaceTop, anchor = AnchorMiddle, buff = 4.0e-2}
data Title = Title
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
    ( defaultTextStyle &
      #size .~ 0.12
    )
    PlaceTop
    AnchorMiddle
    0.04

-- | xy coordinate markings
--
-- >>> defaultTick
-- Tick {tstyle = TickRound (FormatComma (Just 2)) 8 TickExtend, gtick = Just (GlyphStyle {size = 3.0e-2, color = RGBA 0.50 0.50 0.50 1.00, borderColor = RGBA 0.50 0.50 0.50 1.00, borderSize = 5.0e-3, shape = VLineGlyph 5.0e-3, rotation = Nothing, translate = Nothing},1.25e-2), ttick = Just (TextStyle {size = 5.0e-2, color = RGBA 0.50 0.50 0.50 1.00, anchor = AnchorMiddle, hsize = 0.5, vsize = 1.45, nudge1 = -0.2, rotation = Nothing, translate = Nothing},1.5e-2), ltick = Just (LineStyle {width = 5.0e-3, color = RGBA 0.50 0.50 0.50 0.05, linecap = Nothing, linejoin = Nothing, dasharray = Nothing, dashoffset = Nothing},0.0)}
data Tick = Tick
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
    & #borderSize .~ 0
    & #shape .~ VLineGlyph 0.005
    & #color .~ setOpac 0.4 light
    & #borderColor .~ setOpac 0.4 light

-- | The official text tick
defaultTextTick :: TextStyle
defaultTextTick =
  defaultTextStyle & #size .~ 0.05

-- | The official line tick
defaultLineTick :: LineStyle
defaultLineTick =
  defaultLineStyle
    & #width .~ 5.0e-3
    & #color %~ setOpac 0.05

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
defaultTickStyle = TickRound (FormatComma (Just 2)) 8 TickExtend

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
data Adjustments = Adjustments
  { maxXRatio :: Double,
    maxYRatio :: Double,
    angledRatio :: Double,
    allowDiagonal :: Bool
  }
  deriving (Show, Eq, Generic)

-- | The official hud adjustments.
defaultAdjustments :: Adjustments
defaultAdjustments = Adjustments 0.08 0.06 0.12 True

-- | Legend options
--
-- >>> defaultLegendOptions
-- LegendOptions {lsize = 0.1, vgap = 0.2, hgap = 0.1, ltext = TextStyle {size = 8.0e-2, color = RGBA 0.20 0.20 0.20 1.00, anchor = AnchorMiddle, hsize = 0.5, vsize = 1.45, nudge1 = -0.2, rotation = Nothing, translate = Nothing}, lmax = 10, innerPad = 0.1, outerPad = 0.1, legendFrame = Just (RectStyle {borderSize = 2.0e-2, borderColor = RGBA 0.50 0.50 0.50 1.00, color = RGBA 1.00 1.00 1.00 1.00}), lplace = PlaceBottom, lscale = 0.2}
--
-- ![legend example](other/legend.svg)
data LegendOptions = LegendOptions
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
    0.02
    (Just (RectStyle 0.02 (setOpac 0.05 light) (setOpac 0.05 light)))
    PlaceBottom
    0.2

-- | Generically scale an Annotation.
scaleAnn :: Double -> Annotation -> Annotation
scaleAnn x (LineA a) = LineA $ a & #width %~ (* x)
scaleAnn x (RectA a) = RectA $ a & #borderSize %~ (* x)
scaleAnn x (TextA a txs) = TextA (a & #size %~ (* x)) txs
scaleAnn x (GlyphA a) = GlyphA (a & #size %~ (* x))
scaleAnn x (PathA a pxs) = PathA (a & #borderSize %~ (* x)) pxs
scaleAnn _ BlankA = BlankA

-- | Translate the data in a chart.
moveChart :: (Additive a) => XY a -> [Chart a] -> [Chart a]
moveChart sp = fmap (#xys %~ fmap (sp +))

-- | Make huds from a HudOptions.
--
-- Some huds, such as the creation of tick values, can extend the data dimension of a chart, so we return a blank chart with the new data dimension.
-- The complexity internally to this function is due to the creation of ticks and, specifically, 'gridSensible', which is not idempotent. As a result, a tick calculation that does extends the data area, can then lead to new tick values when applying TickRound etc.
makeHud :: Rect Double -> HudOptions -> ([Hud Double], [Chart Double])
makeHud xs cfg =
  ([axes] <> can <> titles <> l, xsext)
  where
    xs' = padBox (Just xs)
    can = maybe [] (\x -> [canvas x]) (cfg ^. #hudCanvas)
    titles = title <$> (cfg ^. #hudTitles)
    ticks =
      (\a -> freezeTicks (a ^. #place) xs' (a ^. #axisTick . #tstyle))
        <$> (cfg ^. #hudAxes)
    hudaxes =
      zipWith
        (\c t -> c & #axisTick . #tstyle .~ fst t)
        (cfg ^. #hudAxes)
        ticks
    tickRects = catMaybes (snd <$> ticks)
    xsext = bool [Chart BlankA (RectXY <$> tickRects)] [] (tickRects == [])
    axes =
      foldr simulHud mempty $
        ( \x ->
            maybe mempty (makeAxisBar (x ^. #place)) (x ^. #axisBar)
              <> makeTick x
        )
          <$> hudaxes
    l =
      maybe
        []
        (\(lo, ats) -> [legendHud lo (legendChart ats lo)])
        (cfg ^. #hudLegend)

-- | convert TickRound to TickPlaced
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

-- | flip an axis from being an X dimension to a Y one or vice-versa.
flipAxis :: AxisOptions -> AxisOptions
flipAxis ac = case ac ^. #place of
  PlaceBottom -> ac & #place .~ PlaceLeft
  PlaceTop -> ac & #place .~ PlaceRight
  PlaceLeft -> ac & #place .~ PlaceBottom
  PlaceRight -> ac & #place .~ PlaceTop
  PlaceAbsolute _ -> ac

addToRect :: (Ord a) => Rect a -> Maybe (Rect a) -> Rect a
addToRect r r' = sconcat $ r :| maybeToList r'

-- | Make a canvas hud element.
canvas :: (Monad m) => RectStyle -> HudT m Double
canvas s = Hud $ \cs -> do
  a <- use #canvasDim
  let c = Chart (RectA s) [RectXY a]
  #canvasDim .= addToRect a (styleBox c)
  pure $ c : cs

axisBar_ :: Place -> AxisBar -> Rect Double -> Rect Double -> Chart Double
axisBar_ pl b (Rect x z y w) (Rect x' z' y' w') =
  case pl of
    PlaceTop ->
      Chart
        (RectA (rstyle b))
        [ R
            x
            z
            (w' + b ^. #buff)
            (w' + b ^. #buff + b ^. #wid)
        ]
    PlaceBottom ->
      Chart
        (RectA (rstyle b))
        [ R
            x
            z
            (y' - b ^. #wid - b ^. #buff)
            (y' - b ^. #buff)
        ]
    PlaceLeft ->
      Chart
        (RectA (rstyle b))
        [ R
            (x' - b ^. #wid - b ^. #buff)
            (x' - b ^. #buff)
            y
            w
        ]
    PlaceRight ->
      Chart
        (RectA (rstyle b))
        [ R
            (z' + (b ^. #buff))
            (z' + (b ^. #buff) + (b ^. #wid))
            y
            w
        ]
    PlaceAbsolute (Point x'' _) ->
      Chart
        (RectA (rstyle b))
        [ R
            (x'' + (b ^. #buff))
            (x'' + (b ^. #buff) + (b ^. #wid))
            y
            w
        ]

makeAxisBar :: (Monad m) => Place -> AxisBar -> HudT m Double
makeAxisBar pl b = Hud $ \cs -> do
  da <- use #chartDim
  ca <- use #canvasDim
  let c = axisBar_ pl b ca da
  #chartDim .= addChartBox c da
  pure $ c : cs

title_ :: Title -> Rect Double -> Chart Double
title_ t a =
  Chart
    ( TextA
        ( style'
            & #translate ?~ (placePos' a + alignPos a)
            & #rotation ?~ rot
        )
        [t ^. #text]
    )
    [zero]
  where
    style'
      | t ^. #anchor == AnchorStart =
        #anchor .~ AnchorStart $ t ^. #style
      | t ^. #anchor == AnchorEnd =
        #anchor .~ AnchorEnd $ t ^. #style
      | otherwise = t ^. #style
    rot
      | t ^. #place == PlaceRight = pi / 2
      | t ^. #place == PlaceLeft = - pi / 2
      | otherwise = 0
    placePos' (Rect x z y w) = case t ^. #place of
      PlaceTop -> Point ((x + z) / 2.0) (w + (t ^. #buff))
      PlaceBottom ->
        Point
          ((x + z) / 2.0)
          ( y - (t ^. #buff)
              - 0.5
              * (t ^. #style . #vsize)
              * (t ^. #style . #size)
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
  PlaceRight -> Just (pi / 2)
  PlaceLeft -> Just (pi / 2)
  _ -> Nothing

textPos :: Place -> TextStyle -> Double -> Point Double
textPos pl tt b = case pl of
  PlaceTop -> Point 0 b
  PlaceBottom -> Point 0 (- b - 0.5 * (tt ^. #vsize) * (tt ^. #size))
  PlaceLeft ->
    Point
      (- b)
      ((tt ^. #nudge1) * (tt ^. #vsize) * (tt ^. #size))
  PlaceRight ->
    Point
      b
      ((tt ^. #nudge1) * (tt ^. #vsize) * (tt ^. #size))
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

placeGridLines :: Place -> Rect Double -> Double -> Double -> [XY Double]
placeGridLines pl (Rect x z y w) a b
  | pl `elem` [PlaceTop, PlaceBottom] = [P a (y - b), P a (w + b)]
  | otherwise = [P (x - b) a, P (z + b) a]

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

data TickComponents = TickComponents
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
    (GlyphA (g & #rotation .~ placeRot pl))
    ( PointXY . (placePos pl b ca +) . placeOrigin pl
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
          [PointXY sp]
    )
    (labels $ ticksPlaced ts pl da xs)
    ( (placePos pl b ca + textPos pl txts b +) . placeOrigin pl
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
    (rangeext xs)
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
  | pl `elem` [PlaceBottom, PlaceTop] =
    if ad
      then
        ( case adjustSizeX > 1 of
            True ->
              ( case pl of
                  PlaceBottom -> #ttick . _Just . _1 . #anchor .~ AnchorEnd
                  PlaceTop -> #ttick . _Just . _1 . #anchor .~ AnchorStart
                  _ -> #ttick . _Just . _1 . #anchor .~ AnchorEnd
              )
                . (#ttick . _Just . _1 . #size %~ (/ adjustSizeA))
                $ (#ttick . _Just . _1 . #rotation ?~ pi / 4) t
            False -> (#ttick . _Just . _1 . #size %~ (/ adjustSizeA)) t
        )
      else t & #ttick . _Just . _1 . #size %~ (/ adjustSizeX)
  | otherwise -- pl `elem` [PlaceLeft, PlaceRight]
    =
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
                . (\x -> styleBoxText (fst tt) x (Point 0 0))
                <$> tickl
        )
        (t ^. #ttick)
    maxHeight =
      maybe
        1
        ( \tt ->
            max' $
              (\(Rect _ _ y w) -> w - y)
                . (\x -> styleBoxText (fst tt) x (Point 0 0))
                <$> tickl
        )
        (t ^. #ttick)
    adjustSizeX :: Double
    adjustSizeX = max' [(maxWidth / (upper asp - lower asp)) / mrx, 1]
    adjustSizeY = max' [(maxHeight / (upper asp - lower asp)) / mry, 1]
    adjustSizeA = max' [(maxHeight / (upper asp - lower asp)) / ma, 1]

makeTick :: (Monad m) => AxisOptions -> HudT m Double
makeTick c = Hud $ \cs -> do
  vb <- use #chartDim
  xs <- use #dataDim
  let adjTick =
        maybe
          (c ^. #axisTick)
          (\x -> adjustTick x vb xs (c ^. #place) (c ^. #axisTick))
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
    lastOnes f (x : xs) = (\(x0, x1) -> reverse $ x0 : x1) $ foldl' step (x, []) xs
      where
        step (a0, rs) a1 = if f a0 a1 then (a1, rs) else (a1, a0 : rs)

-- | Convert a UTCTime list into sensible ticks, placed on the (0,1) space
makeTickDatesContinuous :: PosDiscontinuous -> Maybe Text -> Int -> [UTCTime] -> [(Double, Text)]
makeTickDatesContinuous pc fmt n dates =
  placedTimeLabelContinuous pc fmt n (space1 dates)

-- | Make a legend hud element taking into account the chart.
legendHud :: LegendOptions -> [Chart Double] -> Hud Double
legendHud l lcs = Hud $ \cs -> do
  ca <- use #chartDim
  let cs' = cs <> movedleg ca scaledleg
  #chartDim .= fromMaybe one (styleBoxes cs')
  pure cs'
  where
    scaledleg =
      (#annotation %~ scaleAnn (l ^. #lscale))
        . (#xys %~ fmap (fmap (* l ^. #lscale)))
        <$> lcs
    movedleg ca' leg =
      maybe id (moveChart . PointXY . placel (l ^. #lplace) ca') (styleBoxes leg) leg
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
    Chart (TextA (l ^. #ltext & #anchor .~ AnchorStart) [t]) [zero]
  )
  where
    (ann, sps) = case a of
      RectA rs ->
        ( RectA rs,
          [R 0 (l ^. #lsize) 0 (l ^. #lsize)]
        )
      TextA ts txts ->
        ( TextA (ts & #size .~ (l ^. #lsize)) (take 1 txts),
          [zero]
        )
      GlyphA gs ->
        ( GlyphA (gs & #size .~ (l ^. #lsize)),
          [P (0.5 * l ^. #lsize) (0.33 * l ^. #lsize)]
        )
      LineA ls ->
        ( LineA (ls & #width %~ (/ (l ^. #lscale))),
          [P 0 (1 * l ^. #lsize), P (2 * l ^. #lsize) (1 * l ^. #lsize)]
        )
      PathA ps _ ->
        ( let cs =
                singletonCubic
                  ( CubicPosition
                      (Point 0 0)
                      (Point (0.33 * l ^. #lsize) (0.33 * l ^. #lsize))
                      (Point 0 (0.33 * l ^. #lsize))
                      (Point (0.33 * l ^. #lsize) 0)
                  )
           in (PathA (ps & #borderSize .~ (l ^. #lsize)) (fst <$> cs), PointXY . snd <$> cs)
        )
      BlankA ->
        ( BlankA,
          [zero]
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

-- | Project the xys of a chart to a new XY Space.
--
-- > projectXYs (dataBox cs) cs == cs if cs is non-empty
projectXYs :: Rect Double -> [Chart Double] -> [Chart Double]
projectXYs _ [] = []
projectXYs new cs = projectXYsWith new old cs
  where
    old = fromMaybe one (dataBoxes cs)

-- | Project chart xys to a new XY Space from an old XY Space
--
-- The projections needed are:
--
-- - project the 'xys'
--
-- - project the control points of bezier curves
--
-- - project aspect changes only to radii of ellipticals.
--
-- > projectXYsWith x x == id
projectXYsWith :: Rect Double -> Rect Double -> [Chart Double] -> [Chart Double]
projectXYsWith new old cs = cs'
  where
    xss = fmap (projectOn new old) . xys <$> cs
    ss = projectAnn <$> cs
    cs' = zipWith Chart ss xss
    projectAnn (Chart (PathA ps ips) xys) =
      PathA ps (projectControls ips xys)
    projectAnn x = annotation x

    projectControls pis xys =
      (reverse . snd) (foldl' (\(prevp, l) (i, xy) -> (xy, projectControl prevp xy i : l)) (zero, []) (zip pis xys))

    projectControl _ _ (CubicI c1 c2) =
      CubicI (projectOnP new old c1) (projectOnP new old c2)
    projectControl _ _ (QuadI c) =
      QuadI (projectOnP new old c)
    projectControl p1 p2 (ArcI ai) = ArcI $ projectArcPosition new old (ArcPosition (toPoint p1) (toPoint p2) ai)
    projectControl _ _ x = x

-- | project an ArcPosition given new and old Rects
--
-- The radii of the ellipse can be represented as:
--
-- Point rx 0 & Point 0 ry
--
-- These two points are firstly rotated by p and then undergo scaling...
projectArcPosition :: Rect Double -> Rect Double -> ArcPosition Double -> ArcInfo Double
projectArcPosition new old (ArcPosition _ _ (ArcInfo (Point rx ry) phi l cl)) = ArcInfo (Point rx'' ry'') phi l cl
  where
    rx' = rotateP phi (Point rx zero)
    rx'' = norm $ rx' * NH.width new / NH.width old
    ry' = rotateP phi (Point zero ry)
    ry'' = norm $ ry' * NH.width new / NH.width old

-- | pad a Rect to remove singleton dimensions
padBox :: Maybe (Rect Double) -> Rect Double
padBox = maybe one singletonUnit
  where
    singletonUnit (Rect x z y w)
      | x == z && y == w = Rect (x - 0.5) (x + 0.5) (y - 0.5) (y + 0.5)
      | x == z = Rect (x - 0.5) (x + 0.5) y w
      | y == w = Rect x z (y - 0.5) (y + 0.5)
      | otherwise = Rect x z y w

-- | 'Rect' of a 'Chart', not including style
dataBox :: Chart Double -> Maybe (Rect Double)
dataBox c =
  case c ^. #annotation of
    PathA _ path' -> pathBoxes $ zip path' (toPoint <$> c ^. #xys)
    _ -> foldRect $ fmap toRect (c ^. #xys)

-- | 'Rect' of charts, not including style
dataBoxes :: [Chart Double] -> Maybe (Rect Double)
dataBoxes cs = foldRect $ catMaybes $ dataBox <$> cs

-- | 'Rect' of charts, not including style, with defaults for Nothing and singleton dimensions if any.
dataBoxesS :: [Chart Double] -> Rect Double
dataBoxesS cs = padBox $ foldRect $ catMaybes $ dataBox <$> cs

-- | the extra area from text styling
styleBoxText ::
  TextStyle ->
  Text ->
  Point Double ->
  Rect Double
styleBoxText o t p = move (p + p') $ maybe flat (`rotationBound` flat) (o ^. #rotation)
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
styleBoxGlyph s = move p' $
  sw $ case sh of
    CircleGlyph -> (sz *) <$> one
    SquareGlyph -> (sz *) <$> one
    EllipseGlyph a -> NH.scale (Point sz (a * sz)) one
    RectSharpGlyph a -> NH.scale (Point sz (a * sz)) one
    RectRoundedGlyph a _ _ -> NH.scale (Point sz (a * sz)) one
    VLineGlyph _ -> NH.scale (Point ((s ^. #borderSize) * sz) sz) one
    HLineGlyph _ -> NH.scale (Point sz ((s ^. #borderSize) * sz)) one
    TriangleGlyph a b c -> (sz *) <$> sconcat (toRect . PointXY <$> (a :| [b, c]) :: NonEmpty (Rect Double))
    PathGlyph path' -> (sz *) <$> fromMaybe one (pathBoxes . toPathXYs . parsePath $ path')
  where
    sh = s ^. #shape
    sz = s ^. #size
    sw = padRect (0.5 * s ^. #borderSize)
    p' = fromMaybe (Point 0.0 0.0) (s ^. #translate)

-- | the geometric dimensions of a Chart inclusive of style geometry, but excluding PathA effects
styleBox :: Chart Double -> Maybe (Rect Double)
styleBox (Chart (TextA s ts) xs) = foldRect $ zipWith (\t x -> styleBoxText s t (toPoint x)) ts xs
styleBox (Chart (GlyphA s) xs) = foldRect $ (\x -> move (toPoint x) (styleBoxGlyph s)) <$> xs
styleBox (Chart (RectA s) xs) = foldRect (padRect (0.5 * s ^. #borderSize) . toRect <$> xs)
styleBox (Chart (LineA s) xs) = foldRect (padRect (0.5 * s ^. #width) . toRect <$> xs)
styleBox c@(Chart (PathA s _) _) = padRect (0.5 * s ^. #borderSize) <$> dataBox c
styleBox (Chart BlankA xs) = foldRect (toRect <$> xs)

-- | the extra geometric dimensions of a [Chart]
styleBoxes :: [Chart Double] -> Maybe (Rect Double)
styleBoxes xss = foldRect $ catMaybes (styleBox <$> xss)

-- | the extra geometric dimensions of a [Chart], adjusted for Nothing or singleton dimensions.
styleBoxesS :: [Chart Double] -> Rect Double
styleBoxesS xss = padBox $ foldRect $ catMaybes (styleBox <$> xss)

-- | additively pad a [Chart]
--
-- >>> padChart 0.1 [Chart (RectA defaultRectStyle) [RectXY one]]
-- [Chart {annotation = RectA (RectStyle {borderSize = 1.0e-2, borderColor = RGBA 0.12 0.47 0.71 0.80, color = RGBA 0.12 0.47 0.71 0.30}), xys = [R -0.5 0.5 -0.5 0.5]},Chart {annotation = BlankA, xys = [R -0.605 0.605 -0.605 0.605]}]
padChart :: Double -> [Chart Double] -> [Chart Double]
padChart p cs = cs <> [Chart BlankA (maybeToList (RectXY . padRect p <$> styleBoxes cs))]

-- | overlay a frame on some charts with some additive padding between
--
-- >>> frameChart defaultRectStyle 0.1 [Chart BlankA []]
-- [Chart {annotation = RectA (RectStyle {borderSize = 1.0e-2, borderColor = RGBA 0.12 0.47 0.71 0.80, color = RGBA 0.12 0.47 0.71 0.30}), xys = []},Chart {annotation = BlankA, xys = []}]
frameChart :: RectStyle -> Double -> [Chart Double] -> [Chart Double]
frameChart rs p cs = [Chart (RectA rs) (maybeToList (RectXY . padRect p <$> styleBoxes cs))] <> cs

-- | horizontally stack a list of list of charts (proceeding to the right) with a gap between
hori :: Double -> [[Chart Double]] -> [Chart Double]
hori _ [] = []
hori gap cs = foldl step [] cs
  where
    step x a = x <> (a & fmap (#xys %~ fmap (\s -> P (widthx x) (aligny x - aligny a) + s)))
    widthx xs = maybe 0 (\(Rect x' z' _ _) -> z' - x' + gap) (styleBoxes xs)
    aligny xs = maybe 0 (\(Rect _ _ y' w') -> (y'+w')/2) (styleBoxes xs)

-- | vertically stack a list of charts (proceeding upwards), aligning them to the left
vert :: Double -> [[Chart Double]] -> [Chart Double]
vert _ [] = []
vert gap cs = foldl step [] cs
  where
    step x a = x <> (a & fmap (#xys %~ fmap (\s -> P (alignx x - alignx a) (widthy x) + s)))
    widthy xs = maybe 0 (\(Rect _ _ y' w') -> w' - y' + gap) (styleBoxes xs)
    alignx xs = maybe 0 (\(Rect x' _ _ _) -> x') (styleBoxes xs)

-- | stack a list of charts horizontally, then vertically
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

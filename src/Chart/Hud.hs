{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | A hud stands for <https://en.wikipedia.org/wiki/Head-up_display head-up display>, and is a collective noun used to name chart elements that assist in data interpretation or otherwise annotate and decorate data.
--
-- This includes axes, titles, borders, frames, background canvaii, tick marks and tick value labels.
module Chart.Hud
  ( -- * Hud
    Hud (..),
    Priority (..),
    defaultPriority,
    HudBox,
    CanvasBox,
    DataBox,
    HudChart (..),
    canvasBox',
    hudBox',
    hudStyleBox',

    -- * Hud Processing
    runHudWith,
    runHud,

    -- * HudOptions
    HudOptions (..),
    defaultHudOptions,
    addHud,
    initialCanvas,
    finalCanvas,
    colourHudOptions,
    toHuds,

    -- * Hud primitives
    AxisOptions (..),
    defaultXAxisOptions,
    defaultYAxisOptions,
    FrameOptions (..),
    defaultFrameOptions,
    Place (..),
    flipPlace,
    AxisBar (..),
    defaultAxisBar,
    Title (..),
    defaultTitle,
    Buffered (..),
    Ticks (..),
    defaultGlyphTick,
    defaultTextTick,
    defaultLineTick,
    defaultXTicks,
    defaultYTicks,
    TickStyle (..),
    defaultTickStyle,
    tickStyleText,
    TickExtend (..),
    adjustTicks,
    Adjustments (..),
    defaultAdjustments,
    LegendOptions (..),
    defaultLegendOptions,

    -- * Option to Hud
    axisHud,
    titleHud,
    frameHud,
    legendHud,

    legendChart,
    legendFrame,
    freezeAxes,
    freezeTicks,
    formatN',
    numTicks',
    tickExtend',
    projectChartWith,
    placeLegend,

    fromHudChart,
    makeHuds,
    legendText,
    legendEntry,
    legendizeChart,
  )
where

import Chart.Data
import Chart.Primitive
import Chart.Style
import Data.Bifunctor
import Data.Bool
import Data.Colour
import Data.Foldable hiding (sum)
import Data.FormatN
import Data.List qualified as List
import Data.Maybe
import Data.Path
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Tuple
import GHC.Generics hiding (to)
import NumHask.Prelude qualified as NH
import Optics.Core

-- $setup
--
-- >>> :set -XOverloadedLabels
-- >>> :set -XOverloadedStrings
-- >>> import Chart
-- >>> import Optics.Core

-- * Hud

-- | The priority of a Hud element or transformation, lower value means higher priority.
--
-- Lower priority (higher values) huds will tend to be placed on the outside of a chart.
--
-- Hud elements are rendered in order from high to low priority and the positioning of hud elements can depend on the positioning of elements that have already been included. Equal priority values will be placed in the same process step.
--
-- The first example below, based in 'Chart.Examples.lineExample' but with the legend placed on the right and coloured frames to help accentuate effects, includes (in order of priority):
--
-- - an inner frame, representing the core data area of the chart (Priority 1)
--
-- - the axes (defaultPriority (5))
--
-- - the titles (Priority 12)
--
-- - the legend (Priority 50)
--
-- - an outer frame which is transparent and used to pad out the chart (Priority 100).
--
-- > priorityv1Example = lineExample & (#hudOptions % #frames) .~ [(1, FrameOptions (Just defaultRectStyle) 0), (100, FrameOptions (Just (defaultRectStyle & #color .~ (palette1 4 & opac' .~ 0.05) & #borderColor .~ palette1 4)) 0.1)] & #hudOptions % #legends %~ fmap (first (const (Priority 50))) & #hudOptions % #legends %~ fmap (second (set #place PlaceRight))
--
-- ![priorityv1 example](other/priorityv1.svg)
--
-- The second variation below drops the title priorities to below the legend:
--
-- > priorityv2Example = priorityv1Example & #hudOptions % #titles %~ fmap (first (const (Priority 51)))
--
-- ![priorityv2 example](other/priorityv2.svg)
data Priority a = Priority {priority :: Double, item :: a} deriving (Eq, Ord, Show, Generic, Functor)

-- | An arbitrary 5.0
--
-- >>> defaultPriority
-- Priority {priority = 5.0}
defaultPriority :: a -> Priority a
defaultPriority a = Priority 5.0 a

-- | Heads-up display additions to charts
--
newtype Hud = Hud { phud :: Priority (HudChart -> ChartTree) } deriving (Generic)

-- | Type to track the split of Chart elements into Hud and Canvas
--
-- - charts: charts that form the canvas or data elements of the chart; the rectangular dimension which is considered to be the data representation space.
--
-- - hud: charts that form the Hud.
--
-- - dataBox: The bounding box of the underlying data domain.
--
-- This is done to support functionality where we can choose whether to normalise the chart aspect based on the entire chart (FixedAspect) or on just the data visualisation space (CanvasAspect).
data HudChart = HudChart
  { chartSection :: ChartTree,
    hudSection :: ChartTree
  }
  deriving (Eq, Show, Generic)

-- | A type for Rect to represent the entire bounding box of a chart, including the Hud
type HudBox = Rect Double

-- | A type for Rect to represent the bounding box of the canvas portion of a chart, excluding Hud elements
type CanvasBox = Rect Double

-- | A type for Rect to represent the bounding box of the data elements a chart, which can be a different metric to Canvas and Hud Rects
type DataBox = Rect Double

canvasBox_ :: HudChart -> Maybe CanvasBox
canvasBox_ = boxes . foldOf (#chartSection % charts')

canvasRebox_ :: HudChart -> Maybe (Rect Double) -> HudChart
canvasRebox_ cs r =
  cs
    & over (#chartSection % chart') (maybeProjectWith r (canvasBox_ cs))
    & over (#hudSection % chart') (maybeProjectWith r (canvasBox_ cs))

-- | A lens between a HudChart and the bounding box of the canvas
canvasBox' :: Lens' HudChart (Maybe CanvasBox)
canvasBox' =
  lens canvasBox_ canvasRebox_

hudStyleBox_ :: HudChart -> Maybe HudBox
hudStyleBox_ = styleBoxes . (\x -> foldOf (#chartSection % charts') x <> foldOf (#hudSection % charts') x)

-- | a lens between a HudChart and the bounding box of the hud.
hudStyleBox' :: Getter HudChart (Maybe HudBox)
hudStyleBox' = to hudStyleBox_

hudBox_ :: HudChart -> Maybe HudBox
hudBox_ = boxes . (\x -> foldOf (#chartSection % charts') x <> foldOf (#hudSection % charts') x)

hudRebox_ :: HudChart -> Maybe HudBox -> HudChart
hudRebox_ cs r =
  cs
    & over #chartSection reprojectCharts
    & over #hudSection reprojectCharts
  where
    reprojectCharts = case (hudBox_ cs, hudStyleBox_ cs, r) of
      (Just hb, Just hsb, Just rebox') ->
        over chart' (projectWith (rebox' NH.- (hsb NH.- hb)) hb)
      (Nothing, Just hsb, Just rebox') ->
        over chart' (projectWith rebox' hsb)
      _ -> id

-- | lens between a HudChart and its hud bounding box, not including style.
--
-- Will only reset a HudBox if all dimensions are non-singular.
hudBox' :: Lens' HudChart (Maybe HudBox)
hudBox' =
  lens hudBox_ hudRebox_

appendHud :: ChartTree -> HudChart -> HudChart
appendHud cs x =
  x & over #hudSection (<> cs)

makeHuds :: [HudChart -> ChartTree] -> HudChart -> HudChart
makeHuds hs hc = over #hudSection (<> mconcat (fmap ($ hc) hs)) hc

fromHudChart :: HudChart -> ChartTree
fromHudChart hc = group (Just "chart") [view #chartSection hc] <> group (Just "hud") [view #hudSection hc]

-- | Combine huds and charts to form a new Chart using the supplied initial canvas and data dimensions. Note that chart data is transformed by this computation (a linear type might be useful here).
runHudWith ::
  -- | initial canvas
  CanvasBox ->
  -- | huds to add
  [Hud] ->
  -- | underlying chart
  ChartTree ->
  -- | integrated chart tree
  ChartTree
runHudWith cb hs cs =
  hs
    & List.sortOn (view (#phud % #priority))
    & List.groupBy (\a b -> view (#phud % #priority) a == view (#phud % #priority) b)
    & fmap (fmap (view (#phud % #item)))
    & foldl' (\x a -> makeHuds a x) hc0
    & fromHudChart
  where
    hc0 =
      HudChart
        (cs & set styleBox' (Just cb))
        mempty

-- | Combine huds and charts to form a new ChartTree with a supplied initial canvas dimension.
--
-- Note that the original chart data are transformed and irrevocably forgotten by this computation.
runHud ::
  -- | initial canvas dimension
  CanvasBox ->
  -- | huds
  [Hud] ->
  -- | underlying charts
  ChartTree ->
  -- | integrated chart list
  ChartTree
runHud ca hs cs = runHudWith ca hs cs

-- | Decorate a ChartTree with HudOptions
addHud :: ChartAspect -> HudOptions -> ChartTree -> ChartTree
addHud asp ho cs =
  runHudWith
    (initialCanvas asp (Just cs'))
    hs
    cs'
  where
    db = maybe one padSingletons (view box' cs)
    (mdb, hs) = toHuds ho db
    cs' = cs <> maybe mempty (\r -> bool (named "datapadding" [BlankChart defaultStyle [r]]) mempty (r == db)) mdb

-- | Compute a Rect representing the initial chart canvas from a 'ChartAspect' and maybe a 'ChartTree', typically before the addition of hud elements.
--
-- >>> canvas (FixedAspect 1.5) (Just $ unnamed [RectChart defaultRectStyle [one]])
-- Rect -0.75 0.75 -0.5 0.5
initialCanvas :: ChartAspect -> Maybe ChartTree -> Rect Double
initialCanvas (FixedAspect a) _ = aspect a
initialCanvas (CanvasAspect a) _ = aspect a
initialCanvas ChartAspect cs = maybe one (maybe one (padSingletons . aspect . ratio) . view styleBox') cs
initialCanvas UnscaledAspect cs = maybe one (maybe one padSingletons . view styleBox') cs

-- | Compute a Rect representing the final chart canvas from a 'ChartAspect' and maybe a 'ChartTree'. The difference between 'initialCanvas' and finalCanvas is using the actual chart canvas for CanvasAspect.
--
-- >>> finalCanvas (CanvasAspect 1.5) (Just $ unnamed [RectChart defaultRectStyle [one]])
-- Rect -0.5 0.5 -0.5 0.5
finalCanvas :: ChartAspect -> Maybe ChartTree -> Rect Double
finalCanvas (FixedAspect a) _ = aspect a
finalCanvas (CanvasAspect a) Nothing = aspect a
finalCanvas (CanvasAspect _) cs = finalCanvas ChartAspect cs
finalCanvas ChartAspect cs = maybe one (maybe one (padSingletons . aspect . ratio) . view styleBox') cs
finalCanvas UnscaledAspect cs = maybe one (maybe one padSingletons . view styleBox') cs

projectChartWith :: ChartAspect -> HudOptions -> ChartTree -> ChartTree
projectChartWith asp ho ct = ctFinal
  where
    csAndHud = addHud asp ho ct
    viewbox = finalCanvas asp (Just csAndHud)
    ctFinal = set styleBox' (Just viewbox) csAndHud

-- | Typical, configurable hud elements. Anything else can be hand-coded as a 'Hud'.
--
-- ![hud example](other/hudoptions.svg)
data HudOptions = HudOptions
  { axes :: [Priority AxisOptions],
    frames :: [Priority FrameOptions],
    legends :: [Priority LegendOptions],
    titles :: [Priority Title]
  }
  deriving (Eq, Show, Generic)

instance Semigroup HudOptions where
  (<>) (HudOptions a c l t) (HudOptions a' c' l' t') =
    HudOptions (a <> a') (c <> c') (l <> l') (t <> t')

instance Monoid HudOptions where
  mempty = HudOptions [] [] [] []

-- | The official hud options.
--
-- - A fixed chart aspect (width:height) of 1.5
--
-- - An x axis at the bottom and y axis at the left.
--
-- - The default tick style for each axis of an axis bar, tick glyphs (or marks), automated tick labels, and tick (or grid) lines.
--
-- - A high 'Priority' (and thus inner), low-opacity frame, representing the data area of the chart.
--
-- - A low priority (outer), transparent frame, providing some padding around the chart.
defaultHudOptions :: HudOptions
defaultHudOptions =
  HudOptions
    [ Priority 5 defaultXAxisOptions,
      Priority 5 defaultYAxisOptions
    ]
    [ Priority 1 defaultFrameOptions,
      Priority 20 (defaultFrameOptions & #buffer .~ 0.04 & #frame .~ Just clear)
    ]
    []
    []

-- | Make Huds and potential data box extension; from a HudOption and an initial data box.
toHuds :: HudOptions -> DataBox -> (Maybe DataBox, [Hud])
toHuds o db =
  (mdb,) $ fmap Hud $
    (as' & fmap (over #item (\ho -> \hc -> axisHud ho db' hc)))
      <> (view #frames o & fmap (over #item frameHud))
      <> (view #legends o & fmap (over #item legendHud))
      <> (view #titles o & fmap (over #item titleHud))
  where
    (mdb, as') = freezeAxes db (view #axes o)
    db' = fromMaybe db mdb

freezeAxes :: DataBox -> [Priority AxisOptions] -> (Maybe DataBox, [Priority AxisOptions])
freezeAxes db0 aos =
  foldr
    ( \ao (dbm, as') ->
        let (dbm', ao') = freezeTicks (fromMaybe db0 dbm) (view #item ao)
        in (dbm', as' <> [ao & set #item ao'])
    )
    (Nothing, [])
    aos

freezeTicks :: DataBox -> AxisOptions -> (Maybe DataBox, AxisOptions)
freezeTicks db a =
  bimap
    (fmap (\x -> placeRect (view #place a) x db))
    (\x -> a & set (#ticks % #style) x)
    (placeTicks (placeRange (view #place a) db) (view (#ticks % #style) a))

-- | compute tick components given style, ranges and formatting
makePlacedTicks :: Range Double -> TickStyle -> (Maybe (Range Double), [(Double, Text)])
makePlacedTicks r s =
  case s of
    TickNone -> (Nothing, [])
    TickRound f n e ->
      ( bool (space1 ticks0) Nothing (e == NoTickExtend),
        zip ticks0 (formatNs f ticks0)
      )
      where
        ticks0 = gridSensible OuterPos (e == NoTickExtend) r n
    TickExact f n -> (Nothing, zip ticks0 (formatNs f ticks0))
      where
        ticks0 = grid OuterPos r n
    TickLabels ls ->
      ( Nothing,
        zip
          ( project (Range 0 (fromIntegral $ length ls)) r
              <$> ((\x -> x - 0.5) . fromIntegral <$> [1 .. length ls])
          )
          ls
      )
    TickPlaced xs -> (Nothing, xs)

placeTicks :: Range Double -> TickStyle -> (Maybe (Range Double), TickStyle)
placeTicks r t@TickRound {} = (rExtended, TickPlaced tPlaced)
  where
    (rExtended, tPlaced) = makePlacedTicks r t
placeTicks _ t = (Nothing, t)

placeRect :: Place -> Range Double -> Rect Double -> Rect Double
placeRect pl' (Range a0 a1) (Rect x z y w) = case pl' of
  PlaceRight -> Rect x z a0 a1
  PlaceLeft -> Rect x z a0 a1
  _ -> Rect a0 a1 y w

placeRange :: Place -> HudBox -> Range Double
placeRange pl (Rect x z y w) = case pl of
  PlaceRight -> Range y w
  PlaceLeft -> Range y w
  _ -> Range x z

placeOrigin :: Place -> Double -> Point Double
placeOrigin pl x
  | pl == PlaceTop || pl == PlaceBottom = Point x 0
  | otherwise = Point 0 x

-- | Create an axis.
--
axisHud :: AxisOptions -> DataBox -> HudChart -> ChartTree
axisHud a db hc = group (Just "axis") [b, t]
  where
    b = maybe mempty (\x -> makeAxisBar (view #place a) x hc) (view #bar a)
    t = makeTick a db (appendHud b hc)

-- | alter a colour with a function
colourHudOptions :: (Colour -> Colour) -> HudOptions -> HudOptions
colourHudOptions f o =
  o
    & over (#frames % each % #item) fFrame
    & over (#titles % each % #item % #style % #color) f
    & over (#axes % each % #item) fAxis
    & over (#legends % each % #item) fLegend
  where
    fAxis :: AxisOptions -> AxisOptions
    fAxis a =
      a
        & over (#bar %? #style % #color) f
        & over (#ticks % #glyphTick %? #item % #color) f
        & over (#ticks % #glyphTick %? #item % #borderColor) f
        & over (#ticks % #textTick %? #item % #color) f
        & over
          (#ticks % #lineTick %? #item % #color) f
    fLegend :: LegendOptions -> LegendOptions
    fLegend a =
      a
        & over #textStyle (over #color f)
        & over #frame (fmap (over #color f . over #borderColor f))
    fFrame :: FrameOptions -> FrameOptions
    fFrame a =
      a
        & over (#frame % _Just) (over #color f . over #borderColor f)

-- | Placement of elements around (what is implicity but maybe shouldn't just be) a rectangular canvas
data Place
  = PlaceLeft
  | PlaceRight
  | PlaceTop
  | PlaceBottom
  | PlaceAbsolute (Point Double)
  deriving (Show, Eq, Generic)

-- | Flip Place to the opposite side, or unchanged if 'PlaceAbsolute'.
--
-- >>> flipPlace PlaceLeft
-- PlaceRight
flipPlace :: Place -> Place
flipPlace PlaceLeft = PlaceRight
flipPlace PlaceRight = PlaceLeft
flipPlace PlaceTop = PlaceBottom
flipPlace PlaceBottom = PlaceTop
flipPlace x = x

-- | axis options
data AxisOptions = AxisOptions
  { bar :: Maybe AxisBar,
    adjust :: Maybe Adjustments,
    ticks :: Ticks,
    place :: Place
  }
  deriving (Eq, Show, Generic)

-- | The official X-axis
defaultXAxisOptions :: AxisOptions
defaultXAxisOptions = AxisOptions (Just defaultAxisBar) (Just defaultAdjustments) defaultXTicks PlaceBottom

-- | The official Y-axis
defaultYAxisOptions :: AxisOptions
defaultYAxisOptions = AxisOptions (Just defaultAxisBar) (Just defaultAdjustments) defaultYTicks PlaceLeft

-- | The bar on an axis representing the x or y plane.
--
-- >>> defaultAxisBar
-- AxisBar {style = RectStyle {borderSize = 0.0, borderColor = Colour 0.00 0.00 0.00 0.00, color = Colour 0.05 0.05 0.05 0.40}, size = 4.0e-3, buffer = 1.0e-2, overhang = 2.0e-3}
data AxisBar = AxisBar
  { style :: Style,
    size :: Double,
    buffer :: Double,
    -- | extension over the edges of the axis range
    overhang :: Double
  }
  deriving (Show, Eq, Generic)

-- | The official axis bar
defaultAxisBar :: AxisBar
defaultAxisBar = AxisBar (defaultRectStyle & #borderSize .~ 0 & #borderColor .~ transparent & #color .~ set opac' 0.4 dark) 0.004 0.01 0.002

-- | Options for titles.  Defaults to center aligned, and placed at Top of the hud
--
-- >>> defaultTitle "title"
-- Title {text = "title", style = TextStyle {size = 0.12, color = Colour 0.05 0.05 0.05 1.00, anchor = AnchorMiddle, hsize = 0.45, vsize = 1.1, vshift = -0.25, rotation = Nothing, scalex = ScaleX, frame = Nothing}, place = PlaceTop, anchor = AnchorMiddle, buffer = 4.0e-2}
data Title = Title
  { text :: Text,
    style :: Style,
    place :: Place,
    anchor :: Anchor,
    buffer :: Double
  }
  deriving (Show, Eq, Generic)

-- | The official hud title
defaultTitle :: Text -> Title
defaultTitle txt =
  Title
    txt
    ( defaultTextStyle
        & #size .~ 0.12
    )
    PlaceTop
    AnchorMiddle
    0.04


data Buffered a = Buffered { item :: a, buffer :: Double } deriving (Eq, Show, Generic, Functor)

-- | axis tick markings
--
-- >>> defaultXTicks
data Ticks = Ticks
  { style :: TickStyle,
    glyphTick :: Maybe (Buffered Style),
    textTick :: Maybe (Buffered Style),
    lineTick :: Maybe (Buffered Style)
  }
  deriving (Show, Eq, Generic)

-- | The official glyph tick
defaultGlyphTick :: Buffered Style
defaultGlyphTick =
  Buffered
  (defaultGlyphStyle
    & #borderSize .~ 0.004
    & #shape .~ VLineGlyph
    & #color .~ set opac' 0.4 dark
    & #borderColor .~ set opac' 0.4 dark)
  0

-- | The official text tick
defaultTextTick :: Buffered Style
defaultTextTick =
  Buffered
  (defaultTextStyle & #size .~ 0.04)
  0.01

-- | The official line tick
defaultLineTick :: Buffered Style
defaultLineTick =
  Buffered
  (defaultLineStyle
    & #size .~ 5.0e-3
    & #color %~ set opac' 0.05)
  0

-- | The official X-axis tick
defaultXTicks :: Ticks
defaultXTicks =
  Ticks
    defaultTickStyle
    (Just (defaultGlyphTick & set (#item % #shape) VLineGlyph))
    (Just defaultTextTick)
    (Just defaultLineTick)

-- | The official Y-axis tick
defaultYTicks :: Ticks
defaultYTicks =
  Ticks
    defaultTickStyle
    (Just (defaultGlyphTick & set (#item % #shape) HLineGlyph))
    (Just defaultTextTick)
    (Just defaultLineTick)

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

-- | Lens between a FormatN and a TickStyle.
--
formatN' :: Lens' TickStyle (Maybe FormatN)
formatN' =
  lens formatN_ reformatN_

formatN_ :: TickStyle -> Maybe FormatN
formatN_ = \case
  TickRound f _ _ -> Just f
  TickExact f _ -> Just f
  _ -> Nothing

reformatN_ :: TickStyle -> Maybe FormatN -> TickStyle
reformatN_ ts Nothing = ts
reformatN_ (TickRound _ n e) (Just f) = TickRound f n e
reformatN_ (TickExact _ n) (Just f) = TickExact f n
reformatN_ ts _ = ts

-- | Lens between number of ticks and a TickStyle.
--
-- Only for TickRound and TickExact
numTicks' :: Lens' TickStyle (Maybe Int)
numTicks' =
  lens numTicks_ renumTicks_

numTicks_ :: TickStyle -> Maybe Int
numTicks_ = \case
  TickRound _ n _ -> Just n
  TickExact _ n -> Just n
  _ -> Nothing

renumTicks_ :: TickStyle -> Maybe Int -> TickStyle
renumTicks_ ts Nothing = ts
renumTicks_ (TickRound f _ e) (Just n) = TickRound f n e
renumTicks_ (TickExact f _) (Just n) = TickExact f n
renumTicks_ ts _ = ts
-- | Lens between a FormatN and a TickStyle.
--

tickExtend' :: Lens' TickStyle (Maybe TickExtend)
tickExtend' =
  lens tickExtend_ tickReExtend_

tickExtend_ :: TickStyle -> Maybe TickExtend
tickExtend_ = \case
  TickRound _ _ e -> Just e
  _ -> Nothing

tickReExtend_ :: TickStyle -> Maybe TickExtend -> TickStyle
tickReExtend_ ts Nothing = ts
tickReExtend_ (TickRound f n _) (Just e) = TickRound f n e
tickReExtend_ ts _ = ts

-- | The official tick style
--
-- >>> defaultTickStyle
-- TickRound (FormatN {fstyle = FSCommaPrec, sigFigs = Just 1, maxDistinguishIterations = 4, addLPad = True, cutRightZeros = True}) 8 TickExtend
defaultTickStyle :: TickStyle
defaultTickStyle = TickRound (FormatN FSCommaPrec (Just 1) 4 True True) 5 TickExtend

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
-- LegendOptions {size = 0.3, buffer = 0.1, vgap = 0.2, hgap = 0.1, textStyle = TextStyle {size = 0.18, color = Colour 0.05 0.05 0.05 1.00, anchor = AnchorMiddle, hsize = 0.45, vsize = 1.1, vshift = -0.25, rotation = Nothing, scalex = ScaleX, frame = Nothing}, innerPad = 0.1, outerPad = 2.0e-2, frame = Just (RectStyle {borderSize = 1.0e-2, borderColor = Colour 0.05 0.05 0.05 1.00, color = Colour 0.05 0.05 0.05 0.00}), place = PlaceRight, overallScale = 0.25, legendCharts = []}
data LegendOptions = LegendOptions
  { size :: Double,
    buffer :: Double,
    vgap :: Double,
    hgap :: Double,
    textStyle :: Style,
    innerPad :: Double,
    outerPad :: Double,
    frame :: Maybe Style,
    place :: Place,
    overallScale :: Double,
    scaleP :: ScaleP,
    legendCharts :: [(Text, [Chart])]
  }
  deriving (Show, Eq, Generic)

-- | The official legend options
defaultLegendOptions :: LegendOptions
defaultLegendOptions =
  LegendOptions
    0.3
    0.1
    0.2
    0.1
    (defaultTextStyle & set #size 0.16)
    0.1
    0.02
    (Just (defaultRectStyle & #borderSize .~ 0.005 & #borderColor .~ set opac' 1 dark & #color .~ set opac' 0 dark))
    PlaceRight
    0.25
    ScalePX
    []

-- | Options for hud frames
--
-- >>> defaultFrameOptions
-- FrameOptions {frame = Just (RectStyle {borderSize = 0.0, borderColor = Colour 0.00 0.00 0.00 0.00, color = Colour 1.00 1.00 1.00 0.02}), buffer = 0.0}
data FrameOptions = FrameOptions
  { frame :: Maybe Style,
    buffer :: Double
  }
  deriving (Eq, Show, Generic)

-- | The official hud frame
defaultFrameOptions :: FrameOptions
defaultFrameOptions = FrameOptions (Just (blob (grey 1 0.02))) 0

-- | Make a frame hud transformation.
frameHud :: FrameOptions -> HudChart -> ChartTree
frameHud o hc =
  case r of
    Nothing -> unnamed []
    Just r' -> case view #frame o of
      Nothing -> blank r'
      Just rs -> named "frame" [Chart rs (RectData [r'])]
  where
    r = padRect (view #buffer o) <$> view hudStyleBox' hc

bar_ :: Place -> AxisBar -> CanvasBox -> HudBox -> Chart
bar_ pl b (Rect x z y w) (Rect x' z' y' w') =
  Chart (view #style b) . RectData $
    case pl of
      PlaceTop ->
        [ Rect
            (x - b ^. #overhang)
            (z + b ^. #overhang)
            (w' + b ^. #buffer)
            (w' + b ^. #buffer + b ^. #size)
        ]
      PlaceBottom ->
        [ Rect
            (x - b ^. #overhang)
            (z + b ^. #overhang)
            (y' - b ^. #size - b ^. #buffer)
            (y' - b ^. #buffer)
        ]
      PlaceLeft ->
        [ Rect
            (x' - b ^. #size - b ^. #buffer)
            (x' - b ^. #buffer)
            (y - b ^. #overhang)
            (w + b ^. #overhang)
        ]
      PlaceRight ->
        [ Rect
            (z' + (b ^. #buffer))
            (z' + (b ^. #buffer) + (b ^. #size))
            (y - b ^. #overhang)
            (w + b ^. #overhang)
        ]
      PlaceAbsolute (Point x'' _) ->
        [ Rect
            (x'' + (b ^. #buffer))
            (x'' + (b ^. #buffer) + (b ^. #size))
            (y - b ^. #overhang)
            (w + b ^. #overhang)
        ]

makeAxisBar :: Place -> AxisBar -> HudChart -> ChartTree
makeAxisBar pl b hc = named "axisbar" (maybeToList c)
  where
    cb = view canvasBox' hc
    hb = view hudStyleBox' hc
    c = bar_ pl b <$> cb <*> hb

title_ :: Title -> HudBox -> Chart
title_ t hb =
  Chart
    (style' & #rotation .~ bool (Just rot) Nothing (rot == 0))
    (TextData [(t ^. #text, addp (placePosTitle t hb) (alignPosTitle t hb))])
  where
    style'
      | t ^. #anchor == AnchorStart =
          #anchor .~ AnchorStart $ t ^. #style
      | t ^. #anchor == AnchorEnd =
          #anchor .~ AnchorEnd $ t ^. #style
      | otherwise = t ^. #style
    rot' = fromMaybe 0 (t ^. #style % #rotation)
    rot
      | t ^. #place == PlaceRight = pi / 2 + rot'
      | t ^. #place == PlaceLeft = pi / 2 + rot'
      | otherwise = rot'

placePosTitle :: Title -> HudBox -> Point Double
placePosTitle t (Rect x z y w) =
  case t ^. #place of
    PlaceTop -> Point ((x + z) / 2.0) (w - y' + (t ^. #buffer))
    PlaceBottom -> Point ((x + z) / 2.0) (y - w' - (t ^. #buffer))
    PlaceLeft -> Point (x + y' - (t ^. #buffer)) ((y + w) / 2.0)
    PlaceRight -> Point (z + w' + (t ^. #buffer)) ((y + w) / 2.0)
    PlaceAbsolute p -> p
  where
    (Rect _ _ y' w') = styleBoxText (view #style t) (view #text t) zero

alignPosTitle :: Title -> HudBox -> Point Double
alignPosTitle t (Rect x z y w)
  | t ^. #anchor == AnchorStart
      && (t ^. #place == PlaceTop || t ^. #place == PlaceBottom) =
      Point ((x - z) / 2.0) 0.0
  | t ^. #anchor == AnchorStart
      && t ^. #place == PlaceLeft =
      Point 0.0 ((y - w) / 2.0)
  | t ^. #anchor == AnchorStart
      && t ^. #place == PlaceRight =
      Point 0.0 ((y - w) / 2.0)
  | t ^. #anchor == AnchorEnd
      && (t ^. #place == PlaceTop || t ^. #place == PlaceBottom) =
      Point ((-x + z) / 2.0) 0.0
  | t ^. #anchor == AnchorEnd
      && t ^. #place == PlaceLeft =
      Point 0.0 ((-y + w) / 2.0)
  | t ^. #anchor == AnchorEnd
      && t ^. #place == PlaceRight =
      Point 0.0 ((-y + w) / 2.0)
  | otherwise = Point 0.0 0.0

-- | title append transformation.
titleHud :: Title -> HudChart -> ChartTree
titleHud t hc = do
  named "title" (maybeToList $ title_ t <$> hb)
  where
    hb = view hudStyleBox' hc

placePos :: Place -> Double -> HudBox -> Point Double
placePos pl b (Rect x z y w) = case pl of
  PlaceTop -> Point 0 (w + b)
  PlaceBottom -> Point 0 (y - b)
  PlaceLeft -> Point (x - b) 0
  PlaceRight -> Point (z + b) 0
  PlaceAbsolute p -> p

textPos :: Place -> Style -> Double -> Point Double
textPos pl tt b = case pl of
  PlaceTop -> Point 0 (b - (tt ^. #vshift) * (tt ^. #vsize) * (tt ^. #size))
  PlaceBottom -> Point 0 (-b - (tt ^. #vshift) * (tt ^. #vsize) * (tt ^. #size) - (tt ^. #vsize) * (tt ^. #size))
  PlaceLeft ->
    Point
      (-b)
      ((tt ^. #vshift) * (tt ^. #vsize) * (tt ^. #size))
  PlaceRight ->
    Point
      b
      ((tt ^. #vshift) * (tt ^. #vsize) * (tt ^. #size))
  PlaceAbsolute p -> p

placeTextAnchor :: Place -> (Style -> Style)
placeTextAnchor pl
  | pl == PlaceLeft = #anchor .~ AnchorEnd
  | pl == PlaceRight = #anchor .~ AnchorStart
  | otherwise = id

placeGridLines :: Place -> HudBox -> Double -> Double -> [Point Double]
placeGridLines pl (Rect x z y w) a b
  | pl == PlaceTop || pl == PlaceBottom = [Point a (y - b), Point a (w + b)]
  | otherwise = [Point (x - b) a, Point (z + b) a]

-- | compute tick values and labels given options, ranges and formatting
ticksR :: TickStyle -> Range Double -> Range Double -> [(Double, Text)]
ticksR s d r =
  case s of
    TickNone -> []
    TickRound f n e -> zip (project r d <$> ticks0) (formatNs f ticks0)
      where
        ticks0 = gridSensible OuterPos (e == NoTickExtend) r n
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

-- | compute tick values in canvas space given placement, canvas box & data box
ticksPlacedCanvas :: TickStyle -> Place -> CanvasBox -> DataBox -> [(Double, Text)]
ticksPlacedCanvas ts pl cb db =
  first (project (placeRange pl db) (placeRange pl cb))
    <$> snd (makePlacedTicks (placeRange pl db) ts)


placePos' :: Place -> Double -> HudBox -> Rect Double -> Point Double
placePos' pl b (Rect x z y w) (Rect x' z' y' w') = case pl of
  PlaceTop -> Point 0 (w + b - y')
  PlaceBottom -> Point 0 (y - b - w')
  PlaceLeft -> Point (x - b - z') 0
  PlaceRight -> Point (z + b - x') 0
  PlaceAbsolute p -> p

-- | aka marks
tickGlyph ::
  Place ->
  Buffered Style ->
  TickStyle ->
  DataBox ->
  HudChart ->
  ChartTree
tickGlyph pl s ts db hc = named "tickglyph" (catMaybes $ maybeToList c)
  where
    -- FIXME: hudBox'
    sb = view hudBox' hc
    cb = view canvasBox' hc
    c = tickGlyph_ pl s ts <$> sb <*> cb <*> pure db

tickGlyph_ :: Place -> Buffered Style -> TickStyle -> CanvasBox -> CanvasBox -> DataBox -> Maybe Chart
tickGlyph_ pl (Buffered g b) ts sb cb db =
  case l of
    [] -> Nothing
    l' -> Just $ Chart g (GlyphData l')
  where
    l =
      addp (placePos' pl b sb bb) . placeOrigin pl
        <$> fmap fst (ticksPlacedCanvas ts pl cb db)
    bb = fromMaybe zero $ sbox (Chart g (GlyphData [zero]))



tickText_ ::
  Place ->
  Buffered Style ->
  TickStyle ->
  CanvasBox ->
  CanvasBox ->
  DataBox ->
  Maybe Chart
tickText_ pl (Buffered txts b) ts sb cb db =
  case l of
    [] -> Nothing
    _ -> Just $ Chart (placeTextAnchor pl txts) (TextData l)
  where
    l =
      swap . first (addp (addp (placePos pl b sb) (textPos pl txts b)) . placeOrigin pl)
        <$> ticksPlacedCanvas ts pl cb db

-- | aka tick labels
tickText ::
  Place ->
  Buffered Style ->
  TickStyle ->
  DataBox ->
  HudChart ->
  ChartTree
tickText pl s ts db hc = named "ticktext" (catMaybes $ maybeToList c)
  where
    sb = view hudStyleBox' hc
    cb = view canvasBox' hc
    c = tickText_ pl s ts <$> sb <*> cb <*> pure db

-- | aka grid lines
tickLine ::
  Place ->
  Buffered Style ->
  TickStyle ->
  DataBox ->
  HudChart ->
  ChartTree
tickLine pl (Buffered ls b) ts db hc =
  case cb of
    Nothing -> named "ticklines" []
    Just cb' ->
      let l = (\x -> placeGridLines pl cb' x b) <$> fmap fst (ticksPlacedCanvas ts pl cb' db)
       in named "ticklines" (bool [Chart ls (LineData l)] [] (null l))
  where
    cb = view canvasBox' hc

-- | Create tick glyphs (marks), lines (grid) and text (labels)
applyTicks ::
  Place ->
  Ticks ->
  DataBox ->
  HudChart ->
  ChartTree
applyTicks pl t db hc = group (Just "ticks") [g, l, t']
  where
    g = maybe mempty (\x -> tickGlyph pl x (t ^. #style) db hc) (t ^. #glyphTick)
    l = maybe mempty (\x -> tickText pl x (t ^. #style) db (appendHud g hc)) (t ^. #textTick)
    t' = maybe mempty (\x -> tickLine pl x (t ^. #style) db hc) (t ^. #lineTick)

-- | adjust Tick for sane font sizes etc
adjustTicks ::
  Adjustments ->
  HudBox ->
  DataBox ->
  Place ->
  Ticks ->
  Ticks
adjustTicks (Adjustments mrx ma mry ad) vb cs pl t
  | pl == PlaceBottom || pl == PlaceTop =
      if ad
        then
          ( case adjustSizeX > 1 of
              True ->
                ( case pl of
                    PlaceBottom -> #textTick %? #item % #anchor .~ AnchorEnd
                    PlaceTop -> #textTick %? #item % #anchor .~ AnchorStart
                    _ -> #textTick %? #item % #anchor .~ AnchorEnd
                )
                  . (#textTick %? #item % #size %~ (/ adjustSizeA))
                  $ (#textTick %? #item % #rotation ?~ pi / 4) t
              False -> (#textTick %? #item % #size %~ (/ adjustSizeA)) t
          )
        else t & #textTick %? #item % #size %~ (/ adjustSizeX)
  | otherwise -- pl `elem` [PlaceLeft, PlaceRight]
    =
      (#textTick %? #item % #size %~ (/ adjustSizeY)) t
  where
    max' [] = 1
    max' xs = maximum xs
    ra (Rect x z y w)
      | pl == PlaceTop || pl == PlaceBottom = Range x z
      | otherwise = Range y w
    asp = ra vb
    r = ra cs
    tickl = snd <$> ticksR (t ^. #style) asp r
    maxWidth :: Double
    maxWidth =
      maybe
        1
        ( \tt ->
            max' $
              (\(Rect x z _ _) -> z - x)
                . (\x -> styleBoxText (view #item tt) x (Point 0 0))
                <$> tickl
        )
        (view #textTick t)
    maxHeight =
      maybe
        1
        ( \tt ->
            max' $
              (\(Rect _ _ y w) -> w - y)
                . (\x -> styleBoxText (view #item tt) x (Point 0 0))
                <$> tickl
        )
        (view #textTick t)

    adjustSizeX :: Double
    adjustSizeX = max ((maxWidth / (upper asp - lower asp)) / mrx) 1
    adjustSizeY = max ((maxHeight / (upper asp - lower asp)) / mry) 1
    adjustSizeA = max ((maxHeight / (upper asp - lower asp)) / ma) 1

makeTick :: AxisOptions -> DataBox -> HudChart -> ChartTree
makeTick c db hc =
  case hb of
    Nothing -> named "ticks" []
    Just hb' -> do
      let adjTick = maybe (c ^. #ticks) (\x -> adjustTicks x hb' db (c ^. #place) (c ^. #ticks)) (c ^. #adjust)
      applyTicks (c ^. #place) adjTick db hc
  where
    -- FIXME: hudBox'
    hb = view hudBox' hc

-- | Make a legend from 'LegendOptions'
legendHud :: LegendOptions -> HudChart -> ChartTree
legendHud o hc = legendHud2 o (legendChart o & set (charts' % each % #style % #scaleP) (view #scaleP o)) hc

-- | Make a legend hud element, from a bespoke ChartTree.
legendHud2 :: LegendOptions -> ChartTree -> HudChart -> ChartTree
legendHud2 o lcs hc = do
  case sb of
    Nothing -> named "legend" []
    Just sb' -> placeLegend o sb' (over chart' (scaleChart (o ^. #overallScale)) lcs)
  where
    sb = view hudStyleBox' hc

placeLegend :: LegendOptions -> HudBox -> ChartTree -> ChartTree
placeLegend o hb t =
  case view styleBox' t of
    Nothing -> named "legend" []
    Just sb -> t & over chart' (moveChart (placeBeside_ (o ^. #place) (view #buffer o) hb sb))

placeBeside_ :: Place -> Double -> Rect Double -> Rect Double -> Point Double
placeBeside_ pl buff (Rect x z y w) (Rect x' z' y' w') =
  case pl of
    PlaceTop -> Point ((x + z) / 2.0) (buff + w + (w' - y') / 2.0)
    PlaceBottom -> Point ((x + z) / 2.0) (y - buff - (w' - y'))
    PlaceLeft -> Point (x - buff - (z' - x')) ((y + w) / 2.0)
    PlaceRight -> Point (z + buff) ((y + w) / 2.0)
    PlaceAbsolute p -> p

-- | frame a legend
legendFrame :: LegendOptions -> ChartTree -> ChartTree
legendFrame l content' =
  group (Just "legend") [borders, rename (Just "legendContent") content']
  where
    borders = mconcat $ [outer, inner] <> frame'
    outer = padChart (view #outerPad l) inner
    frame' = foldMap (\r -> [frameChart r 0 inner]) (view #frame l)
    inner = padChart (view #innerPad l) content'

-- | Make the contents portion of a legend
legendChart :: LegendOptions -> ChartTree
legendChart l = legendFrame l content'
  where
    content' =
      vert
        (l ^. #hgap)
        ( ( \(t, a) ->
              hori
                ((l ^. #vgap) + twidth - gapwidth t)
                (fmap unnamed [[t], a])
          )
            <$> es
        )
    es = reverse $ uncurry (legendEntry l) <$> view #legendCharts l
    twidth = maybe zero (\(Rect x z _ _) -> z - x) (styleBoxes (fst <$> es))
    gapwidth t = maybe 0 (\(Rect x z _ _) -> z - x) (sbox t)

legendText ::
  LegendOptions ->
  Text ->
  Chart
legendText l t =
  Chart (l ^. #textStyle & #anchor .~ AnchorStart) (TextData [(t, zero)])

legendizeChart ::
  LegendOptions ->
  Chart ->
  Chart
legendizeChart l c =
  case c of
    (Chart rs (RectData _)) -> Chart rs (RectData [Rect 0 (l ^. #size) 0 (l ^. #size)])
    (Chart ts (TextData t)) -> let txt = fromMaybe "text" (listToMaybe (fst <$> t)) in Chart (ts & #size .~ (l ^. #size / fromIntegral (Text.length txt))) (TextData [(txt, Point (0.5 * l ^. #size) (0.33 * l ^. #size))])
    (Chart gs (GlyphData _)) -> Chart (gs & #size .~ (l ^. #size)) (GlyphData [Point (0.5 * l ^. #size) (0.33 * l ^. #size)])
    (Chart ls (LineData _)) ->
      Chart
        (ls & #size %~ (/ (l ^. #overallScale)))
        (LineData [[Point 0 (1 * l ^. #size), Point (2 * l ^. #size) (1 * l ^. #size)]])
    (Chart ps (PathData _)) ->
      ( let cs =
              singletonQuad
                ( QuadPosition
                    (Point 0 0)
                    (Point (l ^. #size) (l ^. #size))
                    (Point (2 * l ^. #size) ((-1) * l ^. #size))
                )
         in Chart (ps & #borderSize .~ (l ^. #size)) (PathData cs)
      )
    _ -> blankChart (Rect 0 (l ^. #size) 0 (l ^. #size))

legendEntry ::
  LegendOptions ->
  Text ->
  [Chart] ->
  (Chart, [Chart])
legendEntry l t cs =
  (legendText l t, fmap (legendizeChart l) cs)

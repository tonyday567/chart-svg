{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RebindableSyntax #-}

-- | A hud stands for <https://en.wikipedia.org/wiki/Head-up_display head-up display>, and is a collective noun used to name chart elements that assist in data interpretation or otherwise annotate and decorate data.
--
-- This includes axes, titles, borders, frames, background canvaii, tick marks and tick value labels.
module Chart.Hud
  ( -- * Hud
    Hud (..),
    Priority (..),
    ChartBox,
    DataBox,
    HudChart (..),
    HudChartSection (..),
    hudChartBox',

    -- * Hud Processing
    runHudWith,

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
    Ticks (..),
    TickStyle (..),
    defaultGlyphTickStyleX,
    defaultGlyphTickStyleY,
    defaultTextTick,
    defaultLineTick,
    defaultXTicks,
    defaultYTicks,
    Tick (..),
    defaultTick,
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
    projectChartTreeWith,
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
import Optics.Core
import NumHask.Prelude hiding (to)

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
-- - the axes (5)
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

-- | A type for Rect to represent the bounding box of a chart.
type ChartBox = Rect Double

-- | A type for Rect to represent the bounding box of the data.
type DataBox = Rect Double

data HudChartSection = CanvasSection | CanvasStyleSection | HudSection | HudStyleSection deriving (Eq, Show, Generic)

hudChartBox' :: HudChartSection -> Getter HudChart (Maybe (Rect Double))
hudChartBox' CanvasSection = to (boxes . foldOf (#chartSection % charts'))
hudChartBox' CanvasStyleSection = to (styleBoxes . foldOf (#chartSection % charts'))
hudChartBox' HudSection = to (boxes . (\x -> foldOf (#chartSection % charts') x <> foldOf (#hudSection % charts') x))
hudChartBox' HudStyleSection = to (styleBoxes .(\x -> foldOf (#chartSection % charts') x <> foldOf (#hudSection % charts') x))

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
  ChartBox ->
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

-- | Compute a Rect representing the initial chart canvas from a 'ChartAspect' and maybe a 'ChartTree', before the addition of style and hud elements.
--
-- >>> canvas (FixedAspect 1.5) (Just $ unnamed [RectChart defaultRectStyle [one]])
-- Rect -0.75 0.75 -0.5 0.5
initialCanvas :: ChartAspect -> Maybe ChartTree -> Rect Double
initialCanvas (FixedAspect a) _ = aspect a
initialCanvas (CanvasAspect a) _ = aspect a
initialCanvas ChartAspect cs = maybe one (aspect . ratio . view safeBox') cs
initialCanvas UnscaledAspect cs = maybe one (view safeBox') cs

-- | Compute a Rect representing the final chart canvas from a 'ChartAspect' and maybe a 'ChartTree'. The difference between 'initialCanvas' and finalCanvas is using the actual chart canvas for CanvasAspect.
--
-- >>> finalCanvas (CanvasAspect 1.5) (Just $ unnamed [RectChart defaultRectStyle [one]])
-- Rect -0.5 0.5 -0.5 0.5
finalCanvas :: ChartAspect -> Maybe ChartTree -> Rect Double
finalCanvas (FixedAspect a) _ = aspect a
finalCanvas (CanvasAspect a) Nothing = aspect a
finalCanvas (CanvasAspect _) cs = finalCanvas ChartAspect cs
finalCanvas ChartAspect cs = maybe one (aspect . ratio . view safeBox') cs
finalCanvas UnscaledAspect cs = maybe one (view safeBox') cs

projectChartTreeWith :: ChartAspect -> HudOptions -> ChartTree -> ChartTree
projectChartTreeWith asp ho ct = ctFinal
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
    [ Priority 1 (defaultFrameOptions & set #anchorTo CanvasStyleSection),
      Priority 20 (defaultFrameOptions & #buffer .~ 0.04 & #frame .~ Just clear)
    ]
    []
    []

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
        & over (#ticks % #glyphTick %? #style % #color) f
        & over (#ticks % #glyphTick %? #style % #borderColor) f
        & over (#ticks % #textTick %? #style % #color) f
        & over
          (#ticks % #lineTick %? #style % #color) f
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
    overhang :: Double,
    -- | Which hud-chart section to anchor to
    anchorTo :: HudChartSection
  }
  deriving (Show, Eq, Generic)

-- | The official axis bar
defaultAxisBar :: AxisBar
defaultAxisBar = AxisBar (defaultRectStyle & #borderSize .~ 0 & #borderColor .~ transparent & #color .~ set opac' 0.4 dark) 0.004 0.01 0.002 CanvasSection

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

-- | axis tick markings
--
-- >>> defaultXTicks
data Ticks = Ticks
  { tick :: Tick,
    glyphTick :: Maybe TickStyle,
    textTick :: Maybe TickStyle,
    lineTick :: Maybe TickStyle
  }
  deriving (Show, Eq, Generic)

data TickStyle = TickStyle
  { style :: Style,
    anchorTo :: HudChartSection,
    buffer :: Double
  }
  deriving (Show, Eq, Generic)

-- | The official glyph tick
defaultGlyphTickStyleX :: TickStyle
defaultGlyphTickStyleX = TickStyle
  (defaultGlyphStyle
    & #borderSize .~ 0.004
    & #shape .~ VLineGlyph
    & #color .~ set opac' 0.4 dark
    & #borderColor .~ set opac' 0.4 dark
    & #scaleP .~ ScalePY)
  CanvasSection
  0.01

-- | The official glyph tick
defaultGlyphTickStyleY :: TickStyle
defaultGlyphTickStyleY = TickStyle
  (defaultGlyphStyle
    & #borderSize .~ 0.004
    & #shape .~ HLineGlyph
    & #color .~ set opac' 0.4 dark
    & #borderColor .~ set opac' 0.4 dark
    & #scaleP .~ ScalePX)
  CanvasSection
  0.01

-- | The official text tick
defaultTextTick :: TickStyle
defaultTextTick =
  TickStyle
  (defaultTextStyle & #size .~ 0.04)
  HudStyleSection
  0.01

-- | The official line tick
defaultLineTick :: TickStyle
defaultLineTick =
  TickStyle
  (defaultLineStyle
    & #size .~ 5.0e-3
    & #color %~ set opac' 0.05)
  CanvasSection
  0

-- | The official X-axis tick
defaultXTicks :: Ticks
defaultXTicks =
  Ticks
    defaultTick
    (Just defaultGlyphTickStyleX)
    (Just defaultTextTick)
    (Just defaultLineTick)

-- | The official Y-axis tick
defaultYTicks :: Ticks
defaultYTicks =
  Ticks
    defaultTick
    (Just defaultGlyphTickStyleY)
    (Just defaultTextTick)
    (Just defaultLineTick)

-- | Style of tick marks on an axis.
data Tick
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

-- | Lens between a FormatN and a Tick.
--
formatN' :: Lens' Tick (Maybe FormatN)
formatN' =
  lens formatN_ reformatN_

formatN_ :: Tick -> Maybe FormatN
formatN_ = \case
  TickRound f _ _ -> Just f
  TickExact f _ -> Just f
  _ -> Nothing

reformatN_ :: Tick -> Maybe FormatN -> Tick
reformatN_ ts Nothing = ts
reformatN_ (TickRound _ n e) (Just f) = TickRound f n e
reformatN_ (TickExact _ n) (Just f) = TickExact f n
reformatN_ ts _ = ts

-- | Lens between number of ticks and a Tick.
--
-- Only for TickRound and TickExact
numTicks' :: Lens' Tick (Maybe Int)
numTicks' =
  lens numTicks_ renumTicks_

numTicks_ :: Tick -> Maybe Int
numTicks_ = \case
  TickRound _ n _ -> Just n
  TickExact _ n -> Just n
  _ -> Nothing

renumTicks_ :: Tick -> Maybe Int -> Tick
renumTicks_ ts Nothing = ts
renumTicks_ (TickRound f _ e) (Just n) = TickRound f n e
renumTicks_ (TickExact f _) (Just n) = TickExact f n
renumTicks_ ts _ = ts
-- | Lens between a FormatN and a Tick.
--

tickExtend' :: Lens' Tick (Maybe TickExtend)
tickExtend' =
  lens tickExtend_ tickReExtend_

tickExtend_ :: Tick -> Maybe TickExtend
tickExtend_ = \case
  TickRound _ _ e -> Just e
  _ -> Nothing

tickReExtend_ :: Tick -> Maybe TickExtend -> Tick
tickReExtend_ ts Nothing = ts
tickReExtend_ (TickRound f n _) (Just e) = TickRound f n e
tickReExtend_ ts _ = ts

-- | The official tick style
--
-- >>> defaultTick
-- TickRound (FormatN {fstyle = FSCommaPrec, sigFigs = Just 1, maxDistinguishIterations = 4, addLPad = True, cutRightZeros = True}) 8 TickExtend
defaultTick :: Tick
defaultTick = TickRound (FormatN FSCommaPrec (Just 1) 4 True True) 5 TickExtend

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
    anchorTo :: HudChartSection,
    buffer :: Double
  }
  deriving (Eq, Show, Generic)

-- | The official hud frame
defaultFrameOptions :: FrameOptions
defaultFrameOptions = FrameOptions (Just (blob (grey 1 0.02))) HudStyleSection 0

-- * Huds
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
    (\x -> a & set (#ticks % #tick) x)
    (placeTicks (placeRange (view #place a) db) (view (#ticks % #tick) a))

placeRect :: Place -> Range Double -> Rect Double -> Rect Double
placeRect pl' (Range a0 a1) (Rect x z y w) = case pl' of
  PlaceRight -> Rect x z a0 a1
  PlaceLeft -> Rect x z a0 a1
  _ -> Rect a0 a1 y w

placeRange :: Place -> ChartBox -> Range Double
placeRange pl (Rect x z y w) = case pl of
  PlaceRight -> Range y w
  PlaceLeft -> Range y w
  _ -> Range x z

placeTicks :: Range Double -> Tick -> (Maybe (Range Double), Tick)
placeTicks r t@TickRound {} = (rExtended, TickPlaced tPlaced)
  where
    (rExtended, tPlaced) = makePlacedTicks r t
placeTicks _ t = (Nothing, t)

-- | compute tick components given style, ranges and formatting
makePlacedTicks :: Range Double -> Tick -> (Maybe (Range Double), [(Double, Text)])
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

-- | Create an axis.
--
axisHud :: AxisOptions -> DataBox -> HudChart -> ChartTree
axisHud a db hc = group (Just "axis") [b, t]
  where
    b = maybe mempty (\x -> axisBarHud (view #place a) x hc) (view #bar a)
    t = tickHud a db (appendHud b hc)

axisBarHud :: Place -> AxisBar -> HudChart -> ChartTree
axisBarHud pl b hc = maybe mempty (named "axisbar" . pure) c
  where
    canvasBox = view (hudChartBox' CanvasSection) hc
    anchoredBox = view (hudChartBox' (view #anchorTo b)) hc
    c = bar_ pl b <$> canvasBox <*> anchoredBox

bar_ :: Place -> AxisBar -> Rect Double -> Rect Double -> Chart
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

-- * tick hud creation
tickHud :: AxisOptions -> DataBox -> HudChart -> ChartTree
tickHud ao db hc = maybe mempty ts (view (hudChartBox' HudStyleSection) hc)
  where
    ts b = applyTicks (view #place ao) (adjTick b) db hc
    adjTick b = maybe (view #ticks ao) (\x -> adjustTicks x b db (view #place ao) (view #ticks ao)) (view #adjust ao)

-- | Create tick glyphs (marks), lines (grid) and text (labels)
applyTicks ::
  Place ->
  Ticks ->
  DataBox ->
  HudChart ->
  ChartTree
applyTicks pl t db hc = group (Just "ticks") [lt, gt, tt]
  where
    lt = maybe mempty (\x -> tickLine pl x (t ^. #tick) db hc) (t ^. #lineTick)
    gt = maybe mempty (\x -> tickGlyph pl x (t ^. #tick) db hc) (t ^. #glyphTick)
    tt = maybe mempty (\x -> tickText pl x (t ^. #tick) db (appendHud gt hc)) (t ^. #textTick)

-- | adjust Tick for sane font sizes etc
adjustTicks ::
  Adjustments ->
  ChartBox ->
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
                    PlaceBottom -> #textTick %? #style % #anchor .~ AnchorEnd
                    PlaceTop -> #textTick %? #style % #anchor .~ AnchorStart
                    _ -> #textTick %? #style % #anchor .~ AnchorEnd
                )
                  . (#textTick %? #style % #size %~ (/ adjustSizeA))
                  $ (#textTick %? #style % #rotation ?~ pi / 4) t
              False -> (#textTick %? #style % #size %~ (/ adjustSizeA)) t
          )
        else t & #textTick %? #style % #size %~ (/ adjustSizeX)
  | otherwise -- pl `elem` [PlaceLeft, PlaceRight]
    =
      (#textTick %? #style % #size %~ (/ adjustSizeY)) t
  where
    max' [] = 1
    max' xs = maximum xs
    ra (Rect x z y w)
      | pl == PlaceTop || pl == PlaceBottom = Range x z
      | otherwise = Range y w
    asp = ra vb
    r = ra cs
    tickl = snd <$> ticksR (t ^. #tick) asp r
    maxWidth :: Double
    maxWidth =
      maybe
        1
        ( \tt ->
            max' $
              (\(Rect x z _ _) -> z - x)
                . (\x -> styleBoxText (view #style tt) x (Point 0 0))
                <$> tickl
        )
        (view #textTick t)
    maxHeight =
      maybe
        1
        ( \tt ->
            max' $
              (\(Rect _ _ y w) -> w - y)
                . (\x -> styleBoxText (view #style tt) x (Point 0 0))
                <$> tickl
        )
        (view #textTick t)

    adjustSizeX :: Double
    adjustSizeX = max ((maxWidth / (upper asp - lower asp)) / mrx) 1
    adjustSizeY = max ((maxHeight / (upper asp - lower asp)) / mry) 1
    adjustSizeA = max ((maxHeight / (upper asp - lower asp)) / ma) 1

-- | compute tick values and labels given options, ranges and formatting
ticksR :: Tick -> Range Double -> Range Double -> [(Double, Text)]
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


-- | aka marks
tickGlyph ::
  Place ->
  TickStyle ->
  Tick ->
  DataBox ->
  HudChart ->
  ChartTree
tickGlyph pl s ts db hc = maybe mempty (named "tickglyph" . pure) c
  where
    anchorBox = view (hudChartBox' (view #anchorTo s)) hc
    canvasBox = view (hudChartBox' CanvasSection) hc
    c = case (canvasBox, anchorBox) of
          (Just cb, Just ab) -> Just $ Chart (view #style s) (GlyphData ps)
            where
              ps = placePosTick pl (view #buffer s) ab bb . fst <$> ticksPlacedCanvas ts pl cb db
              bb = fromMaybe zero $ sbox (Chart (view #style s) (GlyphData [zero]))
          _ -> Nothing

placePosTick :: Place -> Double -> ChartBox -> Rect Double -> Double -> Point Double
placePosTick pl b (Rect x z y w) (Rect x' z' y' w') pos = case pl of
  PlaceTop -> Point pos (w + b - y')
  PlaceBottom -> Point pos (y - b - w')
  PlaceLeft -> Point (x - b - z') pos
  PlaceRight -> Point (z + b - x') pos
  PlaceAbsolute p -> p + Point 0 pos

-- | compute tick positions and string values in canvas space given placement, the canvas box & data box
ticksPlacedCanvas :: Tick -> Place -> ChartBox -> DataBox -> [(Double, Text)]
ticksPlacedCanvas ts pl cb db =
  first (project (placeRange pl db) (placeRange pl cb))
    <$> snd (makePlacedTicks (placeRange pl db) ts)

-- | aka tick labels
tickText ::
  Place ->
  TickStyle ->
  Tick ->
  DataBox ->
  HudChart ->
  ChartTree
tickText pl s ts db hc = maybe mempty (named "ticktext" . maybeToList) c
  where
    anchorBox = view (hudChartBox' (view #anchorTo s)) hc
    cb = view (hudChartBox' CanvasSection) hc
    c = tickText_ pl s ts <$> anchorBox <*> cb <*> pure db

tickText_ ::
  Place ->
  TickStyle ->
  Tick ->
  ChartBox ->
  ChartBox ->
  DataBox ->
  Maybe Chart
tickText_ pl s ts sb cb db =
  case l of
    [] -> Nothing
    _ -> Just $ Chart (placeTextAnchor pl (view #style s)) (TextData l)
  where
    l =
      swap . first (addp (addp (placePos pl (view #buffer s) sb) (textPos pl (view #style s) (view #buffer s))) . placeOrigin pl)
        <$> ticksPlacedCanvas ts pl cb db

placeOrigin :: Place -> Double -> Point Double
placeOrigin pl x
  | pl == PlaceTop || pl == PlaceBottom = Point x 0
  | otherwise = Point 0 x

-- | aka grid lines
tickLine ::
  Place ->
  TickStyle ->
  Tick ->
  DataBox ->
  HudChart ->
  ChartTree
tickLine pl s ts db hc =
  case cb of
    Nothing -> mempty
    Just cb' ->
      let l = (\x -> placeGridLines pl cb' x (view #buffer s)) <$> fmap fst (ticksPlacedCanvas ts pl cb' db)
       in bool (named "ticklines" [Chart (view #style s) (LineData l)]) mempty (null l)
  where
    cb = view (hudChartBox' (view #anchorTo s)) hc

placeGridLines :: Place -> ChartBox -> Double -> Double -> [Point Double]
placeGridLines pl (Rect x z y w) a b
  | pl == PlaceTop || pl == PlaceBottom = [Point a (y - b), Point a (w + b)]
  | otherwise = [Point (x - b) a, Point (z + b) a]

-- | title append transformation.
titleHud :: Title -> HudChart -> ChartTree
titleHud t hc = maybe mempty ((named "title" . pure) . title_ t) hb
  where
    hb = view (hudChartBox' HudStyleSection) hc

title_ :: Title -> ChartBox -> Chart
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

placePosTitle :: Title -> ChartBox -> Point Double
placePosTitle t (Rect x z y w) =
  case t ^. #place of
    PlaceTop -> Point ((x + z) / 2.0) (w - y' + (t ^. #buffer))
    PlaceBottom -> Point ((x + z) / 2.0) (y - w' - (t ^. #buffer))
    PlaceLeft -> Point (x + y' - (t ^. #buffer)) ((y + w) / 2.0)
    PlaceRight -> Point (z + w' + (t ^. #buffer)) ((y + w) / 2.0)
    PlaceAbsolute p -> p
  where
    (Rect _ _ y' w') = styleBoxText (view #style t) (view #text t) zero

alignPosTitle :: Title -> ChartBox -> Point Double
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

placePos :: Place -> Double -> ChartBox -> Point Double
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

-- | Make a frame hud transformation.
frameHud :: FrameOptions -> HudChart -> ChartTree
frameHud o hc =
  case r of
    Nothing -> mempty
    Just r' -> case view #frame o of
      Nothing -> named "frame" [Chart defaultStyle (BlankData [r'])]
      Just rs -> named "frame" [Chart rs (RectData [r'])]
  where
    r = padRect (view #buffer o) <$> view (hudChartBox' (view #anchorTo o)) hc

-- | Make a legend from 'LegendOptions'
legendHud :: LegendOptions -> HudChart -> ChartTree
legendHud o hc =
  legendHudWith (legendChart o & set (charts' % each % #style % #scaleP) (view #scaleP o)) o hc

-- | Make a legend using a bespoke ChartTree.
legendHudWith :: ChartTree -> LegendOptions -> HudChart -> ChartTree
legendHudWith lcs o hc = maybe mempty (\b -> placeLegend o b (over chart' (scaleChart (o ^. #overallScale)) lcs)) (view (hudChartBox' HudStyleSection) hc)

placeLegend :: LegendOptions -> ChartBox -> ChartTree -> ChartTree
placeLegend o hb t = maybe mempty (\b -> t & over chart' (moveChart (placeBeside_ (o ^. #place) (view #buffer o) hb b))) (view styleBox' t)

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


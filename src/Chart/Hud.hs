{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Chart API
module Chart.Hud
  ( -- * Hud types
    Hud (..),
    Huds (..),
    simAppend,
    simConcat,
    frameHud,
    frame,
    HudOptions (..),
    defaultHudOptions,
    colourHudOptions,
    defaultCanvas,
    runHudWith,
    runHud,
    fromHudOptions,
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
    Adjustments (..),
    defaultAdjustments,
    LegendOptions (..),
    defaultLegendOptions,
    legendHud,
  )
where

import Optics.Core
import Chart.Primitive
import Chart.Style
import Control.Monad.State.Lazy
import Data.Bifunctor
import Data.Bool
import Data.Colour
import Data.Foldable hiding (sum)
import Data.FormatN
import Data.List.NonEmpty (NonEmpty(..), fromList)
import Data.Maybe
import Data.Text (Text)
import GHC.Generics
import Prelude
import Data.Path
import Chart.Data
import qualified Data.List as List

-- * Hud

-- | State for tracking Hud dimensions:
--
-- - hudCharts: charts currently in state. In general, this includes hud elements taht have been processed.
--
-- - chartDim: the rectangular dimension of the physical representation of a chart on the screen so that new hud elements can be appended. Adding a hud piece tends to expand the chart dimension.
--
-- - canvasDim: the rectangular dimension of the canvas on which data will be represented. At times appending a hud element will cause the canvas dimension to shift.
--
-- - dataDim: the rectangular dimension of the data being represented. Adding hud elements can cause this to change.
data HudState = HudState
  { hudCharts :: [Chart],
    chartDim :: Rect Double,
    canvasDim :: Rect Double,
    dataDim :: Rect Double
  }
  deriving (Eq, Show, Generic)

-- | The priority of a Hud element or transformation
type Priority = Int

-- | Heads-up-display additions to charts
data Hud a =
  Hud {
    -- | priority for ordering of transformations
    priority :: Priority,
    -- | state transformation of charts
    hud :: State HudState a
  } deriving (Functor, Generic)

-- | Heads-up-display additions to charts
newtype Huds =
  Huds {
    hudList :: [Hud ()]
  }

instance Semigroup Huds where
  (<>) (Huds h) (Huds h') = Huds $ h <> h'

instance Monoid Huds where
  mempty = Huds []

{-
instance Applicative Huds where
  pure a = Hud maxBound (pure a)
  (<*>) (Hud fpr fa) (Hud apr aa) = Hud (min fpr apr) (fa <*> aa)

instance (Semigroup a) => Semigroup (Hud a) where
  (<>) (Hud p1 h1) (Hud p2 h2) =
    case compare p1 p2 of
      EQ -> Hud p1 (simAppend h1 h2)
      LT -> Hud p1 $ (<>) <$> h1 <*> h2
      GT -> Hud p2 $ (<>) <$> h2 <*> h1

instance (Monoid a) => Monoid (Hud a) where
  mempty = Hud maxBound (pure mempty)
-}

-- | run two transformations simultaneously (process using the initial dimensions).
simAppend :: (Semigroup a) => State HudState a -> State HudState a -> State HudState a
simAppend fa fb = do
  s <- get
  let (a, HudState cs ch ca d) = runState fa s
  let (a', HudState cs' ch' ca' d') = runState fb (s & set #hudCharts cs)
  put (HudState cs' (ch <> ch') (ca <> ca') (d <> d'))
  pure $ a <> a'

simConcat :: (Semigroup a, Foldable f) => f (State HudState a) -> State HudState a
simConcat = foldr1 simAppend

-- | Apply a ChartAspect
-- FIXME:
-- chartDim not updated
chartAspectHud :: Priority -> ChartAspect -> Hud ()
chartAspectHud p fa = Hud p $ do
  canvasd <- gets (view #canvasDim)
  chartd <- gets (view #chartDim)
  let proj =
        case fa of
          FixedAspect a -> projectCharts (aspect a)
          CanvasAspect a ->
            projectCharts (aspect (a * ratio canvasd / ratio chartd))
          ChartAspect -> projectCharts (aspect $ ratio chartd)
          UnadjustedAspect -> id
  modify (over #hudCharts proj)

-- | overlay a frame with additive padding
--
frameHud :: RectStyle -> Double -> Priority -> Hud ()
frameHud rs p pr = Hud pr $ do
  cs <- gets (view #hudCharts)
  let pad = padRect p (styleBoxes cs)
  modify (over #hudCharts (RectChart rs [pad]:))
  modify (over #chartDim (<> pad))

-- | frame some charts
--
frame :: RectStyle -> Double -> [Chart] -> [Chart]
frame rs p cs = runHud Nothing (Huds [frameHud rs p 0]) cs

-- | Combine huds and charts to form a new Chart using the supplied initial canvas and data dimensions. Note that chart data is transformed by this computation (and the use of a linear type is an open question).
runHudWith ::
  -- | initial canvas dimension
  Maybe (Rect Double) ->
  -- | initial data dimension
  Rect Double ->
  -- | huds to add
  Huds ->
  -- | underlying chart
  [Chart] ->
  -- | integrated chart list
  [Chart]
runHudWith ca xs (Huds hs) cs =
   hs &
   List.sortOn (view #priority) &
   List.groupBy (\a b-> view #priority a == view #priority b) &
   mapM_ (simConcat . fmap (view #hud)) &
   flip execState (HudState cs' ca' da' xs) &
   view #hudCharts
  where
    da' = boxes cs'
    ca' = styleBoxes cs'
    cs' = maybe cs (\proj -> projectWith proj xs <$> cs) ca

-- | Combine huds and charts to form a new [Chart] with an optional supplied initial canvas dimension.
--
-- Note that the original chart data are transformed and irrevocably lost by this computation.
--
-- >>> runHud (Just one) (fromHudOptions defaultHudOptions) [RectChart defaultRectStyle [one]]
--
runHud ::
  -- | initial canvas dimension
  Maybe (Rect Double) ->
  -- | hud
  Huds ->
  -- | underlying charts
  [Chart] ->
  -- | integrated chart list
  [Chart]
runHud ca hs cs = runHudWith ca (padSingletons $ boxes cs) hs cs

-- | Typical configurable hud elements. Anything else can be hand-coded as a 'Hud'.
--
-- ![hud example](other/hudoptions.svg)
data HudOptions = HudOptions
  { axes :: [(Priority, AxisOptions)],
    canvii :: [(Priority, RectStyle)],
    legends :: [(Priority, LegendOptions)],
    titles :: [(Priority, Title)]
  } deriving (Eq, Show, Generic)

instance Semigroup HudOptions where
  (<>) (HudOptions a c l t) (HudOptions a' c' l' t') =
    HudOptions (a <> a') (c <> c') (l <> l') (t <> t')

instance Monoid HudOptions where
  mempty = HudOptions [] [] [] []

-- | The official hud options.
defaultHudOptions :: HudOptions
defaultHudOptions =
  HudOptions
    [
      (5, defaultAxisOptions),
      (5, (defaultAxisOptions & set #place PlaceLeft))
    ]
    [(1, defaultCanvas)]
    []
    []

-- | Make a Hud from a HudOption
--
fromHudOptions :: HudOptions -> Huds
fromHudOptions o = Huds $
  (view #axes o & fmap (uncurry Hud . second axis)) <>
  (view #canvii o & fmap (uncurry Hud . second canvas)) <>
  (view #legends o & fmap (uncurry Hud . second legend)) <>
  (view #titles o & fmap (uncurry Hud . second title))

axis :: AxisOptions -> State HudState ()
axis a = do
  a' <- freezeTicks a
  -- hardcodes axis bar before ticks
  maybe (pure ()) (makeAxisBar (view #place a)) (view #axisBar a)
  makeTick a'

-- FIXME:
-- charts are reprojected, but chartDim not recalculated
-- What about canvasDim?
freezeTicks :: AxisOptions -> State HudState AxisOptions
freezeTicks a = do
  dd <- gets (view #dataDim)
  let (newTickStyle, extraDataDim) =
        freezeTicks_ (view #place a) dd (view (#axisTick % #tstyle) a)
  maybe (pure ())
    (\x -> do
        modify (over #hudCharts (fmap (projectWith (dd <> x) dd)))
        modify (over #dataDim (<> x))) extraDataDim
  pure $ a & set (#axisTick % #tstyle) newTickStyle

-- FIXME: chartDim not recalced (occurs in legendHud)
legend :: LegendOptions -> State HudState ()
legend o = do
  ca <- gets (view #chartDim)
  modify (over #hudCharts (placeChart ca <>))
  where
    cs = legendChart o (view #content o)
    placeChart ca = moveChart (placel (o ^. #lplace) ca (styleBoxes cs)) <$> cs
    placel pl (Rect x z y w) (Rect x' z' y' w') =
      case pl of
        PlaceTop -> Point ((x + z) / 2.0) (w + (w' - y') / 2.0)
        PlaceBottom -> Point ((x + z) / 2.0) (y - (w' - y' / 2.0))
        PlaceLeft -> Point (x - (z' - x') / 2.0) ((y + w) / 2.0)
        PlaceRight -> Point (z + (z' - x') / 2.0) ((y + w) / 2.0)
        PlaceAbsolute p -> p

-- | Make a legend hud element taking into account the chart.
legendHud :: LegendOptions -> [Chart] -> State HudState ()
legendHud l lcs = do
  ca <- gets (view #chartDim)
  cs <- gets (view #hudCharts)
  let cs' = place' ca (scaleChart (l ^. #lscale) <$> lcs) <> cs
  modify (set #chartDim (styleBoxes cs'))
  modify (set #hudCharts cs')
  where
    place' ca xs = moveChart (placel (l ^. #lplace) ca (styleBoxes xs)) <$> xs
    placel pl (Rect x z y w) (Rect x' z' y' w') =
      case pl of
        PlaceTop -> Point ((x + z) / 2.0) (w + (w' - y') / 2.0)
        PlaceBottom -> Point ((x + z) / 2.0) (y - (w' - y' / 2.0))
        PlaceLeft -> Point (x - (z' - x') / 2.0) ((y + w) / 2.0)
        PlaceRight -> Point (z + (z' - x') / 2.0) ((y + w) / 2.0)
        PlaceAbsolute p -> p

{-
-- | Make huds from a HudOptions.
--
-- Some huds, such as the creation of tick values, can extend the data dimension of a chart, so we return a blank chart with the new data dimension.
-- The complexity internally to this function is due to the creation of ticks and, specifically, 'gridSensible', which is not idempotent. As a result, a tick calculation that does extends the data area, can then lead to new tick values when applying TickRound etc.
makeHud :: Rect Double -> HudOptions -> ([Hud ()], [Chart])
makeHud xs cfg =
  ([axes] <> can <> titles <> l, xsext)
  where
    xs' = padSingletons xs
    can = foldMap (\x -> [canvas x]) (cfg ^. #hudCanvas)
    titles = title <$> (cfg ^. #hudTitles)
    ticks =
      (\a -> freezeTicks (a ^. #place) xs' (a ^. #axisTick % #tstyle))
        <$> (cfg ^. #hudAxes)
    hudaxes =
      zipWith
        (\c t -> c & #axisTick % #tstyle .~ fst t)
        (cfg ^. #hudAxes)
        ticks
    tickRects = catMaybes (snd <$> ticks)
    xsext = bool [BlankChart (NonEmpty.fromList tickRects)] [] (null tickRects)
    axes =
      simConcat $
        ( \x ->
            maybe mempty (makeAxisBar (x ^. #place)) (x ^. #axisBar)
              <> makeTick x
        )
          <$> hudaxes
    l =
      foldMap
        (\(lo, ats) -> [legendHud lo (legendChart ats lo)])
        (cfg ^. #hudLegend)


-}

-- | alter a colour with a function
colourHudOptions :: (Colour -> Colour) -> HudOptions -> HudOptions
colourHudOptions f o =
  o &
  -- over #canvii (fmap (second (over #color f))) &
  over #titles (fmap (second (over (#style % #color) f))) &
  over #axes (fmap (second fAxis)) &
  over #legends (fmap (second fLegend))
  where
    fAxis :: AxisOptions -> AxisOptions
    fAxis a =
      (a &
       over #axisBar (fmap (over (#rstyle % #color) f)) &
       over (#axisTick % #gtick)
         (fmap (first (over #color f . over #borderColor f))) &
       over (#axisTick % #ttick)
         (fmap (first (over #color f))) &
       over (#axisTick % #ltick)
         (fmap (first (over #color f))))
    fLegend :: LegendOptions -> LegendOptions
    fLegend a =
      (a &
       over #ltext (over #color f) &
       over #legendFrame (fmap (over #color f . over #borderColor f))
      )

-- | The official hud canvas
defaultCanvas :: RectStyle
defaultCanvas = blob (Colour 1 1 1 0.02)

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
-- AxisBar {rstyle = RectStyle {borderSize = 0.0, borderColor = Colour 0.00 0.00 0.00 0.00, color = Colour 0.05 0.05 0.05 0.40}, wid = 4.0e-3, buff = 1.0e-2}
data AxisBar = AxisBar
  { rstyle :: RectStyle,
    wid :: Double,
    buff :: Double,
    -- | extension over the edges of the axis range
    overhang :: Double
  }
  deriving (Show, Eq, Generic)

-- | The official axis bar
defaultAxisBar :: AxisBar
defaultAxisBar = AxisBar (RectStyle 0 transparent (set opac' 0.4 dark)) 0.004 0.01 0.002

-- | Options for titles.  Defaults to center aligned, and placed at Top of the hud
--
-- >>> defaultTitle "title"
-- Title {text = "title", style = TextStyle {size = 0.12, color = Colour 0.05 0.05 0.05 1.00, anchor = AnchorMiddle, hsize = 0.5, vsize = 1.45, nudge1 = -0.2, rotation = Nothing}, place = PlaceTop, anchor = AnchorMiddle, buff = 4.0e-2}
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
    ( defaultTextStyle
        & #size .~ 0.12
    )
    PlaceTop
    AnchorMiddle
    0.04

-- | xy coordinate markings
--
-- >>> defaultTick
-- Tick {tstyle = TickRound (FormatComma (Just 2)) 8 TickExtend, gtick = Just (GlyphStyle {size = 3.0e-2, color = Colour 0.05 0.05 0.05 0.40, borderColor = Colour 0.05 0.05 0.05 0.40, borderSize = 2.0e-3, shape = VLineGlyph, rotation = Nothing, translate = Nothing},1.25e-2), ttick = Just (TextStyle {size = 5.0e-2, color = Colour 0.05 0.05 0.05 1.00, anchor = AnchorMiddle, hsize = 0.5, vsize = 1.45, nudge1 = -0.2, rotation = Nothing},1.5e-2), ltick = Just (LineStyle {size = 5.0e-3, color = Colour 0.05 0.05 0.05 0.05, linecap = Nothing, linejoin = Nothing, dasharray = Nothing, dashoffset = Nothing},0.0)}
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
    & #borderSize .~ 0.004
    & #shape .~ VLineGlyph
    & #color .~ set opac' 0.4 dark
    & #borderColor .~ set opac' 0.4 dark

-- | The official text tick
defaultTextTick :: TextStyle
defaultTextTick =
  defaultTextStyle & #size .~ 0.05

-- | The official line tick
defaultLineTick :: LineStyle
defaultLineTick =
  defaultLineStyle
    & #size .~ 5.0e-3
    & #color %~ set opac' 0.05

-- | The official tick
defaultTick :: Tick
defaultTick =
  Tick
    defaultTickStyle
    (Just (defaultGlyphTick, 0.017))
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
defaultTickStyle = TickRound (FormatComma (Just 2)) 8 NoTickExtend

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
-- LegendOptions {lsize = 0.3, vgap = 0.2, hgap = 0.1, ltext = TextStyle {size = 0.12, color = Colour 0.05 0.05 0.05 1.00, anchor = AnchorMiddle, hsize = 0.5, vsize = 1.45, nudge1 = -0.2, rotation = Nothing}, lmax = 10, innerPad = 0.1, outerPad = 2.0e-2, legendFrame = Just (RectStyle {borderSize = 1.0e-2, borderColor = Colour 0.05 0.05 0.05 1.00, color = Colour 0.05 0.05 0.05 0.00}), lplace = PlaceRight, lscale = 0.25}
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
    lscale :: Double,
    content :: [(Text, Chart)]
  }
  deriving (Show, Eq, Generic)

-- | The official legend options
defaultLegendOptions :: LegendOptions
defaultLegendOptions =
  LegendOptions
    0.3
    0.2
    0.1
    ( defaultTextStyle
        & #size .~ 0.12
    )
    10
    0.1
    0.02
    (Just (RectStyle 0.01 (set opac' 1 dark) (set opac' 0 dark)))
    PlaceRight
    0.25
    []

-- | convert TickRound to TickPlaced and potentially an extension to the data range
freezeTicks_ :: Place -> Rect Double -> TickStyle -> (TickStyle, Maybe (Rect Double))
freezeTicks_ pl xs' ts@TickRound {} = maybe (ts, Nothing) (\x -> (TickPlaced (zip ps ls), Just x)) ((\x -> replaceRange pl x xs') <$> ext)
  where
    (TickComponents ps ls ext) = makeTicks ts (placeRange pl xs')
    replaceRange :: Place -> Range Double -> Rect Double -> Rect Double
    replaceRange pl' (Range a0 a1) (Rect x z y w) = case pl' of
      PlaceRight -> Rect x z a0 a1
      PlaceLeft -> Rect x z a0 a1
      _ -> Rect a0 a1 y w
freezeTicks_ _ _ ts = (ts, Nothing)

-- | flip an axis from being an X dimension to a Y one or vice-versa.
flipAxis :: AxisOptions -> AxisOptions
flipAxis ac = case ac ^. #place of
  PlaceBottom -> ac & #place .~ PlaceLeft
  PlaceTop -> ac & #place .~ PlaceRight
  PlaceLeft -> ac & #place .~ PlaceBottom
  PlaceRight -> ac & #place .~ PlaceTop
  PlaceAbsolute _ -> ac

-- | Make a canvas hud transformation.
-- FIXME:
-- chartDim not recalced
canvas :: RectStyle -> State HudState ()
canvas s = do
  a <- gets (view #canvasDim)
  let c = RectChart s (a:|[])
  modify (set #canvasDim (a <> sbox c))
  modify (over #hudCharts (c:))

axisBar_ :: Place -> AxisBar -> Rect Double -> Rect Double -> Chart
axisBar_ pl b (Rect x z y w) (Rect x' z' y' w') =
  case pl of
    PlaceTop ->
      RectChart
        (rstyle b)
        [ Rect
            (x - b ^. #overhang)
            (z + b ^. #overhang)
            (w' + b ^. #buff)
            (w' + b ^. #buff + b ^. #wid)
        ]
    PlaceBottom ->
      RectChart
        (rstyle b)
        [ Rect
            (x - b ^. #overhang)
            (z + b ^. #overhang)
            (y' - b ^. #wid - b ^. #buff)
            (y' - b ^. #buff)
        ]
    PlaceLeft ->
      RectChart
        (rstyle b)
        [ Rect
            (x' - b ^. #wid - b ^. #buff)
            (x' - b ^. #buff)
            (y - b ^. #overhang)
            (w + b ^. #overhang)
        ]
    PlaceRight ->
      RectChart
        (rstyle b)
        [ Rect
            (z' + (b ^. #buff))
            (z' + (b ^. #buff) + (b ^. #wid))
            (y - b ^. #overhang)
            (w + b ^. #overhang)
        ]
    PlaceAbsolute (Point x'' _) ->
      RectChart
        (rstyle b)
        [ Rect
            (x'' + (b ^. #buff))
            (x'' + (b ^. #buff) + (b ^. #wid))
            (y - b ^. #overhang)
            (w + b ^. #overhang)
        ]

makeAxisBar :: Place -> AxisBar -> State HudState ()
makeAxisBar pl b = do
  da <- gets (view #chartDim)
  ca <- gets (view #canvasDim)
  let c = axisBar_ pl b ca da
  modify (set #chartDim (da <> sbox c))
  modify (over #hudCharts (c:))

title_ :: Title -> Rect Double -> Chart
title_ t a =
  TextChart
    ( style' & #rotation .~ bool (Just rot) Nothing (rot == 0))
    [(t ^. #text, addp (placePosTitle t a) (alignPosTitle t a))]
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

placePosTitle :: Title -> Rect Double -> Point Double
placePosTitle t (Rect x z y w) = case t ^. #place of
      PlaceTop -> Point ((x + z) / 2.0) (w + (t ^. #buff))
      PlaceBottom ->
        Point
          ((x + z) / 2.0)
          ( y - (t ^. #buff)
              - 0.5
              * (t ^. #style % #vsize)
              * (t ^. #style % #size)
          )
      PlaceLeft -> Point (x - (t ^. #buff)) ((y + w) / 2.0)
      PlaceRight -> Point (z + (t ^. #buff)) ((y + w) / 2.0)
      PlaceAbsolute p -> p

alignPosTitle :: Title -> Rect Double -> Point Double
alignPosTitle t (Rect x z y w)
      | t ^. #anchor == AnchorStart
          && (t ^. #place == PlaceTop || t ^. #place == PlaceBottom) =
        Point ((x - z) / 2.0) 0.0
      | t ^. #anchor == AnchorStart
          && t ^. #place == PlaceLeft =
        Point 0.0 ((y - w) / 2.0)
      | t ^. #anchor == AnchorStart
          && t ^. #place == PlaceRight =
        Point 0.0 ((w - y) / 2.0)
      | t ^. #anchor == AnchorEnd
          && (t ^. #place == PlaceTop || t ^. #place == PlaceBottom) =
        Point ((-x + z) / 2.0) 0.0
      | t ^. #anchor == AnchorEnd
          && t ^. #place == PlaceLeft =
        Point 0.0 ((-y + w) / 2.0)
      | t ^. #anchor == AnchorEnd
          && t ^. #place == PlaceRight =
        Point 0.0 ((y - w) / 2.0)
      | otherwise = Point 0.0 0.0

-- | title append transformation.
title :: Title -> State HudState ()
title t = do
  ca <- gets (view #chartDim)
  let c = title_ t ca
  modify (set #chartDim (ca <> sbox c))
  modify (over #hudCharts (c:))

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
  PlaceBottom -> Point 0 (-b - 0.5 * (tt ^. #vsize) * (tt ^. #size))
  PlaceLeft ->
    Point
      (-b)
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
  | pl == PlaceTop || pl == PlaceBottom = Point x 0
  | otherwise = Point 0 x

placeTextAnchor :: Place -> (TextStyle -> TextStyle)
placeTextAnchor pl
  | pl == PlaceLeft = #anchor .~ AnchorEnd
  | pl == PlaceRight = #anchor .~ AnchorStart
  | otherwise = id

placeGridLines :: Place -> Rect Double -> Double -> Double -> NonEmpty (Point Double)
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

tickGlyph_ :: Place -> (GlyphStyle, Double) -> TickStyle -> Rect Double -> Rect Double -> Rect Double -> Maybe Chart
tickGlyph_ pl (g, b) ts ca da xs =
  case l of
    [] -> Nothing
    l' -> Just $ GlyphChart (g & #rotation .~ placeRot pl) $ fromList l'
    where
      l = addp (placePos pl b ca) . placeOrigin pl
        <$> positions
          (ticksPlaced ts pl da xs)

-- | aka marks
tickGlyph ::
  Place ->
  (GlyphStyle, Double) ->
  TickStyle ->
  State HudState ()
tickGlyph pl (g, b) ts = do
  ca <- gets (view #chartDim)
  d <- gets (view #canvasDim)
  xs <- gets (view #dataDim)
  let c = tickGlyph_ pl (g, b) ts ca d xs
  modify (set #chartDim (maybe ca ((ca <>) . sbox) c))
  modify (over #hudCharts (maybeToList c <>))

tickText_ ::
  Place ->
  (TextStyle, Double) ->
  TickStyle ->
  Rect Double ->
  Rect Double ->
  Rect Double ->
  Maybe Chart
tickText_ pl (txts, b) ts ca da xs =
  case l of
    [] -> Nothing
    _ -> Just $ TextChart (placeTextAnchor pl txts) $ fromList l
  where
    l = zip
      (labels $ ticksPlaced ts pl da xs)
      ( addp (addp (placePos pl b ca) (textPos pl txts b)) . placeOrigin pl
        <$> positions (ticksPlaced ts pl da xs)
      )

-- | aka tick labels
tickText ::
  Place ->
  (TextStyle, Double) ->
  TickStyle ->
  State HudState ()
tickText pl (txts, b) ts = do
  ca <- gets (view #chartDim)
  da <- gets (view #canvasDim)
  xs <- gets (view #dataDim)
  let c = tickText_ pl (txts, b) ts ca da xs
  modify (set #chartDim (maybe ca ((ca <>) . sbox) c))
  modify (over #hudCharts (maybeToList c <>))

-- | aka grid lines
tickLine ::
  Place ->
  (LineStyle, Double) ->
  TickStyle ->
  State HudState ()
tickLine pl (ls, b) ts = do
  da <- gets (view #canvasDim)
  xs <- gets (view #dataDim)
  ca <- gets (view #chartDim)
  let l = (\x -> placeGridLines pl da x b) <$> positions (ticksPlaced ts pl da xs)
  let c = case l of
        [] -> Nothing
        l' -> Just $ LineChart ls $ fromList l'
  modify (set #chartDim (maybe ca ((ca <>) . sbox) c))
  modify (over #hudCharts (maybeToList c <>))

-- | Create tick glyphs (marks), lines (grid) and text (labels)
tick ::
  Place ->
  Tick ->
  State HudState ()
tick pl t = do
  maybe (pure ()) (\x -> tickGlyph pl x (t ^. #tstyle)) (t ^. #gtick)
  maybe (pure ()) (\x -> tickText pl x (t ^. #tstyle)) (t ^. #ttick)
  maybe (pure ()) (\x -> tickLine pl x (t ^. #tstyle)) (t ^. #ltick)
  extendData pl t

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

extendData :: Place -> Tick -> State HudState ()
extendData pl t = modify (over #dataDim (tickExtended pl t))

-- | adjust Tick for sane font sizes etc
adjustTick ::
  Adjustments ->
  Rect Double ->
  Rect Double ->
  Place ->
  Tick ->
  Tick
adjustTick (Adjustments mrx ma mry ad) vb cs pl t
  | pl == PlaceBottom || pl == PlaceTop =
    if ad
      then
        ( case adjustSizeX > 1 of
            True ->
              ( case pl of
                  PlaceBottom -> #ttick % _Just % _1 % #anchor .~ AnchorEnd
                  PlaceTop -> #ttick % _Just % _1 % #anchor .~ AnchorStart
                  _ -> #ttick % _Just % _1 % #anchor .~ AnchorEnd
              )
                . (#ttick % _Just % _1 % #size %~ (/ adjustSizeA))
                $ (#ttick % _Just % _1 % #rotation ?~ pi / 4) t
            False -> (#ttick % _Just % _1 % #size %~ (/ adjustSizeA)) t
        )
      else t & #ttick % _Just % _1 % #size %~ (/ adjustSizeX)
  | otherwise -- pl `elem` [PlaceLeft, PlaceRight]
    =
    (#ttick % _Just % _1 % #size %~ (/ adjustSizeY)) t
  where
    max' [] = 1
    max' xs = maximum xs
    ra (Rect x z y w)
      | pl == PlaceTop || pl == PlaceBottom = Range x z
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
    adjustSizeX = max ((maxWidth / (upper asp - lower asp)) / mrx) 1
    adjustSizeY = max ((maxHeight / (upper asp - lower asp)) / mry) 1
    adjustSizeA = max ((maxHeight / (upper asp - lower asp)) / ma) 1

makeTick :: AxisOptions -> State HudState ()
makeTick c = do
  vb <- gets (view #chartDim)
  xs <- gets (view #dataDim)
  let adjTick =
        maybe
          (c ^. #axisTick)
          (\x -> adjustTick x vb xs (c ^. #place) (c ^. #axisTick))
          (c ^. #adjust)
  tick (c ^. #place) adjTick

legendChart :: LegendOptions -> [(Text, Chart)] -> [Chart]
legendChart l lrs =
  fmap (scaleChart (view #lscale l)) $
  padChart (l ^. #outerPad)
    . maybe id (\x -> frameChart x (l ^. #innerPad)) (l ^. #legendFrame)
    . vert (l ^. #hgap)
    $ (\(a, t) -> hori ((l ^. #vgap) + twidth - gapwidth t) [[t], [a]])
      <$> es
  where
    es = reverse $ uncurry (legendEntry l) <$> lrs
    twidth = (\(Rect _ z _ _) -> z) $ styleBoxes (snd <$> es)
    gapwidth t = (\(Rect _ z _ _) -> z) (sbox t)

legendText ::
  LegendOptions ->
  Text ->
  Chart
legendText l t =
    TextChart (l ^. #ltext & #anchor .~ AnchorStart) ((t, zero):|[])

legendizeChart ::
  LegendOptions ->
  Chart ->
  Chart
legendizeChart l c =
  case c of
    (RectChart rs _) -> RectChart rs (Rect 0 (l ^. #lsize) 0 (l ^. #lsize):|[])
    (TextChart ts _) -> TextChart (ts & #size .~ (l ^. #lsize)) (("text",zero):|[])
    (GlyphChart gs _) -> GlyphChart (gs & #size .~ (l ^. #lsize)) (Point (0.5 * l ^. #lsize) (0.33 * l ^. #lsize):|[])
    (LineChart ls _) -> LineChart (ls & #size %~ (/ (l ^. #lscale)))
          [[Point 0 (1 * l ^. #lsize), Point (2 * l ^. #lsize) (1 * l ^. #lsize)]]
    (PathChart ps _) ->
        ( let cs =
                singletonCubic
                  ( CubicPosition
                      (Point 0 0)
                      (Point (0.33 * l ^. #lsize) (0.33 * l ^. #lsize))
                      (Point 0 (0.33 * l ^. #lsize))
                      (Point (0.33 * l ^. #lsize) 0)
                  )
           in PathChart (ps & #borderSize .~ (l ^. #lsize)) cs)
    (BlankChart _) -> BlankChart (Rect 0 (l ^. #lsize) 0 (l ^. #lsize):|[])

legendEntry ::
  LegendOptions ->
  Text ->
  Chart ->
  (Chart, Chart)
legendEntry l t c =
  ( legendizeChart l c,
    legendText l t
  )

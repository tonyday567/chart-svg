{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

-- | Chart API
module Chart.Hud
  ( -- * Hud types
    Hud (..),
    close,
    closes,
    Charts(..),
    frameHud,
    frame,
    applyChartAspect,
    HudOptions (..),
    defaultHudOptions,
    colourHudOptions,
    defaultCanvas,
    runHudWith,
    runHud,
    toHuds,
    fromEffect,

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
import GHC.Generics hiding (to)
import Prelude
import Data.Path
import Chart.Data
import qualified Data.List as List

-- * Hud

-- | Type for splitting Chart elements into Hud and Canvas
--
-- - hudCharts: charts that form the hud.
--
-- - canvasCharts: charts that form the canvas; the rectangular dimension which is considered to be the data representation space.
--
-- This is done to support functionality where we can choose whether to normalise the chart aspect based on the entire chart (FixedAspect) or on just the data visualisation space (CanvasAspect).
data Charts = Charts
  { charts :: [Chart],
    dbox :: DataBox,
    cbox :: CanvasBox
  }
  deriving (Eq, Show, Generic)

type StyleBox = Rect Double
type CanvasBox = Rect Double
type DataBox = Rect Double

sbox_ :: Charts -> StyleBox
sbox_ = styleBoxes . view #charts

rebox_ :: Charts -> Rect Double -> Charts
rebox_ cs r =
  cs &
  over #charts (fmap (projectWith r (sbox_ cs))) &
  over #cbox (projectOnR (sbox_ cs) r)

sbox' :: Lens' Charts StyleBox
sbox' = lens sbox_ rebox_

append :: [Chart] -> Charts -> Charts
append cs x =
  x & over #charts (cs<>) & over #cbox (projectOnR oldbox (view sbox' x))
  where
    oldbox = view sbox' x

-- | The priority of a Hud element or transformation
type Priority = Int

-- | Heads-up-display additions to charts
data Hud =
  Hud {
    -- | priority for ordering of transformations
    priority :: Priority,
    -- | additional charts
    hud :: State Charts [Chart]
  } deriving (Generic)

close :: State Charts [Chart] -> State Charts ()
close a = do
  a' <- a
  modify (append a')

closes :: (Traversable f) => f (State Charts [Chart]) -> State Charts ()
closes xs = do
  xs' <- fmap (mconcat . toList) $ sequence xs
  modify (append xs')

fromEffect :: Priority -> State Charts () -> Hud
fromEffect p s = Hud p (s >> pure [])

-- | Apply a ChartAspect
applyChartAspect :: ChartAspect -> State Charts ()
applyChartAspect fa = do
  hc <- get
  case fa of
    FixedAspect a -> modify (set sbox' (aspect a))
    CanvasAspect a ->
      modify
      (set sbox'
      (aspect (a * ratio (view #cbox hc) / ratio (view sbox' hc))))
    ChartAspect -> pure ()
    UnadjustedAspect -> pure ()

-- | Combine huds and charts to form a new Chart using the supplied initial canvas and data dimensions. Note that chart data is transformed by this computation (a linear type might be useful here).
runHudWith ::
  -- | initial canvas
  CanvasBox ->
  -- | initial data space
  DataBox ->
  -- | huds to add
  [Hud] ->
  -- | underlying chart
  [Chart] ->
  -- | integrated chart list
  [Chart]
runHudWith cb db hs cs =
   hs &
   List.sortOn (view #priority) &
   List.groupBy (\a b-> view #priority a == view #priority b) &
   mapM_ (closes . fmap (view #hud)) &
   flip execState (Charts (projectWith cb db <$> cs) db cb) &
   view #charts

-- | Combine huds and charts to form a new [Chart] with an optional supplied initial canvas dimension.
--
-- Note that the original chart data are transformed and irrevocably lost by this computation.
--
-- >>> runHud (Just one) (fromHudOptions defaultHudOptions) [RectChart defaultRectStyle [one]]
--
runHud ::
  -- | initial canvas dimension
  CanvasBox ->
  -- | huds
  [Hud] ->
  -- | underlying charts
  [Chart] ->
  -- | integrated chart list
  [Chart]
runHud ca hs cs = runHudWith ca (padSingletons $ boxes cs) hs cs

-- | Combine huds and charts to form a new [Chart] with an optional supplied initial canvas dimension.
--
-- Note that the original chart data are transformed and irrevocably lost by this computation.
--
-- >>> runHud (Just one) (fromHudOptions defaultHudOptions) [RectChart defaultRectStyle [one]]
--
runHud_ ::
  -- | huds
  [Hud] ->
  -- | underlying charts
  [Chart] ->
  -- | integrated chart list
  [Chart]
runHud_ hs cs = runHudWith (padSingletons $ boxes cs) (padSingletons $ boxes cs) hs cs

-- | overlay a frame with additive padding
--
frameHud :: RectStyle -> Double -> State Charts [Chart]
frameHud rs p = do
  cs <- get
  let pad = padRect p (view sbox' cs)
  pure [RectChart rs [pad]]

-- | frame some charts
--
frame :: RectStyle -> Double -> [Chart] -> [Chart]
frame rs p cs = runHud_ [Hud 0 (frameHud rs p)] cs

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
      (5, defaultAxisOptions & set #place PlaceLeft)
    ]
    [(1, defaultCanvas)]
    []
    []

-- | Make Huds and potential data box extension; from a HudOption and an initial data box.
--
toHuds :: HudOptions -> DataBox -> ([Hud], DataBox)
toHuds o db =
  (,db''') $
  (as' & fmap (uncurry Hud . second axis)) <>
  (view #canvii o & fmap (uncurry Hud . second canvas)) <>
  (view #legends o & fmap (uncurry Hud . second legend)) <>
  (view #titles o & fmap (uncurry Hud . second title))
  where
    (as', db''') = foldr (\a (as,db') -> let (db'', a') = freezeTicks db' (snd a) in (as <> [(fst a, a')], db'')) ([], db) (view #axes o)

-- Should charts be modified here? Alternative is passing a Charts
freezeTicks :: DataBox -> AxisOptions -> (DataBox, AxisOptions)
freezeTicks db a = do
  case freezeTicks_ (view #place a) db (view (#axisTick % #tstyle) a) of
    Nothing -> (db,a)
    Just (ts',db') -> (db', a & set (#axisTick % #tstyle) ts')

toTickPlaced :: Range Double -> TickStyle -> (Range Double, TickStyle)
toTickPlaced r t@TickRound {} = (fromMaybe r ext, TickPlaced (zip ps ls))
  where
    (TickComponents ps ls ext) = makeTicks t r
toTickPlaced r t = (r,t)

-- | convert TickRound to TickPlaced and potentially an extension to the data range
freezeTicks_ :: Place -> DataBox -> TickStyle -> Maybe (TickStyle, DataBox)
freezeTicks_ pl db ts@TickRound {} =
  case ext of
    Nothing -> Nothing
    Just ext' -> Just (TickPlaced (zip ps ls), replaceRange pl ext' db)
  where
    (TickComponents ps ls ext) = makeTicks ts (placeRange pl db)
    replaceRange :: Place -> Range Double -> Rect Double -> Rect Double
    replaceRange pl' (Range a0 a1) (Rect x z y w) = case pl' of
      PlaceRight -> Rect x z a0 a1
      PlaceLeft -> Rect x z a0 a1
      _ -> Rect a0 a1 y w
freezeTicks_ _ _ _ = Nothing

axis :: AxisOptions -> State Charts [Chart]
axis a = do
  t <- makeTick a
  b <- maybe (pure mempty) (makeAxisBar (view #place a)) (view #axisBar a)
  pure (b <> t)

legend :: LegendOptions -> State Charts [Chart]
legend o = do
  hc <- get
  pure (placeChart (view sbox' hc))
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
legendHud :: LegendOptions -> [Chart] -> State Charts [Chart]
legendHud l lcs = do
  hc <- get
  let ca = view sbox' hc
  pure (place' ca (scaleChart (l ^. #lscale) <$> lcs))
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
      a &
       over #axisBar (fmap (over (#rstyle % #color) f)) &
       over (#axisTick % #gtick)
         (fmap (first (over #color f . over #borderColor f))) &
       over (#axisTick % #ttick)
         (fmap (first (over #color f))) &
       over (#axisTick % #ltick)
         (fmap (first (over #color f)))
    fLegend :: LegendOptions -> LegendOptions
    fLegend a =
      a &
       over #ltext (over #color f) &
       over #legendFrame (fmap (over #color f . over #borderColor f))

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
    (Just (defaultGlyphTick, 0.03)) -- 0.017
    (Just (defaultTextTick, 0.03))  -- 0.015
    (Just (defaultLineTick, 0))      -- 0

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

-- | flip an axis from being an X dimension to a Y one or vice-versa.
flipAxis :: AxisOptions -> AxisOptions
flipAxis ac = case ac ^. #place of
  PlaceBottom -> ac & #place .~ PlaceLeft
  PlaceTop -> ac & #place .~ PlaceRight
  PlaceLeft -> ac & #place .~ PlaceBottom
  PlaceRight -> ac & #place .~ PlaceTop
  PlaceAbsolute _ -> ac

-- | Make a canvas hud transformation.
canvas :: RectStyle -> State Charts [Chart]
canvas s = do
  hc <- get
  pure [RectChart s [view sbox' hc]]

axisBar_ :: Place -> AxisBar -> CanvasBox -> StyleBox -> Chart
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

makeAxisBar :: Place -> AxisBar -> State Charts [Chart]
makeAxisBar pl b = do
  cb <- gets (view #cbox)
  sb <- gets (view sbox')
  let c = axisBar_ pl b cb sb
  pure [c]

title_ :: Title -> StyleBox -> Chart
title_ t sb =
  TextChart
    ( style' & #rotation .~ bool (Just rot) Nothing (rot == 0))
    [(t ^. #text, addp (placePosTitle t sb) (alignPosTitle t sb))]
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

placePosTitle :: Title -> StyleBox -> Point Double
placePosTitle t (Rect x z y w) =
  case t ^. #place of
      PlaceTop -> Point ((x + z) / 2.0) (w - y' + (t ^. #buff))
      PlaceBottom -> Point ((x + z) / 2.0) (y - w' - (t ^. #buff))
      PlaceLeft -> Point (x + y' - (t ^. #buff)) ((y + w) / 2.0)
      PlaceRight -> Point (z + w' + (t ^. #buff)) ((y + w) / 2.0)
      PlaceAbsolute p -> p
  where
    (Rect _ _ y' w') = styleBoxText (view #style t) (view #text t) zero

alignPosTitle :: Title -> StyleBox -> Point Double
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
title :: Title -> State Charts [Chart]
title t = do
  ca <- gets (view sbox')
  pure [title_ t ca]

placePos :: Place -> Double -> StyleBox -> Point Double
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

placeRange :: Place -> StyleBox -> Range Double
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

placeGridLines :: Place -> StyleBox -> Double -> Double -> NonEmpty (Point Double)
placeGridLines pl (Rect x z y w) a b
  | pl == PlaceTop || pl == PlaceBottom = [Point a (y - b), Point a (w + b)]
  | otherwise = [Point (x - b) a, Point (z + b) a]

-- | compute tick values and labels given options, ranges and formatting
--
-- >>> ticksR (TickRound (FormatFixed (Just 1)) 6 TickExtend) (Range (-0.5) 0.5) (Range 0 0.77)
-- [(-0.5,"0.0"),(-0.3701298701298701,"0.1"),(-0.24025974025974023,"0.2"),(-0.11038961038961032,"0.3"),(1.9480519480519543e-2,"0.4"),(0.14935064935064934,"0.5"),(0.27922077922077937,"0.6"),(0.40909090909090917,"0.7"),(0.5389610389610391,"0.8")]
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

-- | compute tick values given placement, canvas box & data box
ticksPlaced :: TickStyle -> Place -> CanvasBox -> DataBox -> TickComponents
ticksPlaced ts pl cb db = TickComponents (project (placeRange pl db) (placeRange pl cb) <$> ps) ls ext
  where
    (TickComponents ps ls ext) = makeTicks ts (placeRange pl db)

tickGlyph_ :: Place -> (GlyphStyle, Double) -> TickStyle -> StyleBox -> CanvasBox -> DataBox -> Maybe Chart
tickGlyph_ pl (g, b) ts sb cb db =
  case l of
    [] -> Nothing
    l' -> Just $ GlyphChart (g & #rotation .~ placeRot pl) $ fromList l'
    where
      l = addp (placePos pl b sb) . placeOrigin pl
        <$> positions
          (ticksPlaced ts pl cb db)

-- | aka marks
tickGlyph ::
  Place ->
  (GlyphStyle, Double) ->
  TickStyle ->
  State Charts [Chart]
tickGlyph pl (g, b) ts = do
  sb <- gets (view sbox')
  cb <- gets (view #cbox)
  db <- gets (view #dbox)
  let c = tickGlyph_ pl (g, b) ts sb cb db
  pure (maybeToList c)

tickText_ ::
  Place ->
  (TextStyle, Double) ->
  TickStyle ->
  StyleBox ->
  CanvasBox ->
  DataBox ->
  Maybe Chart
tickText_ pl (txts, b) ts sb cb db =
  case l of
    [] -> Nothing
    _ -> Just $ TextChart (placeTextAnchor pl txts) $ fromList l
  where
    l = zip
      (labels $ ticksPlaced ts pl cb db)
      ( addp (addp (placePos pl b sb) (textPos pl txts b)) . placeOrigin pl
        <$> positions (ticksPlaced ts pl cb db)
      )

-- | aka tick labels
tickText ::
  Place ->
  (TextStyle, Double) ->
  TickStyle ->
  State Charts [Chart]
tickText pl (txts, b) ts = do
  sb <- gets (view sbox')
  cb <- gets (view #cbox)
  db <- gets (view #dbox)
  let c = tickText_ pl (txts, b) ts sb cb db
  pure (maybeToList c)

-- | aka grid lines
tickLine ::
  Place ->
  (LineStyle, Double) ->
  TickStyle ->
  State Charts [Chart]
tickLine pl (ls, b) ts = do
  cb <- gets (view #cbox)
  db <- gets (view #dbox)
  let l = (\x -> placeGridLines pl cb x b) <$> positions (ticksPlaced ts pl cb db)
  pure (bool [LineChart ls (fromList l)] [] (null l))

-- | Create tick glyphs (marks), lines (grid) and text (labels)
tick ::
  Place ->
  Tick ->
  State Charts [Chart]
tick pl t = do
  g <- maybe (pure mempty) (\x -> tickGlyph pl x (t ^. #tstyle)) (t ^. #gtick)
  l <- maybe (pure mempty) (\x -> tickText pl x (t ^. #tstyle)) (t ^. #ttick)
  t' <- maybe (pure mempty) (\x -> tickLine pl x (t ^. #tstyle)) (t ^. #ltick)
  pure $ g <> l <> t'

-- | adjust Tick for sane font sizes etc
adjustTick ::
  Adjustments ->
  StyleBox ->
  DataBox ->
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

makeTick :: AxisOptions -> State Charts [Chart]
makeTick c = do
  sb <- gets (view sbox')
  db <- gets (view #dbox)
  let adjTick = maybe (c ^. #axisTick) (\x -> adjustTick x sb db (c ^. #place) (c ^. #axisTick)) (c ^. #adjust)
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

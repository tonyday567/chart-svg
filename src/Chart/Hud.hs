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
    HudBox,
    CanvasBox,
    DataBox,
    frameHud,
    legend,
    legendHud,
    legendChart,
    legendFrame,
    HudChart (..),
    canvasBox',
    hudBox',
    hudStyleBox',
    HudOptions (..),
    defaultHudOptions,
    colourHudOptions,
    runHudWith,
    runHud,
    toHuds,
    closes,
    fromEffect,
    applyChartAspect,
    getHudBox,

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
    Ticks (..),
    defaultGlyphTick,
    defaultTextTick,
    defaultLineTick,
    defaultTicks,
    TickStyle (..),
    defaultTickStyle,
    tickStyleText,
    TickExtend (..),
    adjustTicks,
    Adjustments (..),
    defaultAdjustments,
    LegendOptions (..),
    defaultLegendOptions,
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
import Data.Tuple
import qualified NumHask.Prelude as NH

-- * Hud

-- | Type for splitting Chart elements into Hud and Canvas
--
-- - hudCharts: charts that form the hud.
--
-- - canvasCharts: charts that form the canvas; the rectangular dimension which is considered to be the data representation space.
--
-- This is done to support functionality where we can choose whether to normalise the chart aspect based on the entire chart (FixedAspect) or on just the data visualisation space (CanvasAspect).
data HudChart = HudChart
  { chart :: Charts,
    hud :: Charts,
    dataBox :: DataBox
  }
  deriving (Eq, Show, Generic)

type HudBox = Rect Double
type CanvasBox = Rect Double
type DataBox = Rect Double

canvasBox_ :: HudChart -> CanvasBox
canvasBox_ = boxes . foldOf (#chart % charts')

canvasRebox_ :: HudChart -> Rect Double -> HudChart
canvasRebox_ cs r =
  cs &
  over (#chart % chart') (projectWith r (canvasBox_ cs)) &
  over (#hud % chart') (projectWith r (canvasBox_ cs))

canvasBox' :: Lens' HudChart CanvasBox
canvasBox' =
  lens canvasBox_ canvasRebox_

hudStyleBox_ :: HudChart -> HudBox
hudStyleBox_ = styleBoxes . (\x -> foldOf (#chart % charts') x <> foldOf (#hud % charts') x)

hudStyleBox' :: Getter HudChart HudBox
hudStyleBox' = to hudStyleBox_

hudBox_ :: HudChart -> HudBox
hudBox_ = boxes . (\x -> foldOf (#chart % charts') x <> foldOf (#hud % charts') x)

hudRebox_ :: HudChart -> Rect Double -> HudChart
hudRebox_ cs r =
  cs &
  over #chart (over chart' (projectWith r' (hudBox_ cs))) &
  over #hud (over chart' (projectWith r' (hudBox_ cs)))
  where
    r' = r NH.- (hudStyleBox_ cs NH.- hudBox_ cs)

hudBox' :: Lens' HudChart HudBox
hudBox' =
  lens hudBox_ hudRebox_

appendHud :: Charts -> HudChart -> HudChart
appendHud cs x =
  x & over #hud (<> cs)

-- | The priority of a Hud element or transformation
type Priority = Double

-- | Heads-up-display additions to charts
data Hud =
  Hud {
    -- | priority for ordering of transformations
    priority :: Priority,
    -- | additional charts
    hud :: State HudChart Charts
  } deriving (Generic)

closes :: (Traversable f) => f (State HudChart Charts) -> State HudChart ()
closes xs = do
  xs' <- fmap (mconcat . toList) $ sequence xs
  modify (appendHud xs')

fromEffect :: Priority -> State HudChart () -> Hud
fromEffect p s = Hud p (s >> pure mempty)

type Scale = Double

-- | Apply a ChartAspect
applyChartAspect :: ChartAspect -> Scale -> State HudChart ()
applyChartAspect fa x = do
  hc <- get
  case fa of
    ChartAspect -> pure ()
    _ -> modify (set hudBox' (getHudBox fa x hc))

getHudBox :: ChartAspect -> Scale -> HudChart -> HudBox
getHudBox fa x c =
  case fa of
    FixedAspect a -> fmap (x*) (aspect a)
    CanvasAspect a ->
      fmap (x*) (aspect (a * ratio (view hudBox' c) / ratio (view canvasBox' c)))
    ChartAspect -> view hudBox' c


-- | Combine huds and charts to form a new Chart using the supplied initial canvas and data dimensions. Note that chart data is transformed by this computation (a linear type might be useful here).
runHudWith ::
  -- | initial canvas
  CanvasBox ->
  -- | initial data space
  DataBox ->
  -- | huds to add
  [Hud] ->
  -- | underlying chart
  Charts ->
  -- | integrated chart tree
  Charts
runHudWith cb db hs cs =
   hs &
   List.sortOn (view #priority) &
   List.groupBy (\a b-> view #priority a == view #priority b) &
   mapM_ (closes . fmap (view #hud)) &
   flip execState
   (HudChart (cs & over chart' (projectWith cb db))
    mempty db) &
   (\x -> group (Just "chart") [view #chart x] <> group (Just "hud") [view #hud x])

-- | Combine huds and charts to form a new Charts with an optional supplied initial canvas dimension.
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
  Charts ->
  -- | integrated chart list
  Charts
runHud ca hs cs = runHudWith ca (padSingletons $ boxes (foldOf charts' cs)) hs cs

-- | Typical configurable hud elements. Anything else can be hand-coded as a 'Hud'.
--
-- ![hud example](other/hudoptions.svg)
data HudOptions = HudOptions
  { chartAspect :: ChartAspect,
    chartScale :: Double,
    axes :: [(Priority, AxisOptions)],
    frames :: [(Priority, FrameOptions)],
    legends :: [(Priority, LegendOptions)],
    titles :: [(Priority, Title)]
  } deriving (Eq, Show, Generic)

instance Semigroup HudOptions where
  (<>) (HudOptions _ _ a c l t) (HudOptions asp sc a' c' l' t') =
    HudOptions asp sc (a <> a') (c <> c') (l <> l') (t <> t')

instance Monoid HudOptions where
  mempty = HudOptions (FixedAspect 1.5) 1 [] [] [] []

-- | The official hud options.
defaultHudOptions :: HudOptions
defaultHudOptions =
  HudOptions
    (FixedAspect 1.5)
    1
    [
      (5, defaultAxisOptions),
      (5, defaultAxisOptions & set #place PlaceLeft)
    ]
    [(1, defaultCanvas)]
    []
    []

priorities :: HudOptions -> [Priority]
priorities o =
  (fst <$> view #axes o) <>
  (fst <$> view #frames o) <>
  (fst <$> view #legends o) <>
  (fst <$> view #titles o)

lastPriority :: HudOptions -> Priority
lastPriority o = case priorities o of
  [] -> 0
  xs -> maximum xs

-- | Make Huds and potential data box extension; from a HudOption and an initial data box.
--
toHuds :: HudOptions -> DataBox -> ([Hud], DataBox)
toHuds o db =
  (,db''') $
  (as' & fmap (uncurry Hud . second axis)) <>
  (view #frames o & fmap (uncurry Hud . second frameHud)) <>
  (view #legends o & fmap (uncurry Hud . second legend)) <>
  (view #titles o & fmap (uncurry Hud . second title)) <>
  [ fromEffect (lastPriority o + 1) $
        applyChartAspect (view #chartAspect o) (view #chartScale o)
  ]
  where
    (as', db''') =
      foldr (\a (as,db') ->
               let (db'', a') = freezeTicks db' (snd a) in
                 (as <> [(fst a, a')], db''))
      ([], db)
      (view #axes o)

freezeTicks :: DataBox -> AxisOptions -> (DataBox, AxisOptions)
freezeTicks db a =
  bimap
  (\x -> placeRect (view #place a) x db)
  (\x -> a & set (#ticks % #style) x)
  (toTickPlaced (placeRange (view #place a) db) (view (#ticks % #style) a))

-- | compute tick components given style, ranges and formatting
makePlacedTicks :: TickStyle -> Range Double -> ([(Double, Text)], Maybe (Range Double))
makePlacedTicks s r =
  case s of
    TickNone -> ([],Nothing)
    TickRound f n e ->
      (zip
        ticks0
        (formatNs f ticks0),
        bool (Just $ space1 ticks0) Nothing (e == NoTickExtend))
      where
        ticks0 = gridSensible OuterPos (e == NoTickExtend) r (fromIntegral n :: Integer)
    TickExact f n -> (zip ticks0 (formatNs f ticks0), Nothing)
      where
        ticks0 = grid OuterPos r n
    TickLabels ls ->
      (zip
        ( project (Range 0 (fromIntegral $ length ls)) r
            <$> ((\x -> x - 0.5) . fromIntegral <$> [1 .. length ls])
        )
        ls,
        Nothing)
    TickPlaced xs -> (xs, Nothing)

toTickPlaced :: Range Double -> TickStyle -> (Range Double, TickStyle)
toTickPlaced r t@TickRound {} = (fromMaybe r ext, TickPlaced ts)
  where
    (ts, ext) = makePlacedTicks t r
toTickPlaced r t = (r,t)

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

axis :: AxisOptions -> State HudChart Charts
axis a = do
  t <- makeTick a
  b <- maybe (pure mempty) (makeAxisBar (view #place a)) (view #bar a)
  pure (group (Just "axis") [t,b])

-- | alter a colour with a function
colourHudOptions :: (Colour -> Colour) -> HudOptions -> HudOptions
colourHudOptions f o =
  o &
  over #frames (fmap (second fFrame)) &
  over #titles (fmap (second (over (#style % #color) f))) &
  over #axes (fmap (second fAxis)) &
  over #legends (fmap (second fLegend))
  where
    fAxis :: AxisOptions -> AxisOptions
    fAxis a =
      a &
       over #bar (fmap (over (#style % #color) f)) &
       over (#ticks % #gtick)
         (fmap (first (over #color f . over #borderColor f))) &
       over (#ticks % #ttick)
         (fmap (first (over #color f))) &
       over (#ticks % #ltick)
         (fmap (first (over #color f)))
    fLegend :: LegendOptions -> LegendOptions
    fLegend a =
      a &
       over #textStyle (over #color f) &
       over #frame (fmap (over #color f . over #borderColor f))
    fFrame :: FrameOptions -> FrameOptions
    fFrame a =
      a &
       over #frame (fmap (over #color f . over #borderColor f))

-- | The official hud canvas
defaultCanvas :: FrameOptions
defaultCanvas = FrameOptions (Just (blob (Colour 1 1 1 0.02))) 0

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
  { bar :: Maybe AxisBar,
    adjust :: Maybe Adjustments,
    ticks :: Ticks,
    place :: Place
  }
  deriving (Eq, Show, Generic)

-- | The official axis
defaultAxisOptions :: AxisOptions
defaultAxisOptions = AxisOptions (Just defaultAxisBar) (Just defaultAdjustments) defaultTicks PlaceBottom

-- | The bar on an axis representing the x or y plane.
--
-- >>> defaultAxisBar
-- AxisBar {style = RectStyle {borderSize = 0.0, borderColor = Colour 0.00 0.00 0.00 0.00, color = Colour 0.05 0.05 0.05 0.40}, size = 4.0e-3, buffer = 1.0e-2}
data AxisBar = AxisBar
  { style :: RectStyle,
    size :: Double,
    buffer :: Double,
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
-- Title {text = "title", style = TextStyle {size = 0.12, color = Colour 0.05 0.05 0.05 1.00, anchor = AnchorMiddle, hsize = 0.5, vsize = 1.45, vshift = -0.2, rotation = Nothing}, place = PlaceTop, anchor = AnchorMiddle, buffer = 4.0e-2}
data Title = Title
  { text :: Text,
    style :: TextStyle,
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

-- | xy coordinate markings
--
-- >>> defaultTicks
-- Ticks {style = TickRound (FormatComma (Just 2)) 8 TickExtend, gtick = Just (GlyphStyle {size = 3.0e-2, color = Colour 0.05 0.05 0.05 0.40, borderColor = Colour 0.05 0.05 0.05 0.40, borderSize = 2.0e-3, shape = VLineGlyph, rotation = Nothing, translate = Nothing},1.25e-2), ttick = Just (TextStyle {size = 5.0e-2, color = Colour 0.05 0.05 0.05 1.00, anchor = AnchorMiddle, hsize = 0.5, vsize = 1.45, vshift = -0.2, rotation = Nothing},1.5e-2), ltick = Just (LineStyle {size = 5.0e-3, color = Colour 0.05 0.05 0.05 0.05, linecap = Nothing, linejoin = Nothing, dasharray = Nothing, dashoffset = Nothing},0.0)}
data Ticks = Ticks
  { style :: TickStyle,
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
defaultTicks :: Ticks
defaultTicks =
  Ticks
    defaultTickStyle
    (Just (defaultGlyphTick, 0.03))
    (Just (defaultTextTick, 0.033))
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
-- LegendOptions {size = 0.3, vgap = 0.2, hgap = 0.1, style = TextStyle {size = 0.12, color = Colour 0.05 0.05 0.05 1.00, anchor = AnchorMiddle, hsize = 0.5, vsize = 1.45, vshift = -0.2, rotation = Nothing}, lmax = 10, innerPad = 0.1, outerPad = 2.0e-2, frame = Just (RectStyle {borderSize = 1.0e-2, borderColor = Colour 0.05 0.05 0.05 1.00, color = Colour 0.05 0.05 0.05 0.00}), place = PlaceRight, overallScale = 0.25}
--
-- ![legend example](other/legend.svg)
data LegendOptions = LegendOptions
  { size :: Double,
    buffer :: Double,
    vgap :: Double,
    hgap :: Double,
    textStyle :: TextStyle,
    innerPad :: Double,
    outerPad :: Double,
    frame :: Maybe RectStyle,
    place :: Place,
    overallScale :: Double,
    content :: [(Text, Chart)]
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
    ( defaultTextStyle
        & #size .~ 0.12
    )
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

data FrameOptions = FrameOptions
  { frame :: Maybe RectStyle,
    buffer :: Double
  } deriving (Eq, Show, Generic)

-- | Make a frame hud transformation.
frameHud :: FrameOptions -> State HudChart Charts
frameHud o = do
  hc <- get
  let r = padRect (view #buffer o) (view hudStyleBox' hc)
  pure $ case view #frame o of
    Nothing -> blank r
    Just rs -> named "frame" [RectChart rs [r]]

bar_ :: Place -> AxisBar -> CanvasBox -> HudBox -> Chart
bar_ pl b (Rect x z y w) (Rect x' z' y' w') =
  RectChart (view #style b) $
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

makeAxisBar :: Place -> AxisBar -> State HudChart Charts
makeAxisBar pl b = do
  cb <- gets (view canvasBox')
  hb <- gets (view hudStyleBox')
  let c = bar_ pl b cb hb
  pure $ named "axisbar" [c]

title_ :: Title -> HudBox -> Chart
title_ t hb =
  TextChart
    ( style' & #rotation .~ bool (Just rot) Nothing (rot == 0))
    [(t ^. #text, addp (placePosTitle t hb) (alignPosTitle t hb))]
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
title :: Title -> State HudChart Charts
title t = do
  hb <- gets (view hudStyleBox')
  pure $ named "title" [title_ t hb]

placePos :: Place -> Double -> HudBox -> Point Double
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
      ((tt ^. #vshift) * (tt ^. #vsize) * (tt ^. #size))
  PlaceRight ->
    Point
      b
      ((tt ^. #vshift) * (tt ^. #vsize) * (tt ^. #size))
  PlaceAbsolute p -> p

placeTextAnchor :: Place -> (TextStyle -> TextStyle)
placeTextAnchor pl
  | pl == PlaceLeft = #anchor .~ AnchorEnd
  | pl == PlaceRight = #anchor .~ AnchorStart
  | otherwise = id

placeGridLines :: Place -> HudBox -> Double -> Double -> NonEmpty (Point Double)
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

-- | compute tick values in canvas space given placement, canvas box & data box
--
ticksPlacedCanvas :: TickStyle -> Place -> CanvasBox -> DataBox -> [(Double, Text)]
ticksPlacedCanvas ts pl cb db =
  first (project (placeRange pl db) (placeRange pl cb)) <$>
  fst (makePlacedTicks ts (placeRange pl db))

tickGlyph_ :: Place -> (GlyphStyle, Double) -> TickStyle -> CanvasBox -> DataBox -> Maybe Chart
tickGlyph_ pl (g, b) ts cb db =
  case l of
    [] -> Nothing
    l' -> Just $ GlyphChart (g & #rotation .~ placeRot pl) $ fromList l'
    where
      l = addp (placePos pl b cb) . placeOrigin pl
        <$> fmap fst (ticksPlacedCanvas ts pl cb db)

-- | aka marks
tickGlyph ::
  Place ->
  (GlyphStyle, Double) ->
  TickStyle ->
  State HudChart Charts
tickGlyph pl (g, b) ts = do
  cb <- gets (view canvasBox')
  db <- gets (view #dataBox)
  let c = tickGlyph_ pl (g, b) ts cb db
  pure $ named "tickglyph" (maybeToList c)

tickText_ ::
  Place ->
  (TextStyle, Double) ->
  TickStyle ->
  CanvasBox ->
  DataBox ->
  Maybe Chart
tickText_ pl (txts, b) ts cb db =
  case l of
    [] -> Nothing
    _ -> Just $ TextChart (placeTextAnchor pl txts) $ fromList l
  where
    l =
      swap . first (addp (addp (placePos pl b cb) (textPos pl txts b)) . placeOrigin pl)
        <$> ticksPlacedCanvas ts pl cb db

-- | aka tick labels
tickText ::
  Place ->
  (TextStyle, Double) ->
  TickStyle ->
  State HudChart Charts
tickText pl (txts, b) ts = do
  cb <- gets (view canvasBox')
  db <- gets (view #dataBox)
  let c = tickText_ pl (txts, b) ts cb db
  pure $ named "ticktext" (maybeToList c)

-- | aka grid lines
tickLine ::
  Place ->
  (LineStyle, Double) ->
  TickStyle ->
  State HudChart Charts
tickLine pl (ls, b) ts = do
  cb <- gets (view canvasBox')
  db <- gets (view #dataBox)
  let l = (\x -> placeGridLines pl cb x b) <$> fmap fst (ticksPlacedCanvas ts pl cb db)
  pure $ named "ticklines" (bool [LineChart ls (fromList l)] [] (null l))

-- | Create tick glyphs (marks), lines (grid) and text (labels)
applyTicks ::
  Place ->
  Ticks ->
  State HudChart Charts
applyTicks pl t = do
  g <- maybe (pure mempty) (\x -> tickGlyph pl x (t ^. #style)) (t ^. #gtick)
  l <- maybe (pure mempty) (\x -> tickText pl x (t ^. #style)) (t ^. #ttick)
  t' <- maybe (pure mempty) (\x -> tickLine pl x (t ^. #style)) (t ^. #ltick)
  pure $ group (Just "ticks") [g, l, t']

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
    tickl = snd <$> ticksR (t ^. #style) asp r
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

makeTick :: AxisOptions -> State HudChart Charts
makeTick c = do
  hb <- gets (view hudBox')
  db <- gets (view #dataBox)
  let adjTick = maybe (c ^. #ticks) (\x -> adjustTicks x hb db (c ^. #place) (c ^. #ticks)) (c ^. #adjust)
  applyTicks (c ^. #place) adjTick

legend :: LegendOptions -> State HudChart Charts
legend o = legendHud o (legendChart o)

-- | Make a legend hud element, from a bespoke Charts.
--
--
legendHud :: LegendOptions -> Charts -> State HudChart Charts
legendHud o lcs = do
  sb <- gets (view hudStyleBox')
  pure $ placeLegend o sb (over chart' (scaleChart (o ^. #overallScale)) lcs)

placeLegend :: LegendOptions -> HudBox -> Charts -> Charts
placeLegend o hb t =
  t & over chart' (moveChart (placeBeside_ (o ^. #place) (view #buffer o) hb (view styleBox' t)))

placeBeside_ :: Place -> Double -> Rect Double -> Rect Double -> Point Double
placeBeside_ pl buff (Rect x z y w) (Rect x' z' y' w') =
      case pl of
        PlaceTop -> Point ((x + z) / 2.0) (buff + w + (w' - y') / 2.0)
        PlaceBottom -> Point ((x + z) / 2.0) (y - buff - (w' - y'))
        PlaceLeft -> Point (x - buff - (z' - x')) ((y + w) / 2.0)
        PlaceRight -> Point (z + buff) ((y + w) / 2.0)
        PlaceAbsolute p -> p

legendFrame :: LegendOptions -> Charts -> Charts
legendFrame l content' =
  group (Just "legend") [named "legendBorder" borders, rename (Just "legendContent") content']
  where
    borders = [outer, inner] <> frame'
    outer = padChart (view #outerPad l) [inner]
    frame' = foldMap (\r -> [frameChart r 0 [inner]]) (view #frame l)
    inner = padChart (view #innerPad l) (foldOf charts' content')

legendChart :: LegendOptions -> Charts
legendChart l = legendFrame l content'
  where
    content' =
      vert (l ^. #hgap)
      ((\(a, t) -> hori ((l ^. #vgap) + twidth - gapwidth t)
         (fmap unnamed [[t], [a]])) <$> es)
    es = reverse $ uncurry (legendEntry l) <$> view #content l
    twidth = (\(Rect _ z _ _) -> z) $ styleBoxes (snd <$> es)
    gapwidth t = (\(Rect _ z _ _) -> z) (sbox t)

legendText ::
  LegendOptions ->
  Text ->
  Chart
legendText l t =
    TextChart (l ^. #textStyle & #anchor .~ AnchorStart) ((t, zero):|[])

legendizeChart ::
  LegendOptions ->
  Chart ->
  Chart
legendizeChart l c =
  case c of
    (RectChart rs _) -> RectChart rs (Rect 0 (l ^. #size) 0 (l ^. #size):|[])
    (TextChart ts _) -> TextChart (ts & #size .~ (l ^. #size)) (("text",zero):|[])
    (GlyphChart gs _) -> GlyphChart (gs & #size .~ (l ^. #size)) (Point (0.5 * l ^. #size) (0.33 * l ^. #size):|[])
    (LineChart ls _) -> LineChart (ls & #size %~ (/ (l ^. #overallScale)))
          [[Point 0 (1 * l ^. #size), Point (2 * l ^. #size) (1 * l ^. #size)]]
    (PathChart ps _) ->
        ( let cs =
                singletonCubic
                  ( CubicPosition
                      (Point 0 0)
                      (Point (0.33 * l ^. #size) (0.33 * l ^. #size))
                      (Point 0 (0.33 * l ^. #size))
                      (Point (0.33 * l ^. #size) 0)
                  )
           in PathChart (ps & #borderSize .~ (l ^. #size)) cs)
    (BlankChart _) -> BlankChart (Rect 0 (l ^. #size) 0 (l ^. #size):|[])

legendEntry ::
  LegendOptions ->
  Text ->
  Chart ->
  (Chart, Chart)
legendEntry l t c =
  ( legendizeChart l c,
    legendText l t
  )

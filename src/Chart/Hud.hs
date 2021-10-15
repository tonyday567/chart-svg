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
module Chart.Hud
  ( -- * Hud types
    ChartDims (..),
    HudT (..),
    Hud,
    simulHud,
    HudOptions (..),
    defaultHudOptions,
    colourHudOptions,
    scaleOpacHudOptions,
    defaultCanvas,
    runHudWith,
    runHud,
    makeHud,
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

  )
where

import Chart.Chart
import Chart.Style
import Control.Lens
import Control.Monad.State.Lazy
import Data.Bifunctor
import Data.Bool
import Data.Colour
import Data.Foldable hiding (sum)
import Data.FormatN
import Data.Generics.Labels ()
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.String
import Data.Text (Text)
import Data.Time
import GHC.Generics
import GHC.OverloadedLabels
import NumHask.Prelude
import NumHask.Space as NH hiding (Element, singleton)
import Data.Path

-- * Hud
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
    FixedAspect a -> pure $ project_ (aspect a) cs
    CanvasAspect a ->
      pure $
        project_ (aspect (a * ratio canvasd / ratio chartd)) cs
    ChartAspect -> pure $ project_ (aspect $ ratio chartd) cs
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
    da' = boxes cs'
    ca' = sboxes cs'
    cs' = projectWith_ ca xs <$> cs

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
runHud ca hs cs = runHudWith ca (padBox $ boxes cs) hs cs

-- | Typical configurable hud elements. Anything else can be hand-coded as a 'HudT'.
--
-- ![hud example](other/hudoptions.svg)
data HudOptions = HudOptions
  { hudCanvas :: Maybe RectStyle,
    hudTitles :: [Title],
    hudAxes :: [AxisOptions],
    hudLegend :: Maybe (LegendOptions, [(Styles, Text)])
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

-- | alter the colour
colourHudOptions :: Colour -> HudOptions -> HudOptions
colourHudOptions c ho =
  ho
    & #hudCanvas %~ fmap (#color %~ mix c)
    & #hudTitles %~ fmap (#style . #color %~ mix c)
    & #hudAxes %~ fmap (#axisBar %~ fmap (#rstyle . #color %~ mix c))
    & #hudAxes %~ fmap (#axisTick . #gtick %~ fmap (first ((#color %~ mix c) . (#borderColor %~ mix c))))
    & #hudAxes %~ fmap (#axisTick . #ttick %~ fmap (first (#color %~ mix c)))
    & #hudAxes %~ fmap (#axisTick . #ltick %~ fmap (first (#color %~ mix c)))
    & #hudLegend %~ fmap (first (#ltext %~ (#color %~ mix c)))
    & #hudLegend %~ fmap (first (#legendFrame %~ fmap ((#color %~ mix c) . (#borderColor %~ mix c))))

-- | adjust the opacity of HudOptions up or down geometrically (scaling by (*o))
scaleOpacHudOptions :: HudOptions -> Double -> HudOptions
scaleOpacHudOptions ho o =
  ho
    & #hudCanvas %~ fmap (#color %~ scaleOpac o)
    & #hudTitles %~ fmap (#style . #color %~ scaleOpac o)
    & #hudAxes %~ fmap (#axisBar %~ fmap (#rstyle . #color %~ scaleOpac o))
    & #hudAxes %~ fmap (#axisTick . #gtick %~ fmap (first ((#color %~ scaleOpac o) . (#borderColor %~ scaleOpac o))))
    & #hudAxes %~ fmap (#axisTick . #ttick %~ fmap (first (#color %~ scaleOpac o)))
    & #hudAxes %~ fmap (#axisTick . #ltick %~ fmap (first (#color %~ scaleOpac o)))
    & #hudLegend %~ fmap (first (#ltext %~ (#color %~ scaleOpac o)))
    & #hudLegend %~ fmap (first (#legendFrame %~ fmap ((#color %~ scaleOpac o) . (#borderColor %~ scaleOpac o))))
    & #hudLegend %~ fmap (second (fmap (first (scaleOpacStyle o))))

-- | The official hud canvas
defaultCanvas :: RectStyle
defaultCanvas = blob (setOpac 0.05 dark)

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
    buff :: Double
  }
  deriving (Show, Eq, Generic)

-- | The official axis bar
defaultAxisBar :: AxisBar
defaultAxisBar = AxisBar (RectStyle 0 transparent (setOpac 0.4 dark)) 0.004 0.01

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
-- Tick {tstyle = TickRound (FormatComma (Just 2)) 8 TickExtend, gtick = Just (GlyphStyle {size = 3.0e-2, color = Colour 0.05 0.05 0.05 0.40, borderColor = Colour 0.05 0.05 0.05 0.40, borderSize = 2.0e-3, shape = VLineGlyph 5.0e-3, rotation = Nothing, translate = Nothing},1.25e-2), ttick = Just (TextStyle {size = 5.0e-2, color = Colour 0.05 0.05 0.05 1.00, anchor = AnchorMiddle, hsize = 0.5, vsize = 1.45, nudge1 = -0.2, rotation = Nothing},1.5e-2), ltick = Just (LineStyle {width = 5.0e-3, color = Colour 0.05 0.05 0.05 0.05, linecap = Nothing, linejoin = Nothing, dasharray = Nothing, dashoffset = Nothing},0.0)}
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
    & #borderSize .~ 0.002
    & #shape .~ VLineGlyph 0.005
    & #color .~ setOpac 0.4 dark
    & #borderColor .~ setOpac 0.4 dark

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
    lscale :: Double
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
    (Just (RectStyle 0.01 (setOpac 1 dark) (setOpac 0 dark)))
    PlaceRight
    0.25

-- | Make huds from a HudOptions.
--
-- Some huds, such as the creation of tick values, can extend the data dimension of a chart, so we return a blank chart with the new data dimension.
-- The complexity internally to this function is due to the creation of ticks and, specifically, 'gridSensible', which is not idempotent. As a result, a tick calculation that does extends the data area, can then lead to new tick values when applying TickRound etc.
makeHud :: Rect Double -> HudOptions -> ([Hud Double], [Chart Double])
makeHud xs cfg =
  ([axes] <> can <> titles <> l, xsext)
  where
    xs' = padBox xs
    can = foldMap (\x -> [canvas x]) (cfg ^. #hudCanvas)
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
    xsext = bool [BlankChart (NonEmpty.fromList tickRects)] [] (null tickRects)
    axes =
      foldr simulHud mempty $
        ( \x ->
            maybe mempty (makeAxisBar (x ^. #place)) (x ^. #axisBar)
              <> makeTick x
        )
          <$> hudaxes
    l =
      foldMap
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

-- | Make a canvas hud element.
canvas :: (Monad m) => RectStyle -> HudT m Double
canvas s = Hud $ \cs -> do
  a <- use #canvasDim
  let c = RectChart s (a:|[])
  #canvasDim .= a <> sbox_ c
  pure $ c : cs

axisBar_ :: Place -> AxisBar -> Rect Double -> Rect Double -> Chart Double
axisBar_ pl b (Rect x z y w) (Rect x' z' y' w') =
  case pl of
    PlaceTop ->
      RectChart
        (rstyle b)
        (NonEmpty.fromList
        [ Rect
            x
            z
            (w' + b ^. #buff)
            (w' + b ^. #buff + b ^. #wid)
        ])
    PlaceBottom ->
      RectChart
        (rstyle b)
        (NonEmpty.fromList
        [ Rect
            x
            z
            (y' - b ^. #wid - b ^. #buff)
            (y' - b ^. #buff)
        ])
    PlaceLeft ->
      RectChart
        (rstyle b)
        (NonEmpty.fromList
        [ Rect
            (x' - b ^. #wid - b ^. #buff)
            (x' - b ^. #buff)
            y
            w
        ])
    PlaceRight ->
      RectChart
        (rstyle b)
        (NonEmpty.fromList
        [ Rect
            (z' + (b ^. #buff))
            (z' + (b ^. #buff) + (b ^. #wid))
            y
            w
        ])
    PlaceAbsolute (Point x'' _) ->
      RectChart
        (rstyle b)
        (NonEmpty.fromList
        [ Rect
            (x'' + (b ^. #buff))
            (x'' + (b ^. #buff) + (b ^. #wid))
            y
            w
        ])

{-
addChartBox :: Chart Double -> Rect Double -> Rect Double
addChartBox c r = sconcat (r :| maybeToList (styleBox c))

addChartBoxes :: [Chart Double] -> Rect Double -> Rect Double
addChartBoxes c r = sconcat (r :| maybeToList (styleBoxes c))
-}

makeAxisBar :: (Monad m) => Place -> AxisBar -> HudT m Double
makeAxisBar pl b = Hud $ \cs -> do
  da <- use #chartDim
  ca <- use #canvasDim
  let c = axisBar_ pl b ca da
  #chartDim .= da <> sbox_ c
  pure $ c : cs

title_ :: Title -> Rect Double -> Chart Double
title_ t a =
  TextChart
    ( style'& #rotation .~ bool Nothing (Just rot) (rot == 0))
    (NonEmpty.fromList [(t ^. #text, placePos' a + alignPos a)])
  where
    style'
      | t ^. #anchor == AnchorStart =
        #anchor .~ AnchorStart $ t ^. #style
      | t ^. #anchor == AnchorEnd =
        #anchor .~ AnchorEnd $ t ^. #style
      | otherwise = t ^. #style
    rot' = fromMaybe 0 (t ^. #style . #rotation)
    rot
      | t ^. #place == PlaceRight = pi / 2 + rot'
      | t ^. #place == PlaceLeft = -pi / 2 + rot'
      | otherwise = rot'
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
        Point ((-x + z) / 2.0) 0.0
      | t ^. #anchor == AnchorEnd
          && t ^. #place == PlaceLeft =
        Point 0.0 ((-y + w) / 2.0)
      | t ^. #anchor == AnchorEnd
          && t ^. #place == PlaceRight =
        Point 0.0 ((y - w) / 2.0)
      | otherwise = Point 0.0 0.0

-- | Add a title to a chart. The logic used to work out placement is flawed due to being able to freely specify text rotation.  It works for specific rotations (Top, Bottom at 0, Left at 90, Right @ 270)
title :: (Monad m) => Title -> HudT m Double
title t = Hud $ \cs -> do
  ca <- use #chartDim
  let c = title_ t ca
  #chartDim .= ca <> sbox_ c
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
  | pl `elem` [PlaceTop, PlaceBottom] = Point x 0
  | otherwise = Point 0 x

placeTextAnchor :: Place -> (TextStyle -> TextStyle)
placeTextAnchor pl
  | pl == PlaceLeft = #anchor .~ AnchorEnd
  | pl == PlaceRight = #anchor .~ AnchorStart
  | otherwise = id

placeGridLines :: Place -> Rect Double -> Double -> Double -> NonEmpty (Point Double)
placeGridLines pl (Rect x z y w) a b
  | pl `elem` [PlaceTop, PlaceBottom] = NonEmpty.fromList [Point a (y - b), Point a (w + b)]
  | otherwise = NonEmpty.fromList [Point (x - b) a, Point (z + b) a]

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
  GlyphChart
    (g & #rotation .~ placeRot pl)
    (NonEmpty.fromList
    ( (placePos pl b ca +) . placeOrigin pl
        <$> positions
          (ticksPlaced ts pl da xs)
    ))

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
  #chartDim .= a <> sbox_ c
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
        TextChart (placeTextAnchor pl txts)
          (NonEmpty.fromList [(txt, sp)])
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
  #chartDim .= ca <> sboxes c
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
        LineChart ls
        ( sconcat $ NonEmpty.fromList $
          (\x -> placeGridLines pl da x b) <$>
          positions (ticksPlaced ts pl da xs)
        )
  #chartDim %= (<> sbox_ c)
  pure $ c:cs

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
                . (\x -> styleBoxText_ (fst tt) x (Point 0 0))
                <$> tickl
        )
        (t ^. #ttick)
    maxHeight =
      maybe
        1
        ( \tt ->
            max' $
              (\(Rect _ _ y w) -> w - y)
                . (\x -> styleBoxText_ (fst tt) x (Point 0 0))
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
  let cs' = (place' ca . scale_ (l ^. #lscale) <$> lcs) <> cs
  #chartDim .= sboxes cs'
  pure cs'
  where
    place' ca x = move_ (placel (l ^. #lplace) ca (sbox_ x)) x
    placel pl (Rect x z y w) (Rect x' z' y' w') =
      case pl of
        PlaceTop -> Point ((x + z) / 2.0) (w + (w' - y') / 2.0)
        PlaceBottom -> Point ((x + z) / 2.0) (y - (w' - y' / 2.0))
        PlaceLeft -> Point (x - (z' - x') / 2.0) ((y + w) / 2.0)
        PlaceRight -> Point (z + (z' - x') / 2.0) ((y + w) / 2.0)
        PlaceAbsolute p -> p

legendEntry ::
  LegendOptions ->
  Styles ->
  Text ->
  (Chart Double, Chart Double)
legendEntry l a t =
  ( chart1,
    TextChart (l ^. #ltext & #anchor .~ AnchorStart) ((t, zero):|[])
  )
  where
    chart1 = case a of
      RectA rs -> RectChart rs (Rect 0 (l ^. #lsize) 0 (l ^. #lsize):|[])
      TextA ts -> TextChart (ts & #size .~ (l ^. #lsize)) (("text",zero):|[])
      GlyphA gs -> GlyphChart (gs & #size .~ (l ^. #lsize)) (Point (0.5 * l ^. #lsize) (0.33 * l ^. #lsize):|[])
      LineA ls -> LineChart (ls & #width %~ (/ (l ^. #lscale)))
          (NonEmpty.fromList [Point 0 (1 * l ^. #lsize), Point (2 * l ^. #lsize) (1 * l ^. #lsize)])
      PathA ps ->
        ( let cs =
                singletonCubic
                  ( CubicPosition
                      (Point 0 0)
                      (Point (0.33 * l ^. #lsize) (0.33 * l ^. #lsize))
                      (Point 0 (0.33 * l ^. #lsize))
                      (Point (0.33 * l ^. #lsize) 0)
                  )
           in PathChart (ps & #borderSize .~ (l ^. #lsize)) (NonEmpty.fromList cs))

legendChart :: [(Styles, Text)] -> LegendOptions -> [Chart Double]
legendChart lrs l =
  padChart (l ^. #outerPad)
    . maybe id (\x -> frameChart x (l ^. #innerPad)) (l ^. #legendFrame)
    . vert (l ^. #hgap)
    $ (\(a, t) -> hori ((l ^. #vgap) + twidth - gapwidth t) [[t], [a]])
      <$> es
  where
    es = reverse $ uncurry (legendEntry l) <$> lrs
    twidth = (\(Rect _ z _ _) -> z) $ sboxes (snd <$> es)
    gapwidth t = (\(Rect _ z _ _) -> z) (sbox_ t)

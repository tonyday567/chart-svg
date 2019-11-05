{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Chart.Hud where

import Chart.Core
import Chart.Svg
import Chart.Types
import Codec.Picture.Types
import Control.Category (id)
import Control.Lens
import Data.Generics.Labels ()
import Data.List (nub)
import Data.Scientific
import Formatting
import Protolude
import NumHask.Space

data ChartDims a =
  ChartDims
  { chartDim :: Rect a
  , canvasDim :: Rect a
  , dataDim :: Rect a
  } deriving (Eq, Show, Generic)

newtype HudT m a = Hud { unhud :: [Chart a] -> StateT (ChartDims a) m [Chart a] }

type Hud = HudT Identity

instance (Monad m) => Semigroup (HudT m a) where
  (<>) (Hud h1) (Hud h2) = Hud $ h1 >=> h2

instance (Monad m) => Monoid (HudT m a) where
  mempty = Hud pure

data HudConfig = HudConfig
  { hudCanvas :: Maybe RectStyle
  , hudTitles :: [Title Double]
  , hudAxes ::  [AxisConfig Double]
  , hudLegends :: [LegendOptions Double]
  } deriving (Eq, Show, Generic)

defaultHudConfig :: HudConfig
defaultHudConfig =
  HudConfig
  (Just defaultCanvas)
  [defaultTitle "default"]
  [ defaultAxisConfig
  , defaultAxisConfig & #place .~ PlaceLeft
  ]
  []

-- | Placement of elements around (what is implicity but maybe shouldn't just be) a rectangular canvas
data Place a
  = PlaceLeft
  | PlaceRight
  | PlaceTop
  | PlaceBottom
  | PlaceAbsolute (Point a)
  deriving (Show, Eq, Generic)

placeText :: Place a -> Text
placeText p =
  case p of
    PlaceTop -> "Top"
    PlaceBottom -> "Bottom"
    PlaceLeft -> "Left"
    PlaceRight -> "Right"
    PlaceAbsolute _ -> "Absolute"

data AxisConfig a = AxisConfig
  { abar :: Maybe (Bar a)
  , adjust :: Maybe Adjustments
  , atick :: Tick a
  , place :: Place Double
  } deriving (Eq, Show, Generic)

defaultAxisConfig :: AxisConfig Double
defaultAxisConfig = AxisConfig (Just defaultBar) (Just defaultAdjustments) defaultTick PlaceBottom

canvas :: (Monad m, Chartable a) => RectStyle -> HudT m a
canvas s = Hud $ \cs -> do
  a <- use #canvasDim
  let c = Chart (RectA s) [SpotRect a]
  #canvasDim .= addToRect a (styleBox c)
  pure $ c:cs

defaultCanvas :: RectStyle
defaultCanvas = blob grey 0.03

data Bar a = Bar
  { rstyle :: RectStyle
  , wid :: a
  , buff :: a
  } deriving (Show, Eq, Generic)

defaultBar :: (Chartable a) => Bar a
defaultBar = Bar (RectStyle 0 grey 0 (PixelRGB8 95 3 145) 0.5) 0.005 0.01

bar_ :: (Chartable a) => Place a -> Bar a ->  Rect a -> Rect a -> Chart a
bar_ pl b (Rect x z y w) (Rect x' z' y' w') =
  case pl of
    PlaceTop ->
      Chart (RectA (rstyle b))
      [SR x z
       (w' + b ^. #buff)
        (w' + b ^. #buff + b ^. #wid)
      ]
    PlaceBottom ->
      Chart (RectA (rstyle b))
      [SR x z
       (y' - b ^. #wid - b ^. #buff)
       (y' - b ^. #buff)]
    PlaceLeft ->
      Chart (RectA (rstyle b))
      [SR
       (x' - b ^. #wid - b ^. #buff)
       (x' - b ^. #buff) y w]
    PlaceRight ->
      Chart (RectA (rstyle b))
      [SR (z' + (b ^. #buff))
        (z' + (b ^. #buff) + (b ^. #wid)) y w]
    PlaceAbsolute (Point x'' _) ->
      Chart (RectA (rstyle b))
      [SR (x'' + (b ^. #buff))
        (x'' + (b ^. #buff) + (b ^. #wid)) y w]

bar :: (Monad m, Chartable a) => Place a -> Bar a -> HudT m a
bar pl b = Hud $ \cs -> do
  da <- use #chartDim
  ca <- use #canvasDim
  let c = bar_ pl b ca da
  #chartDim .= addChartBox c da
  pure $ c:cs

-- | Options for titles.  Defaults to center aligned, and placed at Top of the hud
data Title a = Title
  { text :: Text
  , style :: TextStyle
  , place :: Place a
  , anchor :: Anchor
  , buff :: a
  } deriving (Show, Eq, Generic)

defaultTitle :: (Chartable a) => Text -> Title a
defaultTitle txt =
    Title
    txt
    ( #size .~ 0.12 $
      #color .~ PixelRGB8 0 0 0 $
      defaultTextStyle)
    PlaceTop
    AnchorMiddle
    0.04

title_ :: (Chartable a) => Title a ->  Rect a -> Chart a
title_ t a =
    Chart (TextA (style' &
      #translate ?~ (realToFrac <$> (placePos' a + alignPos a)) &
      #rotation ?~ rot
                 ) [t ^. #text])
    [p0]
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
          PlaceTop -> Point ((x+z)/2.0) (w + (t ^. #buff))
          PlaceBottom -> Point ((x+z)/2.0)
            (y - (t ^. #buff) -
              0.5 * realToFrac (t ^. #style . #vsize) *
              realToFrac (t ^. #style . #size))
          PlaceLeft -> Point (x - (t ^. #buff)) ((y+w)/2.0)
          PlaceRight -> Point (z + (t ^. #buff)) ((y+w)/2.0)
          PlaceAbsolute p -> p
        alignPos (Rect x z y w)
          | t ^. #anchor == AnchorStart &&
            t ^. #place `elem` [PlaceTop, PlaceBottom] =
            Point ((x-z)/ 2.0) 0.0
          | t ^. #anchor == AnchorStart &&
            t ^. #place == PlaceLeft =
            Point 0.0 ((y-w)/2.0)
          | t ^. #anchor == AnchorStart &&
            t ^. #place == PlaceRight =
            Point 0.0 ((w-y)/2.0)
          | t ^. #anchor == AnchorEnd &&
            t ^. #place `elem` [PlaceTop, PlaceBottom] =
            Point ((-x+z)/2.0) 0.0
          | t ^. #anchor == AnchorEnd &&
            t ^. #place == PlaceLeft =
            Point 0.0 ((-y+w)/2.0)
          | t ^. #anchor == AnchorEnd &&
            t ^. #place == PlaceRight =
            Point 0.0 ((y-w)/2.0)
          | otherwise = Point 0.0 0.0

-- | Add a title to a chart. The logic used to work out placement is flawed due to being able to freely specify text rotation.  It works for specific rotations (Top, Bottom at 0, Left at 90, Right @ 270)
title :: (Monad m, Chartable a) => Title a ->  HudT m a
title t = Hud $ \cs -> do
  ca <- use #chartDim
  let c = title_ t ca
  #chartDim .= addChartBox c ca
  pure $ c:cs

data Tick a = Tick
  { tstyle :: TickStyle a
  , gtick :: Maybe (GlyphStyle, a)
  , ttick :: Maybe (TextStyle, a)
  , ltick :: Maybe (LineStyle, a)
  } deriving (Show, Eq, Generic)

defaultGlyphTick :: GlyphStyle
defaultGlyphTick =
  defaultGlyphStyle &
  #borderSize .~ 0 &
  #color .~ PixelRGB8 95 3 145 &
  #opacity .~ 1 &
  #shape .~ VLineGlyph 0.005

defaultTextTick :: TextStyle
defaultTextTick =
  defaultTextStyle & #size .~ 0.05

defaultLineTick :: LineStyle
defaultLineTick =
  defaultLineStyle &
  #color .~ PixelRGB8 168 229 238 &
  #width .~ 5.0e-3 &
  #opacity .~ 0.3

defaultTick :: (Chartable a) => Tick a
defaultTick =
  Tick
  defaultTickStyle
  (Just (defaultGlyphTick, 0.01))
  (Just (defaultTextTick, 0.02))
  (Just (defaultLineTick, 0.0))

data TickFormat
  = TickFormatDefault
  | TickFormatCommas Int
  | TickFormatFixed Int
  | TickFormatDollars
  deriving (Show, Eq, Generic)

tickFormatText :: TickFormat -> Text
tickFormatText TickFormatDefault = "TickFormatDefault"
tickFormatText TickFormatCommas{} = "TickFormatCommas"
tickFormatText TickFormatFixed{} = "TickFormatFixed"
tickFormatText TickFormatDollars = "TickFormatDollars"

toFormat :: (Chartable a) => TickFormat -> [a] -> [Text]
toFormat TickFormatDefault = precision commas 0
toFormat (TickFormatCommas n) = precision commas n
toFormat (TickFormatFixed n) = precision (fixed n) n
toFormat TickFormatDollars = fmap ("$" <>) . precision commas 2

-- | Style of tick marks on an axis.
data TickStyle a
  = TickNone -- ^ no ticks on axis
  | TickLabels [Text] -- ^ specific labels (equidistant placement)
  | TickRound TickFormat Int TickExtend -- ^ sensibly rounded ticks, a guide to how many, and whether to extend beyond the data bounding box
  | TickExact TickFormat Int -- ^ exactly n equally spaced ticks
  | TickPlaced [(a, Text)] -- ^ specific labels and placement
  deriving (Show, Eq, Generic)

defaultTickStyle :: TickStyle a
defaultTickStyle = TickRound TickFormatDefault 8 TickExtend

tickStyleText :: TickStyle a -> Text
tickStyleText TickNone = "TickNone"
tickStyleText TickLabels{} = "TickLabels"
tickStyleText TickRound{} = "TickRound"
tickStyleText TickExact{} = "TickExact"
tickStyleText TickPlaced{} = "TickPlaced"

data TickExtend = TickExtend | NoTickExtend deriving (Eq, Show, Generic)

-- | Provide formatted text for a list of numbers so that they are just distinguished.  'precision commas 2 ticks' means give the tick labels as much precision as is needed for them to be distinguished, but with at least 2 significant figures, and format Integers with commas.
precision :: (Chartable a) => Format Text (Integer -> Text) -> Int -> [a] -> [Text]
precision f n0 xs
  | foldr max 0 xs < 0.01 = precLoop expt' n0 (fromFloatDigits <$> xs)
  | foldr max 0 xs > 100000 = precLoop expt' n0 (fromFloatDigits <$> xs)
  | foldr max 0 xs > 1000 =
    precLoopInt (const f) n0 (floor <$> xs :: [Integer])
  | otherwise = precLoop fixed n0 xs
  where
    expt' x = scifmt Exponent (Just x)
    precLoop f' n xs' =
      let s = sformat (f' n) <$> xs'
      in if s == nub s
           then s
           else precLoop f' (n + 1) xs'
    precLoopInt f' n xs' =
      let s = sformat (f' n) <$> xs'
      in if s == nub s
           then s
           else precLoopInt f' (n + 1) xs'

placePos :: (Chartable a) => Place a -> a -> Rect a -> Point a
placePos pl b (Rect x z y w) = case pl of
  PlaceTop -> Point 0 (w + b)
  PlaceBottom -> Point 0 (y - b)
  PlaceLeft -> Point (x - b) 0
  PlaceRight -> Point (z + b) 0
  PlaceAbsolute p -> p

placeRot :: (Chartable a) => Place a -> Maybe a
placeRot pl = case pl of
  PlaceRight -> Just (-90.0)
  PlaceLeft -> Just (-90.0)
  _ -> Nothing

textPos :: (Chartable a) => Place a -> TextStyle -> a -> Point a
textPos pl tt b = case pl of
  PlaceTop -> Point 0 b
  PlaceBottom -> Point 0 (-b + -0.5 * realToFrac (tt ^. #vsize) * realToFrac (tt ^. #size))
  PlaceLeft -> Point (-b)
    (realToFrac (tt ^. #nudge1) * realToFrac (tt ^. #vsize) * realToFrac (tt ^. #size))
  PlaceRight -> Point b
    (realToFrac (tt ^. #nudge1) * realToFrac (tt ^. #vsize) * realToFrac (tt ^. #size))
  PlaceAbsolute p -> p

placeRange :: Place a -> Rect a -> Range a
placeRange pl (Rect x z y w) = case pl of
  PlaceRight -> Range y w
  PlaceLeft -> Range y w
  _ -> Range x z

placeOrigin :: (Chartable a) => Place a -> a -> Point a
placeOrigin pl x
  | pl `elem` [PlaceTop, PlaceBottom] = Point x 0
  | otherwise = Point 0 x

placeTextAnchor :: (Chartable a) => Place a -> (TextStyle -> TextStyle)
placeTextAnchor pl
  | pl == PlaceLeft = #anchor .~ AnchorEnd
  | pl == PlaceRight = #anchor .~ AnchorStart
  | otherwise = id

placeGridLines :: (Chartable a) => Place a -> Rect a -> a -> a -> [Spot a]
placeGridLines pl (Rect x z y w) a b
  | pl `elem` [PlaceTop, PlaceBottom] = [SP a (y - b), SP a (w + b)]
  | otherwise = [SP (x - b) a, SP (z + b) a]

-- | compute tick values and labels given options, ranges and formatting
ticksR :: (Chartable a) => TickStyle a -> Range a -> Range a -> [(a, Text)]
ticksR s d r =
    case s of
      TickNone -> []
      TickRound f n e -> zip (project r d <$> ticks0) (toFormat f ticks0)
        where ticks0 = gridSensible OuterPos (e == NoTickExtend) r (fromIntegral n :: Integer)
      TickExact f n -> zip (project r d <$> ticks0) (toFormat f ticks0)
        where ticks0 = grid OuterPos r n
      TickLabels ls ->
          zip (project (Range 0 (fromIntegral $ length ls)) d <$>
            ((\x -> x - 0.5) . fromIntegral <$> [1 .. length ls])) ls
      TickPlaced xs -> zip (project r d . fst <$> xs) (snd <$> xs)

-- | compute tick values and labels given options, ranges and formatting
ticksR' :: (Chartable a) => TickStyle a -> Range a -> ([(a, Text)], Maybe (Range a))
ticksR' s r =
    case s of
      TickNone -> ([], Nothing)
      TickRound f n e -> (zip ticks0 (toFormat f ticks0),
                         bool (Just $ space1 ticks0) Nothing (e==NoTickExtend))
        where ticks0 = gridSensible OuterPos (e == NoTickExtend) r (fromIntegral n :: Integer)
      TickExact f n -> (zip ticks0 (toFormat f ticks0), Nothing)
        where ticks0 = grid OuterPos r n
      TickLabels ls ->
          (zip (project (Range 0 (fromIntegral $ length ls)) r <$>
            ((\x -> x - 0.5) . fromIntegral <$> [1 .. length ls])) ls, Nothing)
      TickPlaced xs -> (xs, Nothing)

-- | compute tick values given placement
ticksA :: (Chartable a) => TickStyle a -> Place a -> Rect a -> Rect a -> [(a, Text)]
ticksA ts pl d xs = ticksR ts (placeRange pl d) (placeRange pl xs)

ticksA' :: (Chartable a) => TickStyle a -> Place a -> Rect a -> ([(a, Text)], Maybe (Rect a))
ticksA' ts pl xs = (tr, maybe Nothing (\x -> Just $ replaceRange pl x xs) ma)
  where
    (tr, ma) = ticksR' ts (placeRange pl xs)

tickGlyph_ :: (Chartable a) => Place a -> (GlyphStyle, a) -> TickStyle a -> Rect a -> Rect a -> Rect a -> Chart a
tickGlyph_ pl (g,b) ts ca da xs =
   Chart (GlyphA (g & #rotation .~ (realToFrac <$> placeRot pl)))
    (SpotPoint . (placePos pl b ca +) . placeOrigin pl . fst <$> ticksA ts pl da xs)

-- | aka marks
tickGlyph :: (Monad m, Chartable a) =>
  Place a -> (GlyphStyle, a) -> TickStyle a ->  HudT m a
tickGlyph pl (g, b) ts = Hud $ \cs -> do
  a <- use #chartDim
  d <- use #canvasDim
  xs <- use #dataDim
  let c = tickGlyph_ pl (g,b) ts a d xs
  #chartDim .= addToRect a (styleBox c)
  pure $ c:cs

tickText_ :: (Chartable a) => Place a -> (TextStyle, a) -> TickStyle a -> 
  Rect a -> Rect a -> Rect a -> [Chart a]
tickText_ pl (txts, b) ts ca da xs =
  zipWith (\txt sp ->
    Chart (TextA
      ( placeTextAnchor pl txts) [txt])
                [SpotPoint sp])
             (snd <$> ticksA ts pl da xs)
             ((placePos pl b ca + textPos pl txts b +) . placeOrigin pl . fst <$> ticksA ts pl da xs)

-- | aka tick labels
tickText :: (Monad m, Chartable a) =>
  Place a -> (TextStyle, a) -> TickStyle a ->  HudT m a
tickText pl (txts, b) ts = Hud $ \cs -> do
  ca <- use #chartDim
  da <- use #canvasDim
  xs <- use #dataDim
  let c = tickText_ pl (txts, b) ts ca da xs
  #chartDim .= addChartBoxes c ca
  pure $ c <> cs

-- | aka grid lines
tickLine :: (Monad m, Chartable a) =>
  Place a -> (LineStyle, a) -> TickStyle a ->  HudT m a
tickLine pl (ls, b) ts = Hud $ \cs -> do
  da <- use #canvasDim
  xs <- use #dataDim
  let c = Chart (LineA ls) . (\x -> placeGridLines pl da x b) . fst <$> ticksA ts pl da xs
  #chartDim %= addChartBoxes c
  pure $ c <> cs

-- | Create tick glyphs (marks), lines (grid) and text (labels)
tick :: (Monad m, Chartable a) =>
  Place a -> Tick a ->  HudT m a
tick pl t =
  maybe mempty (\x -> tickGlyph pl x (t ^. #tstyle)) (t ^. #gtick) <>
  maybe mempty (\x -> tickText pl x (t ^. #tstyle)) (t ^. #ttick) <>
  maybe mempty (\x -> tickLine pl x (t ^. #tstyle)) (t ^. #ltick) <>
  extendData pl t

-- | compute an extension to the Range if a tick went over the data bounding box
computeTickExtension :: (Chartable a) => TickStyle a -> Range a -> Maybe (Range a)
computeTickExtension s r =
    case s of
      TickNone -> Nothing
      TickRound _ n e -> bool Nothing (Just (space1 ticks0 <> r)) (e == TickExtend)
        where ticks0 = gridSensible OuterPos (e == NoTickExtend) r (fromIntegral n :: Integer)
      TickExact _ _ -> Nothing
      TickLabels _ -> Nothing
      TickPlaced xs -> Just $ r <> space1 (fst <$> xs)

-- | Create a style extension for the data, if ticks extend beyond the existing range
tickExtended :: (Chartable a) =>
  Place a -> Tick a -> Rect a -> Rect a
tickExtended pl t xs =
  maybe xs (\x -> rangeext xs x)
  (computeTickExtension (t ^. #tstyle) (ranged xs))
  where
    ranged xs' = case pl of
      PlaceTop -> rangex xs'
      PlaceBottom -> rangex xs'
      PlaceLeft -> rangey xs'
      PlaceRight -> rangey xs'
    rangex (Rect x z _ _) = Range x z
    rangey (Rect _ _ y w) = Range y w
    rangeext (Rect x z y w) (Range a0 a1) = case pl of
      PlaceTop -> Rect a0 a1 y w
      PlaceBottom -> Rect a0 a1 y w
      PlaceLeft -> Rect x z a0 a1
      PlaceRight -> Rect x z a0 a1

extendData :: (Monad m, Chartable a) => Place a -> Tick a -> HudT m a
extendData pl t = Hud $ \cs -> do
  #dataDim %= tickExtended pl t
  pure cs

-- | options for prettifying axis decorations
data Adjustments =
  Adjustments
  { maxXRatio :: Double
  , maxYRatio :: Double
  , angledRatio :: Double
  , allowDiagonal :: Bool
  } deriving (Show, Eq, Generic) 

defaultAdjustments :: Adjustments
defaultAdjustments = Adjustments 0.08 0.06 0.12 True

-- | adjust Tick for sane font sizes etc
adjustTick :: (Chartable a) => Adjustments -> Rect a -> Rect a -> Place a -> 
  Tick a -> Tick a
adjustTick (Adjustments mrx ma mry ad) vb cs pl t
  | pl `elem` [PlaceBottom, PlaceTop] = case ad of
      False -> t & #ttick . _Just . _1 . #size %~ (/adjustSizeX)
      True ->
        case adjustSizeX > 1 of
          True ->
            (case pl of
                PlaceBottom -> #ttick . _Just . _1 . #anchor .~ AnchorEnd
                PlaceTop -> #ttick . _Just . _1 . #anchor .~ AnchorStart
                _ -> #ttick . _Just . _1 . #anchor .~ AnchorEnd) $
             (#ttick . _Just . _1 . #size %~ (/adjustSizeA)) $
             (#ttick . _Just . _1 . #rotation ?~ (-45)) t
          False -> (#ttick . _Just . _1 . #size %~ (/adjustSizeA)) t
  | pl `elem` [PlaceLeft, PlaceRight] =
    (#ttick . _Just . _1 . #size %~ (/adjustSizeY)) t
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
          maybe 1 (\tt -> max' $
          (\(Rect x z _ _) -> z - x)
              . (\x -> styleBoxText (fst tt) x (Point 0 0)) <$> tickl) (t ^. #ttick)
    maxHeight =
      maybe 1
          (\tt -> max' $
          (\(Rect _ _ y w) -> w - y)
              . (\x -> styleBoxText (fst tt) x (Point 0 0)) <$> tickl) (t ^. #ttick)
    adjustSizeX :: Double
    adjustSizeX = max' [(maxWidth / realToFrac (upper asp - lower asp)) / mrx, 1]
    adjustSizeY = max' [(maxHeight / realToFrac (upper asp - lower asp)) / mry, 1]
    adjustSizeA = max' [(maxHeight / realToFrac (upper asp - lower asp)) / ma, 1]

adjustedTickHud :: (Monad m) => AxisConfig Double -> HudT m Double
adjustedTickHud c = Hud $ \cs -> do
  vb <- use #chartDim
  xs <- use #dataDim
  let adjTick =
        maybe
        (c ^. #atick)
        (\x -> adjustTick x vb xs (c ^. #place) (c ^. #atick))
        (c ^. #adjust)
  unhud (tick (c ^. #place) adjTick) cs


-- $combination
-- the complexity here is due to gridSensible, which is not idempotent.  We have to remember the tick calculation that extends the data area, because reapplying TickRound etc creates a new set of ticks different to the original.

-- | combine huds and charts to form a new [Chart] using the supplied canvas and data dimensions.  Note that styling parameters such as #transition do not scale with combination, so results can be not what you expect.
hudChartWith :: (Chartable a) => Rect a -> Rect a -> [Hud a] -> [Chart a] -> [Chart a]
hudChartWith ca xs hs cs = flip evalState (ChartDims ca' da' xs') $
  (unhud $ mconcat hs) cs'
  where
    xs' = defRectS $ dataBox cs
    da' = defRect $ dataBox cs'
    ca' = defRect $ styleBoxes cs'
    cs' = projectSpotsWith ca xs cs

-- | combine huds and charts to form a new [Chart] using the supplied canvas and the actual data dimension.
hudChart :: (Chartable a) => Rect a -> [Hud a] -> [Chart a] -> [Chart a]
hudChart ca hs cs = flip evalState (ChartDims ca' da' xs') $
  (unhud $ mconcat hs) cs'
  where
    xs' = defRectS $ dataBox cs
    da' = defRect $ dataBox cs'
    ca' = defRect $ styleBoxes cs'
    cs' = projectSpotsWith ca xs' cs

-- | combine huds and charts to form a ChartSvg using the supplied canvas and data dimensions
hudChartSvgWith :: (Chartable a) => Rect a -> Rect a -> [Hud a] -> [Chart a] -> ChartSvg a
hudChartSvgWith ca xs hs cs = flip evalState (ChartDims ca' da' xs') $ do
  cs'' <- (unhud $ mconcat hs) cs'
  cd <- use #chartDim
  pure $ chartSvg_ cd cs''
  where
    xs' = defRectS $ dataBox cs
    da' = defRect $ dataBox cs'
    ca' = defRect $ styleBoxes cs'
    cs' = projectSpotsWith ca xs cs

-- | combine huds and charts to form a ChartSvg using the supplied canvas dimension and the actual data range
hudChartSvg :: (Chartable a) =>
  Rect a -> [[Hud a]] -> [Chart a] -> ChartSvg a
hudChartSvg ca hss cs =
  hudChartSvgWith ca (defRect $ dataBox cs) (mconcat <$> hss) cs


-- | combine a HudConfig and charts to form a ChartSvg using the supplied canvas dimensions, and extended the data range if needed by the huds.
hud :: HudConfig -> Rect Double -> [Chart Double] -> ChartSvg Double
hud cfg ca cs =
  hudChartSvgWith ca (defRect $ dataBox (cs <> cs')) hs (cs <> cs')
  where
    (hs, cs') = hudsWithExtend (defRectS $ dataBox cs) cfg

-- | compute huds with frozen tick values and a data range extension
hudsWithExtend :: Rect Double -> HudConfig -> ([Hud Double], [Chart Double])
hudsWithExtend xs cfg =
  (haxes <> [can] <> titles <> ls, [xsext])
  where
    can = maybe mempty (\x -> canvas x) (cfg ^. #hudCanvas)
    titles = title <$> (cfg ^. #hudTitles)
    newticks =
      (\a -> freezeTicks (a ^. #place) xs (a ^. #atick . #tstyle)) <$>
      (cfg ^. #hudAxes)
    axes' = zipWith (\c t -> c & #atick . #tstyle .~ fst t) (cfg ^. #hudAxes) newticks
    xsext = Chart BlankA (SpotRect <$> catMaybes (snd <$> newticks))
    haxes = (\x ->
             maybe mempty (\a -> bar (x ^. #place) a) (x ^. #abar) <>
             adjustedTickHud x) <$> axes'
    ls = legend <$> (cfg ^. #hudLegends)

-- | convert TickRound to TickPlaced
freezeTicks :: (Chartable a) => Place a -> Rect a -> TickStyle a -> (TickStyle a, Maybe (Rect a))
freezeTicks pl xs ts@TickRound{} = maybe (ts, Nothing) (\x -> (TickPlaced ta, Just x)) ma
  where
    (ta, ma) = ticksA' ts pl xs
freezeTicks _ _ ts = (ts, Nothing)

replaceRange :: Place a -> Range a -> Rect a -> Rect a
replaceRange pl (Range a0 a1) (Rect x z y w) = case pl of
  PlaceRight -> Rect x z a0 a1
  PlaceLeft -> Rect x z a0 a1
  _ -> Rect a0 a1 y w

renderHudChartWith :: ChartSvgStyle -> HudConfig -> [Chart Double] -> Text
renderHudChartWith scfg hcfg cs =
  renderChartSvg (scfg ^. #sizex) (scfg ^. #sizey) $
  maybe id pad (scfg ^. #outerPad) $
  maybe id (\x c -> frame x c <> c) (scfg ^. #chartFrame) $
  maybe id pad (scfg ^. #innerPad) $
  hud hcfg (aspect (scfg ^. #chartAspect)) $
  cs <>
  maybe mempty (\g -> [showOriginWith g]) (scfg ^. #orig)

renderCharts :: ChartSvgStyle -> [Chart Double] -> Text
renderCharts scfg cs =
  renderChartSvg (scfg ^. #sizex) (scfg ^. #sizey) $
  maybe id pad (scfg ^. #outerPad) $
  maybe id (\x c -> frame x c <> c) (scfg ^. #chartFrame) $
  maybe id pad (scfg ^. #innerPad) $
  chartSvg (aspect (scfg ^. #chartAspect)) $
  cs <>
  maybe mempty (\g -> [showOriginWith g]) (scfg ^. #orig)

-- | Legend options
data LegendOptions a = LegendOptions
  { lcharts :: [(Annotation, Text)]
  , lsize :: a
  , vgap :: a
  , hgap :: a
  , ltext :: TextStyle
  , lmax :: Int
  , innerPad :: Maybe a
  , outerPad :: Maybe a
  , legendFrame :: Maybe RectStyle
  , lplace :: Place a
  , scale :: a
  } deriving (Show, Eq, Generic)

defaultLegendOptions :: (Chartable a) => LegendOptions a
defaultLegendOptions =
    LegendOptions
      []
      0.1
      0.2
      0.1
      (defaultTextStyle &
       #size .~ 0.1 &
       #color .~ grey
       )
      10
      (Just 1.02)
      (Just 1.10)
      (Just (border 0.01 (PixelRGB8 55 100 160) 1))
      PlaceBottom
      0.2

legend :: (Chartable a) => LegendOptions a -> Hud a
legend l = Hud $ \cs -> do
  ca <- use #chartDim
  let cs' = cs <> movedleg ca (scaledleg ca)
  #chartDim .= defRect (styleBoxes cs')
  pure cs'
    where
      scaledleg ca' =
        (#annotation %~ scaleAnn (realToFrac $ l ^. #scale)) <$>
        projectSpots (fmap (* l ^. #scale) ca') (makeLegend l)
      movedleg ca' leg =
        maybe id (moveChart . SpotPoint . placel (l ^. #lplace) ca') (styleBoxes leg) leg
      placel pl (Rect x z y w) (Rect x' z' y' w') =
        case pl of
          PlaceTop -> Point ((x+z)/2.0) (w + (w' - y')/2.0)
          PlaceBottom -> Point ((x+z)/2.0) (y - (w' - y'/2.0))
          PlaceLeft -> Point (x - (z' - x')/2.0) ((y+w)/2.0)
          PlaceRight -> Point (z + (z' - x')/2.0) ((y+w)/2.0)
          PlaceAbsolute p -> p

legendEntry :: (Chartable a) =>
  LegendOptions a -> Annotation -> Text -> (Chart a, Chart a)
legendEntry l a t =
  ( Chart ann sps
  , Chart (TextA (l ^. #ltext & #anchor .~ AnchorStart) [t]) [SP 0 0]
  )
  where
    (ann, sps) = case a of
      RectA rs ->
        ( RectA rs
        , [SR 0 (l ^. #lsize) 0 (l ^. #lsize)]
        )
      TextA ts txts ->
        ( TextA (ts & #size .~ realToFrac (l ^. #lsize)) (take 1 txts)
        , [SP 0 0]
        )
      GlyphA gs ->
        ( GlyphA (gs & #size .~ realToFrac (l ^. #lsize))
        , [SP 0 (0.33 * l ^. #lsize)]
        )
      LineA ls ->
        ( LineA ls
        , [SP 0 (0.33 * l ^. #lsize), SP (2 * l ^. #lsize) (0.33 * l ^. #lsize)]
        )
      BlankA ->
        ( BlankA
        , [SP 0 0]
        )

-- horizontally stack a list of list of charts (proceeding to the right) with a gap between
hori :: Chartable a => a -> [[Chart a]] -> [Chart a]
hori _ [] = []
hori gap cs = foldl step [] cs
  where
    step x a = x <> (a & fmap (#spots %~ fmap (\s -> SP (z x) 0 + s)))
    z xs = maybe 0 (\(Rect _ z' _ _) -> z' + gap) (styleBoxes xs)

-- vertically stack a list of charts (proceeding upwards)
vert :: Chartable a => a -> [[Chart a]] -> [Chart a]
vert _ [] = []
vert gap cs = foldl step [] cs
  where
    step x a = x <> (a & fmap (#spots %~ fmap (\s -> SP 0 (w x) + s)))
    w xs = maybe 0 (\(Rect _ _ _ w') -> w' + gap) (styleBoxes xs)

moveChart :: Chartable a => Spot a -> [Chart a] -> [Chart a]
moveChart sp cs = fmap (#spots %~ fmap (sp+)) cs

makeLegend :: (Chartable a) => LegendOptions a -> [Chart a]
makeLegend l = cs'
  where
    es = reverse $ uncurry (legendEntry l) <$> (l ^. #lcharts)
    twidth = maybe 1 (\(Rect _ z _ _) -> z) $ foldRect $ catMaybes (styleBox . snd <$> es)
    as = moveChart (SP (twidth * l ^. #lsize) 0.0) (fst <$> es)
    hs = zipWith (\a t -> hori (l ^. #vgap) [[t], [a]]) as (snd <$> es)
    vs = vert (l ^. #hgap) hs
    cs = vs <>
      maybe [] (\x -> [Chart (RectA x)
        (maybeToList (SpotRect <$> (fmap (* maybe 1 realToFrac
        (l ^. #innerPad)) <$> styleBoxes vs)))])
      (l ^. #legendFrame)
    cs' =
      cs <>
      [Chart BlankA (maybeToList (SpotRect . fmap (* maybe 1 realToFrac (l ^. #outerPad)) <$>
       styleBoxes cs))]




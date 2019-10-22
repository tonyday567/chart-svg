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
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Chart.Hud where

import Chart.Core
import Chart.Spot
import Chart.Svg
import Codec.Picture.Types
import Control.Category (id)
import Control.Lens
import Data.Generics.Labels ()
import Data.List (nub)
import Data.Scientific
import Formatting
-- import Graphics.Svg.Types as Svg hiding (Point, Text)
import Lucid.Base
import NumHask.Prelude as P hiding (Group)
-- import Control.Monad.State.Lazy

maximum' :: (Ord a) => [a] -> Maybe a
maximum' [] = Nothing
maximum' xs = Just $ maximum xs

data ChartDims a =
  ChartDims
  { chartDim :: ViewBox a
  , canvasDim :: ViewBox a
  , dataArea :: Area a
  } deriving (Eq, Show, Generic)

newtype HudT m a = Hud { unhud :: [Chart a] -> StateT (ChartDims a) m [Chart a] }

type Hud = HudT Identity

instance (Monad m) => Semigroup (HudT m a) where
  (<>) (Hud h1) (Hud h2) = Hud $ h1 >=> h2

instance (Monad m) => Monoid (HudT m a) where
  mempty = Hud pure

-- | Placement of elements around (what is implicity but maybe shouldn't just be) a rectangular canvas
data Place a
  = PlaceLeft
  | PlaceRight
  | PlaceTop
  | PlaceBottom
  | PlaceAbsolute (Point a)
  deriving (Show, Eq, Generic)

instance (Show a) => ToHtml (Place a) where
  toHtml p = toHtml (show p :: Text)
  toHtmlRaw p = toHtmlRaw (show p :: Text)

fromPlaceText :: Place a -> Text
fromPlaceText p =
  case p of
    PlaceTop -> "Top"
    PlaceBottom -> "Bottom"
    PlaceLeft -> "Left"
    PlaceRight -> "Right"
    PlaceAbsolute _ -> "Absolute"

data Bar a = Bar
  { rstyle :: RectStyle
  , wid :: a
  , buff :: a
  } deriving (Show, Eq, Generic)

defaultBar :: (FromRational a) => Bar a
defaultBar = Bar (RectStyle 0 grey 0 (PixelRGB8 95 3 145) 0.5) 0.005 0.01

canvas :: (Monad m, Chartable a) => RectStyle -> DrawAttributes -> HudT m a
canvas s das = Hud $ \cs -> do
  (ViewBox a) <- use #canvasDim
  let c = Chart (RectA s) das [SpotArea a]
  #canvasDim .= (ViewBox $ a <> styleBox c)
  pure $ c:cs

bar_ :: (Chartable a) => Place a -> Bar a -> DrawAttributes -> Area a -> Area a -> Chart a
bar_ pl b das (Area x z y w) (Area x' z' y' w') =
  case pl of
    PlaceTop ->
      Chart (RectA (rstyle b)) das
      [SA x z
       (w' + b ^. #buff)
        (w' + b ^. #buff + b ^. #wid)
      ]
    PlaceBottom ->
      Chart (RectA (rstyle b)) das
      [SA x z
       (y' - b ^. #wid - b ^. #buff)
       (y' - b ^. #buff)]
    PlaceLeft ->
      Chart (RectA (rstyle b)) das
      [SA
       (x' - b ^. #wid - b ^. #buff)
       (x' - b ^. #buff) y w]
    PlaceRight ->
      Chart (RectA (rstyle b)) das
      [SA (z' + (b ^. #buff))
        (z' + (b ^. #buff) + (b ^. #wid)) y w]
    PlaceAbsolute (Point x'' _) ->
      Chart (RectA (rstyle b)) das
      [SA (x'' + (b ^. #buff))
        (x'' + (b ^. #buff) + (b ^. #wid)) y w]

bar :: (Monad m, Chartable a) => Place a -> Bar a -> DrawAttributes -> HudT m a
bar pl b das = Hud $ \cs -> do
  (ViewBox da) <- use #chartDim
  (ViewBox ca) <- use #canvasDim
  let c = bar_ pl b das ca da
  #chartDim .= (ViewBox $ da <> styleBox c)
  pure $ c:cs

bars :: (Monad m, Chartable a) => Place a -> [Bar a] -> DrawAttributes -> HudT m a
bars pl bs das = mconcat ((\b -> bar pl b das) <$> bs)

-- | Options for titles.  Defaults to center aligned, and placed at Top of the hud
data Title a = Title
  { text :: P.Text
  , style :: TextStyle
  , place :: Place a
  , anchor :: Anchor
  , buff :: a
  } deriving (Show, Eq, Generic)

defaultTitle :: (FromRational a) => P.Text -> Title a
defaultTitle txt =
    Title
    txt
    ( #size .~ 0.12 $
      #color .~ PixelRGB8 0 0 0 $
      defaultTextStyle)
    PlaceTop
    AnchorMiddle
    0.04

title_ :: (Chartable a) => Title a -> DrawAttributes -> Area a -> Chart a
title_ t das a =
    Chart (TextA style' [t ^. #text])
    (das <> translateDA (placePos' a + alignPos a) <> rotateDA (rot :: Double)) [zero]
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
        placePos' (Area x z y w) = case t ^. #place of
          PlaceTop -> Point ((x+z)/2.0) (w + (t ^. #buff))
          PlaceBottom -> Point ((x+z)/2.0)
            (y - (t ^. #buff) -
              0.5 * fromRational' (t ^. #style ^. #vsize) *
              fromRational' (t ^. #style ^. #size))
          PlaceLeft -> Point (x - (t ^. #buff)) ((y+w)/2.0)
          PlaceRight -> Point (z + (t ^. #buff)) ((y+w)/2.0)
          PlaceAbsolute p -> p
        alignPos (Area x z y w)
          | t ^. #anchor == AnchorStart &&
            t ^. #place `elem` [PlaceTop, PlaceBottom] =
            Point ((x-z)/ 2.0) 0.0
          | t ^. #anchor == AnchorStart &&
            t ^. #place `elem` [PlaceLeft] =
            Point 0.0 ((y-w)/2.0)
          | t ^. #anchor == AnchorStart &&
            t ^. #place `elem` [PlaceRight] =
            Point 0.0 ((w-y)/2.0)
          | t ^. #anchor == AnchorEnd &&
            t ^. #place `elem` [PlaceTop, PlaceBottom] =
            Point ((-x+z)/2.0) 0.0
          | t ^. #anchor == AnchorEnd &&
            t ^. #place `elem` [PlaceLeft] =
            Point 0.0 ((-y+w)/2.0)
          | t ^. #anchor == AnchorEnd &&
            t ^. #place `elem` [PlaceRight] =
            Point 0.0 ((y-w)/2.0)
          | otherwise = Point 0.0 0.0

-- | Add a title to a chart. The logic used to work out placement is flawed due to being able to freely specify text rotation.  It works for specific rotations (Top, Bottom at 0, Left at 90, Right @ 270)
title :: (Monad m, Chartable a) => Title a -> DrawAttributes -> HudT m a
title t das = Hud $ \cs -> do
  (ViewBox ca) <- use #chartDim
  let c = title_ t das ca
  #chartDim .= (ViewBox $ ca <> styleBox c)
  pure $ c:cs

-- writeFile "t1.svg" (renderChartWith (defaultChartSvgStyle & #sizex .~ 400 & #sizey .~ 400) (defaultHudConfig & #hudAxes .~ [AxisConfig (Just defaultBar) Nothing ((defaultTick :: Tick Double) & #tstyle .~ (TickRound TickFormatDefault 3 TickExtend :: TickStyle Double)) PlaceBottom]) [Chart BlankA mempty [SpotArea (Area 0 2.2 0 2.5)]])

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

defaultTick :: (FromRational a) => Tick a
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

toFormat :: (Tickable a) => TickFormat -> [a] -> [P.Text]
toFormat TickFormatDefault = precision commas 0
toFormat (TickFormatCommas n) = precision commas n
toFormat (TickFormatFixed n) = precision (fixed n) n
toFormat TickFormatDollars = fmap ("$" <>) . precision commas 2

fromFormat :: TickFormat -> (Text, Int)
fromFormat TickFormatDefault = ("default", 0)
fromFormat (TickFormatCommas n) = ("commas", n)
fromFormat (TickFormatFixed n) = ("fixed", n)
fromFormat TickFormatDollars = ("dollars", 2)

fromSN :: (Text, Int) -> TickFormat
fromSN ("default", _) = TickFormatDefault
fromSN ("commas", n) = TickFormatCommas n
fromSN ("fixed", n) = TickFormatFixed n
fromSN ("dollars", _) = TickFormatDollars
fromSN _ = TickFormatDefault

-- | Style of tick marks on an axis.
data TickStyle a
  = TickNone -- ^ no ticks on axis
  | TickLabels [P.Text] -- ^ specific labels (equidistant placement)
  | TickRound TickFormat Int TickExtend -- ^ sensibly rounded ticks, a guide to how many, and whether to extend beyond the data bounding box
  | TickExact TickFormat Int -- ^ exactly n equally spaced ticks
  | TickPlaced [(a, P.Text)] -- ^ specific labels and placement
  deriving (Show, Eq, Generic)

defaultTickStyle :: TickStyle a
defaultTickStyle = TickRound TickFormatDefault 8 TickExtend

tickStyleText :: TickStyle a -> Text
tickStyleText TickNone = "TickNone"
tickStyleText TickLabels{} = "TickLabels"
tickStyleText TickRound{} = "TickRound"
tickStyleText TickExact{} = "TickExact"
tickStyleText TickPlaced{} = "TickPlaced"

getTickN :: TickStyle a -> Int
getTickN (TickRound _ n _) = n
getTickN (TickExact _ n) = n
getTickN TickNone = 0
getTickN (TickLabels xs) = length xs
getTickN (TickPlaced xs) = length xs

getTickFormat :: TickStyle Double -> TickFormat
getTickFormat (TickRound tf _ _) = tf
getTickFormat (TickExact tf _) = tf
getTickFormat _ = TickFormatDefault

data TickExtend = TickExtend | NoTickExtend deriving (Eq, Show, Generic)

-- | Provide formatted text for a list of numbers so that they are just distinguished.  'precision commas 2 ticks' means give the tick labels as much precision as is needed for them to be distinguished, but with at least 2 significant figures, and format Integers with commas.
precision :: (FromInteger a, FromRational a, QuotientField a Integer, RealFloat a) => Format P.Text (Integer -> P.Text) -> Int -> [a] -> [P.Text]
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

type Tickable a = (Chartable a, FromIntegral a Integer, FromRational a, Epsilon a, QuotientField a Integer, ExpField a, RealFloat a, FromInteger a)

placePos :: (Chartable a) => Place a -> a -> Area a -> Point a
placePos pl b (Area x z y w) = case pl of
  PlaceTop -> Point zero (w + b)
  PlaceBottom -> Point zero (y - b)
  PlaceLeft -> Point (x - b) zero
  PlaceRight -> Point (z + b) zero
  PlaceAbsolute p -> p

textPos :: (Chartable a) => Place a -> TextStyle -> a -> Point a
textPos pl tt b = case pl of
  PlaceTop -> Point zero b
  PlaceBottom -> Point zero (-b + -0.5 * fromRational' (tt ^. #vsize) * fromRational' (tt ^. #size))
  PlaceLeft -> Point (-b)
    (fromRational' (tt ^. #nudge1) * fromRational' (tt ^. #vsize) * fromRational' (tt ^. #size))
  PlaceRight -> Point b
    (fromRational' (tt ^. #nudge1) * fromRational' (tt ^. #vsize) * fromRational' (tt ^. #size))
  PlaceAbsolute p -> p

placeRot :: (Chartable a) => Place a -> a
placeRot pl = case pl of
  PlaceRight -> -90.0
  PlaceLeft -> -90.0
  _ -> zero

placeRange :: Place a -> Area a -> Range a
placeRange pl (Area x z y w) = case pl of
  PlaceRight -> Range y w
  PlaceLeft -> Range y w
  _ -> Range x z

placeOrigin :: (Chartable a) => Place a -> a -> Point a
placeOrigin pl x
  | pl `elem` [PlaceTop, PlaceBottom] = Point x zero
  | otherwise = Point zero x

placeTextAnchor :: (Chartable a) => Place a -> (TextStyle -> TextStyle)
placeTextAnchor pl
  | pl `elem` [PlaceLeft] = #anchor .~ AnchorEnd
  | pl `elem` [PlaceRight] = #anchor .~ AnchorStart
  | otherwise = id

placeGridLines :: (Chartable a) => Place a -> Area a -> a -> a -> [Spot a]
placeGridLines pl (Area x z y w) a b
  | pl `elem` [PlaceTop, PlaceBottom] = [SP a (y - b), SP a (w + b)]
  | otherwise = [SP (x - b) a, SP (z + b) a]

placeGridLines' :: (Chartable a) => Place a -> Area a -> a -> [Spot a]
placeGridLines' pl (Area x z y w) a
  | pl `elem` [PlaceTop, PlaceBottom] = [SP a y, SP a w]
  | otherwise = [SP x a, SP z a]

-- | compute tick values and labels given options, ranges and formatting
ticksR :: (Tickable a) => TickStyle a -> Range a -> Range a -> [(a, Text)]
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

ticksA :: (Tickable a) => TickStyle a -> Place a -> Area a -> Area a -> [(a, Text)]
ticksA ts pl d xs = ticksR ts (placeRange pl d) (placeRange pl xs)

tickGlyph_ :: (Tickable a) => Place a -> (GlyphStyle, a) -> TickStyle a -> DrawAttributes -> Area a -> Area a -> Area a -> Chart a
tickGlyph_ pl (g,b) ts das a d xs =
   Chart (GlyphA g)
    (das <> translateDA (placePos pl b a) <> rotateDA (placeRot pl))
    (SpotPoint <$> (\x -> Point x 0) <$> fst <$> ticksA ts pl d xs)

tickGlyph :: (Monad m, Tickable a) =>
  Place a -> (GlyphStyle, a) -> TickStyle a -> DrawAttributes -> HudT m a
tickGlyph pl (g, b) ts das = Hud $ \cs -> do
  (ViewBox a) <- use #chartDim
  (ViewBox d) <- use #canvasDim
  xs <- use #dataArea
  let c = tickGlyph_ pl (g,b) ts das a d xs
  #chartDim .= (ViewBox $ a <> styleBox c)
  pure $ c:cs


-- FIXME: remove das logic and create a single chart
tickText_ :: (Tickable a) => Place a -> (TextStyle, a) -> TickStyle a -> DrawAttributes ->
  Area a -> Area a -> Area a -> [Chart a]
tickText_ pl (txts, b) ts das ca da xs =
  zipWith
        (\txt sp ->
                Chart (TextA (placeTextAnchor pl txts) [txt])
               (das <> translateDA (placePos pl b ca + textPos pl txts b)) [SpotPoint sp])
             (snd <$> ticksA ts pl da xs)
             (placeOrigin pl <$> fst <$> ticksA ts pl da xs)

tickText :: (Monad m, Tickable a) =>
  Place a -> (TextStyle, a) -> TickStyle a -> DrawAttributes -> HudT m a
tickText pl (txts, b) ts das = Hud $ \cs -> do
  (ViewBox ca) <- use #chartDim
  (ViewBox da) <- use #canvasDim
  xs <- use #dataArea
  let c = tickText_ pl (txts, b) ts das ca da xs
  #chartDim .= (ViewBox $ ca <> styleBoxes c)
  pure $ c <> cs

tickLine :: (Monad m, Tickable a) =>
  Place a -> (LineStyle, a) -> TickStyle a -> DrawAttributes -> HudT m a
tickLine pl (ls, b) ts das = Hud $ \cs -> do
  (ViewBox ca) <- use #chartDim
  (ViewBox da) <- use #canvasDim
  xs <- use #dataArea
  let c = Chart (LineA ls) das <$> (\x -> placeGridLines pl da x b) <$> fst <$> ticksA ts pl da xs
  #chartDim .= (ViewBox $ ca <> styleBoxes c)
  pure $ c <> cs

-- | Create tick glyphs (marks), lines (grid) and text (labels)
tick :: (Monad m, Tickable a) =>
  Place a -> Tick a -> DrawAttributes -> HudT m a
tick pl t das =
  maybe mempty (\x -> tickGlyph pl x (t ^. #tstyle) das) (t ^. #gtick) <>
  maybe mempty (\x -> tickText pl x (t ^. #tstyle) das) (t ^. #ttick) <>
  maybe mempty (\x -> tickLine pl x (t ^. #tstyle) das) (t ^. #ltick) <>
  extendData pl t

-- | compute an extension to the Range if a tick went over the data bounding box
computeTickExtension :: (Epsilon a, FromInteger a, RealFloat a, ExpField a, QuotientField a Integer, Chartable a) => TickStyle a -> Range a -> Maybe (Range a)
computeTickExtension s r =
    case s of
      TickNone -> Nothing
      TickRound _ n e -> bool Nothing (Just (space1 ticks0 <> r)) (e == TickExtend)
        where ticks0 = gridSensible OuterPos (e == NoTickExtend) r (fromIntegral n :: Integer)
      TickExact _ _ -> Nothing
      TickLabels _ -> Nothing
      TickPlaced xs -> Just $ r <> space1 (fst <$> xs)

-- | Create a style extension for the data, if ticks extend beyind the existing range
tickExtended :: (Tickable a) =>
  Place a -> Tick a -> Area a -> Area a
tickExtended pl t xs =
  maybe xs (\x -> rangeext xs x)
  (computeTickExtension (t ^. #tstyle) (ranged xs))
  where
    ranged xs' = case pl of
      PlaceTop -> rangex xs'
      PlaceBottom -> rangex xs'
      PlaceLeft -> rangey xs'
      PlaceRight -> rangey xs'
    rangex (Area x z _ _) = Range x z
    rangey (Area _ _ y w) = Range y w
    rangeext (Area x z y w) (Range a0 a1) = case pl of
      PlaceTop -> Area a0 a1 y w
      PlaceBottom -> Area a0 a1 y w
      PlaceLeft -> Area x z a0 a1
      PlaceRight -> Area x z a0 a1

extendData :: (Monad m, Tickable a) => Place a -> Tick a -> HudT m a
extendData pl t = Hud $ \cs -> do
  #dataArea %= tickExtended pl t
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
adjustTick :: (Tickable a) => Adjustments -> ViewBox a -> Area a -> Place a -> DrawAttributes ->
  Tick a -> Tick a
adjustTick (Adjustments mrx ma mry ad) vb cs pl das t
  | pl `elem` [PlaceBottom, PlaceTop] = case ad of
      False -> t & #ttick . _Just . _1 . #size %~ (/adjustSizeX)
      True ->
        case adjustSizeX > one of
          True ->
            (case pl of
                PlaceBottom -> #ttick . _Just . _1 . #anchor .~ AnchorEnd
                PlaceTop -> #ttick . _Just . _1 . #anchor .~ AnchorStart
                _ -> #ttick . _Just . _1 . #anchor .~ AnchorEnd) $
             (#ttick . _Just . _1 . #size %~ (/adjustSizeA)) $
             (#ttick . _Just . _1 . #rotation .~ Just (-45)) t
          False -> (#ttick . _Just . _1 . #size %~ (/adjustSizeA)) t
  | pl `elem` [PlaceLeft, PlaceRight] =
    (#ttick . _Just . _1 . #size %~ (/adjustSizeY)) t
  where
    ra (Area x z y w)
      | pl `elem` [PlaceTop, PlaceBottom] = Range x z
      | otherwise = Range y w
    asp = ra (vbArea vb)
    r = ra cs
    tickl = snd <$> ticksR (t ^. #tstyle) asp r
    maxWidth :: Double
    maxWidth =
          maybe one (\tt -> maybe one id $ maximum' $
          (\(Area x z _ _) -> z - x)
              <$> styleBoxText (fst tt) das <$> tickl) (t ^. #ttick)
    maxHeight =
      maybe one 
          (\tt -> maybe one id $ maximum' $
          (\(Area _ _ y w) -> w - y)
              <$> styleBoxText (fst tt) das <$> tickl) (t ^. #ttick)
    adjustSizeX :: Double
    adjustSizeX =
      maybe one identity (maximum' [(maxWidth / fromRational' (upper asp - lower asp)) / mrx, one])
    adjustSizeY =
      maybe one identity (maximum' [(maxHeight / fromRational' (upper asp - lower asp)) / mry, one])
    adjustSizeA =
      maybe one identity (maximum' [(maxHeight / fromRational' (upper asp - lower asp)) / ma, one])

adjustedTickHud :: (Monad m) => AxisConfig Double -> HudT m Double
adjustedTickHud c = Hud $ \cs -> do
  vb <- use #chartDim
  xs <- use #dataArea
  let adjTick =
        maybe
        (c ^. #atick)
        (\x -> adjustTick x vb xs (c ^. #place) mempty (c ^. #atick))
        (c ^. #adjust)
  unhud (tick (c ^. #place) adjTick mempty) cs

data AxisConfig a = AxisConfig
  { abar :: Maybe (Bar a)
  , adjust :: Maybe Adjustments
  , atick :: Tick a
  , place :: Place Double
  } deriving (Eq, Show, Generic)

defaultAxisConfig :: AxisConfig Double
defaultAxisConfig = AxisConfig (Just defaultBar) (Just defaultAdjustments) defaultTick PlaceBottom

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

defaultCanvas :: RectStyle
defaultCanvas = blob grey 0.03

-- the complexity here is due to gridSensible, which is not idempotent.  We have to remember the tick calculation tyhat extends the data area, because reapplying TickRound etc creates a new set of ticks different to the original.
hudSvgWith :: (Chartable a) => Area a -> Area a -> [Hud a] -> [Chart a] -> ChartSvg a
hudSvgWith ca xs hs cs = flip evalState (ChartDims (ViewBox ca') (ViewBox da') xs') $ do
  cs'' <- (unhud $ mconcat hs) cs'
  cd <- use #chartDim
  pure $ chartSvg_ cd cs''
  where
    xs' = dataAreaNoSingleton one cs
    da' = dataAreaDef one cs'
    ca' = styleBoxes cs'
    cs' = projectSpotsWith ca xs cs

hudSvg :: (Chartable a) =>
  ViewBox a -> [[Hud a]] -> [Chart a] -> ChartSvg a
hudSvg (ViewBox ca) hss cs =
  hudSvgWith ca (dataAreaDef one cs) (mconcat <$> hss) cs

hudChartWith :: (Chartable a) => Area a -> Area a -> [Hud a] -> [Chart a] -> [Chart a]
hudChartWith ca xs hs cs = flip evalState (ChartDims (ViewBox ca') (ViewBox da') xs') $ do
  cs'' <- (unhud $ mconcat hs) cs'
  pure cs''
  where
    xs' = dataAreaNoSingleton one cs
    da' = dataAreaDef one cs'
    ca' = styleBoxes cs'
    cs' = projectSpotsWith ca xs cs

hudChart :: (Chartable a) => Area a -> [Hud a] -> [Chart a] -> [Chart a]
hudChart ca hs cs = flip evalState (ChartDims (ViewBox ca') (ViewBox da') xs') $ do
  cs'' <- (unhud $ mconcat hs) cs'
  pure cs''
  where
    xs' = dataAreaNoSingleton one cs
    da' = dataAreaDef one cs'
    ca' = styleBoxes cs'
    cs' = projectSpotsWith ca xs' cs

dataAreaExtended :: [Chart Double] -> [AxisConfig Double] -> Area Double
dataAreaExtended cs axes =
      dataAreaDef one cs <>
      mconcat
      ((\a -> tickExtended (a ^. #place) (a ^. #atick) (dataAreaDef one cs)) <$>
        axes)


hudAndChart :: HudConfig -> ViewBox Double -> [Chart Double] -> [Chart Double]
hudAndChart cfg (ViewBox ca) cs =
  hudChartWith ca (dataAreaDef one (cs <> cs')) hs (cs <> cs')
  where
    (hs, cs') = hudsWithExtend (dataAreaNoSingleton one cs) cfg


-- writeFile "t1.svg" (renderChartWith (defaultChartSvgStyle & #sizex .~ 400 & #sizey .~ 400) (defaultHudConfig & #hudAxes .~ [AxisConfig (Just defaultBar) Nothing ((defaultTick :: Tick Double) & #tstyle .~ (TickRound TickFormatDefault 3 TickExtend :: TickStyle Double)) PlaceBottom, AxisConfig (Just defaultBar) Nothing ((defaultTick :: Tick Double) & #tstyle .~ (TickRound TickFormatDefault 2 TickExtend :: TickStyle Double)) PlaceLeft]) [Chart (RectA defaultRectStyle) mempty [SpotArea (Area 0.6 2.2 0.3 2.4)]])
hud :: HudConfig -> ViewBox Double -> [Chart Double] -> ChartSvg Double
hud cfg (ViewBox ca) cs =
  hudSvgWith ca (dataAreaDef one (cs <> cs')) hs (cs <> cs')
  where
    (hs, cs') = hudsWithExtend (dataAreaNoSingleton one cs) cfg

hud' :: HudConfig -> ViewBox Double -> [Chart Double] -> ChartSvg Double
hud' cfg (ViewBox ca) cs =
  hudSvgWith ca (dataAreaNoSingleton one cs) (mconcat $ hudsWith cfg) cs


hudsWith :: HudConfig -> [[Hud Double]]
hudsWith cfg =
  [axes] <> [[can]] <> ((:[]) <$> titles)
  where
    can = maybe mempty (\x -> canvas x mempty) (cfg ^. #hudCanvas)
    titles = (`title` mempty) <$> (cfg ^. #hudTitles)
    axes = (\x ->
             maybe mempty (\a -> bar (x ^. #place) a mempty) (x ^. #abar) <>
             adjustedTickHud x) <$>
           (cfg ^. #hudAxes)

hudsWithExtend :: Area Double -> HudConfig -> ([Hud Double], [Chart Double])
hudsWithExtend xs cfg =
  (haxes <> [can] <> titles <> ls, [xsext])
  where
    can = maybe mempty (\x -> canvas x mempty) (cfg ^. #hudCanvas)
    titles = (`title` mempty) <$> (cfg ^. #hudTitles)
    -- ticks' = adjustedTickHud <$> (cfg ^. #hudAxes)
    axes = xyzzy xs <$> cfg ^. #hudAxes
    axes' = fst <$> axes
    xsext = Chart BlankA mempty (SpotArea <$> catMaybes (snd <$> axes))
    haxes = (\x ->
             maybe mempty (\a -> bar (x ^. #place) a mempty) (x ^. #abar) <>
             adjustedTickHud x) <$> axes'
    ls = legend <$> (cfg ^. #hudLegends)

xyzzy :: Area Double -> AxisConfig Double -> (AxisConfig Double, Maybe (Area Double))
xyzzy xs c = maybe (c, Nothing) (\x -> (c & #atick . #tstyle .~ ts', Just x)) ma
  where
    (ts', ma) = freezeTicks (c ^. #place) xs (c ^. #atick . #tstyle)

-- | convert TickRound to TickPlaced
freezeTicks :: (Tickable a) => Place a -> Area a -> TickStyle a -> (TickStyle a, Maybe (Area a))
freezeTicks pl xs ts@TickRound{} = maybe (ts, Nothing) (\x -> (TickPlaced ta, Just x)) ma
  where
    (ta, ma) = ticksA' ts pl xs
freezeTicks _ _ ts = (ts, Nothing)

-- | compute tick values and labels given options, ranges and formatting
ticksR' :: (Tickable a) => TickStyle a -> Range a -> ([(a, Text)], Maybe (Range a))
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

ticksA' :: (Tickable a) => TickStyle a -> Place a -> Area a -> ([(a, Text)], Maybe (Area a))
ticksA' ts pl xs = (tr, maybe Nothing (\x -> Just $ replaceRange pl x xs) ma)
  where
    (tr, ma) = ticksR' ts (placeRange pl xs)

replaceRange :: Place a -> Range a -> Area a -> Area a
replaceRange pl (Range a0 a1) (Area x z y w) = case pl of
  PlaceRight -> Area x z a0 a1
  PlaceLeft -> Area x z a0 a1
  _ -> Area a0 a1 y w

renderChartWith :: ChartSvgStyle -> HudConfig -> [Chart Double] -> Text
renderChartWith scfg hcfg cs =
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

defaultLegendOptions :: (FromRational a) => LegendOptions a
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

legend :: (Chartable a, Tickable a, FromInteger a) => LegendOptions a -> Hud a
legend l = Hud $ \cs -> do
  (ViewBox ca) <- use #chartDim
  let cs' = cs <> movedleg ca (scaledleg ca)
  #chartDim .= ViewBox (styleBoxes cs')
  pure cs'
    where
      scaledleg ca' =
        (#annotation %~ scaleAnn (fromRational' $ l ^. #scale)) <$>
        projectSpots (fmap (* l ^. #scale) ca') (makeLegend l)
      movedleg ca' leg = move (SpotPoint $ placel (l ^. #lplace) ca' (styleBoxes leg)) leg
      placel pl (Area x z y w) (Area x' z' y' w') =
        case pl of
          PlaceTop -> Point ((x+z)/2.0) (w + (w' - y')/2.0)
          PlaceBottom -> Point ((x+z)/2.0) (y - (w' - y'/2.0))
          PlaceLeft -> Point (x - (z' - x')/2.0) ((y+w)/2.0)
          PlaceRight -> Point (z + (z' - x')/2.0) ((y+w)/2.0)
          PlaceAbsolute p -> p

legendEntry :: (Chartable a, FromInteger a) =>
  LegendOptions a -> Annotation -> Text -> (Chart a, Chart a)
legendEntry l a t =
  ( Chart ann mempty sps
  , Chart (TextA (l ^. #ltext & #anchor .~ AnchorStart) [t]) mempty [SP 0 0]
  )
  where
    (ann, sps) = case a of
      RectA rs ->
        ( RectA rs
        , [SA 0 (l ^. #lsize) 0 (l ^. #lsize)]
        )
      TextA ts txts ->
        ( TextA (ts & #size .~ fromRational' (l ^. #lsize)) (take 1 txts)
        , [SP 0 0]
        )
      GlyphA gs ->
        ( GlyphA (gs & #size .~ fromRational' (l ^. #lsize))
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
    step x a = x <> (a & fmap (#spots %~ fmap (\s -> SP (z x) zero + s)))
    z [] = 0.0
    z xs = (\(Area _ z' _ _) -> z' + gap) $ styleBoxes xs

-- vertically stack a list of charts (proceeding upwards)
vert :: Chartable a => a -> [[Chart a]] -> [Chart a]
vert _ [] = []
vert gap cs = foldl step [] cs
  where
    step x a = x <> (a & fmap (#spots %~ fmap (\s -> SP zero (w x) + s)))
    w [] = 0.0
    w xs = (\(Area _ _ _ w') -> w' + gap) $ styleBoxes xs

move :: Chartable a => Spot a -> [Chart a] -> [Chart a]
move sp cs = fmap (#spots %~ fmap (sp+)) cs

l1 :: [(Annotation, Text)]
l1 = [ (GlyphA defaultGlyphStyle, "glyph")
     , (RectA defaultRectStyle, "rect")
     , (TextA (defaultTextStyle & #anchor .~ AnchorStart) ["content"], "text")
     , (LineA defaultLineStyle, "line")
     , (BlankA, "blank")
     ]

makeLegend :: (Chartable a, FromInteger a) => LegendOptions a -> [Chart a]
makeLegend l = cs'
  where
    es = reverse $ (\(a,t) -> legendEntry l a t) <$> (l ^. #lcharts)
    twidth = (\(Area _ z _ _) -> z) $ fold $ styleBox <$> snd <$> es
    as = move (SP (twidth * l ^. #lsize) 0.0) (fst <$> es)
    hs = zipWith (\a t -> hori (l ^. #vgap) [[t], [a]]) as (snd <$> es)
    vs = vert (l ^. #hgap) hs
    cs =
      vs <>
      maybe mempty (\x -> [Chart (RectA x) mempty
              [SpotArea (widenProp (maybe 1 id
                                    (fromRational' <$> l ^. #innerPad)) (styleBoxes vs))]])
         (l ^. #legendFrame)
    cs' = cs <> [Chart BlankA mempty
                 [SpotArea $ widenProp (maybe 1 id
                                        (fromRational' <$> l ^. #outerPad)) (styleBoxes cs)]]




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
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
import Graphics.Svg.Types as Svg hiding (Point, Text)
import Lucid.Base
import NumHask.Prelude as P hiding (Group)

maximum' :: (Ord a) => [a] -> Maybe a
maximum' [] = Nothing
maximum' xs = Just $ maximum xs

newtype Hud a = Hud { unhud :: ViewBox a -> ViewBox a -> Area a -> [Chart a] }

instance Semigroup (Hud a) where
  (<>) (Hud h1) (Hud h2) = Hud $ \vb s d -> h1 vb s d <> h2 vb s d

instance Monoid (Hud a) where
  mempty = Hud (\_ _ _ -> [])

layer :: Chartable a => Hud a -> Hud a -> Hud a
layer (Hud h1) (Hud h2) =
  Hud $ \vb@(ViewBox asp) s d ->
          h1 vb s d <>
          h2 (ViewBox $ asp <> styleBoxes (h1 vb s d)) s d

hudSvg :: (ToRatio a, FromRatio a, Subtractive a, Field a, BoundedLattice a) =>
  ViewBox a -> [Hud a] -> [Chart a] -> ChartSvg a
hudSvg (ViewBox asp) hs cs =
  chartSvg_
  (ViewBox (styleBoxes (cs' <> h vbStyle vbData xs)))
  (cs' <> h vbStyle vbData xs)
  where
    (Hud h) = foldl layer mempty hs
    cs' = projectSpots asp cs
    vbStyle = ViewBox $ styleBoxes cs'
    vbData = ViewBox $ toArea $ fold $ fold (spots <$> cs')
    xs = toArea $ fold $ fold (spots <$> cs)

hudSvgWith :: (ToRatio a, FromRatio a, Subtractive a, Field a, BoundedLattice a) =>
  ViewBox a -> Area a -> [Hud a] -> [Chart a] -> ChartSvg a
hudSvgWith vb@(ViewBox new) old hs cs =
  chartSvg_
  (ViewBox (new <> styleBoxes (cs' <> h vbStyle vb old)))
  (cs' <> h vbStyle vb old)
  where
    (Hud h) = foldl layer mempty hs
    cs' = projectSpotsWith new old cs
    vbStyle = ViewBox $ new <> styleBoxes cs'

-- | Orientation for an element.  Watch this space for curvature!
data Orientation
  = Hori
  | Vert
  deriving (Show, Eq, Generic)

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

toPlace :: (Eq a, IsString a) => a -> Place b
toPlace sh =
  case sh of
    "Top" -> PlaceTop
    "Bottom" -> PlaceBottom
    "Left" -> PlaceLeft
    "Right" -> PlaceRight
    _ -> PlaceTop

fromPlace :: (IsString a) => Place b -> a
fromPlace p =
  case p of
    PlaceTop -> "Top"
    PlaceBottom -> "Bottom"
    PlaceLeft -> "Left"
    PlaceRight -> "Right"
    _ -> "wtf"

data Bar a = Bar
  { place :: Place a
  , rstyle :: RectStyle
  , wid :: a
  , buff :: a
  } deriving (Show, Eq, Generic)

defaultBar :: (FromRatio a) => Bar a
defaultBar = Bar PlaceBottom defaultRectStyle 0.02 0.02

canvas :: RectStyle -> DrawAttributes -> Hud a
canvas s das = Hud $ \(ViewBox vb) _ _ -> [Chart (RectA s) das [SpotArea vb]]

bar :: (Chartable a) => Bar a -> DrawAttributes -> Hud a
bar b das = Hud $ \(ViewBox (Area x' z' y' w')) (ViewBox (Area x z y w)) _ -> (:[]) $ case b ^. #place of
  PlaceTop ->
    Chart (RectA (rstyle b)) das
    [SA x z
     (w' + b ^. #buff)
     (w' + b ^. #buff + b ^. #wid)
    ]
  PlaceBottom -> Chart (RectA (rstyle b)) das
    [SA x z
     (y' - b ^. #wid - b ^. #buff)
     (y' - b ^. #buff)]
  PlaceLeft -> Chart (RectA (rstyle b)) das
    [SA (x' - b ^. #wid - b ^. #buff)
     (x' - b ^. #buff) y w]
  PlaceRight -> Chart (RectA (rstyle b)) das
    [SA (z' + (b ^. #buff))
     (z' + (b ^. #buff) + (b ^. #wid)) y w]
  PlaceAbsolute (Point x'' _) -> Chart (RectA (rstyle b)) das
    [SA (x'' + (b ^. #buff))
     (x'' + (b ^. #buff) + (b ^. #wid)) y w]

bars :: (Chartable a) => [Bar a] -> DrawAttributes -> Hud a
bars bs das = mconcat ((\b -> bar b das) <$> bs)

instance ToHtml TextAnchor where
  toHtml = toHtml . (show :: TextAnchor -> Text)
  toHtmlRaw = toHtmlRaw . (show :: TextAnchor -> Text)

toTextAnchor :: (Eq a, IsString a) => a -> TextAnchor
toTextAnchor sh =
  case sh of
    "Start" -> TextAnchorStart
    "Middle" -> TextAnchorMiddle
    "End" -> TextAnchorEnd
    _ -> TextAnchorMiddle

fromTextAnchor :: (IsString a) => TextAnchor -> a
fromTextAnchor p =
  case p of
    TextAnchorStart -> "Top"
    TextAnchorMiddle -> "Middle"
    TextAnchorEnd -> "End"

-- | Options for titles.  Defaults to center aligned, and placed at Top of the hud
data Title a = Title
  { text :: P.Text
  , style :: TextStyle
  , place :: Place a
  , align :: TextAnchor
  , buff :: a
  } deriving (Show, Eq, Generic)

defaultTitle :: (Chartable a) => P.Text -> Title a
defaultTitle txt =
    Title
    txt
    ( #size .~ 0.12 $
      #color .~ PixelRGB8 0 0 0 $
      defaultTextStyle)
    PlaceTop
    TextAnchorMiddle
    0.04

-- | Create a title for a chart. The logic used to work out placement is flawed due to being able to freely specify text rotation.  It works for specific rotations (Top, Bottom at 0, Left at 90, Right @ 270)
title :: (Chartable a, FromInteger a) => Title a -> DrawAttributes -> Hud a
title t das =
  Hud $ \(ViewBox a) _ _ -> (:[]) $
    Chart (TextA style' [t ^. #text])
    (das <> translateDA (placePos a + alignPos a) <> rotateDA (rot :: Double)) [zero]
      where
        style'
          | t ^. #align == TextAnchorStart =
            #alignH .~ TextAnchorStart $ t ^. #style
          | t ^. #align == TextAnchorEnd =
            #alignH .~ TextAnchorEnd $ t ^. #style
          | otherwise = t ^. #style
        rot
          | t ^. #place == PlaceRight = 90.0
          | t ^. #place == PlaceLeft = -90.0
          | otherwise = 0
        placePos (Area x z y w) = case t ^. #place of
          PlaceTop -> Point ((x+z)/2.0) (w + (t ^. #buff))
          PlaceBottom -> Point ((x+z)/2.0)
            (y - (t ^. #buff) -
              0.5 * fromRational (t ^. #style ^. #vsize) *
              fromRational (t ^. #style ^. #size))
          PlaceLeft -> Point (x - (t ^. #buff)) ((y+w)/2.0)
          PlaceRight -> Point (z + (t ^. #buff)) ((y+w)/2.0)
          PlaceAbsolute p -> p
        alignPos (Area x z y w)
          | t ^. #align == TextAnchorStart &&
            t ^. #place `elem` [PlaceTop, PlaceBottom] =
            Point ((x-z)/2.0) 0
          | t ^. #align == TextAnchorStart &&
            t ^. #place `elem` [PlaceLeft] =
            Point 0 ((y-w)/2.0)
          | t ^. #align == TextAnchorStart &&
            t ^. #place `elem` [PlaceRight] =
            Point 0 ((w-y)/2.0)
          | t ^. #align == TextAnchorEnd &&
            t ^. #place `elem` [PlaceTop, PlaceBottom] =
            Point ((-x+z)/2.0) 0
          | t ^. #align == TextAnchorEnd &&
            t ^. #place `elem` [PlaceLeft] =
            Point 0 ((-y+w)/2.0)
          | t ^. #align == TextAnchorEnd &&
            t ^. #place `elem` [PlaceRight] =
            Point 0 ((y-w)/2.0)
          | otherwise = Point 0 0

data Tick a = Tick
  { place :: Place a
  , gstyle :: GlyphStyle
  , textStyle :: TextStyle
  , buff :: a
  , textBuff :: a
  , tstyle :: TickStyle a
  } deriving (Show, Eq, Generic)

defaultTick :: (Chartable a) => Tick a
defaultTick = Tick PlaceBottom (#borderOpacity .~ 1 $ #borderColor .~ grey $ #shape .~ VLineGlyph 0.005 $ defaultGlyphStyle) (#size .~ 0.05 $ defaultTextStyle) 0.03 0.05 (TickRound (TickFormatDefault, 8))

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

fromFormat :: IsString a => TickFormat -> (a, Int)
fromFormat TickFormatDefault = ("default", 0)
fromFormat (TickFormatCommas n) = ("commas", n)
fromFormat (TickFormatFixed n) = ("fixed", n)
fromFormat TickFormatDollars = ("dollars", 2)

fromSN :: (IsString a, Eq a) => (a, Int) -> TickFormat
fromSN ("default", _) = TickFormatDefault
fromSN ("commas", n) = TickFormatCommas n
fromSN ("fixed", n) = TickFormatFixed n
fromSN ("dollars", _) = TickFormatDollars
fromSN _ = TickFormatDefault

-- | Style of tick marks on an axis.
data TickStyle a
  = TickNone -- ^ no ticks on axis
  | TickLabels [P.Text] -- ^ specific labels
  | TickRound (TickFormat, Int) -- ^ sensibly rounded ticks and a guide to how many
  | TickExact (TickFormat, Int) -- ^ exactly n equally spaced ticks
  | TickPlaced [(a, P.Text)] -- ^ specific labels and placement
  deriving (Show, Eq, Generic)

tickStyleText :: TickStyle a -> Text
tickStyleText TickNone = "TickNone"
tickStyleText TickLabels{} = "TickLabels"
tickStyleText TickRound{} = "TickRound"
tickStyleText TickExact{} = "TickExact"
tickStyleText TickPlaced{} = "TickPlaced"

getTickN :: TickStyle a -> Int
getTickN (TickRound (_, n)) = n
getTickN (TickExact (_, n)) = n
getTickN TickNone = 0
getTickN (TickLabels xs) = length xs
getTickN (TickPlaced xs) = length xs

getTickFormat :: TickStyle Double -> TickFormat
getTickFormat (TickRound (tf, _)) = tf
getTickFormat (TickExact (tf, _)) = tf
getTickFormat _ = TickFormatDefault

-- | compute tick values and labels given options, ranges and formatting
computeTicks :: (Epsilon a, RealFloat a, ExpField a, QuotientField a Integer, FromInteger a, Chartable a) => TickStyle a -> Range a -> Range a -> ([a], [P.Text])
computeTicks s asp r =
    case s of
      TickNone -> ([], [])
      TickRound (f, n) -> (project r asp <$> ticks0, toFormat f ticks0)
        where ticks0 = gridSensible OuterPos True r (fromIntegral n) 
      TickExact (f, n) -> (project r asp <$> ticks0, toFormat f ticks0)
        where ticks0 = grid OuterPos r n
      TickLabels ls ->
          ( project (Range 0 (fromIntegral $ length ls)) asp <$>
            ((\x -> x - 0.5) . fromIntegral <$> [1 .. length ls])
          , ls)
      TickPlaced xs -> (project r asp . fst <$> xs, snd <$> xs)

-- | Provide formatted text for a list of numbers so that they are just distinguished.  'precision commas 2 ticks' means give the tick labels as much precision as is needed for them to be distinguished, but with at least 2 significant figures, and format Integers with commas.
precision :: (FromInteger a, FromRatio a, QuotientField a Integer, RealFloat a) => Format P.Text (Integer -> P.Text) -> Int -> [a] -> [P.Text]
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

type Tickable a = (Chartable a, Epsilon a, QuotientField a Integer, ExpField a, RealFloat a, FromInteger a)

-- | Create tick labels and marks for an axis
tick :: (Tickable a) =>
  Tick a -> DrawAttributes -> Hud a
tick t das =
  Hud $ \(ViewBox a) (ViewBox d) xs ->
  [ Chart (GlyphA (t ^. #gstyle))
    (das <> translateDA (placePos a) <> rotateDA (rot :: Double))
    (SpotPoint <$> ps' (ra d) (ra xs))
  ] <>
  zipWith
  (\txt sp ->
      Chart (TextA (ta (t ^. #textStyle)) [txt])
       (das <> translateDA (placePos a + textPos)) [SpotPoint sp])
  (snd (ts (ra d) (ra xs)))
  (ps (ra d) (ra xs))
      where
        ps a xs
          | t ^. #place `elem` [PlaceTop, PlaceBottom] =
            (\x -> Point x 0) <$> fst (ts a xs)
          | otherwise =
            (\y -> Point 0 y) <$> fst (ts a xs)
        ps' a xs
          | t ^. #place `elem` [PlaceTop, PlaceBottom] =
            (\x -> Point x 0) <$> fst (ts a xs)
          | otherwise =
            (\x -> Point x 0) <$> fst (ts a xs)
        ra (Area x z y w)
          | t ^. #place `elem` [PlaceTop, PlaceBottom] = Range x z
          | otherwise = Range y w
        ts a xs = computeTicks (t ^. #tstyle) a xs
        rot
          | t ^. #place == PlaceRight = -90.0
          | t ^. #place == PlaceLeft = -90.0
          | otherwise = 0
        placePos (Area x z y w) = case t ^. #place of
          PlaceTop -> Point 0 (w + (t ^. #buff))
          PlaceBottom -> Point 0 (y - (t ^. #buff))
          PlaceLeft -> Point (x - (t ^. #buff)) 0
          PlaceRight -> Point (z + (t ^. #buff)) 0
          PlaceAbsolute p -> p
        textPos = case t ^. #place of
          PlaceTop -> Point 0 (t ^. #buff)
          PlaceBottom -> Point 0 ((-t^. #textBuff) + -0.5 * fromRational (t ^. #textStyle ^. #vsize) * fromRational (t ^. #textStyle ^. #size))
          PlaceLeft -> Point (- (t ^. #textBuff)) (fromRational (t ^. #textStyle ^. #nudge1) * fromRational (t ^. #textStyle ^. #vsize) * fromRational (t ^. #textStyle ^. #size))
          PlaceRight -> Point (t ^. #buff) (fromRational (t ^. #textStyle ^. #nudge1) * fromRational (t ^. #textStyle ^. #vsize) * fromRational (t ^. #textStyle ^. #size))
          PlaceAbsolute p -> p
        ta s = case t ^. #place of
          PlaceBottom -> s
          PlaceTop -> s
          PlaceLeft -> (#alignH .~ TextAnchorEnd :: TextStyle -> TextStyle) s
          PlaceRight -> (#alignH .~ TextAnchorStart :: TextStyle -> TextStyle) s
          PlaceAbsolute _ -> s

-- | options for prettifying axis decorations
data AutoOptions =
  AutoOptions
  { maxXRatio :: Double
  , maxYRatio :: Double
  , angledRatio :: Double
  , allowDiagonal :: Bool
  } deriving (Show, Eq, Generic) 

defaultAutoOptions :: AutoOptions
defaultAutoOptions = AutoOptions 0.08 0.06 0.12 True

-- | adjust Tick for sane font sizes etc
adjustTick :: (Tickable a) => AutoOptions -> ViewBox a -> Area a -> (Tick a, DrawAttributes) -> (Tick a, DrawAttributes)
adjustTick (AutoOptions mrx ma mry ad) vb cs (t, das)
  | t ^. #place `elem` [PlaceBottom, PlaceTop] = case ad of
      False -> ((#textStyle %~ (#size %~ (/adjustSizeX))) t, das)
      True ->
        case adjustSizeX > one of
          True ->
            ((case t ^. #place of
                PlaceBottom -> #textStyle . #alignH .~ TextAnchorEnd
                PlaceTop -> #textStyle . #alignH .~ TextAnchorStart
                _ -> #textStyle . #alignH .~ TextAnchorEnd) $
             (#textStyle . #size %~ (/adjustSizeA)) $
             (#textStyle . #rotation .~ Just (-45)) t, das)
          False -> ((#textStyle . #size %~ (/adjustSizeA)) t, das)
  | t ^. #place `elem` [PlaceLeft, PlaceRight] =
    ((#textStyle . #size %~ (/adjustSizeY)) t, das)
  where
    ra (Area x z y w)
      | t ^. #place `elem` [PlaceTop, PlaceBottom] = Range x z
      | otherwise = Range y w
    asp = ra (vbArea vb)
    r = ra cs
    tickl = snd (computeTicks (t ^. #tstyle) asp r)
    maxWidth =
          maybe one identity (maximum' $
          (\(Area x z _ _) -> z - x)
              <$> styleBoxText (t ^. #textStyle) das <$> tickl)
    maxHeight =
      maybe one identity
          (maximum' $
          (\(Area _ _ y w) -> w - y)
              <$> styleBoxText (t ^. #textStyle) das <$> tickl)
    adjustSizeX = maybe one identity (maximum' [(maxWidth / fromRational (upper asp - lower asp)) / mrx, one])
    adjustSizeY = maybe one identity (maximum' [(maxHeight / fromRational (upper asp - lower asp)) / mry, one])
    adjustSizeA = maybe one identity (maximum' [(maxHeight / fromRational (upper asp - lower asp)) / ma, one])

data CanvasConfig = CanvasConfig
  { color :: PixelRGB8
  , opacity :: Double
  } deriving (Eq, Show, Generic)

defaultCanvasConfig :: CanvasConfig
defaultCanvasConfig  = CanvasConfig grey 0.1

data AxisConfig a = AxisConfig
  { abar :: Maybe (Bar a)
  , hasAuto :: Bool
  , tickN :: Int
  , tickSize :: Double
  , place :: Place Double
  } deriving (Eq, Show, Generic)

defaultAxisConfig :: AxisConfig Double
defaultAxisConfig = AxisConfig (Just defaultBar) True 8 0.1 PlaceBottom

data HudConfig a = HudConfig
  { hudCanvas :: Maybe CanvasConfig
  , hudTitles :: [Title Double]
  , hudAxes ::  [AxisConfig Double]
  , hudAuto :: Bool
  } deriving (Eq, Show, Generic)

defaultHudConfig :: HudConfig Double
defaultHudConfig =
  HudConfig
  (Just defaultCanvasConfig)
  [defaultTitle "defaultHudConfig"]
  [defaultAxisConfig]
  False

hud :: HudConfig Double -> ViewBox Double -> [Chart Double] -> ChartSvg Double
hud cfg vb =
  hudSvg vb ([can] <> titles <> axisBars <> ticks)
  where
    can = maybe mempty (\cfg' -> canvas
      (blob
       (cfg' ^. #color)
       (cfg' ^. #opacity)) mempty)
      (cfg ^. #hudCanvas)
    titles = (`title` mempty) <$> (cfg ^. #hudTitles)
    axisBars = (\x -> maybe mempty (`bar` mempty) (x ^. #abar)) <$> (cfg ^. #hudAxes)
    ts c = (#tstyle .~ TickRound (TickFormatDefault, c ^. #tickN) :: Tick Double -> Tick Double)
      defaultTick
    autoTick c = Hud $ \vb' d a -> let (ts',das') = adjustTick defaultAutoOptions vb' a (ts c, mempty) in let (Hud h) = tick ts' das' in h vb' d a
    ticks = autoTick <$> (cfg ^. #hudAxes)

renderChartWith :: ChartSvgStyle -> HudConfig Double -> [Chart Double] -> Text
renderChartWith scfg hcfg cs =
  renderChartSvg (scfg ^. #sizex) (scfg ^. #sizey) $
  maybe id pad (scfg ^. #outerPad) $
  maybe id (\x c -> frame x c <> c) (scfg ^. #chartFrame) $
  maybe id pad (scfg ^. #innerPad) $
  hud hcfg (aspect (scfg ^. #chartAspect)) $
  cs <>
  maybe mempty (\(s,c) -> [showOriginWith s c]) (scfg ^. #orig)

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

maximum' :: (Ord a) => [a] -> Maybe a
maximum' [] = Nothing
maximum' xs = Just $ maximum xs


-- aspect, style viewbox & raw data area
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

-- | layers folds a double list of huds, mconcatting each inner list and layering the outer list
layers :: Chartable a => [[Hud a]] -> Hud a
layers hss = foldl layer mempty $ mconcat <$> hss

hudSvg :: (Chartable a) =>
  ViewBox a -> [[Hud a]] -> [Chart a] -> ChartSvg a
hudSvg asp hss cs =
  hudSvgWith asp (dataArea cs) (mconcat <$> hss) cs
{-
  chartSvg_
  (ViewBox (styleBoxes (cs' <> h vbStyle (ViewBox xs) xs)))
  (cs' <> h vbStyle (ViewBox xs) xs)
  where
    (Hud h) = layers hss
    cs' = projectSpots asp cs
    vbStyle = ViewBox $ styleBoxes cs'
    xs = dataArea cs
    xs' = dataArea cs'
-}

hudSvgWith :: (Chartable a) =>
  ViewBox a -> Area a -> [Hud a] -> [Chart a] -> ChartSvg a
hudSvgWith asp@(ViewBox vb) vborig hs cs =
  chartSvg_
  (ViewBox (vb <> styleBoxes (cs' <> h vbStyle asp vborig)))
  (cs' <> h vbStyle asp vborig)
  where
    (Hud h) = foldl layer mempty hs
    cs' = projectSpotsWith vb vborig cs
    vbStyle = ViewBox $ vb <> styleBoxes cs'

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
  { rstyle :: RectStyle
  , wid :: a
  , buff :: a
  } deriving (Show, Eq, Generic)

defaultBar :: (FromRational a) => Bar a
defaultBar = Bar defaultRectStyle 0.02 0.02

canvas :: RectStyle -> DrawAttributes -> Hud a
canvas s das = Hud $ \(ViewBox vb) _ _ -> [Chart (RectA s) das [SpotArea vb]]

bar :: (Chartable a) => Place a -> Bar a -> DrawAttributes -> Hud a
bar pl b das = Hud $ \(ViewBox (Area x' z' y' w')) (ViewBox (Area x z y w)) _ -> (:[]) $ case pl of
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

bars :: (Chartable a) => Place a -> [Bar a] -> DrawAttributes -> Hud a
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

-- | Create a title for a chart. The logic used to work out placement is flawed due to being able to freely specify text rotation.  It works for specific rotations (Top, Bottom at 0, Left at 90, Right @ 270)
title :: (Chartable a, FromInteger a) => Title a -> DrawAttributes -> Hud a
title t das =
  Hud $ \(ViewBox a) _ _ -> (:[]) $
    Chart (TextA style' [t ^. #text])
    (das <> translateDA (placePos a + alignPos a) <> rotateDA (rot :: Double)) [zero]
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
        placePos (Area x z y w) = case t ^. #place of
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
            Point ((x-z)/ 2.0) 0
          | t ^. #anchor == AnchorStart &&
            t ^. #place `elem` [PlaceLeft] =
            Point 0 ((y-w)/2.0)
          | t ^. #anchor == AnchorStart &&
            t ^. #place `elem` [PlaceRight] =
            Point 0 ((w-y)/2.0)
          | t ^. #anchor == AnchorEnd &&
            t ^. #place `elem` [PlaceTop, PlaceBottom] =
            Point ((-x+z)/2.0) 0
          | t ^. #anchor == AnchorEnd &&
            t ^. #place `elem` [PlaceLeft] =
            Point 0 ((-y+w)/2.0)
          | t ^. #anchor == AnchorEnd &&
            t ^. #place `elem` [PlaceRight] =
            Point 0 ((y-w)/2.0)
          | otherwise = Point 0 0

data Tick a = Tick
  { gstyle :: GlyphStyle
  , textStyle :: TextStyle
  , buff :: a
  , textBuff :: a
  , tstyle :: TickStyle a
  } deriving (Show, Eq, Generic)

defaultTick :: (FromRational a) => Tick a
defaultTick = Tick (#borderOpacity .~ 1 $ #borderColor .~ grey $ #shape .~ VLineGlyph 0.005 $ defaultGlyphStyle) (#size .~ 0.05 $ defaultTextStyle) 0.03 0.05 (TickRound TickFormatDefault 8 TickExtend)

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

-- | compute tick values and labels given options, ranges and formatting
computeTicks :: (Epsilon a, FromInteger a, RealFloat a, ExpField a, QuotientField a Integer, FromIntegral a Integer, Chartable a) => TickStyle a -> Range a -> Range a -> ([a], [P.Text])
computeTicks s asp r =
    case s of
      TickNone -> ([], [])
      TickRound f n e -> (project r asp <$> ticks0, toFormat f ticks0)
        where ticks0 = gridSensible OuterPos (e == NoTickExtend) r (fromIntegral n :: Integer)
      TickExact f n -> (project r asp <$> ticks0, toFormat f ticks0)
        where ticks0 = grid OuterPos r n
      TickLabels ls ->
          ( project (Range 0 (fromIntegral $ length ls)) asp <$>
            ((\x -> x - 0.5) . fromIntegral <$> [1 .. length ls])
          , ls)
      TickPlaced xs -> (project r asp . fst <$> xs, snd <$> xs)

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

-- | Create tick labels and marks for an axis
tick :: (Tickable a) =>
  Place a -> Tick a -> DrawAttributes -> Hud a
tick pl t das =
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
          | pl `elem` [PlaceTop, PlaceBottom] =
            (\x -> Point x 0) <$> fst (ts a xs)
          | otherwise =
            (\y -> Point 0 y) <$> fst (ts a xs)
        ps' a xs
          | pl `elem` [PlaceTop, PlaceBottom] =
            (\x -> Point x 0) <$> fst (ts a xs)
          | otherwise =
            (\x -> Point x 0) <$> fst (ts a xs)
        ra (Area x z y w)
          | pl `elem` [PlaceTop, PlaceBottom] = Range x z
          | otherwise = Range y w
        ts a xs = computeTicks (t ^. #tstyle) a xs
        rot
          | pl == PlaceRight = -90.0
          | pl == PlaceLeft = -90.0
          | otherwise = 0
        placePos (Area x z y w) = case pl of
          PlaceTop -> Point 0 (w + (t ^. #buff))
          PlaceBottom -> Point 0 (y - (t ^. #buff))
          PlaceLeft -> Point (x - (t ^. #buff)) 0
          PlaceRight -> Point (z + (t ^. #buff)) 0
          PlaceAbsolute p -> p
        textPos = case pl of
          PlaceTop -> Point 0 (t ^. #buff)
          PlaceBottom -> Point 0 ((-t^. #textBuff) + -0.5 * fromRational' (t ^. #textStyle ^. #vsize) * fromRational' (t ^. #textStyle ^. #size))
          PlaceLeft -> Point (- (t ^. #textBuff)) (fromRational' (t ^. #textStyle ^. #nudge1) * fromRational' (t ^. #textStyle ^. #vsize) * fromRational' (t ^. #textStyle ^. #size))
          PlaceRight -> Point (t ^. #buff) (fromRational' (t ^. #textStyle ^. #nudge1) * fromRational' (t ^. #textStyle ^. #vsize) * fromRational' (t ^. #textStyle ^. #size))
          PlaceAbsolute p -> p
        ta s = case pl of
          PlaceBottom -> s
          PlaceTop -> s
          PlaceLeft -> (#anchor .~ AnchorEnd :: TextStyle -> TextStyle) s
          PlaceRight -> (#anchor .~ AnchorStart :: TextStyle -> TextStyle) s
          PlaceAbsolute _ -> s

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
  Place a -> Tick a -> Area a -> [Chart a]
tickExtended pl t xs =
  maybe [] (\x -> [Chart BlankA mempty [SpotArea $ rangeext xs x]])
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
adjustTick :: (Tickable a) => AutoOptions -> ViewBox a -> Area a -> Place a -> (Tick a, DrawAttributes) -> (Tick a, DrawAttributes)
adjustTick (AutoOptions mrx ma mry ad) vb cs pl (t, das)
  | pl `elem` [PlaceBottom, PlaceTop] = case ad of
      False -> ((#textStyle %~ (#size %~ (/adjustSizeX))) t, das)
      True ->
        case adjustSizeX > one of
          True ->
            ((case pl of
                PlaceBottom -> #textStyle . #anchor .~ AnchorEnd
                PlaceTop -> #textStyle . #anchor .~ AnchorStart
                _ -> #textStyle . #anchor .~ AnchorEnd) $
             (#textStyle . #size %~ (/adjustSizeA)) $
             (#textStyle . #rotation .~ Just (-45)) t, das)
          False -> ((#textStyle . #size %~ (/adjustSizeA)) t, das)
  | pl `elem` [PlaceLeft, PlaceRight] =
    ((#textStyle . #size %~ (/adjustSizeY)) t, das)
  where
    ra (Area x z y w)
      | pl `elem` [PlaceTop, PlaceBottom] = Range x z
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
    adjustSizeX = maybe one identity (maximum' [(maxWidth / fromRational' (upper asp - lower asp)) / mrx, one])
    adjustSizeY = maybe one identity (maximum' [(maxHeight / fromRational' (upper asp - lower asp)) / mry, one])
    adjustSizeA = maybe one identity (maximum' [(maxHeight / fromRational' (upper asp - lower asp)) / ma, one])

data CanvasConfig = CanvasConfig
  { color :: PixelRGB8
  , opacity :: Double
  } deriving (Eq, Show, Generic)

defaultCanvasConfig :: CanvasConfig
defaultCanvasConfig  = CanvasConfig grey 0.03

data AxisConfig a = AxisConfig
  { abar :: Maybe (Bar a)
  , hasAuto :: Bool
  , atick :: Tick a
  , place :: Place Double
  } deriving (Eq, Show, Generic)

defaultAxisConfig :: AxisConfig Double
defaultAxisConfig = AxisConfig (Just (Bar (RectStyle 0 grey 0 (PixelRGB8 95 3 145) 0.5) 0.005 0.01)) True defaultTick PlaceBottom

data HudConfig = HudConfig
  { hudCanvas :: Maybe CanvasConfig
  , hudTitles :: [Title Double]
  , hudAxes ::  [AxisConfig Double]
  , hudAuto :: Bool
  } deriving (Eq, Show, Generic)

defaultHudConfig :: HudConfig
defaultHudConfig =
  HudConfig
  (Just defaultCanvasConfig)
  [defaultTitle "default"]
  [ defaultAxisConfig
  , defaultAxisConfig & #place .~ PlaceLeft
  ]
  False

-- let cs = [Chart (TextA defaultTextStyle ["test1", "test2"]) mempty [SP 0 0, SP 1 1]] :: [Chart Double]

hud :: HudConfig -> ViewBox Double -> [Chart Double] -> ChartSvg Double
hud cfg asp cs =
  hudSvgWith asp
  (dataArea $ cs <> exts)
  (mconcat <$> hudsWith cfg)
  (cs <> exts)
  where
    exts =
      mconcat $
      (\c -> tickExtended (c ^. #place) (c ^. #atick) (dataArea cs)) <$>
      (cfg ^. #hudAxes)
 
hudsWith :: HudConfig -> [[Hud Double]]
hudsWith cfg =
  [[can]] <> [axes] <> ((:[]) <$> titles)
  where
    can = maybe mempty (\cfg' -> canvas
      (blob
       (cfg' ^. #color)
       (cfg' ^. #opacity)) mempty)
      (cfg ^. #hudCanvas)
    titles = (`title` mempty) <$> (cfg ^. #hudTitles)
    autoTick c =
      Hud $ \vb' d a ->
        let (ts',das') =
              adjustTick defaultAutoOptions vb' a (c ^. #place)
              (c ^. #atick, mempty) in
        let (Hud h) = tick (c ^. #place) ts' das' in
          h vb' d a
    axes = (\x -> autoTick x <>
             maybe mempty (\a -> bar (x ^. #place) a mempty) (x ^. #abar)) <$>
           (cfg ^. #hudAxes)

renderChartWith :: ChartSvgStyle -> HudConfig -> [Chart Double] -> Text
renderChartWith scfg hcfg cs =
  renderChartSvg (scfg ^. #sizex) (scfg ^. #sizey) $
  maybe id pad (scfg ^. #outerPad) $
  maybe id (\x c -> frame x c <> c) (scfg ^. #chartFrame) $
  maybe id pad (scfg ^. #innerPad) $
  hud hcfg (aspect (scfg ^. #chartAspect)) $
  cs <>
  maybe mempty (\g -> [showOriginWith g]) (scfg ^. #orig)

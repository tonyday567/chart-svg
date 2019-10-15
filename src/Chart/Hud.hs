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
  { tstyle :: TickStyle a
  , gtick :: Maybe (GlyphStyle, a)
  , ttick :: Maybe (TextStyle, a)
  , ltick :: Maybe (LineStyle, a)
  } deriving (Show, Eq, Generic)

defaultGlyphTick :: GlyphStyle
defaultGlyphTick =
  defaultGlyphStyle &
  #borderOpacity .~ 1 &
  #borderColor .~ grey &
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
  (TickRound TickFormatDefault 8 TickExtend)
  (Just (defaultGlyphTick, 0.03))
  (Just (defaultTextTick, 0.05))
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

tickGlyph :: (Tickable a) =>
  Place a -> Tick a -> DrawAttributes -> Hud a
tickGlyph pl t das = Hud $ \(ViewBox a) (ViewBox d) xs ->
  maybe [] (\(g, b) -> [ Chart (GlyphA g)
    (das <> translateDA (placePos pl b a) <> rotateDA (placeRot pl))
    (SpotPoint <$> (\x -> Point x 0) <$>
     fst
       (computeTicks (t ^. #tstyle) (placeRange pl d) (placeRange pl xs)))
  ]) (t ^. #gtick)

tickText :: (Tickable a) =>
  Place a -> Tick a -> DrawAttributes -> Hud a
tickText pl t das = Hud $ \(ViewBox a) (ViewBox d) xs ->
  maybe [] (\(tt, b) ->
              zipWith
             (\txt sp ->
                Chart (TextA (placeTextAnchor pl tt) [txt])
               (das <> translateDA (placePos pl b a + textPos pl tt b)) [SpotPoint sp])
             (snd (computeTicks (t ^. #tstyle) (placeRange pl d) (placeRange pl xs)))
             (placeOrigin pl <$> fst
              (computeTicks (t ^. #tstyle) (placeRange pl d) (placeRange pl xs)))
           )
  (t ^. #ttick)


-- let darea = Area 0.0 6.283185307179586 -0.9945218953682734 0.9945218953682734
-- let vbStyle = ViewBox (Area -0.8664999999999999 0.8664999999999999 -0.5165 0.5165)
-- let hc = HudConfig Nothing [] [AxisConfig Nothing Nothing (Tick (TickRound TickFormatDefault 8 TickExtend) Nothing Nothing (Just (defaultLineTick, 0.02))) PlaceBottom]
-- let (Hud h) = foldl layer mempty $ mconcat <$> (hudsWith hc)
-- let ss = spots <$> h vbStyle (aspect 1.7) (dataArea c2)
-- spots <$> unhud (tickLine PlaceBottom (Tick (TickRound TickFormatDefault 8 TickExtend) Nothing Nothing (Just (defaultLineTick, 0.02))) mempty) vbStyle (aspect 1.7) darea
-- let ts = [-0.85,-0.5794365967437779,-0.3088731934875558,-3.830979023133374e-2,0.23225361302488834,0.5028170162811104,0.7733804195373325,1.0439438227935547]

tickLine :: (Tickable a) =>
  Place a -> Tick a -> DrawAttributes -> Hud a
tickLine pl t das = Hud $ \_ (ViewBox d) xs ->
  maybe [] (\(l, b) -> Chart (LineA l) das <$>
    ((\x -> placeGridLines pl d x b) <$> fst
       (computeTicks (t ^. #tstyle) (placeRange pl d) (placeRange pl xs))))
  (t ^. #ltick)

-- | Create tick glyphs (marks), lines (grid) and text (labels)
tick :: (Tickable a) =>
  Place a -> Tick a -> DrawAttributes -> Hud a
tick pl t das = mconcat [tickGlyph pl t das, tickText pl t das, tickLine pl t das]

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
    tickl = snd (computeTicks (t ^. #tstyle) asp r)
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

data CanvasConfig = CanvasConfig
  { color :: PixelRGB8
  , opacity :: Double
  } deriving (Eq, Show, Generic)

defaultCanvasConfig :: CanvasConfig
defaultCanvasConfig  = CanvasConfig grey 0.03

data AxisConfig a = AxisConfig
  { abar :: Maybe (Bar a)
  , adjust :: Maybe Adjustments
  , atick :: Tick a
  , place :: Place Double
  } deriving (Eq, Show, Generic)

defaultAxisConfig :: AxisConfig Double
defaultAxisConfig = AxisConfig (Just (Bar (RectStyle 0 grey 0 (PixelRGB8 95 3 145) 0.5) 0.005 0.01)) (Just defaultAdjustments) defaultTick PlaceBottom

data HudConfig = HudConfig
  { hudCanvas :: Maybe CanvasConfig
  , hudTitles :: [Title Double]
  , hudAxes ::  [AxisConfig Double]
  } deriving (Eq, Show, Generic)

defaultHudConfig :: HudConfig
defaultHudConfig =
  HudConfig
  (Just defaultCanvasConfig)
  [defaultTitle "default"]
  [ defaultAxisConfig
  , defaultAxisConfig & #place .~ PlaceLeft
  ]

hudSvg :: (Chartable a) =>
  ViewBox a -> [[Hud a]] -> [Chart a] -> ChartSvg a
hudSvg asp hss cs =
  hudSvgWith asp (dataArea cs) (mconcat <$> hss) cs

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

hud :: HudConfig -> ViewBox Double -> [Chart Double] -> ChartSvg Double
hud cfg asp cs =
  hudSvg asp (hudsWith cfg) (cs <> exts)
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
    axes = (\x -> adjustedTickHud x <>
             maybe mempty (\a -> bar (x ^. #place) a mempty) (x ^. #abar)) <$>
           (cfg ^. #hudAxes)

adjustedTickHud :: AxisConfig Double -> Hud Double
adjustedTickHud c = Hud $ \vb d xs ->
  let adjTick =
        maybe
        (c ^. #atick)
        (\x -> adjustTick x vb xs (c ^. #place) mempty (c ^. #atick))
        (c ^. #adjust) in
  let (Hud h) = tick (c ^. #place) adjTick mempty in h vb d xs

renderChartWith :: ChartSvgStyle -> HudConfig -> [Chart Double] -> Text
renderChartWith scfg hcfg cs =
  renderChartSvg (scfg ^. #sizex) (scfg ^. #sizey) $
  maybe id pad (scfg ^. #outerPad) $
  maybe id (\x c -> frame x c <> c) (scfg ^. #chartFrame) $
  maybe id pad (scfg ^. #innerPad) $
  hud hcfg (aspect (scfg ^. #chartAspect)) $
  cs <>
  maybe mempty (\g -> [showOriginWith g]) (scfg ^. #orig)

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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chart.Hud where

-- import qualified Text.Blaze as B
-- import qualified Data.Text as Text
-- import qualified Data.Text.Lazy.IO as Lazy
import NumHask.Prelude as P hiding (Group)
-- import qualified Data.Colour.SRGB as C
-- import Control.Monad.State.Lazy
import Graphics.Svg.Types as Svg hiding (Point, Text)
-- import Graphics.Svg as Svg
-- import NumHask.Data.Rect
-- import NumHask.Data.Pair
-- import NumHask.Data.Range
-- import NumHask.Analysis.Space
-- import qualified Data.Map as Map
import Control.Lens
import Codec.Picture.Types
import Data.Generics.Product (field)
-- import Data.Generics.Sum
-- import Linear.V2
-- import Data.Colour
-- import Control.Exception
import Chart.Svg
import Chart.Spot
import Chart.Core
import Formatting
import Data.Scientific
import Data.List (nub)
import Lucid.Base

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
bar b das = Hud $ \(ViewBox (Area x' z' y' w')) (ViewBox (Area x z y w)) _ -> (:[]) $ case b ^. field @"place" of
  PlaceTop ->
    Chart (RectA (rstyle b)) das
    [SA x z
     (w' + b ^. field @"buff")
     (w' + b ^. field @"buff" + b ^. field @"wid")
    ]
  PlaceBottom -> Chart (RectA (rstyle b)) das
    [SA x z
     (y' - b ^. field @"wid" - b ^. field @"buff")
     (y' - b ^. field @"buff")]
  PlaceLeft -> Chart (RectA (rstyle b)) das
    [SA (x' - b ^. field @"wid" - b ^. field @"buff")
     (x' - b ^. field @"buff") y w]
  PlaceRight -> Chart (RectA (rstyle b)) das
    [SA (z' + (b ^. field @"buff"))
     (z' + (b ^. field @"buff") + (b ^. field @"wid")) y w]
  PlaceAbsolute (Point x'' _) -> Chart (RectA (rstyle b)) das
    [SA (x'' + (b ^. field @"buff"))
     (x'' + (b ^. field @"buff") + (b ^. field @"wid")) y w]

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
    ( field @"size" .~ 0.12 $
      field @"color" .~ PixelRGB8 0 0 0 $
      defaultTextStyle)
    PlaceTop
    TextAnchorMiddle
    0.04

-- | Create a title for a chart. The logic used to work out placement is flawed due to being able to freely specify text rotation.  It works for specific rotations (Top, Bottom at 0, Left at 90, Right @ 270)
title :: (Chartable a, FromInteger a) => Title a -> DrawAttributes -> Hud a
title t das =
  Hud $ \(ViewBox a) _ _ -> (:[]) $
    Chart (TextA style' [t ^. field @"text"])
    (das <> translateDA (placePos a + alignPos a) <> rotateDA (rot :: Double)) [zero]
      where
        style'
          | t ^.field @"align" == TextAnchorStart =
            field @"alignH" .~ TextAnchorStart $ t ^. field @"style"
          | t ^.field @"align" == TextAnchorEnd =
            field @"alignH" .~ TextAnchorEnd $ t ^. field @"style"
          | otherwise = t ^. field @"style"
        rot
          | t ^. field @"place" == PlaceRight = 90.0
          | t ^. field @"place" == PlaceLeft = -90.0
          | otherwise = 0
        placePos (Area x z y w) = case t ^. field @"place" of
          PlaceTop -> Point ((x+z)/2.0) (w + (t ^. field @"buff"))
          PlaceBottom -> Point ((x+z)/2.0)
            (y - (t ^. field @"buff") -
              0.5 * fromRational (t ^. field @"style" ^. field @"vsize") *
              fromRational (t ^. field @"style" ^. field @"size"))
          PlaceLeft -> Point (x - (t ^. field @"buff")) ((y+w)/2.0)
          PlaceRight -> Point (z + (t ^. field @"buff")) ((y+w)/2.0)
          PlaceAbsolute p -> p
        alignPos (Area x z y w)
          | t ^. field @"align" == TextAnchorStart &&
            t ^. field @"place" `elem` [PlaceTop, PlaceBottom] =
            Point ((x-z)/2.0) 0
          | t ^. field @"align" == TextAnchorStart &&
            t ^. field @"place" `elem` [PlaceLeft] =
            Point 0 ((y-w)/2.0)
          | t ^. field @"align" == TextAnchorStart &&
            t ^. field @"place" `elem` [PlaceRight] =
            Point 0 ((w-y)/2.0)
          | t ^. field @"align" == TextAnchorEnd &&
            t ^. field @"place" `elem` [PlaceTop, PlaceBottom] =
            Point ((-x+z)/2.0) 0
          | t ^. field @"align" == TextAnchorEnd &&
            t ^. field @"place" `elem` [PlaceLeft] =
            Point 0 ((-y+w)/2.0)
          | t ^. field @"align" == TextAnchorEnd &&
            t ^. field @"place" `elem` [PlaceRight] =
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
defaultTick = Tick PlaceBottom (field @"borderOpacity" .~ 1 $ field @"borderColor" .~ grey $ field @"shape" .~ VLineGlyph 0.005 $ defaultGlyphStyle) (field @"size" .~ 0.05 $ defaultTextStyle) 0.03 0.05 (TickRound (TickFormatDefault, 8))

data TickFormat
  = TickFormatDefault
  | TickFormatCommas Int
  | TickFormatFixed Int
  | TickFormatDollars
  deriving (Show, Eq, Generic)

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
  [ Chart (GlyphA (t ^. field @"gstyle"))
    (das <> translateDA (placePos a) <> rotateDA (rot :: Double))
    (SpotPoint <$> ps' (ra d) (ra xs))
  ] <>
  zipWith
  (\txt sp ->
      Chart (TextA (ta (t ^. field @"textStyle")) [txt])
       (das <> translateDA (placePos a + textPos)) [SpotPoint sp])
  (snd (ts (ra d) (ra xs)))
  (ps (ra d) (ra xs))
      where
        ps a xs
          | t ^. field @"place" `elem` [PlaceTop, PlaceBottom] =
            (\x -> Point x 0) <$> fst (ts a xs)
          | otherwise =
            (\y -> Point 0 y) <$> fst (ts a xs)
        ps' a xs
          | t ^. field @"place" `elem` [PlaceTop, PlaceBottom] =
            (\x -> Point x 0) <$> fst (ts a xs)
          | otherwise =
            (\x -> Point x 0) <$> fst (ts a xs)
        ra (Area x z y w)
          | t ^. field @"place" `elem` [PlaceTop, PlaceBottom] = Range x z
          | otherwise = Range y w
        ts a xs = computeTicks (t ^. field @"tstyle") a xs
        rot
          | t ^. field @"place" == PlaceRight = -90.0
          | t ^. field @"place" == PlaceLeft = -90.0
          | otherwise = 0
        placePos (Area x z y w) = case t ^. field @"place" of
          PlaceTop -> Point 0 (w + (t ^. field @"buff"))
          PlaceBottom -> Point 0 (y - (t ^. field @"buff"))
          PlaceLeft -> Point (x - (t ^. field @"buff")) 0
          PlaceRight -> Point (z + (t ^. field @"buff")) 0
          PlaceAbsolute p -> p
        textPos = case t ^. field @"place" of
          PlaceTop -> Point 0 (t ^. field @"buff")
          PlaceBottom -> Point 0 ((-t^. field @"textBuff") + -0.5 * fromRational (t ^. field @"textStyle" ^. field @"vsize") * fromRational (t ^. field @"textStyle" ^. field @"size"))
          PlaceLeft -> Point (- (t ^. field @"textBuff")) (fromRational (t ^. field @"textStyle" ^. field @"nudge1") * fromRational (t ^. field @"textStyle" ^. field @"vsize") * fromRational (t ^. field @"textStyle" ^. field @"size"))
          PlaceRight -> Point (t ^. field @"buff") (fromRational (t ^. field @"textStyle" ^. field @"nudge1") * fromRational (t ^. field @"textStyle" ^. field @"vsize") * fromRational (t ^. field @"textStyle" ^. field @"size"))
          PlaceAbsolute p -> p
        ta s = case t ^. field @"place" of
          PlaceBottom -> s
          PlaceTop -> s
          PlaceLeft -> (field @"alignH" .~ TextAnchorEnd :: TextStyle -> TextStyle) s
          PlaceRight -> (field @"alignH" .~ TextAnchorStart :: TextStyle -> TextStyle) s
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
  | t ^. field @"place" `elem` [PlaceBottom, PlaceTop] = case ad of
      False -> ((field @"textStyle" %~ (field @"size" %~ (/adjustSizeX))) t, das)
      True ->
        case adjustSizeX > one of
          True ->
            ((case t ^. field @"place" of
                PlaceBottom -> field @"textStyle" . field @"alignH" .~ TextAnchorEnd
                PlaceTop -> field @"textStyle" . field @"alignH" .~ TextAnchorStart
                _ -> field @"textStyle" . field @"alignH" .~ TextAnchorEnd) $
             (field @"textStyle" . field @"size" %~ (/adjustSizeA)) $
             (field @"textStyle" . field @"rotation" .~ Just (-45)) t, das)
          False -> ((field @"textStyle" . field @"size" %~ (/adjustSizeA)) t, das)
  | t ^. field @"place" `elem` [PlaceLeft, PlaceRight] =
    ((field @"textStyle" . field @"size" %~ (/adjustSizeY)) t, das)

  where
    ra (Area x z y w)
      | t ^. field @"place" `elem` [PlaceTop, PlaceBottom] = Range x z
      | otherwise = Range y w
    asp = ra (vbArea vb)
    r = ra cs
    tickl = snd (computeTicks (t ^. field @"tstyle") asp r)
    maxWidth =
          maybe one identity (maximum' $
          (\(Area x z _ _) -> z - x)
              <$> styleBoxText (t ^. field @"textStyle") das <$> tickl)
    maxHeight =
      maybe one identity
          (maximum' $
          (\(Area _ _ y w) -> w - y)
              <$> styleBoxText (t ^. field @"textStyle") das <$> tickl)
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
  { canvas1 :: Maybe CanvasConfig
  , title1 :: Maybe (Title Double)
  , axis1 :: Maybe (AxisConfig Double)
  } deriving (Eq, Show, Generic)

defaultHudConfig :: HudConfig Double
defaultHudConfig =
  HudConfig
  (Just defaultCanvasConfig)
  (Just (defaultTitle "chart-svg automation testing"))
  (Just defaultAxisConfig)

hud :: HudConfig Double -> ViewBox Double -> [Chart Double] -> ChartSvg Double
hud cfg vb =
  hudSvg vb [c1, bar1 <> tick1, htitle]
  where
    c1 = maybe mempty (\cfg' -> canvas
      (blob
       (cfg' ^. field @"color")
       (cfg' ^. field @"opacity")) mempty)
      (cfg ^. field @"canvas1")
    bar1 = maybe mempty (\x -> maybe mempty (`bar` mempty) (x ^. field @"abar"))
      (cfg ^. field @"axis1")
    ts c = (field @"tstyle" .~ TickRound (TickFormatDefault, c ^. field @"tickN") :: Tick Double -> Tick Double) defaultTick
    autoTick c = Hud $ \vb' d a -> let (ts',das') = adjustTick defaultAutoOptions vb' a (ts c, mempty) in let (Hud h) = tick ts' das' in h vb' d a
    tick1 = maybe mempty autoTick (cfg ^. field @"axis1")
    htitle = maybe mempty (`title` mempty) (cfg ^. field @"title1")

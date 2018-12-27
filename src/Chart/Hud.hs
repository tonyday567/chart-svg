{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}

module Chart.Hud where

-- import qualified Text.Blaze as B
-- import qualified Data.Text as Text
-- import qualified Data.Text.Lazy.IO as Lazy
import NumHask.Prelude as P hiding (Group)
-- import qualified Data.Colour.SRGB as C
-- import Control.Monad.State.Lazy
import Graphics.Svg.Types as Svg hiding (Point)
-- import Graphics.Svg as Svg
-- import NumHask.Data.Rect
-- import NumHask.Data.Pair
-- import NumHask.Data.Range
-- import NumHask.Analysis.Space
-- import qualified Data.Map as Map
import Lens.Micro
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

newtype Hud a = Hud { unhud :: ViewBox a -> Area a -> [Chart a] }

instance Semigroup (Hud a) where
  (<>) (Hud h1) (Hud h2) = Hud $ \vb a -> h1 vb a <> h2 vb a

instance Monoid (Hud a) where
  mempty = Hud (const (const []))

layer :: Chartable a => Hud a -> Hud a -> Hud a
layer (Hud h1) (Hud h2) =
  Hud $ \vb@(ViewBox asp) a -> h1 vb a <> h2 (ViewBox $ asp <> styleBoxes (h1 vb a)) a

hudSvg :: (ToRatio a, FromRatio a, Subtractive a, Field a, BoundedLattice a) =>
  ViewBox a -> [Hud a] -> [Chart a] -> ChartSvg a
hudSvg (ViewBox asp) hs cs =
  chartSvg_ (ViewBox (styleBoxes (cs' <> h vb' xs))) (cs' <> h vb' xs)
  where
    (Hud h) = foldl layer mempty hs
    cs' = projectSpots asp cs
    vb' = ViewBox $ styleBoxes cs'
    xs = toArea $ fold $ fold (spots <$> cs)

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

data Bar a = Bar
  { place :: Place a
  , rstyle :: RectStyle
  , wid :: a
  , buff :: a
  } deriving (Show, Eq, Generic)

canvas :: RectStyle -> DrawAttributes -> Hud a
canvas s das = Hud $ \(ViewBox vb) _ -> [Chart (RectA s) das [SpotArea vb]]

bar :: (Chartable a) => Bar a -> DrawAttributes -> Hud a
bar b das = Hud $ \(ViewBox (Area x z y w)) _ -> (:[]) $ case b ^. field @"place" of
  PlaceTop ->
    Chart (RectA (rstyle b)) das
    [SA x z
     (w + b ^. field @"buff")
     (w + b ^. field @"buff" + b ^. field @"wid")
    ]
  PlaceBottom -> Chart (RectA (rstyle b)) das
    [SA x z
     (y - b ^. field @"wid" - b ^. field @"buff")
     (y - b ^. field @"buff")]
  PlaceLeft -> Chart (RectA (rstyle b)) das
    [SA (x - b ^. field @"wid" - b ^. field @"buff")
     (x - b ^. field @"buff") y w]
  PlaceRight -> Chart (RectA (rstyle b)) das
    [SA (z + (b ^. field @"buff"))
     (z + (b ^. field @"buff") + (b ^. field @"wid")) y w]
  PlaceAbsolute (Point x' _) -> Chart (RectA (rstyle b)) das
    [SA (x' + (b ^. field @"buff"))
     (x' + (b ^. field @"buff") + (b ^. field @"wid")) y w]



bars :: (Chartable a) => [Bar a] -> DrawAttributes -> Hud a
bars bs das = mconcat ((\b -> bar b das) <$> bs)

-- | Options for titles.  Defaults to center aligned, and placed at Top of the hud
data Title a = Title
  { text :: P.Text
  , style :: TextStyle
  , place :: Place a
  , align :: TextAnchor
  , buff :: a
  , wid :: a
  } deriving (Show, Eq, Generic)

defaultTitle :: (Chartable a) => P.Text -> Title a
defaultTitle txt =
    Title
    txt
    ( field @"size" .~ 0.12 $
      field @"color" .~ PixelRGBA8 0 0 0 150 $
      defaultTextStyle)
    PlaceTop
    TextAnchorMiddle
    0.04
    0.0

-- | Create a title for a chart. The logic used to work out placement is flawed due to being able to freely specify text rotation.  It works for specific rotations (Top, Bottom at 0, Left at 90, Right @ 270)
title :: (Chartable a, FromInteger a) => Title a -> DrawAttributes -> Hud a
title t das =
  Hud $ \(ViewBox a) _ -> (:[]) $
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
defaultTick = Tick PlaceBottom (field @"borderOpacity" .~ 1 $ field @"borderColor" .~ grey $ field @"shape" .~ VLineGlyph 0.005 $ defaultGlyphStyle) (field @"size" .~ 0.05 $ defaultTextStyle) 0.03 0.05 (TickRound 8)

-- | Style of tick marks on an axis.
data TickStyle a
  = TickNone -- ^ no ticks on axis
  | TickLabels [P.Text] -- ^ specific labels
  | TickRound Int -- ^ sensibly rounded ticks and a guide to how many
  | TickExact Int -- ^ exactly n equally spaced ticks
  | TickPlaced [(a, P.Text)] -- ^ specific labels and placement
  deriving (Show, Eq, Generic)

-- | compute tick values and labels given options and ranges
computeTicks :: (Epsilon a, RealFloat a, ExpField a, QuotientField a Integer, FromInteger a, Chartable a) => TickStyle a -> Range a -> Range a -> ([a], [P.Text])
computeTicks s asp r =
    case s of
      TickNone -> ([], [])
      TickRound n -> (project r asp <$> ticks0, precision 0 ticks0)
        where ticks0 = gridSensible OuterPos r (fromIntegral n)
      TickExact n -> (project r asp <$> ticks0, precision 3 ticks0)
        where ticks0 = grid OuterPos r n
      TickLabels ls ->
          ( project (Range 0 (fromIntegral $ length ls)) asp <$>
            ((\x -> x - 0.5) . fromIntegral <$> [1 .. length ls])
          , ls)
      TickPlaced xs -> (project r asp . fst <$> xs, snd <$> xs)

-- | Provide formatted text for a list of numbers so that they are just distinguished.  'precision 2 ticks' means give the tick labels as much precision as is needed for them to be distinguished, but with at least 2 significant figues.
precision :: (FromInteger a, FromRatio a, QuotientField a Integer, RealFloat a) => Int -> [a] -> [P.Text]
precision n0 xs
  | foldr max 0 xs < 0.01 = precLoop expt' n0 (fromFloatDigits <$> xs)
  | foldr max 0 xs > 100000 = precLoop expt' n0 (fromFloatDigits <$> xs)
  | foldr max 0 xs > 1000 =
    precLoopInt (const Formatting.commas) n0 (floor <$> xs :: [Integer])
  | otherwise = precLoop fixed n0 xs
  where
    expt' x = scifmt Exponent (Just x)
    precLoop f n xs' =
      let s = sformat (f n) <$> xs'
      in if s == nub s
           then s
           else precLoop f (n + 1) xs'
    precLoopInt f n xs' =
      let s = sformat (f n) <$> xs'
      in if s == nub s
           then s
           else precLoopInt f (n + 1) xs'

-- | Create tickMarks for an axis
tickMarks :: (Chartable a, Epsilon a, QuotientField a Integer, ExpField a, RealFloat a, FromInteger a) =>
  Tick a -> DrawAttributes -> Hud a
tickMarks t das =
  Hud $ \(ViewBox a) xs -> 
  [ Chart (GlyphA (t ^. field @"gstyle"))
    (das <> translateDA (placePos a) <> rotateDA (rot :: Double))
    (ps' (ra a) (ra xs))
  , Chart (TextA (ta (t ^. field @"textStyle")) (snd (ts (ra a) (ra xs))))
    (das <> translateDA (placePos a + textPos))
    (ps (ra a) (ra xs))
  ]
      where
        ps a xs
          | t ^. field @"place" `elem` [PlaceTop, PlaceBottom] =
            (\x -> SP x 0) <$> fst (ts a xs)
          | otherwise =
            (\y -> SP 0 y) <$> fst (ts a xs)
        ps' a xs
          | t ^. field @"place" `elem` [PlaceTop, PlaceBottom] =
            (\x -> SP x 0) <$> fst (ts a xs)
          | otherwise =
            (\x -> SP x 0) <$> fst (ts a xs)
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
          PlaceRight -> Point ((t ^. field @"buff")) (fromRational (t ^. field @"textStyle" ^. field @"nudge1") * fromRational (t ^. field @"textStyle" ^. field @"vsize") * fromRational (t ^. field @"textStyle" ^. field @"size"))
          PlaceAbsolute p -> p
        ta s = case t ^. field @"place" of
          PlaceBottom -> s
          PlaceTop -> s
          PlaceLeft -> (field @"alignH" .~ TextAnchorEnd :: TextStyle -> TextStyle) $ s
          PlaceRight -> (field @"alignH" .~ TextAnchorStart :: TextStyle -> TextStyle) $ s
          PlaceAbsolute _ -> s

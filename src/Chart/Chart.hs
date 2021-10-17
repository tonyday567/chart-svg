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
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- | Chart API
module Chart.Chart where

import Chart.Style
import Data.Bifunctor
import Data.Generics.Labels ()
import Data.Path
import qualified Data.Text as Text
import Data.Text (Text)
import NumHask.Prelude
import NumHask.Space as NH hiding (Element)
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty(..))
import GHC.OverloadedLabels
import Control.Lens
import Text.HTML.TagSoup (maybeTagText, parseTags)
import Data.Colour
import Data.Typeable
import Unsafe.Coerce

class (Eq d, Show d) => DataD d where
  boxD :: d -> Rect Double
  moveD :: Point Double -> d -> d
  scaleD :: Double -> d -> d
  projectWithD :: Rect Double -> Rect Double -> d -> d

-- This is existential quantification
data SomeData where
  SomeData :: (DataD d, Eq d, Show d, Typeable d, Typeable a) => d -> SomeData
  -- deriving (Show)

-- Non-GADT version would be (still existential quantification)
-- data SomeStyle = forall a. (StyleC a, Show a) => SomeStyle a

instance Show SomeData
  where
    show (SomeData a) = show a

instance Eq SomeData
  where
    (==) (SomeData a) (SomeData b) =
      bool False (Just a == cast b) (typeOf a == typeOf b)

instance DataD (Rect Double) where
  boxD = id
  moveD p = addPoint p
  scaleD p = fmap (*p)
  projectWithD = projectOnR

class ChartC c where
  boxC :: c -> Rect Double
  scaleC :: Double -> c -> c

data ChartZ f s d = (Traversable f, Style s, DataD d, Eq (f d), Show (f d)) => ChartZ
  { chartStyle :: s,
    chartData :: f d }

instance Eq (ChartZ f s d)
  where
    (==) (ChartZ s d) (ChartZ s' d') = s==s' && d==d'

instance Show (ChartZ f s d)
  where
    show (ChartZ s d) = "ChartZ " <> "(" <> show s <> ") (" <> show d <> ")"

-- This is existential quantification
data SomeChart where
  SomeChart :: (Traversable f, Typeable f, Style s, Eq s, Show s, Typeable s, DataD d, Eq d, Show (f d), Typeable d, Typeable (f d)) => ChartZ f s d -> SomeChart

instance Show SomeChart
  where
    show (SomeChart a) = show a

instance Eq SomeChart
  where
    (==) (SomeChart a) (SomeChart b) =
      bool False (Just a == cast b) (typeOf a == typeOf b)

-- |
--
-- >>> withSomeChart (SomeChart c1) (foldRectUnsafe . fmap boxD . chartData)
-- Rect -0.5 0.5 -0.5 0.5
--
-- equivalent to
-- (\(SomeChart c) -> boxSS c) (SomeChart c1)
-- Rect -0.5 0.5 -0.5 0.5
--
-- Which has an easier type:
--
-- >>> :t \(SomeChart c) -> (foldRectUnsafe . fmap boxD . chartData) c
-- \(SomeChart c) -> (foldRectUnsafe . fmap boxD . chartData) c
--   :: SomeChart -> Rect Double
withSomeChart :: SomeChart -> (forall d f s. (Traversable f, Typeable f, Style s, Eq s, Show s, Typeable s, DataD d, Eq d, Show (f d), Typeable d, Typeable (f d)) => ChartZ f s d -> b) -> b
withSomeChart (SomeChart c) f = f c

-- |
--
-- >>> mapSomeChart (toList . fmap boxD . chartData) [SomeChart c1, SomeChart c1]
-- [[Rect -0.5 0.5 -0.5 0.5],[Rect -0.5 0.5 -0.5 0.5]]
mapSomeChart :: (Traversable g) => (forall d f s. (Traversable f, Typeable f, Style s, Eq s, Show s, Typeable s, DataD d, Eq d, Show (f d), Typeable d, Typeable (f d)) => ChartZ f s d -> b) -> g SomeChart -> g b
mapSomeChart f cs = fmap f' cs
  where
    f' (SomeChart c) = f c

-- |
--
-- >>> modifySomeChart (\c -> c {chartStyle = scaleOpacStyle 1 (chartStyle c)}) (SomeChart c1) == SomeChart c1
-- True
modifySomeChart :: (forall d f s. (Traversable f, Style s, Eq s, Show s, Typeable s, DataD d, Eq d, Show (f d)) => ChartZ f s d -> ChartZ f s d) -> SomeChart -> SomeChart
modifySomeChart f (SomeChart c) = SomeChart (f c)

boxSS :: ChartZ f s d -> Rect Double
boxSS (ChartZ _ d) = foldRectUnsafe $ boxD <$> d

boxG :: SomeChart -> Rect Double
boxG (SomeChart c) = boxSS c

-- >>> boxesSS [SomeChart c1, SomeChart c1]
-- Rect -0.5 0.5 -0.5 0.5
-- boxesSS :: (Traversable f) => f SomeChart -> Rect Double
-- boxesSS cs = foldRectUnsafe $ flip withSomeChart boxSS <$> cs

-- >>> withSomeChartSS (SomeChartSS c1SS) boxSS
c1 :: ChartZ [] RectStyle (Rect Double)
c1 = ChartZ defaultRectStyle [one]

instance (Traversable f) => ChartC (ChartZ f RectStyle (Rect Double)) where
  boxC (ChartZ s d) =
    padRect (0.5 * view #borderSize s)
    (foldRectUnsafe $ fmap boxD d)

  scaleC a (ChartZ s d) = ChartZ (scaleStyle a s) (fmap (scaleD a) d)

-- * non-family
data BlankStyle

data Chart a =
  RectChart RectStyle (NonEmpty (Rect a)) |
  TextChart TextStyle (NonEmpty (Text, Point a)) |
  LineChart LineStyle (NonEmpty (Point a)) |
  GlyphChart GlyphStyle (NonEmpty (Point a)) |
  PathChart PathStyle (NonEmpty (PathInfo a, Point a)) |
  BlankChart (NonEmpty (Rect a))

box_ :: Chart Double -> Rect Double
box_ (RectChart _ a) = foldRectUnsafe a
box_ (TextChart _ a) = space1 $ snd <$> a
box_ (LineChart _ a) = space1 a
box_ (GlyphChart _ a) = space1 a
box_ (PathChart _ a) = pathBoxes' a
box_ (BlankChart a) = foldRectUnsafe a

sbox_ :: Chart Double -> Rect Double
sbox_ (RectChart s a) = foldRectUnsafe $ padRect (0.5 * view #borderSize s) <$> a
sbox_ (TextChart s a) = foldRectUnsafe $ uncurry (styleBoxText_ s) <$> a
sbox_ (LineChart s a) = foldRectUnsafe $ padRect (0.5 * Chart.Style.width s) . singleton <$> a
sbox_ (GlyphChart s a) = foldRectUnsafe $ (\p -> addPoint p (styleBoxGlyph_ s)) <$> a
sbox_ (PathChart s a) = padRect (0.5 * view #borderSize s) (pathBoxes' a)
sbox_ (BlankChart a) = foldRectUnsafe a

move_ :: Point Double -> Chart Double -> Chart Double
move_ p (RectChart s a) = RectChart s (addPoint p <$> a)
move_ p (TextChart s a) = TextChart s (second (p+) <$> a)
move_ p (LineChart s a) = LineChart s ((p+) <$> a)
move_ p (GlyphChart s a) = GlyphChart s ((p+) <$> a)
move_ p (PathChart s a) = PathChart s (second (p+) <$> a)
move_ p (BlankChart a) = BlankChart (addPoint p <$> a)


scale_ = undefined
{-
-- | Scale both the chart data and the style
--
scale_ :: Double -> Chart Double -> Chart Double
scale_ p (RectChart s a) =
  RectChart (scaleStyle_ p s) (fmap (fmap (*p)) a)
scale_ p (LineChart s a) =
  LineChart (scaleStyle_ p s) (fmap (fmap (*p)) a)
scale_ p (TextChart s a) =
  TextChart (scaleStyle_ p s) (fmap (second (fmap (*p))) a)
scale_ p (GlyphChart s a) =
  GlyphChart (scaleStyle_ p s) (fmap (fmap (*p)) a)
scale_ p (PathChart s a) =
  PathChart (scaleStyle_ p s) (fmap (second (fmap (*p))) a)
scale_ p (BlankChart a) =
  BlankChart (fmap (fmap (*p)) a)


-}
-- | the extra area from text styling
styleBoxText_ ::
  TextStyle ->
  Text ->
  Point Double ->
  Rect Double
styleBoxText_ o t p = move p $ maybe flat (`rotationBound` flat) (o ^. #rotation)
  where
    flat = Rect ((-x' / 2.0) + x' * a') (x' / 2 + x' * a') ((-y' / 2) - n1') (y' / 2 - n1')
    s = o ^. #size
    h = o ^. #hsize
    v = o ^. #vsize
    n1 = o ^. #nudge1
    x' = s * h * fromIntegral (sum $ maybe 0 Text.length . maybeTagText <$> parseTags t)
    y' = s * v
    n1' = s * n1
    a' = case o ^. #anchor of
      AnchorStart -> 0.5
      AnchorEnd -> -0.5
      AnchorMiddle -> 0.0

-- | the extra area from glyph styling
styleBoxGlyph_ :: GlyphStyle -> Rect Double
styleBoxGlyph_ s = move p' $
  sw $ case sh of
    CircleGlyph -> (sz *) <$> one
    SquareGlyph -> (sz *) <$> one
    EllipseGlyph a -> NH.scale (Point sz (a * sz)) one
    RectSharpGlyph a -> NH.scale (Point sz (a * sz)) one
    RectRoundedGlyph a _ _ -> NH.scale (Point sz (a * sz)) one
    VLineGlyph _ -> NH.scale (Point ((s ^. #borderSize) * sz) sz) one
    HLineGlyph _ -> NH.scale (Point sz ((s ^. #borderSize) * sz)) one
    TriangleGlyph a b c -> (sz *) <$> sconcat (toRect . PointXY <$> (a :| [b, c]) :: NonEmpty (Rect Double))
    PathGlyph path' -> (sz *) <$> fromMaybe one (pathBoxes . toPathXYs . parsePath $ path')
  where
    sh = s ^. #shape
    sz = s ^. #size
    sw = padRect (0.5 * s ^. #borderSize)
    p' = fromMaybe (Point 0.0 0.0) (s ^. #translate)

projectWith_ :: Rect Double -> Rect Double -> Chart Double -> Chart Double
projectWith_ new old (RectChart s a) = RectChart s (projectOnR new old <$> a)
projectWith_ new old (TextChart s a) = TextChart s (second (projectOnP new old) <$> a)
projectWith_ new old (LineChart s a) = LineChart s (projectOnP new old <$> a)
projectWith_ new old (GlyphChart s a) = GlyphChart s (projectOnP new old <$> a)
projectWith_ new old (BlankChart a) = BlankChart (projectOnR new old <$> a)
projectWith_ new old (PathChart s a) = PathChart s (NonEmpty.fromList $ projectControls' new old a)

project_ :: Rect Double -> [Chart Double] -> [Chart Double]
project_ new cs = projectWith_ new (sboxes cs) <$> cs

boxes :: (Foldable f, Functor f) => f (Chart Double) -> Rect Double
boxes cs = foldRectUnsafe $ box_ <$> cs

sboxes :: (Foldable f, Functor f) => f (Chart Double) -> Rect Double
sboxes cs = foldRectUnsafe $ sbox_ <$> cs

-- | additive padding
padRect :: (Subtractive a) => a -> Rect a -> Rect a
padRect p (Rect x z y w) = Rect (x - p) (z + p) (y - p) (w + p)

-- | pad a Rect to remove singleton dimensions
padBox :: Rect Double -> Rect Double
padBox (Rect x z y w)
      | x == z && y == w = Rect (x - 0.5) (x + 0.5) (y - 0.5) (y + 0.5)
      | x == z = Rect (x - 0.5) (x + 0.5) y w
      | y == w = Rect x z (y - 0.5) (y + 0.5)
      | otherwise = Rect x z y w

-- | overlay a frame on some charts with some additive padding between
--
-- >>> frameChart defaultRectStyle 0.1 [Chart BlankA []]
-- [Chart {annotation = RectA (RectStyle {borderSize = 1.0e-2, borderColor = Colour 0.65 0.81 0.89 1.00, color = Colour 0.12 0.47 0.71 1.00}), xys = []},Chart {annotation = BlankA, xys = []}]
frameChart :: RectStyle -> Double -> [Chart Double] -> [Chart Double]
frameChart rs p cs = RectChart rs (padRect p (sboxes cs):|[]):cs

-- | additively pad a [Chart]
--
-- >>> import NumHask.Prelude (one)
-- >>> padChart 0.1 [Chart (RectA defaultRectStyle) [RectXY one]]
-- [Chart {annotation = RectA (RectStyle {borderSize = 1.0e-2, borderColor = Colour 0.65 0.81 0.89 1.00, color = Colour 0.12 0.47 0.71 1.00}), xys = [R -0.5 0.5 -0.5 0.5]},Chart {annotation = BlankA, xys = [R -0.605 0.605 -0.605 0.605]}]
padChart :: Double -> [Chart Double] -> [Chart Double]
padChart p cs = BlankChart (padRect p (sboxes cs):|[]):cs

-- | horizontally stack a list of list of charts (proceeding to the right) with a gap between
hori :: Double -> [[Chart Double]] -> [Chart Double]
hori _ [] = []
hori gap cs = foldl' step [] cs
  where
    step x c = x <> (move_ (Point (widthx x) (aligny x - aligny c)) <$> c)
    widthx xs = (\(Rect x' z' _ _) -> z' - x' + gap) (sboxes xs)
    aligny xs = (\(Rect _ _ y' w') -> (y' + w') / 2) (sboxes xs)

-- | vertically stack a list of charts (proceeding upwards), aligning them to the left
vert :: Double -> [[Chart Double]] -> [Chart Double]
vert _ [] = []
vert gap cs = foldl' step [] cs
  where
    step x c = x <> (move_ (Point (alignx x - alignx c) (widthy x)) <$> c)
    widthy xs = (\(Rect _ _ y' w') -> w' - y' + gap) (sboxes xs)
    alignx xs = (\(Rect x' _ _ _) -> x') (sboxes xs)

-- | stack a list of charts horizontally, then vertically
stack :: Int -> Double -> [[Chart Double]] -> [Chart Double]
stack _ _ [] = []
stack n gap cs = vert gap (hori gap <$> group' cs [])
  where
    group' [] acc = NumHask.Prelude.reverse acc
    group' x acc = group' (NumHask.Prelude.drop n x) (NumHask.Prelude.take n x : acc)

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

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Chart API
module Chart.Chart
  ( -- * Chart
    CF (..),
  )
where

import Chart.Types (RectStyle(..), TextStyle(..), LineStyle(..), GlyphStyle(..), PathStyle(..), padRect, styleBoxText, styleBoxGlyph, SvgOptions(..), HudOptions(..), Hud)
import Chart.Render
import Data.Bifunctor
import Data.Generics.Labels ()
import Data.Path
import Data.Text (Text)
import NumHask.Prelude
import NumHask.Space as NH hiding (Element, width)
import Lucid
import Data.List.NonEmpty as NonEmpty
import GHC.OverloadedLabels
import Control.Lens

-- data Labled' a = Labled' { nameL :: Text, annL :: Annotation' b, dataL :: [(b, XY a)] } deriving (Eq, Show, Generic)

-- data Chart' a = Chart' { tree :: Tree a }

-- data Chart a
-- data Style s

class CF s a | s -> a
  where
    -- data ChartData :: Type
    -- data StyleType :: Type
    box :: a -> Rect Double
    sbox :: s -> a -> Rect Double
    project :: Rect Double -> Rect Double -> a -> a
    move :: Point Double -> a -> a
    -- rotate :: Double -> ChartData -> ChartData
    draw :: s -> a -> Html ()
    drawAtts :: s -> [Attribute]


data Chart s a = Chart { style :: s, data_ :: a }

data Chart_ =
  RectChart RectStyle (NonEmpty (Rect Double)) |
  TextChart TextStyle (NonEmpty (Text, Point Double)) |
  LineChart LineStyle (NonEmpty (Point Double)) |
  GlyphChart GlyphStyle (NonEmpty (Point Double)) |
  PathChart PathStyle (NonEmpty (PathInfo Double, Point Double)) |
  BlankChart BlankStyle (NonEmpty (Rect Double))

box_ :: Chart_ -> Rect Double
box_ (RectChart _ a) = foldRectUnsafe a
box_ (TextChart _ a) = space1 $ snd <$> a
box_ (LineChart _ a) = space1 a
box_ (GlyphChart _ a) = space1 a
box_ (PathChart _ a) = pathBoxes' a
box_ (BlankChart _ a) = foldRectUnsafe a

sbox_ :: Chart_ -> Rect Double
sbox_ (RectChart s a) = foldRectUnsafe $ padRect (0.5 * view #borderSize s) <$> a
sbox_ (TextChart s a) = foldRectUnsafe $ uncurry (styleBoxText s) <$> a
sbox_ (LineChart s a) = foldRectUnsafe $ padRect (0.5 * width s) . singleton <$> a
sbox_ (GlyphChart s a) = foldRectUnsafe $ (\p -> addPoint p (styleBoxGlyph s)) <$> a
sbox_ (PathChart s a) = padRect (0.5 * view #borderSize s) (pathBoxes' a)
sbox_ (BlankChart _ a) = foldRectUnsafe a

move_ :: Point Double -> Chart_ -> Chart_
move_ p (RectChart s a) = RectChart s (addPoint p <$> a)
move_ p (TextChart s a) = TextChart s (second (p+) <$> a)
move_ p (LineChart s a) = LineChart s ((p+) <$> a)
move_ p (GlyphChart s a) = GlyphChart s ((p+) <$> a)
move_ p (PathChart s a) = PathChart s (second (p+) <$> a)
move_ p (BlankChart s a) = BlankChart s (addPoint p <$> a)

draw_ :: Chart_ -> Html ()
draw_ (RectChart _ a) = sconcat $ svgRect <$> a
draw_ (TextChart s a) = sconcat $ uncurry (svgText s) <$> a
draw_ (LineChart _ a) = svgLine (NumHask.Prelude.toList a)
draw_ (GlyphChart s a) = sconcat $ svgGlyph s <$> a
draw_ (PathChart _ a) = svgPath (NonEmpty.toList $ fst <$> a) (NonEmpty.toList $ snd <$> a)
draw_ (BlankChart _ _) = mempty

atts_ :: Chart_ -> [Attribute]
atts_ (RectChart s _) = attsRect s
atts_ (TextChart s _) = attsText s
atts_ (LineChart s _) = attsLine s
atts_ (GlyphChart s _) = attsGlyph s
atts_ (PathChart s _) = attsPath s
atts_ (BlankChart _ _) = mempty

instance CF RectStyle (Rect Double) where
  box = id
  sbox s r = padRect (0.5 * view #borderSize s) r
  project = projectOnR
  move = addPoint
  draw _ a = svgRect a
  drawAtts = attsRect

instance CF TextStyle (Text, Point Double) where
  box = singleton . snd
  sbox s (t,p) = styleBoxText s t p
  project new old = second (projectOnP new old)
  move p = second (p+)
  draw s (t,p) = svgText s t p
  drawAtts = attsText

instance CF LineStyle (NonEmpty (Point Double)) where
  box = space1
  sbox s ps = foldRectUnsafe $ padRect (0.5 * width s) . singleton <$> ps
  project new old = fmap (projectOnP new old)
  move p = fmap (p+)
  draw _ ps = svgLine (NumHask.Prelude.toList ps)
  drawAtts = attsLine

instance CF GlyphStyle (Point Double) where
  box = singleton
  sbox s p = addPoint p (styleBoxGlyph s)
  project = projectOnP
  move = (+)
  draw = svgGlyph
  drawAtts = attsGlyph

data BlankStyle deriving (Eq, Show)

instance CF BlankStyle (Rect Double) where
  box = id
  sbox _ r = r
  project = projectOnR
  move = addPoint
  draw _ _ = mempty
  drawAtts _ = mempty

instance CF PathStyle (NonEmpty (PathInfo Double, Point Double)) where
  box = pathBoxes'
  sbox s r = padRect (0.5 * view #borderSize s) (pathBoxes' r)
  project new old ps = NonEmpty.fromList $ projectControls' new old ps
  move p = fmap (second (p+))
  draw _ ps = svgPath (NonEmpty.toList $ fst <$> ps) (NonEmpty.toList $ snd <$> ps)
  drawAtts = attsPath

-- data Chart'' = Chart'' { name :: Maybe Text, style :: Maybe Styles, d :: [Chart'] } deriving (Eq, Show)

-- newtype CTree = Tree Chart'' deriving (Eq, Show)

data Styles = StyleR RectStyle | StyleL LineStyle | StyleP PathStyle | StyleT TextStyle | StyleG GlyphStyle deriving (Eq, Show)

data Data' =
  Data'Rect (NonEmpty (Rect Double)) |
  Data'Point (NonEmpty (Point Double)) |
  Data'Path (NonEmpty (PathInfo Double, Point Double)) |
  Data'Text (NonEmpty (Text, Point Double)) deriving (Eq, Show)

data Chart' =
  ChartRect RectStyle (NonEmpty (Rect Double)) |
  ChartLine LineStyle (NonEmpty (Point Double)) |
  ChartGlyph GlyphStyle (NonEmpty (Point Double)) |
  ChartText TextStyle (NonEmpty (Text, Point Double)) |
  ChartPath PathStyle (NonEmpty (PathInfo Double, Point Double)) |
  ChartBlank BlankStyle (NonEmpty (Rect Double)) deriving (Eq, Show)


{-
box' :: Chart' -> Rect Double
box' (ChartRect _ rs) = foldRectUnsafe $ box <$> rs
box' (ChartBlank _ rs) = foldRectUnsafe $ box <$> rs
box' (ChartLine _ ps) = foldRectUnsafe $ box <$> ps
-}

{-
box' :: Data' -> Rect Double
box' (Data'Rect r) = foldRectUnsafe $ box <$> r
-}

sbox' :: Data' -> Styles -> Rect Double
sbox' (Data'Rect d) (StyleR r)= foldRectUnsafe $ sbox r <$> d


draw' :: Styles -> Data' -> Html ()
draw' (StyleR r) (Data'Rect d) = sconcat $ draw r <$> d


-- | Specification of a chart for rendering to SVG
data ChartSvg' = ChartSvg'
  { svgOptions :: SvgOptions,
    hudOptions :: HudOptions,
    hudList :: [Hud Double],
    cfs :: [(Data', Styles)]
  }
  deriving (Generic)

-- boxes :: NonEmpty (Data', Styles) -> Rect Double
-- boxes cfs = uncurry box <$> cfs

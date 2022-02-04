{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | The primitive 'Chart' Type and support
module Chart.Primitive
  ( Chart (..),
    hasNoData,
    Charts (..),
    filterCharts,
    chart',
    charts',
    named,
    unnamed,
    rename,
    blank,
    group,
    box,
    sbox,
    projectWith,
    maybeProjectWith,
    moveChart,
    scaleChart,
    scaleStyle,
    colourChart,
    projectCharts,
    boxes,
    box',
    styleBoxes,
    styleBox',
    vert,
    hori,
    stack,
    frameChart,
    padChart,
    rectangularize,
    glyphize,
    overText,
    Orientation (..),
    ChartAspect (..),
  )
where

import Chart.Data
import Chart.Style
import Data.Bifunctor
import Data.Bool
import Data.Colour
import Data.Foldable
import Data.Maybe
import Data.Path
import Data.Text (Text)
import Data.Tree
import GHC.Generics
import qualified NumHask.Prelude as NH
import Optics.Core
import Prelude

-- $setup
--
-- >>> :set -XOverloadedLabels
-- >>> :set -XOverloadedStrings
-- >>> import Chart
-- >>> import Optics.Core
-- >>> let r = RectChart defaultRectStyle [one]

-- | There are 6 Chart primitives, unified as the Chart type.
--
-- - 'RectChart': based on a rectangular space in the XY-domain. A 'Rect 0 1 0 1' is the set of points on the XY Plane bounded by (0,0), (0,1), (1,0) & (1,1). Much of the library is built on Rect Double's but the base types are polymorphic.
-- - 'LineChart': based on a 'NonEmpty' 'Point' a - A non-empty list of connected straight lines. [Point 0 0, Point 1 1, Point 2 2, Point 3 3] is an example; three lines connected up to form a line from (0,0) to (3,3).
-- - 'GlyphChart': based on 'GlyphShape' which is a predefined shaped centered at a Point in XY space.
-- - 'TextChart': text centered at a point in XY space.
-- - 'PathChart': based on curvilinear paths using the SVG standards.
-- - 'BlankChart': a rectangular space that has no visual representation.
--
-- What is a Chart is usually a combination of these primitives into a tree or list of Chart
--
-- Each Chart primitive is a product of a (single) style and a NonEmpty list of data with the appropriate type.
--
-- A simple example is:
--
-- >>> let r = RectChart defaultRectStyle [one]
-- >>> r
-- RectChart (RectStyle {borderSize = 1.0e-2, borderColor = Colour 0.02 0.29 0.48 1.00, color = Colour 0.02 0.73 0.80 0.10}) [Rect -0.5 0.5 -0.5 0.5]
--
-- Using the defaults, this chart is rendered as:
--
-- > writeChartSvg "other/unit.hs" $ mempty & #hudOptions .~ defaultHudOptions & #charts .~ unnamed [r]
--
-- ![unit example](other/unit.svg)
data Chart where
  RectChart :: RectStyle -> [Rect Double] -> Chart
  LineChart :: LineStyle -> [[Point Double]] -> Chart
  GlyphChart :: GlyphStyle -> [Point Double] -> Chart
  TextChart :: TextStyle -> [(Text, Point Double)] -> Chart
  PathChart :: PathStyle -> [PathData Double] -> Chart
  BlankChart :: [Rect Double] -> Chart
  deriving (Eq, Show)

hasNoData :: Chart -> Bool
hasNoData (RectChart _ []) = True
hasNoData (LineChart _ []) = True
hasNoData (GlyphChart _ []) = True
hasNoData (TextChart _ []) = True
hasNoData (PathChart _ []) = True
hasNoData (BlankChart _) = True
hasNoData _ = False

newtype Charts a = Charts {tree :: Tree (a, [Chart])} deriving (Eq, Show, Generic)

filterCharts :: (Chart -> Bool) -> Charts a -> Charts a
filterCharts p (Charts (Node (a, cs) xs)) =
  Charts (Node (a, catMaybes (rem' <$> cs)) (tree . filterCharts p . Charts <$> xs))
  where
    rem' x = bool Nothing (Just x) (p x)

tree' :: Iso' (Charts a) (Tree (a, [Chart]))
tree' = iso tree Charts

charts' :: Traversal' (Charts a) [Chart]
charts' = tree' % traversed % _2

chart' :: Traversal' (Charts a) Chart
chart' = tree' % traversed % _2 % traversed

named :: Text -> [Chart] -> Charts (Maybe Text)
named l cs = Charts $ Node (Just l, cs) []

unnamed :: [Chart] -> Charts (Maybe Text)
unnamed cs = Charts $ Node (Nothing, cs) []

rename :: Maybe Text -> Charts (Maybe Text) -> Charts (Maybe Text)
rename l (Charts (Node (_, cs) xs)) = Charts (Node (l, cs) xs)

blank :: Rect Double -> Charts (Maybe Text)
blank r = unnamed [BlankChart [r]]

group :: Maybe Text -> [Charts (Maybe Text)] -> Charts (Maybe Text)
group name cs = Charts $ Node (name, []) (tree <$> cs)

instance Semigroup (Charts (Maybe Text)) where
  (<>) (Charts x@(Node (n, cs) xs)) (Charts x'@(Node (n', cs') xs')) =
    case (n, n') of
      (Nothing, Nothing) -> Charts $ Node (Nothing, cs <> cs') (xs <> xs')
      _ -> Charts $ Node (Nothing, []) [x, x']

instance Monoid (Charts (Maybe Text)) where
  mempty = Charts $ Node (Nothing, []) []

-- | Library functionality (rescaling, combining charts, working out axes and generally putting charts together) is driven by a box model.
--
-- 'box' provides a 'Rect' which defines the rectangle that encloses the chart (the bounding box) of the data elements of the chart.
-- >>> box r
-- Just Rect -0.5 0.5 -0.5 0.5
box :: Chart -> Maybe (Rect Double)
box (RectChart _ a) = foldRect a
box (TextChart _ a) = space1 $ snd <$> a
box (LineChart _ a) = space1 $ mconcat a
box (GlyphChart _ a) = space1 a
box (PathChart _ a) = pathBoxes a
box (BlankChart a) = foldRect a

-- | the bounding box for a chart including both data and style elements.
--
-- >>> sbox r
-- Just Rect -0.505 0.505 -0.505 0.505
--
-- In the above example, the border of the rectangle adds an extra 0.1 to the height and width of the bounding box enclosing the chart.
sbox :: Chart -> Maybe (Rect Double)
sbox (RectChart s a) = foldRect $ padRect (0.5 * view #borderSize s) <$> a
sbox (TextChart s a) = foldRect $ uncurry (styleBoxText s) <$> a
sbox (LineChart s a) = padRect (0.5 * s ^. #size) <$> (space1 $ mconcat a)
sbox (GlyphChart s a) = foldRect $ (\p -> addPoint p (styleBoxGlyph s)) <$> a
sbox (PathChart s a) = padRect (0.5 * view #borderSize s) <$> pathBoxes a
sbox (BlankChart a) = foldRect a

-- | projects a Chart to a new rectangular space from an old rectangular space, preserving linear metric structure.
--
-- >>> projectWith (fmap (2*) one) one r
-- RectChart (RectStyle {borderSize = 1.0e-2, borderColor = Colour 0.02 0.29 0.48 1.00, color = Colour 0.02 0.73 0.80 0.10}) [Rect -1.0 1.0 -1.0 1.0]
projectWith :: Rect Double -> Rect Double -> Chart -> Chart
projectWith new old (RectChart s a) = RectChart s (projectOnR new old <$> a)
projectWith new old (TextChart s a) = TextChart (projectX s) (second (projectOnP new old) <$> a)
  where
    projectX :: TextStyle -> TextStyle
    projectX s' = case view #scalex s' of
      NoScaleX -> s' & over #hsize (* (width ox / width nx)) & over #vsize (* (width ox / width nx))
      ScaleX -> s' & over #size (* (width nx / width ox))
    (Ranges nx _) = new
    (Ranges ox _) = old
projectWith new old (LineChart s a) = LineChart s (fmap (projectOnP new old) <$> a)
projectWith new old (GlyphChart s a) = GlyphChart s (projectOnP new old <$> a)
projectWith new old (BlankChart a) = BlankChart (projectOnR new old <$> a)
projectWith new old (PathChart s a) = PathChart s (projectPaths new old a)

maybeProjectWith :: Maybe (Rect Double) -> Maybe (Rect Double) -> Chart -> Chart
maybeProjectWith new old = fromMaybe id (projectWith <$> new <*> old)

-- | move a chart
moveChart :: Point Double -> Chart -> Chart
moveChart p (RectChart s a) = RectChart s (addPoint p <$> a)
moveChart p (TextChart s a) = TextChart s (second (addp p) <$> a)
moveChart p (LineChart s a) = LineChart s (fmap (addp p) <$> a)
moveChart p (GlyphChart s a) = GlyphChart s (addp p <$> a)
moveChart p (PathChart s a) = PathChart s (movePath p <$> a)
moveChart p (BlankChart a) = BlankChart (addPoint p <$> a)

-- | Scale a chart (effecting both the chart data and the style)
scaleChart :: Double -> Chart -> Chart
scaleChart p (RectChart s a) =
  RectChart (s & #borderSize %~ (* p)) (fmap (fmap (* p)) a)
scaleChart p (LineChart s a) =
  LineChart (s & #size %~ (* p)) (fmap (fmap (fmap (* p))) a)
scaleChart p (TextChart s a) =
  TextChart (s & #size %~ (* p)) (fmap (second (fmap (* p))) a)
scaleChart p (GlyphChart s a) =
  GlyphChart (s & #size %~ (* p)) (fmap (fmap (* p)) a)
scaleChart p (PathChart s a) =
  PathChart (s & #borderSize %~ (* p)) (scalePath p <$> a)
scaleChart p (BlankChart a) =
  BlankChart (fmap (fmap (* p)) a)

-- | Scale just the chart style
scaleStyle :: Double -> Chart -> Chart
scaleStyle x (LineChart a d) = LineChart (a & #size %~ (* x)) d
scaleStyle x (RectChart a d) = RectChart (a & #borderSize %~ (* x)) d
scaleStyle x (TextChart a d) = TextChart (a & #size %~ (* x)) d
scaleStyle x (GlyphChart a d) = GlyphChart (a & #size %~ (* x)) d
scaleStyle x (PathChart a d) = PathChart (a & #borderSize %~ (* x)) d
scaleStyle _ (BlankChart d) = BlankChart d

-- | Modify chart color
colourChart :: (Colour -> Colour) -> Chart -> Chart
colourChart f (RectChart s d) = RectChart s' d
  where
    s' = s & #color %~ f & #borderColor %~ f
colourChart f (TextChart s d) = TextChart s' d
  where
    s' = s & #color %~ f
colourChart f (LineChart s d) = LineChart s' d
  where
    s' = s & #color %~ f
colourChart f (GlyphChart s d) = GlyphChart s' d
  where
    s' = s & #color %~ f & #borderColor %~ f
colourChart f (PathChart s d) = PathChart s' d
  where
    s' = s & #color %~ f & #borderColor %~ f
colourChart _ (BlankChart d) = BlankChart d

-- | expands singleton dimensions, avoiding zero divides
projectCharts :: Rect Double -> [Chart] -> [Chart]
projectCharts new cs = case styleBoxes cs of
  Nothing -> cs
  Just b -> projectWith new b <$> cs

boxes :: [Chart] -> Maybe (Rect Double)
boxes cs = foldRect $ mconcat $ maybeToList . box <$> cs

box_ :: Charts a -> Maybe (Rect Double)
box_ = boxes . foldOf charts'

rebox_ :: Charts a -> Maybe (Rect Double) -> Charts a
rebox_ cs r =
  cs
    & over chart' (fromMaybe id $ projectWith <$> r <*> box_ cs)

box' :: Lens' (Charts a) (Maybe (Rect Double))
box' =
  lens box_ rebox_

styleBoxes :: [Chart] -> Maybe (Rect Double)
styleBoxes cs = foldRect $ mconcat $ maybeToList . sbox <$> cs

styleBox_ :: Charts a -> Maybe (Rect Double)
styleBox_ = styleBoxes . foldOf charts'

styleRebox_ :: Charts a -> Maybe (Rect Double) -> Charts a
styleRebox_ cs r =
  cs
    & over chart' (fromMaybe id $ projectWith <$> r' <*> box_ cs)
  where
    r' = (NH.-) <$> r <*> ((NH.-) <$> styleBox_ cs <*> box_ cs)

-- |
--
-- Note that a round trip may be only approximately isomorphic ie
--
-- > forall c r. \c -> view styleBox' . set styleBox r c ~= r
--
-- - SVG is, in general, an additive model eg a border adds a constant amount no matter the scale or aspect. TextCharts, in particular, can have small data boxes but large style additions to the box.
--
-- - Style reboxing is a multiplicative model.
--
-- In practice, this can lead to weird corner cases and unrequited distortion.
--
-- The example below starts with the unit chart, and a simple axis bar, with a dynamic overhang, so that the axis bar represents the x-axis extremity.
--
-- >>> t1 = unnamed [RectChart defaultRectStyle [one]]
-- >>> x1 h = toCharts $ mempty & set #charts t1 & set (#hudOptions % #chartAspect) (ChartAspect) & set (#hudOptions % #axes) [(1,defaultAxisOptions & over #bar (fmap (set #overhang h)) & set (#ticks % #ttick) Nothing & set (#ticks % #gtick) Nothing & set (#ticks % #ltick) Nothing)]
--
-- With a significant overhang, the axis bar dominates the extrema:
--
-- >>> view styleBox' $ set styleBox' (Just one) (x1 0.1)
-- Just Rect -0.5 0.5 -0.5 0.5
--
-- With no overhang, the style additions caused by the chart dominates:
--
-- >>> view styleBox' $ set styleBox' (Just one) (x1 0)
-- Just Rect -0.5 0.5 -0.5 0.5
--
-- In between:
--
-- >>> view styleBox' $ set styleBox' (Just one) (x1 0.002)
-- Just Rect -0.5000199203187251 0.5000199203187251 -0.5 0.5
--
--
-- If having an exact box is important, try running set styleBox' multiple times eg
--
-- >>> view styleBox' $ foldr ($) (x1 0.002) (replicate 10 (set styleBox' (Just one)))
-- Just Rect -0.5 0.5000000000000001 -0.5 0.4999999999999999
styleBox' :: Lens' (Charts a) (Maybe (Rect Double))
styleBox' =
  lens styleBox_ styleRebox_

-- | Create a frame over some charts with (additive) padding.
--
-- >>> frameChart defaultRectStyle 0.1 [BlankChart []]
-- RectChart (RectStyle {borderSize = 1.0e-2, borderColor = Colour 0.02 0.29 0.48 1.00, color = Colour 0.02 0.73 0.80 0.10}) []
frameChart :: RectStyle -> Double -> [Chart] -> Chart
frameChart rs p cs = RectChart rs (maybeToList (padRect p <$> styleBoxes cs))

-- | additively pad a [Chart]
padChart :: Double -> [Chart] -> Chart
padChart p cs = BlankChart (maybeToList (padRect p <$> styleBoxes cs))

-- | horizontally stack a list of list of charts (proceeding to the right) with a gap between
hori :: (Monoid (Charts a)) => Double -> [Charts a] -> Charts a
hori _ [] = mempty
hori gap cs = foldl' step mempty cs
  where
    step x c = x <> over chart' (moveChart (Point (widthx x) (aligny x - aligny c))) c
    widthx x = case foldOf charts' x of
      [] -> zero
      xs -> maybe zero (\(Rect x' z' _ _) -> z' - x' + gap) (styleBoxes xs)
    aligny x = case foldOf charts' x of
      [] -> zero
      xs -> maybe zero (\(Rect _ _ y' w') -> (y' + w') / 2) (styleBoxes xs)

-- | vertically stack a list of Charts (proceeding upwards), aligning them to the left
vert :: (Monoid (Charts a)) => Double -> [Charts a] -> Charts a
vert _ [] = mempty
vert gap cs = foldl' step mempty cs
  where
    step x c = x <> over chart' (moveChart (Point (alignx x - alignx c) (widthy x))) c
    widthy x = case foldOf charts' x of
      [] -> zero
      xs -> maybe zero (\(Rect _ _ y' w') -> w' - y' + gap) (styleBoxes xs)
    alignx x = case foldOf charts' x of
      [] -> zero
      xs -> maybe zero (\(Rect x' _ _ _) -> x') (styleBoxes xs)

-- | stack a list of charts horizontally, then vertically
stack :: (Monoid (Charts a)) => Int -> Double -> [Charts a] -> Charts a
stack _ _ [] = mempty
stack n gap cs = vert gap (hori gap <$> group' cs [])
  where
    group' [] acc = reverse acc
    group' x acc = group' (drop n x) (take n x : acc)

rectangularize :: RectStyle -> Charts (Maybe Text) -> Charts (Maybe Text)
rectangularize r c = group (Just "rectangularize") [over chart' (rectangularize_ r) c]

rectangularize_ :: RectStyle -> Chart -> Chart
rectangularize_ rs (TextChart s xs) = TextChart (s & #frame .~ Just rs) xs
rectangularize_ rs c = RectChart rs (maybeToList $ sbox c)

glyphize :: GlyphStyle -> Charts (Maybe Text) -> Charts (Maybe Text)
glyphize g c =
  group (Just "glyphize") [over chart' (glyphize_ g) c]

glyphize_ :: GlyphStyle -> Chart -> Chart
glyphize_ g (TextChart _ xs) = GlyphChart g (snd <$> xs)
glyphize_ g (PathChart _ xs) = GlyphChart g (pointPath <$> xs)
glyphize_ g (LineChart _ xs) = GlyphChart g (mconcat xs)
glyphize_ g (BlankChart xs) = GlyphChart g (mid <$> xs)
glyphize_ g (RectChart _ xs) = GlyphChart g (mid <$> xs)
glyphize_ g (GlyphChart _ xs) = GlyphChart g xs

overText :: (TextStyle -> TextStyle) -> Chart -> Chart
overText f (TextChart s xs) = TextChart (f s) xs
overText _ x = x

-- | Verticle or Horizontal
data Orientation = Vert | Hori deriving (Eq, Show, Generic)

-- | The basis for the x-y ratio of a chart
--
-- Default style features tend towards assuming that the usual height of the overall svg image is around 1, and ChartAspect is based on this assumption, so that a ChartAspect of "FixedAspect 1.5", say, means a height of 1 and a width of 1.5.
data ChartAspect
  = -- | Rescale charts to a fixed x-y ratio, inclusive of hud and style features
    FixedAspect Double
  | -- | Rescale charts to an overall height of 1, preserving the x-y ratio of the data canvas.
    CanvasAspect Double
  | -- | Rescale charts to a height of 1, preserving the existing x-y ratio of the underlying charts, inclusive of hud and style.
    ChartAspect
  deriving (Show, Eq, Generic)

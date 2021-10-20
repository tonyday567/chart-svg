{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wall #-}

-- | The primitive 'Chart' Type and support
module Chart.Primitive
  ( Chart(..),
    box,
    sbox,
    projectChartWith,
    moveChart,
    scaleChart,
    projectCharts,
    boxes,
    unsafeBoxes,
    styleBoxes,
    unsafeStyleBoxes,
    vert,
    hori,
    stack,
    frameChart,
    padChart,
  ) where

import Chart.Style
import Data.Bifunctor
import Data.Path
import Data.Text (Text)
import Prelude
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty(..))
import Control.Lens
import Chart.Data
import Data.Foldable
import Data.Semigroup
import Data.Maybe

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
-- >>> let r = Chart defaultRectStyle [one]
-- >>> r
-- RectChart (RectStyle {borderSize = 1.0e-2, borderColor = Colour 0.65 0.81 0.89 1.00, color = Colour 0.12 0.47 0.71 1.00}) (Rect -0.5 0.5 -0.5 0.5 :| [])
--
-- Using the defaults, this chart is rendered as:
--
-- >>> writeChartSvg "other/unit.hs" $ mempty & #chartTree .~ [r]
--
--
data Chart a =
  RectChart RectStyle (NonEmpty (Rect a)) |
  LineChart LineStyle (NonEmpty (NonEmpty (Point a))) |
  GlyphChart GlyphStyle (NonEmpty (Point a)) |
  TextChart TextStyle (NonEmpty (Text, Point a)) |
  PathChart PathStyle (NonEmpty (PathInfo a, Point a)) |
  BlankChart (NonEmpty (Rect a)) deriving (Eq, Show)

-- | Library functionality (rescaling, combining charts, working out axes and generally putting charts together) is driven by a box model.
--
-- 'box' provides a 'Rect' which defines the rectangle that encloses the chart (the bounding box) of the data elements of the chart.
-- >>> box r
--
box :: Chart Double -> Rect Double
box (RectChart _ a) = foldRectUnsafe a
box (TextChart _ a) = space1 $ snd <$> a
box (LineChart _ a) = space1 $ sconcat a
box (GlyphChart _ a) = space1 a
box (PathChart _ a) = pathBoxes' a
box (BlankChart a) = foldRectUnsafe a

-- | the bonding box for a chart including both data and style elements.
--
-- >>> sbox r
-- Rect -0.505 0.505 -0.505 0.505
--
-- In our simplest example, the border of the rectangle adds an extra 0.1 to the height and width of the bounding box enclosing the chart.
--
sbox :: Chart Double -> Rect Double
sbox (RectChart s a) = foldRectUnsafe $ padRect (0.5 * view #borderSize s) <$> a
sbox (TextChart s a) = foldRectUnsafe $ uncurry (styleBoxText s) <$> a
sbox (LineChart s a) = padRect (0.5 * Chart.Style.width s) $ space1 $ sconcat a
sbox (GlyphChart s a) = foldRectUnsafe $ (\p -> addPoint p (styleBoxGlyph s)) <$> a
sbox (PathChart s a) = padRect (0.5 * view #borderSize s) (pathBoxes' a)
sbox (BlankChart a) = foldRectUnsafe a

-- | projects a Chart to a new rectangular space from an old rectangular space, preserving linear metric structure.
--
-- >>> projectChartWith (fmap (2*) one) one r
--
projectChartWith :: Rect Double -> Rect Double -> Chart Double -> Chart Double
projectChartWith new old (RectChart s a) = RectChart s (projectOnR new old <$> a)
projectChartWith new old (TextChart s a) = TextChart s (second (projectOnP new old) <$> a)
projectChartWith new old (LineChart s a) = LineChart s (fmap (projectOnP new old) <$> a)
projectChartWith new old (GlyphChart s a) = GlyphChart s (projectOnP new old <$> a)
projectChartWith new old (BlankChart a) = BlankChart (projectOnR new old <$> a)
projectChartWith new old (PathChart s a) = PathChart s (NonEmpty.fromList $ projectControls' new old a)

-- | move a chart
moveChart :: Point Double -> Chart Double -> Chart Double
moveChart p (RectChart s a) = RectChart s (addPoint p <$> a)
moveChart p (TextChart s a) = TextChart s (second (addp p) <$> a)
moveChart p (LineChart s a) = LineChart s (fmap (addp p) <$> a)
moveChart p (GlyphChart s a) = GlyphChart s (addp p <$> a)
moveChart p (PathChart s a) = PathChart s (second (addp p) <$> a)
moveChart p (BlankChart a) = BlankChart (addPoint p <$> a)

-- | Scale a chart (effecting both the chart data and the style)
--
scaleChart :: Double -> Chart Double -> Chart Double
scaleChart p (RectChart s a) =
  RectChart (s & #borderSize %~ (* p)) (fmap (fmap (*p)) a)
scaleChart p (LineChart s a) =
  LineChart (s & #width %~ (* p)) (fmap (fmap (fmap (*p))) a)
scaleChart p (TextChart s a) =
  TextChart (s & #size %~ (* p)) (fmap (second (fmap (*p))) a)
scaleChart p (GlyphChart s a) =
  GlyphChart (s & #size %~ (* p)) (fmap (fmap (*p)) a)
scaleChart p (PathChart s a) =
  PathChart (s & #borderSize %~ (* p)) (fmap (second (fmap (*p))) a)
scaleChart p (BlankChart a) =
  BlankChart (fmap (fmap (*p)) a)

-- | expands singleton dimensions, avoiding zero divides
projectCharts :: Rect Double -> [Chart Double] -> [Chart Double]
projectCharts new cs = projectChartWith new (styleBoxes cs) <$> cs

boxes :: (Foldable f, Functor f) => f (Chart Double) -> Rect Double
boxes cs = padSingletons $ fromMaybe one $ foldRect $ toList $ box <$> cs

styleBoxes :: (Foldable f, Functor f) => f (Chart Double) -> Rect Double
styleBoxes cs = padSingletons $ fromMaybe one $ foldRect $ toList $ sbox <$> cs

unsafeBoxes :: (Foldable f, Functor f) => f (Chart Double) -> Rect Double
unsafeBoxes cs = foldRectUnsafe $ box <$> cs

unsafeStyleBoxes :: (Foldable f, Functor f) => f (Chart Double) -> Rect Double
unsafeStyleBoxes cs = foldRectUnsafe $ sbox <$> cs

-- | overlay a frame on some charts with some additive padding between
--
-- >>> frameChart defaultRectStyle 0.1 [Chart BlankA []]
-- [Chart {annotation = RectA (RectStyle {borderSize = 1.0e-2, borderColor = Colour 0.65 0.81 0.89 1.00, color = Colour 0.12 0.47 0.71 1.00}), xys = []},Chart {annotation = BlankA, xys = []}]
frameChart :: RectStyle -> Double -> [Chart Double] -> [Chart Double]
frameChart rs p cs = RectChart rs (padRect p (styleBoxes cs):|[]):cs

-- | additively pad a [Chart]
--
-- >>> import NumHask.Prelude (one)
-- >>> padChart 0.1 [Chart (RectA defaultRectStyle) [RectXY one]]
-- [Chart {annotation = RectA (RectStyle {borderSize = 1.0e-2, borderColor = Colour 0.65 0.81 0.89 1.00, color = Colour 0.12 0.47 0.71 1.00}), xys = [R -0.5 0.5 -0.5 0.5]},Chart {annotation = BlankA, xys = [R -0.605 0.605 -0.605 0.605]}]
padChart :: Double -> [Chart Double] -> [Chart Double]
padChart p cs = BlankChart (padRect p (styleBoxes cs):|[]):cs

-- | horizontally stack a list of list of charts (proceeding to the right) with a gap between
hori :: Double -> [[Chart Double]] -> [Chart Double]
hori _ [] = []
hori gap cs = foldl' step [] cs
  where
    step x c = x <> (moveChart (Point (widthx x) (aligny x - aligny c)) <$> c)
    widthx [] = zero
    widthx xs = (\(Rect x' z' _ _) -> z' - x' + gap) (styleBoxes xs)
    aligny [] = zero
    aligny xs = (\(Rect _ _ y' w') -> (y' + w') / 2) (styleBoxes xs)

-- | vertically stack a list of charts (proceeding upwards), aligning them to the left
vert :: Double -> [[Chart Double]] -> [Chart Double]
vert _ [] = []
vert gap cs = foldl' step [] cs
  where
    step x c = x <> (moveChart (Point (alignx x - alignx c) (widthy x)) <$> c)
    widthy [] = zero
    widthy xs = (\(Rect _ _ y' w') -> w' - y' + gap) (styleBoxes xs)
    alignx [] = zero
    alignx xs = (\(Rect x' _ _ _) -> x') (styleBoxes xs)

-- | stack a list of charts horizontally, then vertically
stack :: Int -> Double -> [[Chart Double]] -> [Chart Double]
stack _ _ [] = []
stack n gap cs = vert gap (hori gap <$> group' cs [])
  where
    group' [] acc = reverse acc
    group' x acc = group' (drop n x) (take n x : acc)

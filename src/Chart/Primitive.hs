{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}

-- | The primitive 'Chart' Type and support
module Chart.Primitive
  ( Chart(..),
    Charts(..),
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
    moveChart,
    scaleChart,
    scaleStyle,
    colourChart,
    projectCharts,
    boxes,
    box',
    unsafeBoxes,
    styleBoxes,
    styleBox',
    unsafeStyleBoxes,
    vert,
    hori,
    stack,
    frameChart,
    padChart,
    rectangularize,
    glyphize,
    overText,
    Orientation (..),
    fromOrientation,
    toOrientation,
  ) where

import Chart.Style
import Data.Bifunctor
import Data.Path
import Data.Text (Text)
import Prelude
import Data.List.NonEmpty (NonEmpty(..))
import Optics.Core
import Chart.Data
import Data.Foldable
import Data.Semigroup
import Data.Maybe
import Data.Colour
import Data.String
import GHC.Generics
import Data.Tree

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
-- >>> writeChartSvg "other/unit.hs" $ mempty & #charts .~ [r]
--
--
data Chart =
  RectChart RectStyle (NonEmpty (Rect Double)) |
  LineChart LineStyle (NonEmpty (NonEmpty (Point Double))) |
  GlyphChart GlyphStyle (NonEmpty (Point Double)) |
  TextChart TextStyle (NonEmpty (Text, Point Double)) |
  PathChart PathStyle (NonEmpty (PathData Double)) |
  BlankChart (NonEmpty (Rect Double)) deriving (Eq, Show)

newtype Charts = Charts { tree :: Tree (Maybe Text, [Chart]) } deriving (Eq, Show, Generic)

tree' :: Iso' Charts (Tree (Maybe Text, [Chart]))
tree' = iso tree Charts

charts' :: Traversal' Charts [Chart]
charts' = tree' % traversed % _2

chart' :: Traversal' Charts Chart
chart' = tree' % traversed % _2 % traversed

named :: Text -> [Chart] -> Charts
named l cs = Charts $ Node (Just l,cs) []

unnamed :: [Chart] -> Charts
unnamed cs = Charts $ Node (Nothing,cs) []

rename :: Maybe Text -> Charts -> Charts
rename l (Charts (Node (_,cs) xs)) = Charts (Node (l,cs) xs)

blank :: Rect Double -> Charts
blank r = unnamed [BlankChart [r]]

group :: Maybe Text -> [Charts] -> Charts
group name cs = Charts $ Node (name, []) (tree <$> cs)

instance Semigroup Charts where
  (<>) (Charts x@(Node (n, cs) xs)) (Charts x'@(Node (n', cs') xs')) =
    case (n,n') of
      (Nothing, Nothing) -> Charts $ Node (Nothing, cs <> cs') (xs <> xs')
      _ -> Charts $ Node (Nothing, []) [x, x']

instance Monoid Charts where
  mempty = Charts $ Node (Nothing, []) []

-- | Library functionality (rescaling, combining charts, working out axes and generally putting charts together) is driven by a box model.
--
-- 'box' provides a 'Rect' which defines the rectangle that encloses the chart (the bounding box) of the data elements of the chart.
-- >>> box r
--
box :: Chart -> Rect Double
box (RectChart _ a) = foldRectUnsafe a
box (TextChart _ a) = space1 $ snd <$> a
box (LineChart _ a) = space1 $ sconcat a
box (GlyphChart _ a) = space1 a
box (PathChart _ a) = pathBoxes a
box (BlankChart a) = foldRectUnsafe a

-- | the bounding box for a chart including both data and style elements.
--
-- >>> sbox r
-- Rect -0.505 0.505 -0.505 0.505
--
-- In our simplest example, the border of the rectangle adds an extra 0.1 to the height and width of the bounding box enclosing the chart.
--
sbox :: Chart -> Rect Double
sbox (RectChart s a) = foldRectUnsafe $ padRect (0.5 * view #borderSize s) <$> a
sbox (TextChart s a) = foldRectUnsafe $ uncurry (styleBoxText s) <$> a
sbox (LineChart s a) = padRect (0.5 * s ^. #size) $ space1 $ sconcat a
sbox (GlyphChart s a) = foldRectUnsafe $ (\p -> addPoint p (styleBoxGlyph s)) <$> a
sbox (PathChart s a) = padRect (0.5 * view #borderSize s) (pathBoxes a)
sbox (BlankChart a) = foldRectUnsafe a

-- | projects a Chart to a new rectangular space from an old rectangular space, preserving linear metric structure.
--
-- >>> projectWith (fmap (2*) one) one r
--
projectWith :: Rect Double -> Rect Double -> Chart -> Chart
projectWith new old (RectChart s a) = RectChart s (projectOnR new old <$> a)
projectWith new old (TextChart s a) = TextChart (projectX s) (second (projectOnP new old) <$> a)
  where
    projectX :: TextStyle -> TextStyle
    projectX s' = case view #scalex s' of
      NoScaleX -> s' & over #hsize (*(width ox/width nx)) & over #vsize (*(width ox/width nx))
      ScaleX -> s' & over #size (*(width nx/width ox))
    (Ranges nx _) = new
    (Ranges ox _) = old
projectWith new old (LineChart s a) = LineChart s (fmap (projectOnP new old) <$> a)
projectWith new old (GlyphChart s a) = GlyphChart s (projectOnP new old <$> a)
projectWith new old (BlankChart a) = BlankChart (projectOnR new old <$> a)
projectWith new old (PathChart s a) = PathChart s (projectPaths new old a)

-- | move a chart
moveChart :: Point Double -> Chart -> Chart
moveChart p (RectChart s a) = RectChart s (addPoint p <$> a)
moveChart p (TextChart s a) = TextChart s (second (addp p) <$> a)
moveChart p (LineChart s a) = LineChart s (fmap (addp p) <$> a)
moveChart p (GlyphChart s a) = GlyphChart s (addp p <$> a)
moveChart p (PathChart s a) = PathChart s (movePath p <$> a)
moveChart p (BlankChart a) = BlankChart (addPoint p <$> a)

-- | Scale a chart (effecting both the chart data and the style)
--
scaleChart :: Double -> Chart -> Chart
scaleChart p (RectChart s a) =
  RectChart (s & #borderSize %~ (* p)) (fmap (fmap (*p)) a)
scaleChart p (LineChart s a) =
  LineChart (s & #size %~ (* p)) (fmap (fmap (fmap (*p))) a)
scaleChart p (TextChart s a) =
  TextChart (s & #size %~ (* p)) (fmap (second (fmap (*p))) a)
scaleChart p (GlyphChart s a) =
  GlyphChart (s & #size %~ (* p)) (fmap (fmap (*p)) a)
scaleChart p (PathChart s a) =
  PathChart (s & #borderSize %~ (* p)) (scalePath p <$> a)
scaleChart p (BlankChart a) =
  BlankChart (fmap (fmap (*p)) a)

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
projectCharts new cs = projectWith new (styleBoxes cs) <$> cs

boxes :: (Foldable f, Functor f) => f Chart -> Rect Double
boxes cs = padSingletons $ fromMaybe one $ foldRect $ toList $ box <$> cs

box_ :: Charts -> Rect Double
box_ = boxes . foldOf charts'

rebox_ :: Charts -> Rect Double -> Charts
rebox_ cs r =
  cs &
  over chart' (projectWith r (box_ cs))

box' :: Lens' Charts (Rect Double)
box' =
  lens box_ rebox_

styleBoxes :: (Foldable f, Functor f) => f Chart -> Rect Double
styleBoxes cs = padSingletons $ fromMaybe one $ foldRect $ toList $ sbox <$> cs

styleBox_ :: Charts -> Rect Double
styleBox_ = styleBoxes . foldOf charts'

styleRebox_ :: Charts -> Rect Double -> Charts
styleRebox_ cs r =
  cs &
  over chart' (projectWith r (styleBox_ cs))

styleBox' :: Lens' Charts (Rect Double)
styleBox' =
  lens styleBox_ styleRebox_

unsafeBoxes :: (Foldable f, Functor f) => f Chart -> Rect Double
unsafeBoxes cs = foldRectUnsafe $ box <$> cs

unsafeStyleBoxes :: (Foldable f, Functor f) => f Chart -> Rect Double
unsafeStyleBoxes cs = foldRectUnsafe $ sbox <$> cs

-- | Create a frame over some charts with (additive) padding.
--
-- >>> frameChart defaultRectStyle 0.1 [Chart BlankA []]
-- [Chart {annotation = RectA (RectStyle {borderSize = 1.0e-2, borderColor = Colour 0.65 0.81 0.89 1.00, color = Colour 0.12 0.47 0.71 1.00}), xys = []},Chart {annotation = BlankA, xys = []}]
frameChart :: RectStyle -> Double -> [Chart] -> Chart
frameChart rs p cs = RectChart rs [padRect p (styleBoxes cs)]

-- | additively pad a [Chart]
--
padChart :: Double -> [Chart] -> Chart
padChart p cs = BlankChart [padRect p (styleBoxes cs)]

-- | horizontally stack a list of list of charts (proceeding to the right) with a gap between
hori :: Double -> [Charts] -> Charts
hori _ [] = mempty
hori gap cs = foldl' step mempty cs
  where
    step x c = x <> over chart' (moveChart (Point (widthx x) (aligny x - aligny c))) c
    widthx x = case foldOf charts' x of
      [] -> zero
      xs -> (\(Rect x' z' _ _) -> z' - x' + gap) (styleBoxes xs)
    aligny x = case foldOf charts' x of
      [] -> zero
      xs -> (\(Rect _ _ y' w') -> (y' + w') / 2) (styleBoxes xs)

-- | vertically stack a list of Charts (proceeding upwards), aligning them to the left
vert :: Double -> [Charts] -> Charts
vert _ [] = mempty
vert gap cs = foldl' step mempty cs
  where
    step :: Charts -> Charts -> Charts
    step x c = x <> over chart' (moveChart (Point (alignx x - alignx c) (widthy x))) c
    widthy x = case foldOf charts' x of
      [] -> zero
      xs -> (\(Rect _ _ y' w') -> w' - y' + gap) (styleBoxes xs)
    alignx x = case foldOf charts' x of
      [] -> zero
      xs -> (\(Rect x' _ _ _) -> x') (styleBoxes xs)

-- | stack a list of charts horizontally, then vertically
stack :: Int -> Double -> [Charts] -> Charts
stack _ _ [] = mempty
stack n gap cs = vert gap (hori gap <$> group' cs [])
  where
    group' [] acc = reverse acc
    group' x acc = group' (drop n x) (take n x : acc)

rectangularize :: RectStyle -> Charts -> Charts
rectangularize r c = group (Just "rectangularize") [over chart' (rectangularize_ r) c]

rectangularize_ :: RectStyle -> Chart -> Chart
rectangularize_ rs (TextChart s xs) = TextChart (s & #frame .~ Just rs) xs
rectangularize_ rs c = RectChart rs [sbox c]

glyphize :: GlyphStyle -> Charts -> Charts
glyphize g c =
  group (Just "glyphize") [over chart' (glyphize_ g) c]

glyphize_ :: GlyphStyle -> Chart -> Chart
glyphize_ g (TextChart _ xs) = GlyphChart g (snd <$> xs)
glyphize_ g (PathChart _ xs) = GlyphChart g (pointPath <$> xs)
glyphize_ g (LineChart _ xs) = GlyphChart g (sconcat xs)
glyphize_ g (BlankChart xs) = GlyphChart g (mid <$> xs)
glyphize_ g (RectChart _ xs) = GlyphChart g (mid <$> xs)
glyphize_ g (GlyphChart _ xs) = GlyphChart g xs

overText :: (TextStyle -> TextStyle) -> Chart -> Chart
overText f (TextChart s xs) = TextChart (f s) xs
overText _ x = x

-- | Verticle or Horizontal
data Orientation = Vert | Hori deriving (Eq, Show, Generic)

-- | textifier
fromOrientation :: (IsString s) => Orientation -> s
fromOrientation Hori = "Hori"
fromOrientation Vert = "Vert"

-- | readifier
toOrientation :: (Eq s, IsString s) => s -> Orientation
toOrientation "Hori" = Hori
toOrientation "Vert" = Vert
toOrientation _ = Hori

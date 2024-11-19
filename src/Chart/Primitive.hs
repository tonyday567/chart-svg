{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RebindableSyntax #-}

-- | Base 'Chart' and 'ChartTree' types and support
module Chart.Primitive
  ( -- * Charts
    Chart (..),
    ChartData (..),
    rectData',
    lineData',
    glyphData',
    textData',
    pathData',
    blankData',
    pattern RectChart,
    pattern LineChart,
    pattern GlyphChart,
    pattern TextChart,
    pattern PathChart,
    pattern BlankChart,
    pattern LineChart1,
    blankChart1,
    ChartTree (..),
    tree',
    chart',
    charts',
    named,
    unnamed,
    renamed,
    rename,
    blank,
    group,
    filterChartTree,
    Orientation (..),
    Stacked (..),
    ChartAspect (..),

    -- * Boxes
    -- $boxes
    box,
    sbox,
    projectWith,
    projectChartDataWith,
    moveChartData,
    moveChart,
    scaleChart,
    scaleChartData,
    colourStyle,
    projectChartTree,
    boxes,
    box',
    styleBoxes,
    styleBox',
    safeBox',
    safeStyleBox',

    -- * Combinators
    vert,
    hori,
    stack,
    besideChart,
    frameChart,
    isEmptyChart,
    padChart,
    rectangularize,
    glyphize,

    -- * Relative position
    Align (..),
    Place (..),
    flipPlace,
    beside,
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
import NumHask.Prelude
import NumHask.Space
import Optics.Core

-- $setup
--
-- >>> :m -Prelude
-- >>> :set -XOverloadedLabels
-- >>> :set -XOverloadedStrings
-- >>> import Chart
-- >>> import Optics.Core
-- >>> import NumHask.Prelude
-- >>> let r = RectChart defaultRectStyle [one]

-- | A product type consisting of a 'Style', which is the stylistic manifestation of chart data, and 'ChartData' representing where data is located on the chart canvas (an xy-plane).
--
-- A simple example is:
--
-- >>> Chart defaultRectStyle (RectData [one])
-- Chart {chartStyle = Style {size = 6.0e-2, borderSize = 1.0e-2, color = Colour 0.02 0.73 0.80 0.10, borderColor = Colour 0.02 0.29 0.48 1.00, scaleP = NoScaleP, textAnchor = AnchorMiddle, rotation = Nothing, translate = Nothing, escapeText = EscapeText, frame = Nothing, lineCap = Nothing, lineJoin = Nothing, dasharray = Nothing, dashoffset = Nothing, hsize = 0.6, vsize = 1.1, vshift = -0.25, glyphShape = SquareGlyph}, chartData = RectData [Rect (-0.5) 0.5 (-0.5) 0.5]}
--
-- Using the defaults, this chart is rendered as:
--
-- > writeChartOptions "other/unit.hs" $ mempty & #hudOptions .~ defaultHudOptions & #chartTree .~ unnamed [r]
--
-- ![unit example](other/unit.svg)
data Chart = Chart {chartStyle :: Style, chartData :: ChartData} deriving (Eq, Show, Generic)

-- | Data of a 'Chart'
--
-- A sum type representing the data behind six different types of chart:
--
-- - 'RectData': a list of rectangles in the XY-domain. For example, a @'Rect' 0 1 0 1@ is the set of points on the XY Plane bounded by (0,0), (0,1), (1,0) & (1,1). Much of the library is built on 'Rect' Doubles.
-- - 'LineData': a list of (list of points) which represent connected straight lines. ['Point' 0 0, 'Point' 1 1, 'Point' 2 2, 'Point' 3 3] is an example; three lines connected up to form a line from (0,0) to (3,3).
-- - 'GlyphData': a list of points to draw a 'GlyphShape'.
-- - 'TextData': A list of Text,Point tuples representing text centered at a 'Point' in XY space.
-- - 'PathData': specification of curvilinear paths using the SVG standards.
-- - 'BlankData': a rectangular space that has no visual representation.
data ChartData
  = -- | List of rectangles
    RectData [Rect Double]
  | -- | List of (List of Points)
    LineData [[Point Double]]
  | -- | List of Points (to place the 'GlyphShape')
    GlyphData [Point Double]
  | -- | List of text and point to place it.
    TextData [(Text, Point Double)]
  | -- | List of paths
    PathData [PathData Double]
  | -- | List of rectangles with no 'Style' representation
    BlankData [Rect Double]
  deriving (Eq, Show, Generic)

-- | RectData partial lens
rectData' :: Lens' ChartData (Maybe [Rect Double])
rectData' =
  lens getData setData
  where
    getData (RectData xs) = Just xs
    getData _ = Nothing
    setData (RectData _) (Just xs) = RectData xs
    setData cd _ = cd

-- | LineData partial lens
lineData' :: Lens' ChartData (Maybe [[Point Double]])
lineData' =
  lens getData setData
  where
    getData (LineData xs) = Just xs
    getData _ = Nothing
    setData (LineData _) (Just xs) = LineData xs
    setData cd _ = cd

-- | GlyphData partial lens
glyphData' :: Lens' ChartData (Maybe [Point Double])
glyphData' =
  lens getData setData
  where
    getData (GlyphData xs) = Just xs
    getData _ = Nothing
    setData (GlyphData _) (Just xs) = GlyphData xs
    setData cd _ = cd

-- | TextData partial lens
textData' :: Lens' ChartData (Maybe [(Text, Point Double)])
textData' =
  lens getData setData
  where
    getData (TextData xs) = Just xs
    getData _ = Nothing
    setData (TextData _) (Just xs) = TextData xs
    setData cd _ = cd

-- | PathData partial lens
pathData' :: Lens' ChartData (Maybe [PathData Double])
pathData' =
  lens getData setData
  where
    getData (PathData xs) = Just xs
    getData _ = Nothing
    setData (PathData _) (Just xs) = PathData xs
    setData cd _ = cd

-- | BlankData partial lens
blankData' :: Lens' ChartData (Maybe [Rect Double])
blankData' =
  lens getData setData
  where
    getData (BlankData xs) = Just xs
    getData _ = Nothing
    setData (BlankData _) (Just xs) = BlankData xs
    setData cd _ = cd

-- | pattern of a Chart with RectData
pattern RectChart :: Style -> [Rect Double] -> Chart
pattern RectChart s xs = Chart s (RectData xs)

{-# COMPLETE RectChart #-}

-- | pattern of a Chart with LineData
pattern LineChart :: Style -> [[Point Double]] -> Chart
pattern LineChart s xss = Chart s (LineData xss)

{-# COMPLETE LineChart #-}

-- | pattern of a Chart with a singleton LineData
pattern LineChart1 :: Style -> [Point Double] -> Chart
pattern LineChart1 s xs = Chart s (LineData [xs])

{-# COMPLETE LineChart1 #-}

-- | pattern of a Chart with GlyphData
pattern GlyphChart :: Style -> [Point Double] -> Chart
pattern GlyphChart s xs = Chart s (GlyphData xs)

{-# COMPLETE GlyphChart #-}

-- | pattern of a Chart with TextData
pattern TextChart :: Style -> [(Text, Point Double)] -> Chart
pattern TextChart s xs = Chart s (TextData xs)

{-# COMPLETE TextChart #-}

-- | pattern of a Chart with PathData
pattern PathChart :: Style -> [PathData Double] -> Chart
pattern PathChart s xs = Chart s (PathData xs)

{-# COMPLETE PathChart #-}

-- | pattern of a Chart with BlankData
pattern BlankChart :: Style -> [Rect Double] -> Chart
pattern BlankChart s xs = Chart s (BlankData xs)

{-# COMPLETE BlankChart #-}

-- | Create a blank Chart with a single Rect
blankChart1 :: Rect Double -> Chart
blankChart1 r = Chart defaultStyle (BlankData [r])

-- | A group of charts represented by a 'Tree' of chart lists with labelled branches. The labelling is particularly useful downstream, when groupings become grouped SVG elements with classes or ids.
newtype ChartTree = ChartTree {tree :: Tree (Maybe Text, [Chart])} deriving (Eq, Show, Generic)

-- | Group a list of trees into a new tree.
group :: Maybe Text -> [ChartTree] -> ChartTree
group name cs = ChartTree $ Node (name, []) (tree <$> cs)

instance Semigroup ChartTree where
  (<>) (ChartTree x@(Node (n, cs) xs)) (ChartTree x'@(Node (n', cs') xs')) =
    case (n, n') of
      (Nothing, Nothing) -> ChartTree $ Node (Nothing, cs <> cs') (xs <> xs')
      _ -> ChartTree $ Node (Nothing, []) [x, x']

instance Monoid ChartTree where
  mempty = ChartTree $ Node (Nothing, []) []

-- | Apply a filter to a 'ChartTree'
filterChartTree :: (Chart -> Bool) -> ChartTree -> ChartTree
filterChartTree p (ChartTree (Node (a, cs) xs)) =
  ChartTree (Node (a, mapMaybe rem' cs) (tree . filterChartTree p . ChartTree <$> xs))
  where
    rem' x = bool Nothing (Just x) (p x)

-- | Lens between ChartTree and the underlying Tree representation
tree' :: Iso' ChartTree (Tree (Maybe Text, [Chart]))
tree' = iso tree ChartTree

-- | A traversal of each chart list in a tree.
charts' :: Traversal' ChartTree [Chart]
charts' = tree' % traversed % _2

-- | A traversal of each chart in a tree.
chart' :: Traversal' ChartTree Chart
chart' = tree' % traversed % _2 % traversed

-- | Convert a chart list to a tree, adding a specific text label.
named :: Text -> [Chart] -> ChartTree
named l cs = ChartTree $ Node (Just l, cs) []

-- | Convert a chart list to a tree, with no text label.
unnamed :: [Chart] -> ChartTree
unnamed cs = ChartTree $ Node (Nothing, cs) []

-- | Rename a ChartTree, removing descendent names
renamed :: Text -> ChartTree -> ChartTree
renamed l ct = named l $ foldOf charts' ct

-- | Rename a top-level label in a tree.
rename :: Maybe Text -> ChartTree -> ChartTree
rename l (ChartTree (Node (_, cs) xs)) = ChartTree (Node (l, cs) xs)

-- | A tree with no charts and no label.
blank :: Rect Double -> ChartTree
blank r = unnamed [Chart defaultStyle (BlankData [r])]

-- $boxes
--
-- Library functionality (rescaling, combining charts, working out axes and generally putting charts together) is driven by a box model. A box is a rectangular space that bounds chart elements.

-- | The 'Rect' which encloses the data elements of the chart. /Bounding box/ is a synonym.
--
-- >>> box (chartData r)
-- Just Rect (-0.5) 0.5 (-0.5) 0.5
box :: ChartData -> Maybe (Rect Double)
box (RectData a) = foldRect a
box (TextData a) = space1 $ snd <$> a
box (LineData a) = space1 $ mconcat a
box (GlyphData a) = space1 a
box (PathData a) = pathBoxes a
box (BlankData a) = foldRect a

-- | The bounding box for a chart including both data and style elements.
--
-- >>> sbox r
-- Just Rect (-0.505) 0.505 (-0.505) 0.505
--
-- In the above example, the border of the rectangle adds an extra 0.1 to the height and width of the bounding box enclosing the chart.
sbox :: Chart -> Maybe (Rect Double)
sbox (Chart s (RectData a)) = foldRect $ padRect (0.5 * view #borderSize s) <$> a
sbox (Chart s (TextData a)) = foldRect $ uncurry (styleBoxText s) <$> a
sbox (Chart s (LineData a)) = padRect (0.5 * view #size s) <$> (space1 $ mconcat a)
sbox (Chart s (GlyphData a)) = foldRect $ (\x -> addPoint x (styleBoxGlyph s)) <$> a
sbox (Chart s (PathData a)) = padRect (0.5 * view #borderSize s) <$> pathBoxes a
sbox (Chart _ (BlankData a)) = foldRect a

-- | projects a Chart to a new space from an old rectangular space, preserving linear metric structure.
--
-- >>> projectWith (fmap (2*) one) one r
-- Chart {chartStyle = Style {size = 6.0e-2, borderSize = 1.0e-2, color = Colour 0.02 0.73 0.80 0.10, borderColor = Colour 0.02 0.29 0.48 1.00, scaleP = NoScaleP, textAnchor = AnchorMiddle, rotation = Nothing, translate = Nothing, escapeText = EscapeText, frame = Nothing, lineCap = Nothing, lineJoin = Nothing, dasharray = Nothing, dashoffset = Nothing, hsize = 0.6, vsize = 1.1, vshift = -0.25, glyphShape = SquareGlyph}, chartData = RectData [Rect (-1.0) 1.0 (-1.0) 1.0]}
projectWith :: Rect Double -> Rect Double -> Chart -> Chart
projectWith new old c = c & over #chartStyle (scaleStyle (scaleRatio (view (#chartStyle % #scaleP) c) new old)) & over #chartData (projectChartDataWith new old)

-- | Projects 'ChartData' from an old space to a new space.
projectChartDataWith :: Rect Double -> Rect Double -> ChartData -> ChartData
projectChartDataWith new old (RectData a) = RectData (projectOnR new old <$> a)
projectChartDataWith new old (TextData a) = TextData (second (projectOnP new old) <$> a)
projectChartDataWith new old (LineData a) = LineData (fmap (projectOnP new old) <$> a)
projectChartDataWith new old (GlyphData a) = GlyphData (projectOnP new old <$> a)
projectChartDataWith new old (PathData a) = PathData (projectPaths new old a)
projectChartDataWith new old (BlankData a) = BlankData (projectOnR new old <$> a)

-- | Move 'ChartData' by a 'Point'
moveChartData :: Point Double -> ChartData -> ChartData
moveChartData p (RectData a) = RectData (addPoint p <$> a)
moveChartData p (TextData a) = TextData (second (addp p) <$> a)
moveChartData p (LineData a) = LineData (fmap (addp p) <$> a)
moveChartData p (GlyphData a) = GlyphData (addp p <$> a)
moveChartData p (PathData a) = PathData (movePath p <$> a)
moveChartData p (BlankData a) = BlankData (addPoint p <$> a)

-- | Move a chart.
moveChart :: Point Double -> Chart -> Chart
moveChart p c = c & over #chartData (moveChartData p)

-- | Scale 'ChartData'
scaleChartData :: Double -> ChartData -> ChartData
scaleChartData p (RectData a) =
  RectData (fmap (fmap (* p)) a)
scaleChartData p (LineData a) =
  LineData (fmap (fmap (fmap (* p))) a)
scaleChartData p (TextData a) =
  TextData (fmap (second (fmap (* p))) a)
scaleChartData p (GlyphData a) =
  GlyphData (fmap (fmap (* p)) a)
scaleChartData p (PathData a) =
  PathData (scalePath p <$> a)
scaleChartData p (BlankData a) =
  BlankData (fmap (fmap (* p)) a)

-- | Scale a chart (effecting both the chart data and the style, if /#style % #scaleP/ is a scaling value).
scaleChart :: Double -> Chart -> Chart
scaleChart p c = c & over #chartData (scaleChartData p) & over #chartStyle (bool (scaleStyle p) id (view (#chartStyle % #scaleP) c == NoScaleP))

-- | Modify chart colors, applying to both border and main colors.
colourStyle :: (Colour -> Colour) -> Style -> Style
colourStyle f s = s & over #color f & over #borderColor f

-- | Project a chart tree to a new bounding box, guarding against singleton bounds.
projectChartTree :: Rect Double -> ChartTree -> ChartTree
projectChartTree new ct = case view styleBox' ct of
  Nothing -> ct
  Just b -> ct & over charts' (fmap (projectWith new b))

-- | Compute the bounding box of a list of charts, not including style allowances.
boxes :: [Chart] -> Maybe (Rect Double)
boxes cs = foldRect $ mconcat $ (maybeToList . box) . chartData <$> cs

box_ :: ChartTree -> Maybe (Rect Double)
box_ = boxes . foldOf charts'

rebox_ :: ChartTree -> Maybe (Rect Double) -> ChartTree
rebox_ cs r =
  cs
    & over chart' (fromMaybe id $ projectWith <$> r <*> box_ cs)

-- | Lens between a ChartTree and its bounding box.
box' :: Lens' ChartTree (Maybe (Rect Double))
box' =
  lens box_ rebox_

-- | Compute the bounding box of the data and style elements contained in a list of charts.
styleBoxes :: [Chart] -> Maybe (Rect Double)
styleBoxes cs = foldRect $ mconcat $ maybeToList . sbox <$> cs

styleBox_ :: ChartTree -> Maybe (Rect Double)
styleBox_ = styleBoxes . foldOf charts'

styleRebox_ :: ChartTree -> Maybe (Rect Double) -> ChartTree
styleRebox_ cs r =
  cs
    & over chart' (fromMaybe id $ projectWith <$> r <*> styleBox_ cs)

-- | Lens between a style bounding box and a ChartTree tree.
--
-- Note that a round trip may be only approximately isomorphic ie
--
-- > forall c r. \c -> view styleBox' . set styleBox' r c ~= r
styleBox' :: Lens' ChartTree (Maybe (Rect Double))
styleBox' =
  lens styleBox_ styleRebox_

-- | Getter of a ChartTree bounding box, including style, with singleton dimension guards, defaulting to one:
safeStyleBox' :: Getter ChartTree (Rect Double)
safeStyleBox' = Optics.Core.to (safeBox_ styleBox')

-- | Getter of a ChartTree bounding box, excluding style, with singleton dimension guards, defaulting to one:
safeBox' :: Getter ChartTree (Rect Double)
safeBox' = Optics.Core.to (safeBox_ box')

safeBox_ :: Lens' ChartTree (Maybe (Rect Double)) -> ChartTree -> Rect Double
safeBox_ l ct
  | b == Nothing || (Just True == fmap isSingleton b) = maybe one padSingletons (view l ct)
  | otherwise = fromMaybe one b
  where
    b = view l ct

-- | Create a frame over some charts with (additive) padding.
--
-- >>> frameChart defaultRectStyle 0.1 (unnamed [BlankChart defaultStyle []])
-- ChartTree {tree = Node {rootLabel = (Just "frame",[Chart {chartStyle = Style {size = 6.0e-2, borderSize = 1.0e-2, color = Colour 0.02 0.73 0.80 0.10, borderColor = Colour 0.02 0.29 0.48 1.00, scaleP = NoScaleP, textAnchor = AnchorMiddle, rotation = Nothing, translate = Nothing, escapeText = EscapeText, frame = Nothing, lineCap = Nothing, lineJoin = Nothing, dasharray = Nothing, dashoffset = Nothing, hsize = 0.6, vsize = 1.1, vshift = -0.25, glyphShape = SquareGlyph}, chartData = RectData []}]), subForest = []}}
frameChart :: Style -> Double -> ChartTree -> ChartTree
frameChart rs p cs = named "frame" [Chart rs (RectData (maybeToList (padRect p <$> view styleBox' cs)))]

-- | Additive padding, framing or buffering for a chart list.
padChart :: Double -> ChartTree -> ChartTree
padChart p ct = named "padding" [Chart defaultStyle (BlankData (maybeToList (padRect p <$> view styleBox' ct)))]

-- | Whether a chart is empty of data to be represented.
isEmptyChart :: ChartData -> Bool
isEmptyChart (RectData []) = True
isEmptyChart (LineData []) = True
isEmptyChart (GlyphData []) = True
isEmptyChart (TextData []) = True
isEmptyChart (PathData []) = True
isEmptyChart (BlankData _) = True
isEmptyChart _ = False

-- | Horizontally stack a list of trees (proceeding to the right), at the supplied Align and with the supplied gap intercalated.
hori :: Align -> Double -> [ChartTree] -> ChartTree
hori align gap cs = foldl' step mempty cs
  where
    step x c = x <> over chart' (moveChart (Point (widthx x) (aligny x - aligny c))) c
    widthx x = case foldOf charts' x of
      [] -> zero
      xs -> maybe zero (\(Rect x' z' _ _) -> z' - x' + gap) (styleBoxes xs)
    aligny x = case foldOf charts' x of
      [] -> zero
      xs -> case align of
        AlignLeft -> maybe zero (\(Rect _ _ y' _) -> y') (styleBoxes xs)
        AlignRight -> maybe zero (\(Rect _ _ _ y') -> y') (styleBoxes xs)
        AlignMid -> maybe zero (\(Rect _ _ y' w') -> (y' + w') / 2) (styleBoxes xs)
        NoAlign -> zero

-- | Vertically stack a list of trees (proceeding upwards), at the supplied Align and with the supplied gap intercalated.
vert :: Align -> Double -> [ChartTree] -> ChartTree
vert align gap cs = foldl' step mempty cs
  where
    step x c = x <> over chart' (moveChart (Point (alignx x - alignx c) (widthy x))) c
    widthy x = case foldOf charts' x of
      [] -> zero
      xs -> maybe zero (\(Rect _ _ y' w') -> w' - y' + gap) (styleBoxes xs)
    alignx x = case foldOf charts' x of
      [] -> zero
      xs -> case align of
        AlignLeft -> maybe zero (\(Rect x' _ _ _) -> x') (styleBoxes xs)
        AlignRight -> maybe zero (\(Rect _ x' _ _) -> x') (styleBoxes xs)
        AlignMid -> maybe zero (\(Rect x' z' _ _) -> (x' + z') / 2) (styleBoxes xs)
        NoAlign -> zero

-- | Stack a list of tree charts horizontally, then vertically (proceeding downwards which is opposite to the usual coordinate reference system but intuitively the way people read charts)
stack :: Int -> Align -> Align -> Double -> [ChartTree] -> ChartTree
stack n alignV alignH gap cs = vert alignV gap (reverse $ hori alignH gap <$> group' cs [])
  where
    group' [] acc = reverse acc
    group' x acc = group' (drop n x) (take n x : acc)

-- | Place a ChartTree beside a 'Rect'.
besideChart :: Place -> Double -> Double -> Rect Double -> ChartTree -> ChartTree
besideChart place anc buff cb ct = maybe ct (\b -> ct & over chart' (moveChart (beside place anc buff cb b))) (view styleBox' ct)

-- | Make a new chart tree out of the bounding boxes of a chart tree.
--
-- This includes any extra space for style elements.
rectangularize :: Style -> ChartTree -> ChartTree
rectangularize r ct = group (Just "rectangularize") [over chart' (\c -> set #chartStyle r $ set #chartData (rectangularize_ c) c) ct]

rectangularize_ :: Chart -> ChartData
rectangularize_ c = RectData (maybeToList $ sbox c)

-- | Make a new chart tree out of the data points of a chart tree, using the supplied style (for glyphs).
glyphize :: Style -> ChartTree -> ChartTree
glyphize s ct =
  group (Just "glyphize") [over chart' (set #chartStyle s . over #chartData pointize_) ct]

pointize_ :: ChartData -> ChartData
pointize_ (TextData xs) = GlyphData (snd <$> xs)
pointize_ (PathData xs) = GlyphData (pointPath <$> xs)
pointize_ (LineData xs) = GlyphData (mconcat xs)
pointize_ (BlankData xs) = GlyphData (mid <$> xs)
pointize_ (RectData xs) = GlyphData (mid <$> xs)
pointize_ (GlyphData xs) = GlyphData xs

-- | Verticle or Horizontal
data Orientation = Vert | Hori deriving (Eq, Show, Generic)

-- | Whether to stack chart data
data Stacked = Stacked | NonStacked deriving (Eq, Show, Generic)

-- | The basis for the x-y ratio of a chart
--
-- Default style features tend towards assuming that the usual height of the overall svg image is around 1, and ChartAspect is based on this assumption, so that a ChartAspect of @FixedAspect 1.5@, say, means a height of 1 and a width of 1.5.
data ChartAspect
  = -- | Rescale charts to a fixed x-y ratio, inclusive of hud and style features
    FixedAspect Double
  | -- | Rescale charts to an overall height of 1, preserving the x-y ratio of the data canvas.
    CanvasAspect Double
  | -- | Rescale charts to a height of 1, preserving the existing x-y ratio of the underlying charts, inclusive of hud and style.
    ChartAspect
  | -- | Do not rescale charts. The style values should make sense in relation to the data ranges.
    UnscaledAspect
  deriving (Show, Eq, Generic)

-- | Rectangular placement
data Place
  = PlaceLeft
  | PlaceRight
  | PlaceTop
  | PlaceBottom
  | PlaceAbsolute (Point Double)
  deriving (Show, Eq, Generic)

-- | Flip Place to the opposite side, or negate if 'PlaceAbsolute'.
--
-- >>> flipPlace PlaceLeft
-- PlaceRight
flipPlace :: Place -> Place
flipPlace PlaceLeft = PlaceRight
flipPlace PlaceRight = PlaceLeft
flipPlace PlaceTop = PlaceBottom
flipPlace PlaceBottom = PlaceTop
flipPlace (PlaceAbsolute p) = PlaceAbsolute (negate p)

-- | Point that moves a 'Rect' in relation to another 'Rect'
--
-- above and centered
-- >>> beside PlaceTop 0 0.01 one half
-- Point 0.0 0.76
--
-- above and right-aligned
-- >>> beside PlaceTop 0.5 0.01 one half
-- Point 0.25 0.76
--
-- left and with tops inline
-- >>> beside PlaceLeft (-0.5) 0 one half
-- Point (-0.75) 0.25
--
-- left and with bottoms aligned
-- >>> beside PlaceLeft 0.5 0 one half
-- Point (-0.75) (-0.25)
beside :: Place -> Double -> Double -> Rect Double -> Rect Double -> Point Double
beside pl anc buff r r' = mid r - mid r' + p + b + a
  where
    wplus = width r |/ 2 + width r' |/ 2
    wneg = width r - width r'
    b = case pl of
      PlaceTop -> Point zero buff
      PlaceBottom -> Point zero (-buff)
      PlaceLeft -> Point (-buff) zero
      PlaceRight -> Point buff zero
      PlaceAbsolute _ -> zero
    p = case pl of
      PlaceTop -> wplus {_x = zero}
      PlaceBottom -> -wplus {_x = zero}
      PlaceLeft -> -wplus {_y = zero}
      PlaceRight -> wplus {_y = zero}
      PlaceAbsolute p' -> p'
    -- This is the opposite of the usual convention, but aligns
    -- with TextAnchor usage when text is vertical.
    a = case pl of
      PlaceTop -> wneg {_y = zero} |* anc
      PlaceBottom -> wneg {_y = zero} |* anc
      PlaceLeft -> -(wneg {_x = zero} |* anc)
      PlaceRight -> -(wneg {_x = zero} |* anc)
      PlaceAbsolute _ -> zero

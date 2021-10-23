{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

-- | SVG path manipulation
module Data.Path
  ( -- * Svg Paths

    -- $path
    PathData(..),
    pointPath,
    movePath,
    scalePath,
    projectPaths,
    pathBoxes,
    pathBox,

    -- * Path maths
    ArcInfo (..),
    ArcPosition (..),
    ArcCentroid (..),
    arcCentroid,
    arcPosition,
    arcBox,
    arcDerivs,
    ellipse,
    QuadPosition (..),
    QuadPolar (..),
    quadPosition,
    quadPolar,
    quadBox,
    quadBezier,
    quadDerivs,
    CubicPosition (..),
    CubicPolar (..),
    cubicPosition,
    cubicPolar,
    cubicBox,
    cubicBezier,
    cubicDerivs,
    singletonCubic,
    singletonQuad,
    singletonArc,
    singletonPie,
  )
where

import qualified Control.Foldl as L
import Data.Generics.Labels ()
import GHC.Generics
import qualified Geom2D.CubicBezier as B
import NumHask.Prelude hiding (head, last, tail)
import Chart.Data as CD
import Data.List.NonEmpty (NonEmpty (..))
import Control.Monad.State.Lazy

-- $setup
-- >>> import Chart

-- $path
-- Every element of an svg path can be thought of as exactly two points in space, with instructions of how to draw a curve between them.  From this point of view, one which this library adopts, a path chart is thus very similar to a line chart.  There's just a lot more information about the style of this line to deal with.
--
-- References:
--
-- [SVG d](https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/d)
--
-- [SVG path](https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths)


-- | Representation of a single SVG path data point
data PathData a
  -- | Starting position
  = StartP (Point a)
  -- | line (from previous position)
  | LineP (Point a)
  -- | cubic bezier curve
  | CubicP (Point a) (Point a) (Point a)
  -- | quad bezier curve
  | QuadP (Point a) (Point a)
  -- arc
  | ArcP (ArcInfo a) (Point a)
  deriving (Show, Eq, Generic)

pointPath :: PathData a -> Point a
pointPath (StartP p) = p
pointPath (LineP p) = p
pointPath (CubicP _ _ p) = p
pointPath (QuadP _ p) = p
pointPath (ArcP _ p) = p

movePath :: (Additive a) => Point a -> PathData a -> PathData a
movePath x (StartP p) = StartP (p+x)
movePath x (LineP p) = LineP (p+x)
movePath x (CubicP c1 c2 p) = CubicP (c1+x) (c2+x) (p+x)
movePath x (QuadP c p) = QuadP (c+x) (p+x)
movePath x (ArcP i p) = ArcP i (p+x)

scalePath :: (Multiplicative a) => a -> PathData a -> PathData a
scalePath x (StartP p) = StartP (fmap (x*) p)
scalePath x (LineP p) = LineP (fmap (x*) p)
scalePath x (CubicP c1 c2 p) = CubicP (fmap (x*) c1) (fmap (x*) c2) (fmap (x*) p)
scalePath x (QuadP c p) = QuadP (fmap (x*) c) (fmap (x*) p)
scalePath x (ArcP i p) = ArcP i (fmap (x*) p)

projectPaths :: Rect Double -> Rect Double -> NonEmpty (PathData Double) -> NonEmpty (PathData Double)
projectPaths new old ps =
  flip evalState zero $
  sequence $ (\p -> do
  x <- get
  let d = projectPath new old x p
  put (pointPath d)
  pure d) <$> ps

projectPath
  :: Rect Double
  -> Rect Double
  -> Point Double
  -> PathData Double
  -> PathData Double
projectPath new old _ (CubicP c1 c2 p) =
      CubicP (projectOnP new old c1) (projectOnP new old c2) (projectOnP new old p)
projectPath new old _ (QuadP c p) =
      QuadP (projectOnP new old c) (projectOnP new old p)
projectPath new old p1 (ArcP ai p2) = ArcP (projectArcPosition new old (ArcPosition p1 p2 ai)) (projectOnP new old p2)
projectPath new old _ (LineP p) = LineP (projectOnP new old p)
projectPath new old _ (StartP p) = StartP (projectOnP new old p)

-- | convert cubic position to path data.
singletonCubic :: CubicPosition Double -> NonEmpty (PathData Double)
singletonCubic (CubicPosition s e c1 c2) = [StartP s, CubicP c1 c2 e]

-- | convert quad position to path data.
singletonQuad :: QuadPosition Double -> NonEmpty (PathData Double)
singletonQuad (QuadPosition s e c) = [StartP s, QuadP c e]

-- | convert arc position to path data.
singletonArc :: ArcPosition Double -> NonEmpty (PathData Double)
singletonArc (ArcPosition s e i) = [StartP s, ArcP i e]

-- | convert arc position to a pie slice, with a specific center.
singletonPie :: Point Double -> ArcPosition Double -> NonEmpty (PathData Double)
singletonPie c (ArcPosition s e i) = [StartP c, LineP s, ArcP i e, LineP c]

-- * Arc types

-- | Information about an individual arc path.
data ArcInfo a = ArcInfo
  { -- | ellipse radii
    radii :: Point a,
    -- | rotation of the ellipse. positive means counter-clockwise (which is different to SVG).
    phi :: a,
    large :: Bool,
    -- | sweep means clockwise
    clockwise :: Bool
  }
  deriving (Eq, Show, Generic)

-- | Specification of an Arc using positional referencing as per SVG standard.
data ArcPosition a = ArcPosition
  { posStart :: Point a,
    posEnd :: Point a,
    posInfo :: ArcInfo a
  }
  deriving (Eq, Show, Generic)

-- | Arc specification based on centroidal interpretation.
--
-- See: https://www.w3.org/TR/SVG/implnote.html#ArcConversionEndpointToCenter
data ArcCentroid a = ArcCentroid
  { -- | ellipse center
    centroid :: Point a,
    -- | ellipse radii
    radius :: Point a,
    -- | ellipse rotation
    cphi :: a,
    -- | starting point angle to the x-axis
    ang0 :: a,
    -- | difference between ending point angle and starting point angle
    angdiff :: a
  }
  deriving (Eq, Show, Generic)

-- | convert from an ArcPosition spec to ArcCentroid spec.
--
-- See also [this](https://math.stackexchange.com/questions/55627/how-to-find-the-center-of-an-scaled-ellipse)
--
-- >>> let p = ArcPosition (Point 0 0) (Point 1 0) (ArcInfo (Point 1 0.5) (pi/4) False True)
-- >>> arcCentroid p
-- ArcCentroid {centroid = Point 0.20952624903444356 -0.48412291827592724, radius = Point 1.0 0.5, cphi = 0.7853981633974483, ang0 = 1.3753858999692936, angdiff = -1.823476581936975}
arcCentroid :: (Ord a, FromInteger a, TrigField a, ExpField a) => ArcPosition a -> ArcCentroid a
arcCentroid (ArcPosition p1@(Point x1 y1) p2@(Point x2 y2) (ArcInfo rad phi' large' clockwise')) = ArcCentroid c (Point rx ry) phi' ang1 angd
  where
    (Point x1' y1') = rotateP (-phi') ((p1 - p2) /. two)
    (Point rx' ry') = rad
    l = x1' ** 2 / rx' ** 2 + y1' ** 2 / ry' ** 2
    (rx, ry) = bool (rx', ry') (rx' * sqrt l, ry' * sqrt l) (l > 1)
    snumer = max 0 $ (rx * rx * ry * ry) - (rx * rx * y1' * y1') - (ry * ry * x1' * x1')
    s =
      bool (-1) 1 (large' == clockwise')
        * sqrt
          (snumer / (rx * rx * y1' * y1' + ry * ry * x1' * x1'))
    cx' = s * rx * y1' / ry
    cy' = s * (-ry) * x1' / rx
    cx = (x1 + x2) / 2 + cos phi' * cx' - sin phi' * cy'
    cy = (y1 + y2) / 2 + sin phi' * cx' + cos phi' * cy'
    c = Point cx cy
    ang1 = angle (Point (-(cx' - x1') / rx) (-(cy' - y1') / ry))
    ang2 = angle (Point (-(cx' + x1') / rx) (-(cy' + y1') / ry))
    angd' = ang2 - ang1
    angd =
      bool 0 (2 * pi) (not clockwise' && angd' < 0)
        + bool 0 (-2 * pi) (clockwise' && angd' > 0)
        + angd'

-- | convert from an ArcCentroid to an ArcPosition specification.
--
-- Morally,
-- > arcPosition . arcCentroid == id
--
-- Not isomorphic if:
--
-- - angle diff is pi and large is True
--
-- - radii are less than they should be and thus get scaled up.
arcPosition :: (Ord a, Signed a, TrigField a) => ArcCentroid a -> ArcPosition a
arcPosition (ArcCentroid c r phi' ang1 angd) =
  ArcPosition p1 p2 (ArcInfo r phi' large' clockwise')
  where
    p1 = ellipse c r phi' ang1
    p2 = ellipse c r phi' (ang1 + angd)
    large' = abs angd > pi
    clockwise' = angd < zero

-- | ellipse formulae
--
-- >>> ellipse zero (Point 1 2) (pi/6) pi
-- Point -0.8660254037844388 -0.4999999999999997
--
-- Compare this "elegent" definition from [stackexchange](https://math.stackexchange.com/questions/426150/what-is-the-general-equation-of-the-ellipse-that-is-not-in-the-origin-and-rotate)
--
-- \[\dfrac{((x-h)\cos(A)+(y-k)\sin(A))^2}{a^2}+\dfrac{((x-h) \sin(A)-(y-k) \cos(A))^2}{b^2}=1\]
--
-- with the haskell code:
--
-- > c + (rotate phi |. (r * ray theta))
--
-- See also: [wolfram](https://mathworld.wolfram.com/Ellipse.html)
ellipse :: (Direction b a, Affinity b a, TrigField a) => b -> b -> a -> a -> b
ellipse c r phi' theta = c + (rotate phi' |. (r * ray theta))

-- | compute the bounding box for an arcBox
--
-- > let p = ArcPosition (Point 0 0) (Point 1 0) (ArcInfo (Point 1 0.5) (pi/4) False True)
-- > arcBox p
-- Rect -8.326672684688674e-17 0.9999999999999998 -5.551115123125783e-17 0.30644649676616753
arcBox :: ArcPosition Double -> Rect Double
arcBox p = space1 pts
  where
    (ArcCentroid c r phi' ang0' angd) = arcCentroid p
    (x', y') = arcDerivs r phi'
    angr = ang0' ... (ang0' + angd) :: Range Double
    angs =
      filter
        (|.| angr)
        [ x',
          x' - 2 * pi,
          x' + pi,
          x' - pi,
          y',
          y' - 2 * pi,
          y' + pi,
          y' - pi,
          ang0',
          ang0' + angd
        ]
    pts = ellipse c r phi' <$> angs

-- | potential arc turning points.
--
-- >>> arcDerivs (Point 1 0.5) (pi/4)
-- (-0.4636476090008061,0.4636476090008062)
arcDerivs :: Point Double -> Double -> (Double, Double)
arcDerivs (Point rx ry) phi' = (thetax1, thetay1)
  where
    thetax1 = atan2 (-sin phi' * ry) (cos phi' * rx)
    thetay1 = atan2 (cos phi' * ry) (sin phi' * rx)

-- * bezier

-- | Quadratic bezier curve expressed in positional terms.
data QuadPosition a = QuadPosition
  { -- | starting point
    qposStart :: Point a,
    -- | ending point
    qposEnd :: Point a,
    -- | control point
    qposControl :: Point a
  }
  deriving (Eq, Show, Generic)

-- | Quadratic bezier curve with control point expressed in polar terms normalised to the start - end line.
data QuadPolar a = QuadPolar
  { -- | starting point
    qpolStart :: Point a,
    -- | ending point
    qpolEnd :: Point a,
    -- | control point in terms of distance from and angle to the qp0 - qp2 line
    qpolControl :: Polar a a
  }
  deriving (Eq, Show, Generic)

-- | Convert from a positional to a polar representation of a cubic bezier.
--
-- >>> quadPolar (QuadPosition (Point 0 0) (Point 1 1) (Point 2 -1))
-- QuadPolar {qpolStart = Point 0.0 0.0, qpolEnd = Point 1.0 1.0, qpolControl = Polar {magnitude = 2.1213203435596424, direction = -0.7853981633974483}}
quadPolar :: (Eq a, TrigField a, ExpField a) => QuadPosition a -> QuadPolar a
quadPolar (QuadPosition start' end control) = QuadPolar start' end control'
  where
    mp = (start' + end) /. two
    control' = polar (control - mp)

-- | Convert from a polar to a positional representation of a quadratic bezier.
--
-- > quadPosition . quadPolar == id
-- > quadPolar . quadPosition == id
--
-- >>> quadPosition $ quadPolar (QuadPosition (Point 0 0) (Point 1 1) (Point 2 -1))
-- QuadPosition {qposStart = Point 0.0 0.0, qposEnd = Point 1.0 1.0, qposControl = Point 2.0 -0.9999999999999998}
quadPosition :: (TrigField a) => QuadPolar a -> QuadPosition a
quadPosition (QuadPolar start' end control) = QuadPosition start' end control'
  where
    control' = coord control + (start' + end) /. two

-- | The quadratic bezier equation
--
-- >>> quadBezier (QuadPosition (Point 0 0) (Point 1 1) (Point 2 -1)) 0.33333333
-- Point 0.9999999933333332 -0.33333333333333326
quadBezier :: (FromInteger a, ExpField a) => QuadPosition a -> a -> Point a
quadBezier (QuadPosition start' end control) theta =
  (1 - theta) ^ 2 .* start'
    + 2 * (1 - theta) * theta .* control
    + theta ^ 2 .* end

-- | QuadPosition turning points.
--
-- >>> quadDerivs (QuadPosition (Point 0 0) (Point 1 1) (Point 2 -1))
-- [0.6666666666666666,0.3333333333333333]
quadDerivs :: QuadPosition Double -> [Double]
quadDerivs (QuadPosition start' end control) = [x', y']
  where
    (Point detx dety) = start' - 2 .* control + end
    x' = bool ((_x start' - _x control) / detx) (2 * (_x control - _x start')) (detx == 0)
    y' = bool ((_y start' - _y control) / dety) (2 * (_y control - _y start')) (dety == 0)

-- | Bounding box for a QuadPosition
--
-- >>> quadBox (QuadPosition (Point 0 0) (Point 1 1) (Point 2 -1))
-- Rect 0.0 1.3333333333333335 -0.33333333333333337 1.0
quadBox :: QuadPosition Double -> Rect Double
quadBox p = space1 pts
  where
    ts = quadDerivs p
    pts = quadBezier p <$> ([0, 1] <> ts)

-- | cubic bezier curve
--
-- Note that the ordering is different to the svg standard.
data CubicPosition a = CubicPosition
  { -- | starting point
    cposStart :: Point a,
    -- | ending point
    cposEnd :: Point a,
    -- | control point 1
    cposControl1 :: Point a,
    -- | control point 2
    cposControl2 :: Point a
  }
  deriving (Eq, Show, Generic)

-- | A polar representation of a cubic bezier with control points expressed as polar and normalised to the start - end line.
data CubicPolar a = CubicPolar
  { -- | starting point
    cpolStart :: Point a,
    -- | ending point
    cpolEnd :: Point a,
    -- | control point in terms of distance from and angle to the start end line
    cpolControl1 :: Polar a a,
    -- | control point in terms of distance from and angle to the start end line
    cpolControl2 :: Polar a a
  }
  deriving (Eq, Show, Generic)

-- | Convert from a positional to a polar representation of a cubic bezier.
--
-- > cubicPosition . cubicPolar == id
-- > cubicPolar . cubicPosition == id
--
-- >>> cubicPolar (CubicPosition (Point 0 0) (Point 1 1) (Point 1 -1) (Point 0 2))
-- CubicPolar {cpolStart = Point 0.0 0.0, cpolEnd = Point 1.0 1.0, cpolControl1 = Polar {magnitude = 1.1180339887498947, direction = -1.2490457723982544}, cpolControl2 = Polar {magnitude = 1.1180339887498947, direction = 1.8925468811915387}}
cubicPolar :: (Eq a, ExpField a, TrigField a) => CubicPosition a -> CubicPolar a
cubicPolar (CubicPosition start' end control1 control2) = CubicPolar start' end control1' control2'
  where
    mp = (start' + end) /. two
    control1' = polar $ (control1 - mp) /. norm (end - start')
    control2' = polar $ (control2 - mp) /. norm (end - start')

-- | Convert from a polar to a positional representation of a cubic bezier.
--
-- > cubicPosition . cubicPolar == id
-- > cubicPolar . cubicPosition == id
--
-- >>> cubicPosition $ cubicPolar (CubicPosition (Point 0 0) (Point 1 1) (Point 1 -1) (Point 0 2))
-- CubicPosition {cposStart = Point 0.0 0.0, cposEnd = Point 1.0 1.0, cposControl1 = Point 1.0 -1.0, cposControl2 = Point 1.6653345369377348e-16 2.0}
cubicPosition :: (Eq a, TrigField a, ExpField a) => CubicPolar a -> CubicPosition a
cubicPosition (CubicPolar start' end control1 control2) = CubicPosition start' end control1' control2'
  where
    control1' = norm (end - start') .* coord control1 + (start' + end) /. two
    control2' = norm (end - start') .* coord control2 + (start' + end) /. two

-- | The cubic bezier equation
--
-- >>> cubicBezier (CubicPosition (Point 0 0) (Point 1 1) (Point 1 -1) (Point 0 2)) 0.8535533905932737
-- Point 0.6767766952966369 1.2071067811865475
cubicBezier :: (FromInteger a, TrigField a) => CubicPosition a -> a -> Point a
cubicBezier (CubicPosition start' end control1 control2) theta =
  (1 - theta) ^ 3 .* start'
    + 3 * (1 - theta) ^ 2 * theta .* control1
    + 3 * (1 - theta) * theta ^ 2 .* control2
    + theta ^ 3 .* end

-- | Turning point positions for a CubicPosition (0,1 or 2)
--
-- >>> cubicDerivs (CubicPosition (Point 0 0) (Point 1 1) (Point 1 -1) (Point 0 2))
-- [0.8535533905932737,0.14644660940672624,0.5]
cubicDerivs :: CubicPosition Double -> [Double]
cubicDerivs
  ( CubicPosition
      (Point c0x c0y)
      (Point c3x c3y)
      (Point c1x c1y)
      (Point c2x c2y)
    ) =
    B.bezierHoriz b <> B.bezierVert b
    where
      b =
        B.CubicBezier
          (B.Point c0x c0y)
          (B.Point c1x c1y)
          (B.Point c2x c2y)
          (B.Point c3x c3y)

-- | Bounding box for a CubicPosition
--
-- >>> cubicBox (CubicPosition (Point 0 0) (Point 1 1) (Point 1 -1) (Point 0 2))
-- Rect 0.0 1.0 -0.20710678118654752 1.2071067811865475
cubicBox :: CubicPosition Double -> Rect Double
cubicBox p = space1 pts
  where
    ts = cubicDerivs p
    pts =
      cubicBezier p
        <$> filter
          (|.| Range 0 1)
          ([0, 1] <> ts)

-- | Bounding box for a list of path XYs.
pathBoxes :: NonEmpty (PathData Double) -> Rect Double
pathBoxes (x :| xs) =
  L.fold (L.Fold step begin snd) xs
  where
    begin :: (Point Double, Rect Double)
    begin = (pointPath x, singleton (pointPath x))
    step (start', r) a = (pointPath a, pathBox start' a <> r)

-- | Bounding box for a path info, start and end Points.
pathBox :: Point Double -> PathData Double -> Rect Double
pathBox start' info =
  case info of
    StartP p -> singleton p
    LineP p -> space1 ([start', p] :: NonEmpty (Point Double))
    CubicP c1 c2 p -> cubicBox (CubicPosition start' p c1 c2)
    QuadP c p -> quadBox (QuadPosition start' p c)
    ArcP i p -> arcBox (ArcPosition start' p i)

-- | project an ArcPosition given new and old Rects
--
-- The radii of the ellipse can be represented as:
--
-- Point rx 0 & Point 0 ry
--
-- These two points are firstly rotated by p and then undergo scaling...
projectArcPosition :: Rect Double -> Rect Double -> ArcPosition Double -> ArcInfo Double
projectArcPosition new old (ArcPosition _ _ (ArcInfo (Point rx ry) phi' l cl)) = ArcInfo (Point rx'' ry'') phi' l cl
  where
    rx' = rotateP phi' (Point rx zero)
    rx'' = norm $ rx' * CD.width new / CD.width old
    ry' = rotateP phi' (Point zero ry)
    ry'' = norm $ ry' * CD.width new / CD.width old

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | SVG path manipulation
module Data.Path
  ( -- * Path fundamental
    -- $path
    PathInfo (..),
    ArcInfo (..),
    ArcPosition (..),
    parsePath,
    toPathAbsolute,
    toPathCommand,
    toPathAbsolutes,
    toPathXYs,
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
    singletonPie',
    toSingletonArc,
    pathBoxes,
    pathBox,
  ) where

import qualified Graphics.SvgTree as SvgTree
import Graphics.SvgTree (PathCommand (..), Origin(..))
import Graphics.SvgTree.PathParser
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as Text
import NumHask.Space
import NumHask.Prelude hiding (rotate)
import qualified Linear
import qualified Control.Foldl as L
import Control.Lens hiding ((...))
import Data.Generics.Labels ()
import qualified Geom2D.CubicBezier as B
import Data.FormatN

-- $setup
-- >>> :set -XRebindableSyntax
-- >>> :set -XNegativeLiterals
-- >>> import NumHask.Prelude
-- >>> import Chart

-- $path
-- Every element of an svg path can be thought of as exactly two points in space, with instructions of how to draw a curve between them.  From this point of view, one which this library adopts, a path chart is thus very similar to a line chart.  There's just a lot more information about the style of this line to deal with.
--
-- References:
--
-- [SVG d](https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/d)
--
-- [SVG path](https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths)
--

-- | parse a raw path string
--
-- >>> let outerseg1 = "M-1.0,0.5 A0.5 0.5 0.0 1 1 0.0,-1.2320508075688774 1.0 1.0 0.0 0 0 -0.5,-0.3660254037844387 1.0 1.0 0.0 0 0 -1.0,0.5 Z"
-- >>> parsePath outerseg1
-- [MoveTo OriginAbsolute [V2 (-1.0) 0.5],EllipticalArc OriginAbsolute [(0.5,0.5,0.0,True,True,V2 0.0 (-1.2320508075688774)),(1.0,1.0,0.0,False,False,V2 (-0.5) (-0.3660254037844387)),(1.0,1.0,0.0,False,False,V2 (-1.0) 0.5)],EndPath]
--
-- https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/d
--
parsePath :: Text -> [PathCommand]
parsePath t = either (const []) id $ A.parseOnly pathParser t

-- | To fit in with the requirements of the 'Chart' interface, path instructions need to be decontructed into:
--
-- - define a single chart element as a line.
--
-- - split a single path element into the start and end points of the line, which become the 'Chart.Types.xys' of a 'Chart.Types.Chart', and the rest of the information, which is called 'PathInfo' and incorporated into the 'Chart.Types.Chart' 'Chart.Types.annotation'.
--
-- A lot of detail rides on whether the PathInfo is invariant to affine transformations of the 'Chart.Types.xys' points. Cubic and quadratic bezier paths need to be re-projected whenever 'Chart.Types.xys' are re-projected. Arcs are problematic (See 'Chart.Examples.problematic2' & 'Chart.Examples.problematic2').
data PathInfo a =
  StartI |
  LineI |
  CubicI (Point a) (Point a) |
  QuadI (Point a) |
  ArcI (ArcInfo a)
  deriving (Show, Eq, Generic)

-- | convert from a path info, start point, end point triple to a path text clause.
--
-- Note that morally,
--
-- > toPathsAbsolute . toInfos . parsePath == id
--
-- but the round trip destroys much information, including:
--
-- - path text spacing
--
-- - "Z", which is replaced by a LineI instruction from the end point back to the original start of the path.
--
-- - Sequences of the same instruction type are uncompressed
--
-- - As the name suggests, relative paths are translated to absolute ones.
--
-- - implicit L's in multiple M instructions are separated.
--
-- In converting between chart-svg and SVG there are two changes in reference:
--
-- - arc rotation is expressed as positive degrees for a clockwise rotation in SVG, and counter-clockwise in radians for chart-svg
--
-- - A positive y-direction is down for SVG and up for chart-svg
--
toPathAbsolute ::
  -- | (info, start, end)
  (PathInfo Double, Point Double) ->
  -- | path text
  Text
toPathAbsolute (StartI,p) = "M " <> pp p
toPathAbsolute (LineI,p) = "L " <> pp p
toPathAbsolute (CubicI c1 c2, next) =
  "C " <>
  pp c1 <> " " <>
  pp c2 <> " " <>
  pp next
toPathAbsolute (QuadI control, next) =
  "Q " <>
  pp control <> " " <>
  pp next
toPathAbsolute (ArcI (ArcInfo (Point x y) phi' l sw), x2) =
  "A " <>
  show x <> " " <>
  show y <> " " <>
  show (-phi' * 180 / pi) <> " " <>
  bool "0" "1" l <> " " <>
  bool "0" "1" sw <> " " <>
  pp x2

-- | render a point (including a flip of the y dimension).
pp :: Point Double -> Text
pp (Point x y) =
  showOr (FormatFixed (Just 4)) x <> "," <>
  showOr (FormatFixed (Just 4)) (bool (-y) y (y==zero))

-- | convert an (info, point) list to an svg d path text.
toPathAbsolutes :: [(PathInfo Double, Point Double)] -> Text
toPathAbsolutes = L.fold (L.Fold step begin done)
  where
    done = Text.intercalate " " . reverse
    begin = []
    step ts (info, next) = toPathAbsolute (info, next):ts

-- | Convert from PathInfo to PathCommand
toPathCommand ::
  (PathInfo Double, Point Double) ->
  -- | path text
  PathCommand
toPathCommand (StartI,p) = MoveTo OriginAbsolute [toV2 p]
toPathCommand (LineI,p) = LineTo OriginAbsolute [toV2 p]
toPathCommand (CubicI c1 c2, p) = CurveTo OriginAbsolute [(toV2 c1, toV2 c2, toV2 p)]
toPathCommand (QuadI c, p) = QuadraticBezier OriginAbsolute [(toV2 c, toV2 p)]
toPathCommand (ArcI (ArcInfo (Point rx ry) phi' l sw), p) =
  EllipticalArc OriginAbsolute [(rx,ry,phi',l,sw,toV2 p)]

toV2 :: Point a -> Linear.V2 a
toV2 (Point x y) = Linear.V2 x y

data StateInfo =
  StateInfo
  {
    -- | previous position
    cur :: Point Double,
    -- | start point (to close out the path)
    start :: Point Double,
    -- | last control point
    infoControl :: Point Double
  } deriving (Eq, Show, Generic)

stateInfo0 :: StateInfo
stateInfo0 = StateInfo zero zero zero

-- | Convert a path command fragment to an instruction + point.
--
-- flips the y-dimension of points.
toInfo :: StateInfo -> SvgTree.PathCommand -> (StateInfo, [(PathInfo Double, Point Double)])
toInfo s (MoveTo _ []) = (s, [])
toInfo _ (MoveTo OriginAbsolute (x:xs)) = L.fold (L.Fold step begin (second reverse)) (fromV2 <$> xs)
  where
    x0 = fromV2 x
    begin = (StateInfo x0 x0 zero, [(StartI, x0)])
    step (s, p) a = (s & #cur .~ a, (LineI, a):p)
toInfo s (MoveTo OriginRelative (x:xs)) = L.fold (L.Fold step begin (second reverse)) (fromV2 <$> xs)
  where
    x0 = s ^. #cur + fromV2 x
    begin = (StateInfo x0 x0 zero, [(StartI, x0)])
    step (s, p) a = let a' = a + s ^. #cur in (s & #cur .~ a', (LineI, a'):p)
toInfo s EndPath = (s, [(LineI, s ^. #start)])
toInfo s (LineTo OriginAbsolute xs) = L.fold (L.Fold step (s,[]) (second reverse)) (fromV2 <$> xs)
  where
    step (s, p) a = (s & #cur .~ a, (LineI, a):p)
toInfo s (LineTo OriginRelative xs) = L.fold (L.Fold step (s,[]) (second reverse)) (fromV2 <$> xs)
  where
    step (s, p) a = let a' = a + s ^. #cur in (s & #cur .~ a', (LineI, a'):p)
toInfo s (HorizontalTo OriginAbsolute xs) = L.fold (L.Fold step (s,[]) (second reverse)) xs
  where
    step (s@(StateInfo (Point _ cy) _ _), p) a =
      let a' = Point a cy in (s & #cur .~ a', (LineI, a'):p)
toInfo s (HorizontalTo OriginRelative xs) = L.fold (L.Fold step (s,[]) (second reverse)) xs
  where
    step (s@(StateInfo (Point cx cy) _ _), p) a =
      let a' = Point (a+cx) cy in (s & #cur .~ a', (LineI, a'):p)
toInfo s (VerticalTo OriginAbsolute xs) = L.fold (L.Fold step (s,[]) (second reverse)) xs
  where
    step (s@(StateInfo (Point cx _) _ _), p) a =
      let a' = Point cx a in (s & #cur .~ a', (LineI, a'):p)
toInfo s (VerticalTo OriginRelative xs) = L.fold (L.Fold step (s,[]) (second reverse)) xs
  where
    step (s@(StateInfo (Point cx cy) _ _), p) a =
      let a' = Point cx (a+cy) in (s & #cur .~ a', (LineI, a'):p)
toInfo s (CurveTo OriginAbsolute xs) =
  L.fold (L.Fold step (s,[]) (second reverse)) xs'
  where
    xs' = (\(c1,c2,x2) -> (fromV2 c1, fromV2 c2, fromV2 x2)) <$> xs
    step (s, p) (c1,c2,x2) =
          (s & #cur .~ x2 & #infoControl .~ c2, (CubicI c1 c2, x2):p)
toInfo s (CurveTo OriginRelative xs) =
  L.fold (L.Fold step (s,[]) (second reverse)) xs'
  where
    xs' = (\(c1,c2,x2) -> (fromV2 c1, fromV2 c2, fromV2 x2)) <$> xs
    step (s, p) (c1,c2,x2) =
          (s & #cur .~ (x2 + s ^. #cur) & #infoControl .~ (c2 + s ^. #cur), (CubicI (c1+ s ^. #cur) (c2 + s ^. #cur), x2 + s ^. #cur):p)
toInfo s (SmoothCurveTo OriginAbsolute xs) =
  L.fold (L.Fold step (s,[]) (second reverse)) xs'
  where
    xs' = bimap fromV2 fromV2 <$> xs
    step (s, p) (c2,x2) =
          (s & #cur .~ x2, (CubicI (s ^. #cur - (s ^. #infoControl - s^. #cur)) c2, x2):p)
toInfo s (SmoothCurveTo OriginRelative xs) =
  L.fold (L.Fold step (s,[]) (second reverse)) xs'
  where
    xs' = bimap fromV2 fromV2 <$> xs
    step (s, p) (c2,x2) =
          (s &
           #cur .~ (x2 + s ^. #cur) &
           #infoControl .~ (c2 + s ^. #cur),
           (CubicI (s ^. #cur - (s ^. #infoControl - s^. #cur)) (c2 + s ^. #cur), x2 + s ^. #cur):p)
toInfo s (QuadraticBezier OriginAbsolute xs) =
  L.fold (L.Fold step (s,[]) (second reverse)) xs'
  where
    xs' = bimap fromV2 fromV2 <$> xs
    step (s, p) (c1, x2) =
          (s &
           #cur .~ x2 &
           #infoControl .~ c1,
           (QuadI c1, x2):p)
toInfo s (QuadraticBezier OriginRelative xs) =
  L.fold (L.Fold step (s,[]) (second reverse)) xs'
  where
    xs' = bimap fromV2 fromV2 <$> xs
    step (s, p) (c1,x2) =
          (s & #cur .~ x2 & #infoControl .~ (c1 + s ^. #cur), (QuadI (c1 + s ^. #cur), x2 + s ^. #cur):p)
toInfo s (SmoothQuadraticBezierCurveTo OriginAbsolute xs) =
  L.fold (L.Fold step (s,[]) (second reverse)) xs'
  where
    xs' = fromV2 <$> xs
    step (s, p) x2 =
          (s &
           #cur .~ x2 &
           #infoControl .~ (s ^. #cur - (s ^. #infoControl - s ^. #cur)),
           (QuadI (s ^. #cur - (s ^. #infoControl - s^. #cur)), x2):p)
toInfo s (SmoothQuadraticBezierCurveTo OriginRelative xs) =
  L.fold (L.Fold step (s,[]) (second reverse)) xs'
  where
    xs' = fromV2 <$> xs
    step (s, p) x2 =
          (s &
           #cur .~ (x2 + s ^. #cur) &
           #infoControl .~ (s ^. #cur - (s ^. #infoControl - s ^. #cur)),
           (QuadI (s ^. #cur - (s ^. #infoControl - s^. #cur)), x2 + s ^. #cur):p)
toInfo s (EllipticalArc OriginAbsolute xs) =
  L.fold (L.Fold step (s,[]) (second reverse)) xs'
  where
    xs' = (\(x,y,r,l,sw,x2) -> (x,y,r,l,sw,fromV2 x2)) <$> xs
    step (s, p) a@(_,_,_,_,_,x2) =
          (s & #cur .~ x2, (fromPathEllipticalArc (s ^. #cur) a, x2):p)
toInfo s (EllipticalArc OriginRelative xs) =
  L.fold (L.Fold step (s,[]) (second reverse)) xs'
  where
    xs' = (\(x,y,r,l,sw,x2) -> (x,y,r,l,sw,fromV2 x2)) <$> xs
    step (s, p) a@(_,_,_,_,_,x2) =
      let x2' = x2 + s ^. #cur in
          (s & #cur .~ x2', (fromPathEllipticalArc (s ^. #cur) a, x2'):p)

fromPathEllipticalArc :: Point a -> (a, a, a, Bool, Bool, Point a) -> PathInfo a
fromPathEllipticalArc _ (x, y, r, l, s, _) = ArcI (ArcInfo (Point x y) r l s)

fromV2 :: (Subtractive a) => Linear.V2 a -> Point a
fromV2 (Linear.V2 x y) = Point x (-y)

-- | Convert from a path command list to a PathA specification
--
toPathXYs :: [SvgTree.PathCommand] -> [(PathInfo Double, Point Double)]
toPathXYs [] = []
toPathXYs xs =
  snd (foldl' (\(x,l) a -> second (l<>) $ toInfo x a) (stateInfo0,[]) xs)

-- | convert cubic position to path info.
singletonCubic :: CubicPosition Double -> [(PathInfo Double, Point Double)]
singletonCubic (CubicPosition s e c1 c2) = [(StartI, s), (CubicI c1 c2, e)]

-- | convert quad position to path info.
singletonQuad :: QuadPosition Double -> [(PathInfo Double, Point Double)]
singletonQuad (QuadPosition s e c) = [(StartI, s), (QuadI c, e)]

-- | convert arc position to path info.
singletonArc :: ArcPosition Double -> [(PathInfo Double, Point Double)]
singletonArc (ArcPosition s e i) = [(StartI, s), (ArcI i, e)]

-- | convert arc position to a pie slice.
singletonPie :: ArcPosition Double -> [(PathInfo Double, Point Double)]
singletonPie p@(ArcPosition s e i) = [(StartI, c), (LineI, s), (ArcI i, e), (LineI, c)]
  where
    ac = arcCentroid p
    c = ac ^. #centroid

-- | convert arc position to a pie slice, with a specific center.
singletonPie' :: Point Double -> ArcPosition Double -> [(PathInfo Double, Point Double)]
singletonPie' c (ArcPosition s e i) = [(StartI, c), (LineI, s), (ArcI i, e), (LineI, c)]

-- | convert path info to an ArcPosition.
toSingletonArc :: [(PathInfo Double, Point Double)] -> Maybe (ArcPosition Double)
toSingletonArc ((StartI, s):(ArcI i, e):_) = Just $ ArcPosition s e i
toSingletonArc _ = Nothing

-- * Arc types
-- | Information about an individual arc path.
--
data ArcInfo a =
  ArcInfo
  { -- | ellipse radii
    radii :: Point a,
    -- | rotation of the ellipse. positive means counter-clockwise (which is different to SVG).
    phi :: a,
    large :: Bool,
    -- | sweep means clockwise
    clockwise :: Bool
  } deriving (Eq, Show, Generic)

-- | Specification of an Arc using positional referencing as per SVG standard.
data ArcPosition a =
  ArcPosition
  { posStart :: Point a,
    posEnd :: Point a,
    posInfo :: ArcInfo a
  } deriving (Eq, Show, Generic)

-- | Arc specification based on centroidal interpretation.
--
-- See: https://www.w3.org/TR/SVG/implnote.html#ArcConversionEndpointToCenter
--
data ArcCentroid a =
  ArcCentroid
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
  } deriving (Eq, Show, Generic)

-- | convert from an ArcPosition spec to ArcCentroid spec.
--
-- FIXME: simplify, maybe following https://math.stackexchange.com/questions/55627/how-to-find-the-center-of-an-scaled-ellipse
--
-- >>> let p = ArcPosition (Point 0 0) (Point 1 0) (ArcInfo (Point 1 0.5) (pi/4) False True)
-- >>> arcCentroid p
-- ArcCentroid {centroid = Point 0.20952624903444356 -0.48412291827592724, radius = Point 1.0 0.5, cphi = 0.7853981633974483, ang0 = 1.3753858999692936, angdiff = -1.823476581936975}
arcCentroid :: (FromInteger a, Ord a, TrigField a, ExpField a) => ArcPosition a -> ArcCentroid a
arcCentroid (ArcPosition p1@(Point x1 y1) p2@(Point x2 y2) (ArcInfo rad phi large clockwise)) = ArcCentroid c (Point rx ry) phi ang1 angd
  where
    (Point x1' y1') = rotateP (-phi) ((p1 - p2) /. two)
    (Point rx' ry') = rad
    l = x1'**2/rx'**2 + y1'**2/ry'**2
    (rx,ry) = bool (rx',ry') (rx'*sqrt l, ry'*sqrt l) (l > 1)
    snumer = max 0 $ (rx*rx*ry*ry) - (rx*rx*y1'*y1') - (ry*ry*x1'*x1')
    s = bool -1 1 (large == clockwise) * sqrt
      (snumer / (rx*rx*y1'*y1' + ry*ry*x1'*x1'))
    cx' = s *  rx * y1' / ry
    cy' = s * (-ry) * x1' / rx
    cx = (x1 + x2) / 2 + cos phi * cx' - sin phi * cy'
    cy = (y1 + y2) / 2 + sin phi * cx' + cos phi * cy'
    c = Point cx cy
    ang1 = angle (Point (-(cx'-x1')/rx) (-(cy'-y1')/ry))
    ang2 = angle (Point (-(cx'+x1')/rx) (-(cy'+y1')/ry))
    angd' = ang2 - ang1
    angd =
      bool 0 (2*pi) (not clockwise && angd'<0) +
      bool 0 (-2*pi) (clockwise && angd'>0) +
      angd'

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
--
arcPosition :: (Ord a, Signed a, TrigField a) => ArcCentroid a -> ArcPosition a
arcPosition (ArcCentroid c r phi ang1 angd) =
  ArcPosition p1 p2 (ArcInfo r phi large clockwise)
  where
    p1 = ellipse c r phi ang1
    p2 = ellipse c r phi (ang1+angd)
    large = abs angd > pi
    clockwise = angd < zero

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
--
ellipse :: (Direction b a, Affinity b a, TrigField a) => b -> b -> a -> a -> b
ellipse c r phi theta = c + (rotate phi |. (r * ray theta))

-- | compute the bounding box for an arcBox
--
-- > let p = ArcPosition (Point 0 0) (Point 1 0) (ArcInfo (Point 1 0.5) (pi/4) False True)
-- > arcBox p
-- Rect -8.326672684688674e-17 0.9999999999999998 -5.551115123125783e-17 0.30644649676616753
arcBox :: ArcPosition Double -> Rect Double
arcBox p = space1 pts
  where
    (ArcCentroid c r phi ang0 angd) = arcCentroid p
    (x',y') = arcDerivs r phi
    angr = ang0 ... (ang0 + angd) :: Range Double
    angs =
      filter (|.| angr)
      [ x',
        x' - 2 * pi,
        x'+pi,
        x'-pi,
        y',
        y' - 2 * pi,
        y'+pi,
        y'-pi,
        ang0,
        ang0+angd
      ]
    pts = ellipse c r phi <$> angs

-- | potential arc turning points.
--
-- >>> arcDerivs (Point 1 0.5) (pi/4)
-- (-0.4636476090008061,0.4636476090008062)
arcDerivs :: Point Double -> Double -> (Double, Double)
arcDerivs (Point rx ry) phi = (thetax1, thetay1)
  where
    thetax1 = atan2 (-sin phi * ry) (cos phi * rx)
    thetay1 = atan2 (cos phi * ry) (sin phi * rx)

-- * bezier
-- | Quadratic bezier curve expressed in positional terms.
--
data QuadPosition a =
  QuadPosition
  { -- | starting point
    qposStart :: Point a,
    -- | ending point
    qposEnd :: Point a,
    -- | control point
    qposControl :: Point a
  } deriving (Eq, Show, Generic)

-- | Quadratic bezier curve with control point expressed in polar terms normalised to the start - end line.
data QuadPolar a =
  QuadPolar
  { -- | starting point
    qpolStart :: Point a,
    -- | ending point
    qpolEnd :: Point a,
    -- | control point in terms of distance from and angle to the qp0 - qp2 line
    qpolControl :: Polar a a
  } deriving (Eq, Show, Generic)

-- | Convert from a positional to a polar representation of a cubic bezier.
--
-- >>> quadPolar (QuadPosition (Point 0 0) (Point 1 1) (Point 2 -1))
-- QuadPolar {qpolStart = Point 0.0 0.0, qpolEnd = Point 1.0 1.0, qpolControl = Polar {magnitude = 2.1213203435596424, direction = -0.7853981633974483}}
quadPolar :: (ExpField a, TrigField a) => QuadPosition a -> QuadPolar a
quadPolar (QuadPosition start end control) = QuadPolar start end control'
  where
    mp = (start + end) /. two
    control' = polar (control - mp)

-- | Convert from a polar to a positional representation of a quadratic bezier.
--
-- > quadPosition . quadPolar == id
-- > quadPolar . quadPosition == id
--
-- >>> quadPosition $ quadPolar (QuadPosition (Point 0 0) (Point 1 1) (Point 2 -1))
-- QuadPosition {qposStart = Point 0.0 0.0, qposEnd = Point 1.0 1.0, qposControl = Point 2.0 -0.9999999999999998}
quadPosition :: (ExpField a, TrigField a) => QuadPolar a -> QuadPosition a
quadPosition (QuadPolar start end control) = QuadPosition start end control'
  where
    control' = coord control + (start + end) /. two

-- | The quadratic bezier equation
--
-- >>> quadBezier (QuadPosition (Point 0 0) (Point 1 1) (Point 2 -1)) 0.33333333
-- Point 0.9999999933333332 -0.33333333333333326
quadBezier :: (ExpField a, FromInteger a) => QuadPosition a -> a -> Point a
quadBezier (QuadPosition start end control) theta =
  (1 - theta)^2 .* start +
  2 * (1-theta) * theta .* control +
  theta^2 .* end

-- | QuadPosition turning points.
--
-- >>> quadDerivs (QuadPosition (Point 0 0) (Point 1 1) (Point 2 -1))
-- [0.6666666666666666,0.3333333333333333]
quadDerivs :: QuadPosition Double -> [Double]
quadDerivs (QuadPosition start end control) = [x',y']
  where
    (Point detx dety) = start - 2 .* control + end
    x' = bool ((_x start - _x control)/detx) (2 * (_x control - _x start)) (detx==0)
    y' = bool ((_y start - _y control)/dety) (2 * (_y control - _y start)) (dety==0)

-- | Bounding box for a QuadPosition
--
-- >>> quadBox (QuadPosition (Point 0 0) (Point 1 1) (Point 2 -1))
-- Rect 0.0 1.3333333333333335 -0.33333333333333337 1.0
quadBox :: QuadPosition Double -> Rect Double
quadBox p = space1 pts
  where
    ts = quadDerivs p
    pts = quadBezier p <$> ([0,1] <> ts)

-- | cubic bezier curve
--
-- Note that the ordering is different to the svg standard.
data CubicPosition a =
  CubicPosition
  { -- | starting point
    cposStart :: Point a,
    -- | ending point
    cposEnd :: Point a,
    -- | control point 1
    cposControl1 :: Point a,
    -- | control point 2
    cposControl2 :: Point a
  } deriving (Eq, Show, Generic)

-- | A polar representation of a cubic bezier with control points expressed as polar and normalised to the start - end line.
data CubicPolar a =
  CubicPolar
  { -- | starting point
    cpolStart :: Point a,
    -- | ending point
    cpolEnd :: Point a,
    -- | control point in terms of distance from and angle to the start end line
    cpolControl1 :: Polar a a,
    -- | control point in terms of distance from and angle to the start end line
    cpolControl2 :: Polar a a
  } deriving (Eq, Show, Generic)

-- | Convert from a positional to a polar representation of a cubic bezier.
--
-- > cubicPosition . cubicPolar == id
-- > cubicPolar . cubicPosition == id
--
-- >>> cubicPolar (CubicPosition (Point 0 0) (Point 1 1) (Point 1 -1) (Point 0 2))
-- CubicPolar {cpolStart = Point 0.0 0.0, cpolEnd = Point 1.0 1.0, cpolControl1 = Polar {magnitude = 1.1180339887498947, direction = -1.2490457723982544}, cpolControl2 = Polar {magnitude = 1.1180339887498947, direction = 1.8925468811915387}}
cubicPolar :: (ExpField a, TrigField a) => CubicPosition a -> CubicPolar a
cubicPolar (CubicPosition start end control1 control2) = CubicPolar start end control1' control2'
  where
    mp = (start + end) /. two
    control1' = polar $ (control1 - mp) /. norm (end - start)
    control2' = polar $ (control2 - mp) /. norm (end - start)

-- | Convert from a polar to a positional representation of a cubic bezier.
--
-- > cubicPosition . cubicPolar == id
-- > cubicPolar . cubicPosition == id
--
-- >>> cubicPosition $ cubicPolar (CubicPosition (Point 0 0) (Point 1 1) (Point 1 -1) (Point 0 2))
-- CubicPosition {cposStart = Point 0.0 0.0, cposEnd = Point 1.0 1.0, cposControl1 = Point 1.0 -1.0, cposControl2 = Point 1.6653345369377348e-16 2.0}
cubicPosition :: (ExpField a, TrigField a) => CubicPolar a -> CubicPosition a
cubicPosition (CubicPolar start end control1 control2) = CubicPosition start end control1' control2'
  where
    control1' = norm (end - start) .* coord control1 + (start + end) /. two
    control2' = norm (end - start) .* coord control2 + (start + end) /. two

-- | The cubic bezier equation
--
-- >>> cubicBezier (CubicPosition (Point 0 0) (Point 1 1) (Point 1 -1) (Point 0 2)) 0.8535533905932737
-- Point 0.6767766952966369 1.2071067811865475
cubicBezier :: (ExpField a, FromInteger a) => CubicPosition a -> a -> Point a
cubicBezier (CubicPosition start end control1 control2) theta =
  (1 - theta)^3 .* start +
  3 * (1-theta)^2 * theta .* control1 +
  3 * (1-theta) * theta^2 .* control2 +
  theta^3 .* end

-- | Turning point positions for a CubicPosition (0,1 or 2)
--
-- >>> cubicDerivs (CubicPosition (Point 0 0) (Point 1 1) (Point 1 -1) (Point 0 2))
-- [0.8535533905932737,0.14644660940672624,0.5]
cubicDerivs :: CubicPosition Double -> [Double]
cubicDerivs (CubicPosition (Point c0x c0y) (Point c3x c3y)
             (Point c1x c1y) (Point c2x c2y)) =
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
--
-- ![Cubic]("other/cubic.svg")
cubicBox :: CubicPosition Double -> Rect Double
cubicBox p = space1 pts
  where
    ts = cubicDerivs p
    pts = cubicBezier p <$>
          filter
          (|.| Range 0 1)
          ([0,1] <> ts)

-- | Bounding box for a list of path XYs.
--
pathBoxes :: [(PathInfo Double, Point Double)] -> Maybe (Rect Double)
pathBoxes [] = Nothing
pathBoxes (x:xs) =
  L.fold (L.Fold step begin (Just . snd)) xs
  where
    begin :: (Point Double, Rect Double)
    begin = (snd x, singleton (snd x))
    step ::
      (Point Double, Rect Double) ->
      (PathInfo Double, Point Double) ->
      (Point Double, Rect Double)
    step (start, r) a = (snd a, pathBox start a <> r)

-- | Bounding box for a path info, start and end Points.
--
pathBox :: Point Double -> (PathInfo Double, Point Double) -> Rect Double
pathBox start (info, end) =
  case info of
    StartI -> singleton end
    LineI -> space1 [start, end]
    CubicI c1 c2 -> cubicBox (CubicPosition start end c1 c2)
    QuadI c -> quadBox (QuadPosition start end c)
    ArcI i -> arcBox (ArcPosition start end i)

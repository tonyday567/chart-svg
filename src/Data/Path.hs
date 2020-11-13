{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Data.Path
  ( PathInfo (..),
    ArcInfo (..),
    ArcPosition (..),
    parsePath,
    toInfo,
    toInfos,
    toPathAbsolute,
    toPathAbsolutes,
    ArcCentroid (..),
    arcCentroid,
    arcPosition,
    arcBox,
    arcBoxes,
    arcDerivs,
    ellipse,
    toRadii,
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

-- | Every element of an svg path can be thought of as exactly two points in space, with instructions of how to draw a curve between them.  A path chart is thus very similar to a line chart, with a lot more information about style.
--
-- <https://www.iquilezles.org/www/articles/bezierbbox/bezierbbox.htm>
--
-- [bounding box calcs](https://eliot-jones.com/2019/12/cubic-bezier-curve-bounding-boxes)
-- [alternative bounding box calc](https://stackoverflow.com/questions/24809978/calculating-the-bounding-box-of-cubic-bezier-curve)
--
-- [wiki](https://en.wikipedia.org/wiki/B%C3%A9zier_curve#:~:text=A%20B%C3%A9zier%20curve%20is%20defined,not%20lie%20on%20the%20curve.)
--

-- | parse a raw path string
--
-- > let outerseg1 = "M-1.0,0.5 A0.5 0.5 0.0 1 1 0.0,-1.2320508075688774 1.0 1.0 0.0 0 0 -0.5,-0.3660254037844387 1.0 1.0 0.0 0 0 -1.0,0.5 Z"
-- > parsePath outerseg1
-- [MoveTo OriginAbsolute [V2 (-1.0) 0.5],EllipticalArc OriginAbsolute [(0.5,0.5,0.0,True,True,V2 0.0 (-1.2320508075688774)),(1.0,1.0,0.0,False,False,V2 (-0.5) (-0.3660254037844387)),(1.0,1.0,0.0,False,False,V2 (-1.0) 0.5)],EndPath]
--
-- https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/d
--
parsePath :: Text -> [PathCommand]
parsePath t = either (const []) id $ A.parseOnly pathParser t

-- | Path instructions can be split into the points between lines and the instructions for creating each line.
--
-- [(PathInfo, Point Double)]
--
-- PathInfo needs to be invariant to affine transformations of the points.
data PathInfo a =
  StartI |
  LineI |
  CubicI (Polar a a) (Polar a a) |
  QuadI (Polar a a) |
  ArcI (ArcInfo a)
  deriving (Show, Eq, Generic)

-- | convert from a path info, start point, end point triple to a path text clause.
--
-- Note that morally,
--
-- > toPathsAbsolute . toInfos . parsePath == id
--
-- but the round trip is forgetful on many details:
--
-- - spacing
--
-- - "Z" is replaced by a line instruction
--
-- - sequences of the same instruction type are uncompressed
--
-- - relative is replaced by absolute
--
-- - implicit L in multiple M instructions is separated.
--
toPathAbsolute ::
  -- | (info, start, end)
  (PathInfo Double, Point Double, Point Double) ->
  -- | path text
  Text
toPathAbsolute (StartI,p,_) = "M " <> pp p
toPathAbsolute (LineI,p,_) = "L " <> pp p
toPathAbsolute ((CubicI c1 c2), next, prev) =
  "C " <>
  pp c1' <> " " <>
  pp c2' <> " " <>
  pp next
  where
    (CubicPosition _ _ c1' c2') = cubicPosition (CubicPolar prev next c1 c2)
toPathAbsolute ((QuadI control), next, prev) =
  "Q " <>
  pp control' <> " " <>
  pp next
  where
    (QuadPosition _ _ control') = quadPosition (QuadPolar prev next control)
toPathAbsolute (ArcI (ArcInfo (Point x y) phi' sw l), x2, x1) =
  "A " <>
  show (x * norm (x2 - x1)) <> " " <>
  show (y * norm (x2 - x1)) <> " " <>
  -- in degrees and clockwise is positive
  show (-phi' * 180 / pi) <> " " <>
  bool "0" "1" sw <> " " <>
  bool "0" "1" l <> " " <>
  pp x2

-- | render a point, including polarity reversal
pp :: Point Double -> Text
pp (Point x y) = show x <> "," <> show (bool (-y) y (y==zero))

-- | convert a (info, point) list to an svg d path text.
toPathAbsolutes :: [(PathInfo Double, Point Double)] -> Text
toPathAbsolutes xs = L.fold (L.Fold step begin done) xs
  where
    -- reversing y polarity
    done = Text.intercalate " " . reverse . snd
    -- (previous point, accumulated text)
    begin = (zero, [])
    step (prev, ts) (info, next) = (next , toPathAbsolute (info, next, prev):ts)


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
toInfo s EndPath = (s, [(LineI, (s ^. #start))])
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
      let pos = CubicPosition (s ^. #cur) x2 c1 c2 in
        let (CubicPolar _ _ c1' c2') = cubicPolar pos in
          (s & #cur .~ x2 & #infoControl .~ c2, (CubicI c1' c2', x2):p)
toInfo s (CurveTo OriginRelative xs) =
  L.fold (L.Fold step (s,[]) (second reverse)) xs'
  where
    xs' = (\(c1,c2,x2) -> (fromV2 c1, fromV2 c2, fromV2 x2)) <$> xs
    step (s, p) (c1,c2,x2) =
      let pos = CubicPosition (s ^. #cur) (x2 + s ^. #cur) (c1 + s ^. #cur) (c2 + s ^. #cur) in
        let (CubicPolar _ _ c1' c2') = cubicPolar pos in
          (s & #cur .~ (x2 + s ^. #cur) & #infoControl .~ (c2 + s ^. #cur), (CubicI c1' c2', x2 + s ^. #cur):p)
toInfo s (SmoothCurveTo OriginAbsolute xs) =
  L.fold (L.Fold step (s,[]) (second reverse)) xs'
  where
    xs' = (\(c2,x2) -> (fromV2 c2, fromV2 x2)) <$> xs
    step (s, p) (c2,x2) =
      let pos = CubicPosition (s ^. #cur) x2 (s ^. #cur - (s ^. #infoControl - s^. #cur)) c2 in
        let (CubicPolar _ _ c1' c2') = cubicPolar pos in
          (s & #cur .~ x2, (CubicI c1' c2', x2):p)
toInfo s (SmoothCurveTo OriginRelative xs) =
  L.fold (L.Fold step (s,[]) (second reverse)) xs'
  where
    xs' = (\(c2,x2) -> (fromV2 c2, fromV2 x2)) <$> xs
    step (s, p) (c2,x2) =
      let pos = CubicPosition (s ^. #cur) (x2 + s ^. #cur) (s ^. #cur - (s ^. #infoControl - s^. #cur)) (c2 + s ^. #cur) in
        let (CubicPolar _ _ c1' c2') = cubicPolar pos in
          (s &
           #cur .~ (x2 + s ^. #cur) &
           #infoControl .~ (c2 + s ^. #cur),
           (CubicI c1' c2', (x2 + s ^. #cur)):p)
toInfo s (QuadraticBezier OriginAbsolute xs) =
  L.fold (L.Fold step (s,[]) (second reverse)) xs'
  where
    xs' = (\(c1,x2) -> (fromV2 c1, fromV2 x2)) <$> xs
    step (s, p) (c1, x2) =
      let pos = QuadPosition (s ^. #cur) x2 c1 in
        let (QuadPolar _ _ c') = quadPolar pos in
          (s &
           #cur .~ x2 &
           #infoControl .~ c1,
           (QuadI c', x2):p)
toInfo s (QuadraticBezier OriginRelative xs) =
  L.fold (L.Fold step (s,[]) (second reverse)) xs'
  where
    xs' = (\(c1,x2) -> (fromV2 c1, fromV2 x2)) <$> xs
    step (s, p) (c1,x2) =
      let pos = QuadPosition (s ^. #cur) (x2 + s ^. #cur) (c1 + s ^. #cur) in
        let (QuadPolar _ _ c') = quadPolar pos in
          (s & #cur .~ x2 & #infoControl .~ (c1 + s ^. #cur), (QuadI c', (x2 + s ^. #cur)):p)
toInfo s (SmoothQuadraticBezierCurveTo OriginAbsolute xs) =
  L.fold (L.Fold step (s,[]) (second reverse)) xs'
  where
    xs' = fromV2 <$> xs
    step (s, p) x2 =
      let pos = QuadPosition (s ^. #cur) x2 (s ^. #cur - (s ^. #infoControl - s^. #cur)) in
        let (QuadPolar _ _ c') = quadPolar pos in
          (s &
           #cur .~ x2 &
           #infoControl .~ (s ^. #cur - (s ^. #infoControl - s ^. #cur)),
           (QuadI c', x2):p)
toInfo s (SmoothQuadraticBezierCurveTo OriginRelative xs) =
  L.fold (L.Fold step (s,[]) (second reverse)) xs'
  where
    xs' = fromV2 <$> xs
    step (s, p) x2 =
      let pos = QuadPosition (s ^. #cur) (x2 + s ^. #cur) (s ^. #cur - (s ^. #infoControl - s^. #cur)) in
        let (QuadPolar _ _ c') = quadPolar pos in
          (s &
           #cur .~ (x2 + s ^. #cur) &
           #infoControl .~ (s ^. #cur - (s ^. #infoControl - s ^. #cur)),
           (QuadI c', (x2 + s ^. #cur)):p)
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

-- FIXME: rotation sign is reversed
fromPathEllipticalArc :: (ExpField a) => Point a -> (a, a, a, Bool, Bool, Point a) -> PathInfo a
fromPathEllipticalArc x1 (x, y, r, l, s, x2) = ArcI (ArcInfo (Point x' y') (-r) l s)
  where
    x' = x / norm (x2 - x1)
    y' = y / norm (x2 - x1)

fromV2 :: Linear.V2 a -> Point a
fromV2 (Linear.V2 x y) = Point x y

-- | Convert from a path command list to path info tuples
--
-- FIXME: reversal of y-axis polarity is embedded here.  Where should it go?
--
toInfos :: [SvgTree.PathCommand] -> [(PathInfo Double, Point Double)]
toInfos [] = []
toInfos xs =
  second (\(Point x y) -> Point x (-y)) <$>
  (snd $ foldl' (\(x,l) a -> second (l<>) $ toInfo x a) (stateInfo0,[]) xs)

-- * Arc types

-- | Information specific to an arc path.
data ArcInfo a =
  ArcInfo
  { -- | ellipse radii expressed as a ratio to distance between the two points defining the arc.
    radii :: Point a,
    -- | rotation of the ellipse. Counter-clockwise is positive (which is the opposite to the path command).
    phi :: a,
    large :: Bool,
    -- | sweep means clockwise
    sweep :: Bool
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

-- | convert from ArcInfo spec to ArcCentroid spec.
--
-- FIXME: refactor
--
-- >>> let p1@(Point x1 y1) = Point 0.0 1.2320508075688774
-- >>> let p2@(Point x2 y2) = Point 1.0 -0.5
-- >>> let info@(ArcInfo r phi' l sw) = ArcInfo (Point 1 1) 0.0 True True
-- >>> let arcp1 = ArcPosition (Point 0.0 1.2320508075688774) (Point 1.0 -0.5) (ArcInfo (Point 1 1) 0.0 True True)
-- >>> let arcc1 = arcCentroid arcp1
-- >>> arcc1
-- ArcCentroid {centroid = Point -1.0 -0.4999999999999999, radius = Point 2.0 2.0, cphi = 0.0, ang0 = 1.0471975511965979, angdiff = 5.235987755982988}
--
arcCentroid :: (FromInteger a, Ord a, TrigField a, ExpField a) => ArcPosition a -> ArcCentroid a
arcCentroid (ArcPosition p1@(Point x1 y1) p2@(Point x2 y2) (ArcInfo rad phi large sweep)) = ArcCentroid c (Point rx ry) phi ang1 angd
  where
    (Point x1' y1') = rotate phi ((p1 - p2) /. two)
    (Point rx' ry') = toRadii p1 p2 rad
    l = x1'**2/rx'**2 + y1'**2/ry'**2
    (rx,ry) = bool (rx',ry') (rx'*sqrt l, ry'*sqrt l) (l > 1)
    snumer = max 0 $ (rx*rx*ry*ry) - (rx*rx*y1'**2) - (ry*ry*x1'**2)
    s = (bool -1 1 (large == sweep)) * sqrt
      (snumer / (rx*rx*y1'**2 + ry*ry*x1'**2))
    cx' = s *  rx * y1' / ry
    cy' = s * (-ry) * x1' / rx
    cx = (x1 + x2) / 2 + cos (phi) * cx' - sin (phi) * cy'
    cy = (y1 + y2) / 2 + sin (phi) * cx' + cos (phi) * cy'
    c = Point cx cy
    -- c = p1 + p2 - Point cx cy
    ang1 = angle (Point ((x1'-cx')/rx) ((y1'-cy')/ry))
    ang2 = angle (Point ((-x1'-cx')/rx) ((-y1'-cy')/ry))
    angd' = ang2 - ang1
    angd = bool 0 (-2*pi) (sweep && angd'>0) + bool 0 (2*pi) (not sweep && angd'<0) + angd'

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
-- >>> arcPosition $ arcCentroid (ArcPosition (Point 0.0 0.0) (Point 0.0 1.0) (ArcInfo (Point 1.0 0.5) (pi/6) True True))
-- ArcPosition {posStart = Point -1.1102230246251565e-16 -5.551115123125783e-17, posEnd = Point -5.551115123125783e-17 1.0, posInfo = ArcInfo {radii = Point 1.0 0.5, phi = 0.5235987755982988, large = True, sweep = True}}
--
arcPosition :: (Ord a, Signed a, TrigField a) => ArcCentroid a -> ArcPosition a
arcPosition (ArcCentroid (Point cx cy) r@(Point rx ry) phi ang1 angd) = ArcPosition (Point x1 y1) (Point x2 y2) (ArcInfo r phi large sweep)
  where
    x1 = cos phi * rx * cos ang1 - sin phi * ry * sin ang1 + cx
    y1 = sin phi * rx * cos ang1 + cos phi * ry * sin ang1 + cy
    x2 = cos phi * rx * cos (ang1-angd) - sin phi * ry * sin (ang1-angd) + cx
    y2 = sin phi * rx * cos (ang1-angd) + cos phi * ry * sin (ang1-angd) + cy
    large = (abs angd) >= pi
    sweep = angd < zero

arcBoxes :: [(PathInfo Double, Point Double)] -> Maybe (Rect Double)
arcBoxes [] = Nothing
arcBoxes (x:xs) = Just $ snd $ foldl' (\(prev,r) (i, p) ->
                                  (p, r & case i of
                                      (ArcI a) -> (<>) (arcBox (ArcPosition prev p a))
                                      _ -> id)
                               )
              (snd x, let (Point x' y') = snd x in Rect x' x' y' y')
              xs

-- | Convert from ratio radii to absolute radii
toRadii :: (ExpField a) => Point a -> Point a -> Point a -> Point a
toRadii p1 p2 (Point ratiox ratioy) =
  Point
  (ratiox * norm (p1 - p2))
  (ratioy * norm (p1 - p2))

-- | ellipse formulae
--
-- x-axis rotation is counter-clockwise.
ellipse :: Point Double -> Point Double -> Double -> Double -> Point Double
ellipse (Point cx cy) (Point rx ry) phi theta =
  Point
  (cx + rx * cos theta * cos phi - ry * sin theta * sin phi)
  (cy + rx * cos theta * sin phi + ry * sin theta * cos phi)

-- | compute the bounding box for an arcBox
--
-- >>> arcBox (Point 0.0 1.2320508075688774) (Point 1.0 -0.5) (ArcInfo (Point 0.5 0.5) 0.0 True True)
-- Rect -1.1102230246251565e-16 1.5 -0.4999999999999998 1.3660254037844388
--
-- Refs:
--
-- https://github.polettix.it/ETOOBUSY/2020/07/21/ellipses-in-svg-center/
--
-- https://github.polettix.it/ETOOBUSY/2020/07/23/ellipses-in-svg-transformation/
--
-- http://fridrich.blogspot.com/2011/06/bounding-box-of-svg-elliptical-arc.html
--
-- sweep and large explanations:
--
-- https://stackoverflow.com/questions/47684885/arc-svg-parameters
--
-- svg d doc
--
-- https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/d
--
-- path docs
--
-- https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths
--
-- file:///Users/tonyday/haskell/chart-svg/other/path.svg
--
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

arcDerivs :: Point Double -> Double -> (Double, Double)
arcDerivs (Point rx ry) phi = (thetax1, thetay1)
  where
    thetax1 = atan2 (-sin phi * ry) (cos phi * rx)
    thetay1 = atan2 (cos phi * ry) (sin phi * rx)

-- * bezier
-- | quadratic bezier curve
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

-- | Control point expressed relative to end points
data QuadPolar a =
  QuadPolar
  { -- | starting point
    qpolStart :: Point a,
    -- | ending point
    qpolEnd :: Point a,
    -- | control point in terms of distance from and angle to the qp0 - qp2 line
    qpolControl :: Polar a a
  } deriving (Eq, Show, Generic)

-- |
--
-- >>> quadPolar (QuadPosition (Point 0 0) (Point 1 1) (Point 1 0))
-- QuadPolar {qpolStart = Point 0.0 0.0, qpolEnd = Point 1.0 1.0, qpolControl = Polar {magnitude = 0.7071067811865476, direction = -0.7853981633974483}}
--
-- "M 0.0,-0.0 Q 1.0,0.0 1.0,-1.0 L 0.0,-0.0"
quadPolar :: (ExpField a, TrigField a) => QuadPosition a -> QuadPolar a
quadPolar (QuadPosition start end control) = QuadPolar start end control'
  where
    mp = (start + end) /. two
    control' = polar (control - mp)

-- |
--
-- > quadPosition . quadPolar == id
-- > quadPolar . quadPosition == id
--
-- >>> quadPosition $ quadPolar (QuadPosition (Point 0 0) (Point 1 1) (Point 1 0))
-- QuadPosition {qposStart = Point 0.0 0.0, qposEnd = Point 1.0 1.0, qposControl = Point 1.0 0.0}
quadPosition :: (ExpField a, TrigField a) => QuadPolar a -> QuadPosition a
quadPosition (QuadPolar start end control) = QuadPosition start end control'
  where
    control' = coord control + (start + end) /. two

-- |
--
quadBezier :: (ExpField a, FromInteger a) => QuadPosition a -> a -> Point a
quadBezier (QuadPosition start end control) theta =
  (1 - theta) ^ (2 :: Int) .* start +
  2 * (1-theta) * theta .* control +
  theta ^ (2 :: Int) .* end

-- |
--
-- <https://www.iquilezles.org/www/articles/bezierbbox/bezierbbox.htm>
--
quadDerivs :: QuadPosition Double -> Point Double
quadDerivs (QuadPosition start end control) = (start - control) / (start - 2 .* control + end)

-- |
--
quadBox :: QuadPosition Double -> Rect Double
quadBox p = space1 pts
  where
    (Point tx ty) = quadDerivs p
    pts = quadBezier p <$> [0,1,tx,ty]

-- | cubic bezier curve
--
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

-- | Control point expressed relative to end points
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

-- |
--
-- >>> cubicPolar (CubicPosition (Point 0 0) (Point 1 1) (Point 2 -1) (Point -1 2))
-- CubicPolar {cpolStart = Point 0.0 0.0, cpolEnd = Point 1.0 1.0, cpolControl1 = Polar {magnitude = 2.1213203435596424, direction = -0.7853981633974483}, cpolControl2 = Polar {magnitude = 2.1213203435596424, direction = 2.356194490192345}}
--
-- FIXME: <path xmlns="http://www.w3.org/2000/svg" d="M 0.0,0.0 C 2.1213203435596433,2.1213203435596433 -2.1213203435596433,-2.1213203435596433 1.0000000000000007,-1.0 L 0.0,0.0"/>
--
-- "M 0.0,-0.0 C 1.0,0.0 0.0,1.0 1.0,-1.0 L 0.0,-0.0"
cubicPolar :: (ExpField a, TrigField a) => CubicPosition a -> CubicPolar a
cubicPolar (CubicPosition start end control1 control2) = CubicPolar start end control1' control2'
  where
    mp = (start + end) /. two
    control1' = polar (control1 - mp)
    control2' = polar (control2 - mp)

-- |
--
-- > cubicPosition . cubicPolar == id
-- > cubicPolar . cubicPosition == id
--
-- >>> cubicPosition $ cubicPolar (CubicPosition (Point 0 0) (Point 1 1) (Point 2 -1) (Point -1 2))
-- CubicPosition {cposStart = Point 0.0 0.0, cposEnd = Point 1.0 1.0, cposControl1 = Point 2.0 -0.9999999999999998, cposControl2 = Point -0.9999999999999998 2.0}
cubicPosition :: (ExpField a, TrigField a) => CubicPolar a -> CubicPosition a
cubicPosition (CubicPolar start end control1 control2) = CubicPosition start end control1' control2'
  where
    control1' = coord control1 + (start + end) /. two
    control2' = coord control2 + (start + end) /. two

-- |
--
cubicBezier :: (ExpField a, FromInteger a) => CubicPosition a -> a -> Point a
cubicBezier (CubicPosition start end control1 control2) theta =
  (1 - theta) ^ (3::Int) .* start +
  3 * (1-theta) ^ (2::Int) * theta .* control1 +
  3 * (1-theta) * theta ^ (2::Int) .* control2 +
  theta ^ (3 :: Int) .* end

-- |
--
-- <https://www.iquilezles.org/www/articles/bezierbbox/bezierbbox.htm>
--
cubicDerivs :: CubicPosition Double -> (Point Double, Point Double)
cubicDerivs (CubicPosition start end control1 control2) = (t0,t1)
  where
    a = -start + 3 .* control1 - 3 .* control2 + end
    b = start - 2 .* control1 + control2
    c = -start + end
    n = fmap (sqrt) (b ^ (2::Int) - a * c)
    t0 = (-b + n) / a
    t1 = (-b - n) / a

-- |
-- FIXME: slightly out???
--
-- [cubicBox](other/cubic.svg)
--
cubicBox :: CubicPosition Double -> Rect Double
cubicBox p = space1 pts
  where
    (Point tx0 ty0, Point tx1 ty1) = cubicDerivs p
    pts = cubicBezier p <$>
          filter
          (|.| (Range 0 1))
          [0,1,tx0,ty0,tx1,ty1]

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedLabels #-}
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
  ) where

import qualified Graphics.SvgTree as SvgTree
import Graphics.SvgTree (PathCommand (..), Origin(..))
import Graphics.SvgTree.PathParser
import qualified Data.Attoparsec.Text as A
import NumHask.Space
import NumHask.Prelude hiding (rotate)
import qualified Linear

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
toPathAbsolute :: (PathInfo Double, Point Double, Point Double) -> Text
toPathAbsolute (StartI,p,_) = "M " <> pp p
toPathAbsolute (LineI,p,_) = "L " <> pp p
toPathAbsolute ((CubicI c1 c2), x2, x1) =
  "C " <>
  pp ((norm (x2 - x1) :: Double) .* coord c1) <> " " <>
  pp ((norm (x2 - x1) :: Double) .* coord c2) <> " " <>
  pp x2
toPathAbsolute ((QuadI c1), x2, x1) =
  "Q " <>
  pp ((norm (x2 - x1) :: Double) .* coord c1) <> " " <>
  pp x2
toPathAbsolute (ArcI (ArcInfo (Point x y) phi' sw l), x2, x1) =
  "A " <>
  show (x * norm (x2 - x1)) <> " " <>
  show (y * norm (x2 - x1)) <> " " <>
  -- in degrees and clockwise is positive
  show (-phi' * 180 / pi) <> " " <>
  bool "0" "1" sw <> " " <>
  bool "0" "1" l <> " " <>
  pp x2

pp :: Point Double -> Text
pp (Point x y) = show x <> "," <> show y

-- | convert a path info, point list to an svg d path text.
toPathAbsolutes :: [(PathInfo Double, Point Double)] -> Text
toPathAbsolutes xs =
  snd $ foldl' (\(prev,t) (i, p) -> (p,t<>" "<>toPathAbsolute (i,p,prev))) (zero,mempty) (second (\(Point x y) -> Point x (-y)) <$> xs)

-- | Convert a path command fragment to an instruction + point.
--
toInfo :: (Point Double, Point Double) -> SvgTree.PathCommand -> ((Point Double, Point Double), [(PathInfo Double, Point Double)])
toInfo (current, start) (MoveTo _ []) = ((current, start), [])
toInfo _ (MoveTo OriginAbsolute (x:xs)) =
  second reverse $
  foldl' (\((_,s),st) a -> ((a,s),(LineI, a):st)) ((fromV2 x, fromV2 x), [(StartI, fromV2 x)]) (fromV2 <$> xs)
toInfo (current, _) (MoveTo OriginRelative (x:xs)) =
  second reverse $ foldl' (\((c,s),st) a -> ((c+a,s),(LineI, c+a):st)) ((current, current), [(StartI, current+fromV2 x)]) (fromV2 <$> xs)
toInfo (current, start) (LineTo OriginAbsolute xs) =
  second reverse $ foldl' (\((_,s),x) a -> ((a,s),(LineI, a):x)) ((current, start), []) (fromV2 <$> xs)
toInfo (current, start) (LineTo OriginRelative xs) =
  second reverse $ foldl' (\((c,s),x) a -> ((c+a,s),(LineI, c+a):x)) ((current, start), []) (fromV2 <$> xs)
toInfo (current, start) (HorizontalTo OriginAbsolute xs) =
  second reverse $ foldl' (\((Point _ cy,s),x) a -> ((Point a cy,s), (LineI, Point a cy):x)) ((current,start), []) xs
toInfo (current,start) (HorizontalTo OriginRelative xs) =
  second reverse $ foldl' (\((Point cx cy,s),x) a -> ((Point (cx+a) cy,s),(LineI, Point (cx+a) cy):x)) ((current,start), []) xs
toInfo (current,start) (VerticalTo OriginAbsolute xs) =
  second reverse $ foldl' (\((Point cx _,s),x) a -> ((Point cx a,s), (LineI, Point cx a):x)) ((current,start), []) xs
toInfo (current,start) (VerticalTo OriginRelative xs) =
  second reverse $ foldl' (\((Point cx cy,s),x) a -> ((Point cx (cy+a),s),(LineI, Point cx (cy+a)):x)) ((current,start), []) xs
toInfo (current,start) (CurveTo OriginAbsolute xs) =
  second reverse $ foldl' (\((c,s),st) a@(_,_,x2) -> ((x2,s), (fromPathCurve c a, x2):st)) ((current,start), []) ((\(c1,c2,x2) -> (fromV2 c1, fromV2 c2, fromV2 x2)) <$> xs)
toInfo (current,start) (CurveTo OriginRelative xs) =
  second reverse $ foldl' (\((c,s),st) (c1,c2,x2) -> ((x2+c,s), ((fromPathCurve c (c1+c, c2+c, x2+c), x2+c):st))) ((current,start), []) ((\(c1,c2,x2) -> (fromV2 c1, fromV2 c2, fromV2 x2)) <$> xs)
toInfo (current,start) (QuadraticBezier OriginAbsolute xs) =
  second reverse $ foldl' (\((c,s),st) a@(_,x2) -> ((x2,s), (fromPathQuad c a, x2):st)) ((current,start), []) ((\(c1,x2) -> (fromV2 c1, fromV2 x2)) <$> xs)
toInfo (current,start) (QuadraticBezier OriginRelative xs) =
  second reverse $ foldl' (\((c,s),st) (c1,x2) -> ((x2+c,s), ((fromPathQuad c (c1+c, x2+c), x2+c):st))) ((current,start), []) ((\(c1,x2) -> (fromV2 c1, fromV2 x2)) <$> xs)
toInfo (current,start) EndPath =
  ((current,start), [(LineI, start)])
toInfo (current,start) (EllipticalArc OriginAbsolute xs) =
  second reverse $ foldl' (\((c,s),st) a@(_,_,_,_,_,x2) -> ((x2,s), (fromPathEllipticalArc c a, x2):st)) ((current,start), []) ((\(x,y,r,l,sw,x2) -> (x,y,r,l,sw,fromV2 x2)) <$> xs)
toInfo (current,start) (EllipticalArc OriginRelative xs) =
  second reverse $ foldl' (\((c,s),st) a@(_,_,_,_,_,x2) -> ((x2+c,s), (fromPathEllipticalArc c a, (x2+c)):st)) ((current,start), []) ((\(x,y,r,l,sw,x2) -> (x,y,r,l,sw,fromV2 x2)) <$> xs)
-- FIXME: needs to remember previous control point in State
toInfo (c,s) (SmoothCurveTo _ _) = ((c,s),[])
toInfo (c,s) (SmoothQuadraticBezierCurveTo _ _) = ((c,s),[])

fromPathCurve :: (ExpField a, TrigField a) => Point a -> (Point a, Point a, Point a) -> PathInfo a
fromPathCurve x1 (c1, c2, x2) = CubicI (Polar mag1 angle1) (Polar mag2 angle2)
  where
    mag1 = norm (c1 - x1) / norm (x2 - x1)
    angle1 = angle (c1 - x2)
    mag2 = norm (c2 - x2) / norm (x1 - x2)
    angle2 = angle (c2 - x1)

-- rotation sign is reversed
fromPathEllipticalArc :: (ExpField a) => Point a -> (a, a, a, Bool, Bool, Point a) -> PathInfo a
fromPathEllipticalArc x1 (x, y, r, l, s, x2) = ArcI (ArcInfo (Point x' y') (-r) l s)
  where
    x' = x / norm (x2 - x1)
    y' = y / norm (x2 - x1)

fromPathQuad :: (ExpField a, TrigField a) => Point a -> (Point a, Point a) -> PathInfo a
fromPathQuad x1 (c1, x2) = QuadI (Polar mag1 angle1)
  where
    mag1 = norm (c1 - x1) / norm (x2 - x1)
    angle1 = angle (c1 - x2)

fromV2 :: Linear.V2 a -> Point a
fromV2 (Linear.V2 x y) = Point x y

-- | Convert from a path command list to path info tuples
--
-- FIXME: reversal of y-axis polarity is embedded here.  Where should it go?
--
toInfos :: [SvgTree.PathCommand] -> [(PathInfo Double, Point Double)]
toInfos [] = []
toInfos xs = second (\(Point x y) -> Point x (-y)) <$> (snd $ foldl' (\(x,l) a -> second (l<>) $ toInfo x a) ((zero,zero),[]) xs)


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

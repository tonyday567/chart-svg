{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Data.Path
  ( -- writePath,
    -- writePaths,
    ArcStuff (..),
    PathInstruction (..),
    parsePath,
    toInstruction,
    toInstructions,
    toPathAbsolute,
    toPathAbsolutes,
    arcCenter,
    arcBox,
    arcBoxes,
    fromPathEllipticalArc,
    fromV2,
    arcC',
    arcExtrema,
    ellipse,
    toAbsRadius,
  ) where

import qualified Graphics.SvgTree as SvgTree
import Graphics.SvgTree (PathCommand (..), Origin(..))
import Graphics.SvgTree.PathParser
import qualified Data.Attoparsec.Text as A
import NumHask.Space
import NumHask.Prelude hiding (rotate)
import qualified Linear
import Control.Lens hiding ((...))

-- Every element of an svg path can be thought of as exactly two points in space, with instructions of how to draw a line between them.  Thus a Path chart is very similar to a line chart, with a lot more information about style.
--
-- <https://www.iquilezles.org/www/articles/bezierbbox/bezierbbox.htm>
--
-- [bounding box calcs](https://eliot-jones.com/2019/12/cubic-bezier-curve-bounding-boxes)
-- [alternative bounding box calc](https://stackoverflow.com/questions/24809978/calculating-the-bounding-box-of-cubic-bezier-curve)
--
-- [wiki](https://en.wikipedia.org/wiki/B%C3%A9zier_curve#:~:text=A%20B%C3%A9zier%20curve%20is%20defined,not%20lie%20on%20the%20curve.)
--
-- Should be able to translate a path into:
--
-- [(PathInstruction, Point Double)]
--
-- where PathStyle is invariant to wholesale scale, translation and rotation changes over the Points.

-- Start with a shape from venn:
--
-- > let outerseg1 = "M-1.0,0.5 A0.5 0.5 0.0 1 1 0.0,-1.2320508075688774 1.0 1.0 0.0 0 0 -0.5,-0.3660254037844387 1.0 1.0 0.0 0 0 -1.0,0.5 Z"
-- > parsePath outerseg1
-- [MoveTo OriginAbsolute [V2 (-1.0) 0.5],EllipticalArc OriginAbsolute [(0.5,0.5,0.0,True,True,V2 0.0 (-1.2320508075688774)),(1.0,1.0,0.0,False,False,V2 (-0.5) (-0.3660254037844387)),(1.0,1.0,0.0,False,False,V2 (-1.0) 0.5)],EndPath]
--
-- https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/d
--
parsePath :: Text -> [PathCommand]
parsePath t = either (const []) id $ A.parseOnly pathParser t

data ArcStuff a = ArcStuff { radiusRatio :: Point a, phi :: a, sweep :: Bool, large :: Bool} deriving (Eq, Show, Generic)

data PathInstruction a = StartI | LineI | CubicI (Polar a a) (Polar a a) | QuadI (Polar a a) | ArcI (ArcStuff a) deriving (Show, Eq, Generic)

pp :: Point Double -> Text
pp (Point x y) = show x <> "," <> show y

-- | convert from instructions to path text.
--
-- morally,
-- > toPathAbsolute . toInstructions . parsePath == id
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
toPathAbsolute :: (PathInstruction Double, Point Double, Point Double) -> Text
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
toPathAbsolute (ArcI (ArcStuff (Point x y) r sw l), x2, x1) =
  "A " <>
  show (x * norm (x2 - x1)) <> " " <>
  show (y * norm (x2 - x1)) <> " " <>
  show r <> " " <>
  bool "0" "1" sw <> " " <>
  bool "0" "1" l <> " " <>
  pp x2

toPathAbsolutes :: [(PathInstruction Double, Point Double)] -> Text
toPathAbsolutes xs = snd $ foldl' (\(prev,t) (i, p) -> (p,t<>" "<>toPathAbsolute (i,p,prev))) (zero,mempty) (second (\(Point x y) -> Point x (-y)) <$> xs)

fromV2 :: Linear.V2 a -> Point a
fromV2 (Linear.V2 x y) = Point x y

fromPathCurve :: (ExpField a, TrigField a) => Point a -> (Point a, Point a, Point a) -> PathInstruction a
fromPathCurve x1 (c1, c2, x2) = CubicI (Polar mag1 angle1) (Polar mag2 angle2)
  where
    mag1 = norm (c1 - x1) / norm (x2 - x1)
    angle1 = angle (c1 - x2)
    mag2 = norm (c2 - x2) / norm (x1 - x2)
    angle2 = angle (c2 - x1)

fromPathEllipticalArc :: (ExpField a) => Point a -> (a, a, a, Bool, Bool, Point a) -> PathInstruction a
fromPathEllipticalArc x1 (x, y, r, l, s, x2) = ArcI (ArcStuff (Point x' y') r l s)
  where
    x' = x / norm (x2 - x1)
    y' = y / norm (x2 - x1)

fromPathQuad :: (ExpField a, TrigField a) => Point a -> (Point a, Point a) -> PathInstruction a
fromPathQuad x1 (c1, x2) = QuadI (Polar mag1 angle1)
  where
    mag1 = norm (c1 - x1) / norm (x2 - x1)
    angle1 = angle (c1 - x2)

-- | Convert a path command fragment to an instruction + point.
--
toInstruction :: (Point Double, Point Double) -> SvgTree.PathCommand -> ((Point Double, Point Double), [(PathInstruction Double, Point Double)])
toInstruction (current, start) (MoveTo _ []) = ((current, start), [])
toInstruction _ (MoveTo OriginAbsolute (x:xs)) =
  second reverse $
  foldl' (\((_,s),st) a -> ((a,s),(LineI, a):st)) ((fromV2 x, fromV2 x), [(StartI, fromV2 x)]) (fromV2 <$> xs)
toInstruction (current, _) (MoveTo OriginRelative (x:xs)) =
  second reverse $ foldl' (\((c,s),st) a -> ((c+a,s),(LineI, c+a):st)) ((current, current), [(StartI, current+fromV2 x)]) (fromV2 <$> xs)
toInstruction (current, start) (LineTo OriginAbsolute xs) =
  second reverse $ foldl' (\((_,s),x) a -> ((a,s),(LineI, a):x)) ((current, start), []) (fromV2 <$> xs)
toInstruction (current, start) (LineTo OriginRelative xs) =
  second reverse $ foldl' (\((c,s),x) a -> ((c+a,s),(LineI, c+a):x)) ((current, start), []) (fromV2 <$> xs)
toInstruction (current, start) (HorizontalTo OriginAbsolute xs) =
  second reverse $ foldl' (\((Point _ cy,s),x) a -> ((Point a cy,s), (LineI, Point a cy):x)) ((current,start), []) xs
toInstruction (current,start) (HorizontalTo OriginRelative xs) =
  second reverse $ foldl' (\((Point cx cy,s),x) a -> ((Point (cx+a) cy,s),(LineI, Point (cx+a) cy):x)) ((current,start), []) xs
toInstruction (current,start) (VerticalTo OriginAbsolute xs) =
  second reverse $ foldl' (\((Point cx _,s),x) a -> ((Point cx a,s), (LineI, Point cx a):x)) ((current,start), []) xs
toInstruction (current,start) (VerticalTo OriginRelative xs) =
  second reverse $ foldl' (\((Point cx cy,s),x) a -> ((Point cx (cy+a),s),(LineI, Point cx (cy+a)):x)) ((current,start), []) xs
toInstruction (current,start) (CurveTo OriginAbsolute xs) =
  second reverse $ foldl' (\((c,s),st) a@(_,_,x2) -> ((x2,s), (fromPathCurve c a, x2):st)) ((current,start), []) ((\(c1,c2,x2) -> (fromV2 c1, fromV2 c2, fromV2 x2)) <$> xs)
toInstruction (current,start) (CurveTo OriginRelative xs) =
  second reverse $ foldl' (\((c,s),st) (c1,c2,x2) -> ((x2+c,s), ((fromPathCurve c (c1+c, c2+c, x2+c), x2+c):st))) ((current,start), []) ((\(c1,c2,x2) -> (fromV2 c1, fromV2 c2, fromV2 x2)) <$> xs)
toInstruction (current,start) (QuadraticBezier OriginAbsolute xs) =
  second reverse $ foldl' (\((c,s),st) a@(_,x2) -> ((x2,s), (fromPathQuad c a, x2):st)) ((current,start), []) ((\(c1,x2) -> (fromV2 c1, fromV2 x2)) <$> xs)
toInstruction (current,start) (QuadraticBezier OriginRelative xs) =
  second reverse $ foldl' (\((c,s),st) (c1,x2) -> ((x2+c,s), ((fromPathQuad c (c1+c, x2+c), x2+c):st))) ((current,start), []) ((\(c1,x2) -> (fromV2 c1, fromV2 x2)) <$> xs)
toInstruction (current,start) EndPath =
  ((current,start), [(LineI, start)])
toInstruction (current,start) (EllipticalArc OriginAbsolute xs) =
  second reverse $ foldl' (\((c,s),st) a@(_,_,_,_,_,x2) -> ((x2,s), (fromPathEllipticalArc c a, x2):st)) ((current,start), []) ((\(x,y,r,l,sw,x2) -> (x,y,r,l,sw,fromV2 x2)) <$> xs)
toInstruction (current,start) (EllipticalArc OriginRelative xs) =
  second reverse $ foldl' (\((c,s),st) a@(_,_,_,_,_,x2) -> ((x2+c,s), (fromPathEllipticalArc c a, (x2+c)):st)) ((current,start), []) ((\(x,y,r,l,sw,x2) -> (x,y,r,l,sw,fromV2 x2)) <$> xs)
-- FIXME: needs to remember previous control point in State
toInstruction (c,s) (SmoothCurveTo _ _) = ((c,s),[])
toInstruction (c,s) (SmoothQuadraticBezierCurveTo _ _) = ((c,s),[])

-- FIXME: reversal of y-axis polarity here???
toInstructions :: [SvgTree.PathCommand] -> [(PathInstruction Double, Point Double)]
toInstructions [] = []
toInstructions xs = second (\(Point x y) -> Point x (-y)) <$> (snd $ foldl' (\(x,l) a -> second (l<>) $ toInstruction x a) ((zero,zero),[]) xs)

{-
-- | for testing
renderPathToSvg :: Point Double -> Rect Double -> RectStyle -> Text -> Html ()
renderPathToSvg (Point w' h') (Rect x z y w) rs path' =
  with
    ( svg2Tag
        ( (term "g" (attsRect rs) (terms "path" [term "d" path']))
        )
    )
    [ width_ (show w'),
      height_ (show h'),
      makeAttribute "viewBox" (show x <> " " <> show (- w) <> " " <> show (z - x) <> " " <> show (w - y))
    ]

renderPathsToSvg :: Point Double -> Rect Double -> [(RectStyle, Text)] -> Html ()
renderPathsToSvg (Point w' h') (Rect x z y w) paths' =
  with
    ( svg2Tag
        ( mconcat $ ((\(rs, path') -> term "g" (attsRect rs) (terms "path" [term "d" path'])) <$> paths')
        )
    )
    [ width_ (show w'),
      height_ (show h'),
      makeAttribute "viewBox" (show x <> " " <> show (- w) <> " " <> show (z - x) <> " " <> show (w - y))
    ]

writePath :: Rect Double -> Text -> IO ()
writePath vb path' = writeFile "other/path.svg" $ Lazy.toStrict $ renderText $ renderPathToSvg (Point 400 400) vb defaultRectStyle path'

writePaths :: Rect Double -> [Text] -> IO ()
writePaths vb paths' = writeFile "other/path.svg" $ Lazy.toStrict $ renderText $ renderPathsToSvg (Point 400 400) vb (zipWith (\c p -> (defaultRectStyle & #color .~ (setOpac 0.2 c), p)) palette1 paths')


-}

data ArcCenter = ArcCenter { arcC :: Point Double, startAngle :: Double, diffAngle :: Double } deriving (Eq, Show, Generic)

-- | center of the arc
-- following https://www.w3.org/TR/SVG/implnote.html#ArcConversionEndpointToCenter
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
arcCenter :: Point Double -> Point Double -> ArcStuff Double -> ArcCenter
arcCenter p1 p2 (ArcStuff (Point rx' ry') rot' l sw) = ArcCenter c rot difftheta
  where
    (Point x' y') = rotate rot' ((p1-p2) /. (2::Double))
    cmag = sqrt ((rx'*rx'*ry'*ry') - (rx'*rx'*y'*y') - (ry'*ry'*x'*x') /
              ((rx'*rx'*y'*y') + (ry'*ry'*x'*x'))
             )
    (Point cx' cy') = ((bool 1 -1 (l==sw)) * cmag) .* Point (rx' * y' / ry') (-(ry'*x'/rx'))
    c =
      Point
      (cos rot' * cx' - sin rot' * cy')
      (sin rot' * cx' + cos rot' * cy')
      + ((p1 + p2) /. (2::Double))
    rot = angle $ (Point (1::Double) 0) - (Point ((x'-cx')/rx') ((y'-cy')/ry')) :: Double
    difftheta = angle $ (Point ((x'-cx')/rx') ((y'-cy')/ry')) - (Point ((-x'-cx')/rx') ((-y'-cy')/ry')) :: Double

arcBoxes :: [(PathInstruction Double, Point Double)] -> Maybe (Rect Double)
arcBoxes [] = Nothing
arcBoxes (x:xs) = Just $ snd $ foldl' (\(prev,r) (i, p) ->
                                  (p, r & case i of
                                      (ArcI a) -> (<>) (arcBox prev p a)
                                      _ -> id)
                               )
              (snd x, let (Point x' y') = snd x in Rect x' x' y' y')
              xs

{-
arcBox p1@(Point x1 y1) p2@(Point x2 y2) (ArcStuff ratiox ratioy rot l sw) =
  bool (normalise (Rect x1 x2 y1 y2)) (Rect x1'' x2'' y1'' y2'') (radicant'<0 && radicant<0)
  where
    rx0 = ratiox * norm (p2 - p1)
    ry0 = ratioy * norm (p2 - p1)
    (Point xprime yprime) = rotate rot ((p1 - p2) /. (2::Double))
    radicant =
      (rx0*rx0*ry0*ry0-rx0*rx0*yprime*yprime-ry0*ry0*xprime*xprime) /
      (rx0*rx0*yprime*yprime + ry0*ry0*xprime*xprime)
    radicant' = yprime*yprime+xprime*xprime/(rx0*rx0/(ry0*ry0))
    (rx,ry) = bool (rx0,ry0) (sqrt radicant', rx0/ry0*sqrt radicant') (radicant<0)
    (Point cxprime cyprime) =
      bool
      (Point 0 0)
      (Point (fac*rx0*yprime/ry0) (-fac*ry0*xprime/rx0))
      (radicant<0)
    fac = (bool 1 (-1) (l==sw)) * (sqrt $ abs radicant)
    (Point cx cy) =
      Point
      (cxprime * cos rot - cyprime*sin rot + (x1+x2)/2)
      (cxprime * sin rot + cyprime*cos rot + (y1+y2)/2)

    ((Rect x1' x2' y1' y2'),(Rect tx1' tx2' ty1' ty2'))
      | rot == 0 || rot == 180 =
        (Rect (cx-rx) (cx+rx) (cy-ry) (cy+ry),
         Rect (getAngle (-rx) 0) (getAngle rx 0) (getAngle 0 (-ry)) (getAngle 0 ry)
        )
      | rot == 90 || rot == 270 =
        (Rect (cx-ry) (cx+ry) (cy-rx) (cy+rx),
         Rect (getAngle (-ry) 0) (getAngle ry 0) (getAngle 0 (-rx)) (getAngle 0 rx)
        )
      | otherwise =
        (Rect xmin' xmax' ymin' ymax',
         Rect txmin'' txmax'' tymin'' tymax''
        )
        where
          txmin = -atan(ry*tan rot / rx)
          txmax = 180 - atan (ry*tan(rot)/rx)
          xmin = (cx + rx*cos txmin*cos rot - ry*sin txmin*sin rot)
          xmax = (cx + rx*cos txmax*cos rot - ry*sin txmax*sin rot)
          (xmin',xmax') = bool (xmin,xmax) (xmax,xmin) (xmin>xmax)
          (txmin',txmax') = bool (txmin,txmax) (txmax,txmin) (xmin>xmax)
          tmpY = cy + rx*cos txmin'*sin rot + ry*sin txmin'*cos rot
          txmin'' = getAngle (xmin' - cx) (tmpY - cy)
          tmpY' = cy + rx*cos txmax'*sin rot + ry*sin txmax*cos rot
          txmax'' = getAngle(xmax' - cx) (tmpY' - cy)
          tymin = atan(ry/(tan rot*rx))
          tymax = atan(ry/(tan rot*rx))+180
          ymin = cy + rx*cos tymin*sin rot + ry*sin tymin*cos rot
          ymax = cy + rx*cos tymax*sin rot + ry*sin tymax*cos rot
          (ymin',ymax') = bool (ymin,ymax) (ymax,ymin) (ymin>ymax)
          (tymin',tymax') = bool (tymin,tymax) (tymax,tymin) (ymin>ymax)
          tmpX = cx + rx*cos tymin'*cos rot - ry*sin tymin*sin rot
          tymin'' = getAngle (tmpX - cx) (ymin' - cy)
          tmpX' = cx + rx*cos tymax'*cos rot - ry*sin tymax'*sin rot
          tymax'' = getAngle (tmpX' - cx) (ymax' - cy)
    getAngle bx by = fmod (360.0 + (bool -1 1 (by>0)) * acos (bx / sqrt (bx*bx+by*by))) (360.0 :: Double)
    fmod :: Double -> Double -> Double
    fmod n d = n - fromIntegral (floor (n/d) :: Int) * d

    a1 = getAngle (x1-cx) (y1-cy)
    a2 = getAngle (x2-cx) (y2-cy)
    (a1',a2') = bool (a2,a1) (a1,a2) sw
    ((a1'',a2''), otherArc) = bool ((a1',a2'), False) ((a2',a1'),True) (a1'>a2')
    x1'' = bool x1' (bool x2 x1 (x1<x2)) ((not otherArc && (a1'' > tx1' || a2'' < tx1')) || (otherArc && not (a1'' > tx1' || a2'' < tx1')))
    x2'' = bool x2' (bool x2 x1 (x1>x2)) ((not otherArc && (a1'' > tx2' || a2'' < tx2')) || (otherArc && not (a1'' > tx2' || a2'' < tx2')))
    y1'' = bool y1' (bool y2 y1 (y1<y2)) ((not otherArc && (a1'' > ty1' || a2'' < ty1')) || (otherArc && not (a1'' > ty1' || a2'' < ty1')))
    y2'' = bool y2' (bool y2 y1 (y1>y2)) ((not otherArc && (a1'' > ty2' || a2'' < ty2')) || (otherArc && not (a1'' > ty2' || a2'' < ty2')))

-}

data ArcCentroid = ArcCentroid { centroid :: Point Double, radius :: Point Double, ang0:: Double, angdiff::Double} deriving (Eq, Show, Generic)

arcCentroid :: Point Double -> Point Double -> ArcStuff Double -> ArcCentroid
arcCentroid last@(Point lastX lastY) p@(Point x y) (ArcStuff (Point rxRatio ryRatio) xAxisRotation largeArcFlag sweepFlag) = ArcCentroid (Point centpX centpY) (Point rx ry) ang1 angd
  where
    currpX = cos xAxisRotation * (lastX - x) / 2.0 + sin xAxisRotation * (lastY - y) / 2.0
    currpY = -sin xAxisRotation * (lastX - x) / 2.0 + cos xAxisRotation * (lastY - y) / 2.0
    (rx',ry') = (rxRatio * norm (p - last), ryRatio * norm (p - last))
    l = currpX*currpX / (rx'*rx') + currpY*currpY / ry'*ry'
    (rx,ry) = bool (rx',ry') (rx'*sqrt l, ry'*sqrt l) (l > 1)
    snumer = max 0 $ (rx*rx*ry*ry) - (rx*rx*currpY*currpY) - (ry*ry*currpX*currpX)
    s = (bool 1 -1 (largeArcFlag == sweepFlag)) * sqrt
      (snumer / (rx*rx*currpY*currpY + ry*ry*currpX*currpX))
    cppX = s *  rx * currpY / ry
    cppY = s * (-ry) * currpX / rx
    centpX = (lastX + x) / 2.0 + cos (xAxisRotation) * cppX - sin (xAxisRotation) * cppY
    centpY = (lastY + y) / 2.0 + sin (xAxisRotation) * cppX + cos (xAxisRotation) * cppY
    m (Point x y) = sqrt (x*x+y*y)
    r p@(Point x y) p'@(Point x' y') = ( x*x' + y*y') / (m p * m p')
    ang p@(Point x y) p'@(Point x' y') = (bool 1 -1 (x*y' < y*x')) * acos (r p p')
    ang1 = ang (Point 1 0) (Point ((currpX-cppX)/rx) ((currpY-cppY)/ry))
    a = Point ((currpX-cppX)/rx) ((currpY-cppY)/ry)
    b = Point ((-currpX-cppX)/rx) ((-currpY-cppY)/ry)
    angd = bool (bool (ang a b) 0 (r a b >= 1)) pi (r a b <= -1)

arcC' :: Point Double -> Point Double -> ArcStuff Double -> (Point Double, Point Double, Double, Double)
arcC' last@(Point lastX lastY) p@(Point x y) (ArcStuff (Point rxRatio ryRatio) xAxisRotation largeArcFlag sweepFlag) = (Point centpX centpY, Point rx ry, ang1, angd)
  where
    currpX = cos xAxisRotation * (lastX - x) / 2.0 + sin xAxisRotation * (lastY - y) / 2.0
    currpY = -sin xAxisRotation * (lastX - x) / 2.0 + cos xAxisRotation * (lastY - y) / 2.0
    (rx',ry') = (rxRatio * norm (p - last), ryRatio * norm (p - last))
    l = currpX*currpX / (rx'*rx') + currpY*currpY / ry'*ry'
    (rx,ry) = bool (rx',ry') (rx'*sqrt l, ry'*sqrt l) (l > 1)
    snumer = max 0 $ (rx*rx*ry*ry) - (rx*rx*currpY*currpY) - (ry*ry*currpX*currpX)
    s = (bool 1 -1 (largeArcFlag == sweepFlag)) * sqrt
      (snumer / (rx*rx*currpY*currpY + ry*ry*currpX*currpX))
    cppX = s *  rx * currpY / ry
    cppY = s * (-ry) * currpX / rx
    centpX = (lastX + x) / 2.0 + cos (xAxisRotation) * cppX - sin (xAxisRotation) * cppY
    centpY = (lastY + y) / 2.0 + sin (xAxisRotation) * cppX + cos (xAxisRotation) * cppY
    m (Point x y) = sqrt (x*x+y*y)
    r p@(Point x y) p'@(Point x' y') = ( x*x' + y*y') / (m p * m p')
    ang p@(Point x y) p'@(Point x' y') = (bool 1 -1 (x*y' < y*x')) * acos (r p p')

    -- (/pi) $ angle (Point (-0.5000000000000001) 0.8660254037844387) :: Double
    -- 0.6666666666666667
    ang1 = ang (Point 1 0) (Point ((currpX-cppX)/rx) ((currpY-cppY)/ry))
    a = Point ((currpX-cppX)/rx) ((currpY-cppY)/ry)
    b = Point ((-currpX-cppX)/rx) ((-currpY-cppY)/ry)
    angd = bool (bool (ang a b) 0 (r a b >= 1)) pi (r a b <= -1)

toAbsRadius :: Point Double -> Point Double -> Point Double -> Point Double
toAbsRadius p1 p2 (Point ratiox ratioy) =
  Point
  (ratiox * norm (p1 - p2))
  (ratioy * norm (p1 - p2))

arcExtrema :: Point Double -> Point Double -> ArcStuff Double -> Point Double -> (Double, Double)
arcExtrema last@(Point lastX lastY) p@(Point x y) (ArcStuff (Point rxRatio ryRatio) xAxisRotation largeArcFlag sweepFlag) c@(Point cx cy) = (thetax1, thetay1)
  where
    (rx,ry) = (rxRatio * norm (p - last), ryRatio * norm (p - last))
    thetax1 = atan2 (-sin xAxisRotation * ry) (cos xAxisRotation * rx)
    thetay1 = atan2 (cos xAxisRotation * ry) (sin xAxisRotation * rx)

ellipse :: Point Double -> Point Double -> Double -> Double -> Point Double
ellipse (Point cx cy) (Point rx ry) phi theta =
  Point
  (cx + rx * cos theta * cos phi - ry * sin theta * sin phi)
  (cy + rx * cos theta * sin phi + ry * sin theta * cos phi)

-- | compute the bounding box for an arcBox
--
-- >>> arcBox (Point 0.0 1.2320508075688774) (Point 1.0 -0.5) (ArcStuff (Point 0.5 0.5) 0.0 True True)
-- Rect -1.1102230246251565e-16 1.5 -0.4999999999999998 1.3660254037844388
--
arcBox :: Point Double -> Point Double -> ArcStuff Double -> Rect Double
arcBox p1 p2 stuff@(ArcStuff r phi l sw) = space1 pts
  where
    (ArcCentroid c r ang1 angd) = arcCentroid p1 p2 stuff
    (x',y') = arcDerivs r phi
    ang2 = ang1 + (bool 1 -1 sw) * angd
    angr = ang1 ... ang2 :: Range Double
    angs =
      filter (|.| angr)
      [ x',
        x'+pi,
        y',
        y'+pi,
        ang1,
        ang2
      ]
    pts = ellipse c r phi <$> angs

arcDerivs :: Point Double -> Double -> (Double, Double)
arcDerivs (Point rx ry) phi = (thetax1, thetay1)
  where
    thetax1 = atan2 (-sin phi * ry) (cos phi * rx)
    thetay1 = atan2 (cos phi * ry) (sin phi * rx)

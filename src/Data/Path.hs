{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

-- | SVG path manipulation
module Data.Path
  ( -- * Path fundamental
    -- $path
    PathInfo (..),
    PathData(..),
    pointPathData,
    ArcInfo (..),
    ArcPosition (..),
    parsePath,
    toPathAbsolute,
    toPathAbsolutes,
    toPathAbsolute_,
    toPathAbsolutes_,
    toPathDatas,
    toInfo_,
    stateCur0,
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
    pathBoxes,
    pathBox,
    pathBoxes_,
    pathBox_,
    projectArcPosition,
    projectControls',
    projectControls_,
    pointToSvgCoords,
    pathInfoToSvgCoords,
  )
where

import qualified Control.Foldl as L
import Control.Lens hiding ((...), (<|))
import qualified Data.Attoparsec.Text as A
import Data.Bifunctor
import Data.Either
import Data.FormatN
import Data.Generics.Labels ()
import Data.Text (Text, pack)
import qualified Data.Text as Text
import GHC.Generics
import qualified Geom2D.CubicBezier as B
import NumHask.Prelude hiding (head, last, tail)
import Chart.Data as CD
import Data.Functor
import Control.Applicative
import Data.Scientific (toRealFloat)
import Data.List.NonEmpty (NonEmpty (..), last, head, tail, (<|))
-- import qualified Data.List.NonEmpty as NonEmpty
import GHC.OverloadedLabels
import Control.Monad.State.Lazy

-- $setup
-- :set -XRebindableSyntax gets timed out by cabal-docspec here, but NoImplicitPrelude works.
-- >>> :set -XNoImplicitPrelude
-- >>> :set -XNegativeLiterals
-- >>> import Chart
-- >>> import NumHask.Prelude

-- $path
-- Every element of an svg path can be thought of as exactly two points in space, with instructions of how to draw a curve between them.  From this point of view, one which this library adopts, a path chart is thus very similar to a line chart.  There's just a lot more information about the style of this line to deal with.
--
-- References:
--
-- [SVG d](https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/d)
--
-- [SVG path](https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths)

-- | parse a raw path string
--
-- >>> let outerseg1 = "M-1.0,0.5 A0.5 0.5 0.0 1 1 0.0,-1.2320508075688774 1.0 1.0 0.0 0 0 -0.5,-0.3660254037844387 1.0 1.0 0.0 0 0 -1.0,0.5 Z"
-- >>> parsePath outerseg1
-- [MoveTo OriginAbsolute [V2 (-1.0) 0.5],EllipticalArc OriginAbsolute [(0.5,0.5,0.0,True,True,V2 0.0 (-1.2320508075688774)),(1.0,1.0,0.0,False,False,V2 (-0.5) (-0.3660254037844387)),(1.0,1.0,0.0,False,False,V2 (-1.0) 0.5)],EndPath]
--
-- https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/d
parsePath :: Text -> Either String (NonEmpty PathCommand)
parsePath t = A.parseOnly pathParser t

commaWsp :: A.Parser ()
commaWsp = A.skipSpace *> A.option () (A.string "," $> ()) <* A.skipSpace

point :: A.Parser (Point Double)
point = Point <$> num <* commaWsp <*> num

points :: A.Parser (NonEmpty (Point Double))
points = fromList <$> point `A.sepBy1` commaWsp

pointPair :: A.Parser (Point Double, Point Double)
pointPair = (,) <$> point <* commaWsp <*> point

pointPairs :: A.Parser (NonEmpty (Point Double, Point Double))
pointPairs = fromList <$> pointPair `A.sepBy1` commaWsp



pathParser :: A.Parser (NonEmpty PathCommand)
pathParser = fromList <$> (A.skipSpace *> A.many1 command)

num :: A.Parser Double
num = realToFrac <$> (A.skipSpace *> plusMinus <* A.skipSpace)
  where doubleNumber :: A.Parser Double
        doubleNumber = toRealFloat <$> A.scientific <|> shorthand

        plusMinus = negate <$ A.string "-" <*> doubleNumber
                 <|> A.string "+" *> doubleNumber
                 <|> doubleNumber

        shorthand = process' <$> (A.string "." *> A.many1 A.digit)
        process' = fromRight 0 . A.parseOnly doubleNumber . pack . (++) "0."

nums :: A.Parser (NonEmpty Double)
nums = fromList <$> num `A.sepBy1` commaWsp

flag :: A.Parser Bool
flag = fmap (/='0') A.digit

command :: A.Parser PathCommand
command =  (MoveTo OriginAbsolute <$ A.string "M" <*> points)
       <|> (MoveTo OriginRelative <$ A.string "m" <*> points)
       <|> (LineTo OriginAbsolute <$ A.string "L" <*> points)
       <|> (LineTo OriginRelative <$ A.string "l" <*> points)
       <|> (HorizontalTo OriginAbsolute <$ A.string "H" <*> nums)
       <|> (HorizontalTo OriginRelative <$ A.string "h" <*> nums)
       <|> (VerticalTo OriginAbsolute <$ A.string "V" <*> nums)
       <|> (VerticalTo OriginRelative <$ A.string "v" <*> nums)
       <|> (CurveTo OriginAbsolute <$ A.string "C" <*> fmap fromList (manyComma curveToArgs))
       <|> (CurveTo OriginRelative <$ A.string "c" <*> fmap fromList (manyComma curveToArgs))
       <|> (SmoothCurveTo OriginAbsolute <$ A.string "S" <*> pointPairs)
       <|> (SmoothCurveTo OriginRelative <$ A.string "s" <*> pointPairs)
       <|> (QuadraticBezier OriginAbsolute <$ A.string "Q" <*> pointPairs)
       <|> (QuadraticBezier OriginRelative <$ A.string "q" <*> pointPairs)
       <|> (SmoothQuadraticBezierCurveTo OriginAbsolute <$ A.string "T" <*> points)
       <|> (SmoothQuadraticBezierCurveTo OriginRelative <$ A.string "t" <*> points)
       <|> (EllipticalArc OriginAbsolute <$ A.string "A" <*> manyComma ellipticalArgs)
       <|> (EllipticalArc OriginRelative <$ A.string "a" <*> manyComma ellipticalArgs)
       <|> (EndPath <$ A.string "Z" <* commaWsp)
       <|> (EndPath <$ A.string "z" <* commaWsp)
    where curveToArgs = (,,) <$> (point <* commaWsp)
                             <*> (point <* commaWsp)
                             <*> point
          manyComma a = fromList <$> a `A.sepBy1` commaWsp

          numComma = num <* commaWsp
          flagComma = flag <* commaWsp
          ellipticalArgs = (,,,,,) <$> numComma
                                   <*> numComma
                                   <*> numComma
                                   <*> flagComma
                                   <*> flagComma
                                   <*> point

-- | Path command definition (ripped from reanimate-svg).
data PathCommand
  = -- | 'M' or 'm' command
    MoveTo !Origin !(NonEmpty (Point Double))
  | -- | Line to, 'L' or 'l' Svg path command.
    LineTo !Origin !(NonEmpty (Point Double))
  | -- | Equivalent to the 'H' or 'h' svg path command.
    HorizontalTo !Origin !(NonEmpty Double)
  | -- | Equivalent to the 'V' or 'v' svg path command.
    VerticalTo !Origin !(NonEmpty Double)
  | -- | Cubic bezier, 'C' or 'c' command
    CurveTo !Origin !(NonEmpty (Point Double, Point Double, Point Double))
  | -- | Smooth cubic bezier, equivalent to 'S' or 's' command
    SmoothCurveTo !Origin !(NonEmpty (Point Double, Point Double))
  | -- | Quadratic bezier, 'Q' or 'q' command
    QuadraticBezier !Origin !(NonEmpty (Point Double, Point Double))
  | -- | Quadratic bezier, 'T' or 't' command
    SmoothQuadraticBezierCurveTo !Origin !(NonEmpty (Point Double))
  | -- | Elliptical arc, 'A' or 'a' command.
    EllipticalArc !Origin !(NonEmpty (Double, Double, Double, Bool, Bool, Point Double))
  | -- | Close the path, 'Z' or 'z' svg path command.
    EndPath
  deriving (Eq, Show, Generic)

-- | Tell if a path command is absolute (in the current
-- user coordiante) or relative to the previous point.
data Origin
  = OriginAbsolute -- ^ Next point in absolute coordinate
  | OriginRelative -- ^ Next point relative to the previous
  deriving (Eq, Show, Generic)

-- | To fit in with the requirements of the library design, specifically the separation of what a chart is into XY data Points from representation of these points, path instructions need to be decontructed into:
--
-- - define a single chart element as a line.
--
-- - split a single path element into the start and end points of the line, which become the 'Chart.Types.xys' of a 'Chart.Types.Chart', and the rest of the information, which is called 'PathInfo' and incorporated into the 'Chart.Types.Chart' 'Chart.Types.annotation'.
--
-- An arc path is variant to affine transformations of the 'Chart.Types.xys' points: angles are not presevred in the new reference frame.
data PathInfo a
  = StartI
  | LineI
  | CubicI (Point a) (Point a)
  | QuadI (Point a)
  | ArcI (ArcInfo a)
  deriving (Show, Eq, Generic)

pointToSvgCoords :: Point Double -> Point Double
pointToSvgCoords (Point x y) = Point x (-y)

pathInfoToSvgCoords :: (PathInfo Double, Point Double) -> (PathInfo Double, Point Double)
pathInfoToSvgCoords (CubicI a b, p) = (CubicI (pointToSvgCoords a) (pointToSvgCoords b), pointToSvgCoords p)
pathInfoToSvgCoords (QuadI a, p) = (QuadI (pointToSvgCoords a), pointToSvgCoords p)
pathInfoToSvgCoords (i, p) = (i, pointToSvgCoords p)

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
toPathAbsolute ::
  -- | (info, start, end)
  (PathInfo Double, Point Double) ->
  -- | path text
  Text
toPathAbsolute (StartI, p) = "M " <> pp p
toPathAbsolute (LineI, p) = "L " <> pp p
toPathAbsolute (CubicI c1 c2, next) =
  "C "
    <> pp c1
    <> " "
    <> pp c2
    <> " "
    <> pp next
toPathAbsolute (QuadI control, next) =
  "Q "
    <> pp control
    <> " "
    <> pp next
toPathAbsolute (ArcI (ArcInfo (Point x y) phi' l sw), x2) =
  "A "
    <> (pack . show) x
    <> " "
    <> (pack . show) y
    <> " "
    <> (pack . show) (-phi' * 180 / pi)
    <> " "
    <> bool "0" "1" l
    <> " "
    <> bool "0" "1" sw
    <> " "
    <> pp x2

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
toPathAbsolute_ ::
  PathData Double ->
  -- | path text
  Text
toPathAbsolute_ (StartP p) = "M " <> pp p
toPathAbsolute_ (LineP p) = "L " <> pp p
toPathAbsolute_ (CubicP c1 c2 p) =
  "C "
    <> pp c1
    <> " "
    <> pp c2
    <> " "
    <> pp p
toPathAbsolute_ (QuadP control p) =
  "Q "
    <> pp control
    <> " "
    <> pp p
toPathAbsolute_ (ArcP (ArcInfo (Point x y) phi' l sw) x2) =
  "A "
    <> (pack . show) x
    <> " "
    <> (pack . show) y
    <> " "
    <> (pack . show) (-phi' * 180 / pi)
    <> " "
    <> bool "0" "1" l
    <> " "
    <> bool "0" "1" sw
    <> " "
    <> pp x2

-- | Render a point (including conversion to SVG Coordinates).
pp :: Point Double -> Text
pp (Point x y) =
  showOr (FormatFixed (Just 4)) x <> ","
    <> showOr (FormatFixed (Just 4)) (bool (-y) y (y == zero))

-- | convert an (info, point) list to an svg d path text.
--
toPathAbsolutes :: NonEmpty (PathInfo Double, Point Double) -> Text
toPathAbsolutes = L.fold (L.Fold step begin done)
  where
    done = Text.intercalate " " . reverse
    begin = []
    step ts (info, next) = toPathAbsolute (info, next) : ts

-- | convert [PathData] to an svg d path text.
--
toPathAbsolutes_ :: NonEmpty (PathData Double) -> Text
toPathAbsolutes_ xs = Text.intercalate " " $ toList $ fmap toPathAbsolute_ xs

data StateInfo = StateInfo
  { -- | previous position
    cur :: Point Double,
    -- | start point (to close out the path)
    start :: Point Double,
    -- | last control point
    infoControl :: Point Double
  }
  deriving (Eq, Show, Generic)

stateInfo0 :: StateInfo
stateInfo0 = StateInfo zero zero zero

data PathCursor = PathCursor
  { -- | previous position
    curPrevious :: Point Double,
    -- | start point (to close out the path)
    curStart :: Point Double,
    -- | last control point
    curControl :: Maybe (Point Double)
  }
  deriving (Eq, Show, Generic)

stateCur0 :: PathCursor
stateCur0 = PathCursor zero zero Nothing

-- | The minimum data interpreting a single SVG path data point
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

pointPathData :: PathData a -> Point a
pointPathData (StartP p) = p
pointPathData (LineP p) = p
pointPathData (CubicP _ _ p) = p
pointPathData (QuadP _ p) = p
pointPathData (ArcP _ p) = p

-- | Convert from a path command list to a PathA specification
toPathDatas :: Traversable t => t PathCommand -> t (NonEmpty (PathData Double))
toPathDatas xs = flip evalState stateCur0 $ sequence $ toInfo_ <$> xs

-- | Convert relative points to absolute points
relToAbs :: (Additive a) => a -> NonEmpty a -> NonEmpty a
relToAbs p (x:|xs) = fmap (p+) $ foldr (\a ps -> a+head ps<|ps) [x] xs

moveTo :: NonEmpty (Point Double) -> State PathCursor (NonEmpty (PathData Double))
moveTo xs = do
  put (PathCursor (last xs) (head xs) Nothing)
  pure (StartP (head xs) :| (LineP <$> tail xs))

lineTo :: NonEmpty (Point Double) -> State PathCursor (NonEmpty (PathData Double))
lineTo xs = do
  modify ((#curPrevious .~ last xs) . (#curControl .~ Nothing))
  pure $ LineP <$> xs

horTo :: NonEmpty Double -> State PathCursor (NonEmpty (PathData Double))
horTo xs = do
  (PathCursor (Point _ y) _ _) <- get
  lineTo (fmap (\x -> Point x y) xs)

verTo :: NonEmpty Double -> State PathCursor (NonEmpty (PathData Double))
verTo ys = do
  (PathCursor (Point x _) _ _) <- get
  lineTo (fmap (\y -> Point x y) ys)

curveTo :: NonEmpty (Point Double, Point Double, Point Double) -> State PathCursor (NonEmpty (PathData Double))
curveTo xs = do
  modify ((#curPrevious .~ (\(_,_,p) -> p) (last xs)) .
          (#curControl .~ Just ((\(_,c2,_) -> c2) (last xs))))
  pure $ (\(c1, c2, x2) -> CubicP c1 c2 x2) <$> xs

-- | Convert relative points to absolute points
relToAbs3 :: Additive a => a -> NonEmpty (a,a,a) -> NonEmpty (a,a,a)
relToAbs3 p (x:|xs) = fmap (\(y,z,w) -> (p+y,p+z,p+w)) $ foldr (\(a,a',a'') ps -> (\(q,q',q'') -> (q+a,q'+a',q''+a'')) (head ps)<|ps) [x] xs

reflControlPoint :: State PathCursor (Point Double)
reflControlPoint = do
  (PathCursor p _ c) <- get
  case c of
    Nothing -> pure p
    Just c' -> pure (p - (c' - p))

smoothCurveToStep :: (Point Double, Point Double) -> State PathCursor (PathData Double)
smoothCurveToStep (c2, x2) = do
  c1 <- reflControlPoint
  modify ((#curControl .~ Just c2) . (#curPrevious .~ x2))
  pure (CubicP c1 c2 x2)

smoothCurveTo :: NonEmpty (Point Double, Point Double) -> State PathCursor (NonEmpty (PathData Double))
smoothCurveTo xs = do
  sequence (smoothCurveToStep <$> xs)

-- | Convert relative points to absolute points
relToAbs2 :: Additive a => a -> NonEmpty (a,a) -> NonEmpty (a,a)
relToAbs2 p (x:|xs) = fmap (\(y,z) -> (p+y,p+z)) $ foldr (\(a,a') ps -> (\(q,q') -> (q+a,q'+a')) (head ps)<|ps) [x] xs

quad :: NonEmpty (Point Double, Point Double) -> State PathCursor (NonEmpty (PathData Double))
quad xs = do
  modify ((#curPrevious .~ (\(_,p) -> p) (last xs)) .
          (#curControl .~ Just ((\(c1,_) -> c1) (last xs))))
  pure $ (\(c1, x2) -> QuadP c1 x2) <$> xs

smoothQuadStep :: Point Double -> State PathCursor (PathData Double)
smoothQuadStep x2 = do
  c1 <- reflControlPoint
  modify ((#curControl .~ Just c1) . (#curPrevious .~ x2))
  pure (QuadP c1 x2)

smoothQuad :: NonEmpty (Point Double) -> State PathCursor (NonEmpty (PathData Double))
smoothQuad xs = do
  sequence (smoothQuadStep <$> xs)

arcTo :: NonEmpty (Double, Double, Double, Bool, Bool, Point Double) -> State PathCursor (NonEmpty (PathData Double))
arcTo xs = do
  modify ((#curPrevious .~ (\(_,_,_,_,_,p) -> p) (last xs)) . (#curControl .~ Nothing))
  pure $ fromPathEllipticalArc <$> xs

fromPathEllipticalArc :: (a, a, a, Bool, Bool, Point a) -> PathData a
fromPathEllipticalArc (x, y, r, l, s, p) = ArcP (ArcInfo (Point x y) r l s) p

-- | Convert relative points to absolute points
relToAbsArc :: Additive a => Point a -> NonEmpty (a,a,a,Bool,Bool,Point a) -> NonEmpty (a,a,a,Bool,Bool,Point a)
relToAbsArc p (x:|xs) =
  fmap (\(y0,y1,y2,y3,y4,y5) -> (y0,y1,y2,y3,y4,p+y5)) $
  foldr (\(_,_,_,_,_,a) ps -> (\(y0,y1,y2,y3,y4,y5) -> (y0,y1,y2,y3,y4,y5+a)) (head ps)<|ps) [x] xs

-- | Convert a path command fragment to PathData
--
-- flips the y-dimension of points.
--
toInfo_ :: PathCommand -> State PathCursor (NonEmpty (PathData Double))
toInfo_ (MoveTo OriginAbsolute xs) = moveTo xs
toInfo_ (MoveTo OriginRelative xs) = do
  (PathCursor p _ _) <- get
  moveTo (relToAbs p xs)
toInfo_ EndPath = do
  (PathCursor _ s _) <- get
  pure [LineP s]
toInfo_ (LineTo OriginAbsolute xs) = lineTo xs
toInfo_ (LineTo OriginRelative xs) = do
  (PathCursor p _ _) <- get
  lineTo (relToAbs p xs)
toInfo_ (HorizontalTo OriginAbsolute xs) = horTo xs
toInfo_ (HorizontalTo OriginRelative xs) = do
  (PathCursor (Point x _) _ _) <- get
  horTo (relToAbs x xs)
toInfo_ (VerticalTo OriginAbsolute xs) = verTo xs
toInfo_ (VerticalTo OriginRelative ys) = do
  (PathCursor (Point _ y) _ _) <- get
  verTo (relToAbs y ys)
toInfo_ (CurveTo OriginAbsolute xs) = curveTo xs
toInfo_ (CurveTo OriginRelative xs) = do
  (PathCursor p _ _) <- get
  curveTo (relToAbs3 p xs)
toInfo_ (SmoothCurveTo OriginAbsolute xs) = smoothCurveTo xs
toInfo_ (SmoothCurveTo OriginRelative xs) = do
  (PathCursor p _ _) <- get
  smoothCurveTo (relToAbs2 p xs)
toInfo_ (QuadraticBezier OriginAbsolute xs) = quad xs
toInfo_ (QuadraticBezier OriginRelative xs) = do
  (PathCursor p _ _) <- get
  quad (relToAbs2 p xs)
toInfo_ (SmoothQuadraticBezierCurveTo OriginAbsolute xs) = smoothQuad xs
toInfo_ (SmoothQuadraticBezierCurveTo OriginRelative xs) = do
  (PathCursor p _ _) <- get
  smoothQuad (relToAbs p xs)
toInfo_ (EllipticalArc OriginAbsolute xs) = arcTo xs
toInfo_ (EllipticalArc OriginRelative xs) = do
  (PathCursor p _ _) <- get
  arcTo (relToAbsArc p xs)

-- | Convert a path command fragment to an instruction + point.
--
-- flips the y-dimension of points.
--
toInfo :: StateInfo -> PathCommand -> (StateInfo, NonEmpty (PathInfo Double, Point Double))
toInfo _ (MoveTo OriginAbsolute (x:|xs)) = L.fold (L.Fold step begin (second (fromList . reverse))) xs
  where
    begin = (StateInfo x x zero, [(StartI, x)])
    step (s, p) a = (s & #cur .~ a, (LineI, a) : p)
toInfo s (MoveTo OriginRelative (x:|xs)) = L.fold (L.Fold step begin (second (fromList . reverse))) xs
  where
    x0 = s ^. #cur + x
    begin = (StateInfo x0 x0 zero, [(StartI, x0)])
    step (s', p) a = let a' = a + s' ^. #cur in (s' & #cur .~ a', (LineI, a') : p)
toInfo s EndPath = (s, [(LineI, s ^. #start)])
toInfo s (LineTo OriginAbsolute xs) = L.fold (L.Fold step begin (second (fromList . reverse))) xs
  where
    begin = (s,[])
    step (s', p) a = (s' & #cur .~ a, (LineI, a) : p)
toInfo s (LineTo OriginRelative xs) = L.fold (L.Fold step (s, []) (second (fromList . reverse))) xs
  where
    step (s', p) a = let a' = a + s' ^. #cur in (s' & #cur .~ a', (LineI, a') : p)
toInfo s (HorizontalTo OriginAbsolute xs) = L.fold (L.Fold step (s, []) (second (fromList . reverse))) xs
  where
    step (s'@(StateInfo (Point _ cy) _ _), p) a =
      let a' = Point a cy in (s' & #cur .~ a', (LineI, a') : p)
toInfo s (HorizontalTo OriginRelative xs) = L.fold (L.Fold step (s, []) (second (fromList . reverse))) xs
  where
    step (s'@(StateInfo (Point cx cy) _ _), p) a =
      let a' = Point (a + cx) cy in (s' & #cur .~ a', (LineI, a') : p)
toInfo s (VerticalTo OriginAbsolute xs) = L.fold (L.Fold step (s, []) (second (fromList . reverse))) xs
  where
    step (s'@(StateInfo (Point cx _) _ _), p) a =
      let a' = Point cx a in (s' & #cur .~ a', (LineI, a') : p)
toInfo s (VerticalTo OriginRelative xs) = L.fold (L.Fold step (s, []) (second (fromList . reverse))) xs
  where
    step (s'@(StateInfo (Point cx cy) _ _), p) a =
      let a' = Point cx (a + cy) in (s' & #cur .~ a', (LineI, a') : p)
toInfo s (CurveTo OriginAbsolute xs) =
  L.fold (L.Fold step (s, []) (second (fromList . reverse))) xs
  where
    step (s', p) (c1, c2, x2) =
      (s' & #cur .~ x2 & #infoControl .~ c2, (CubicI c1 c2, x2) : p)
toInfo s (CurveTo OriginRelative xs) =
  L.fold (L.Fold step (s, []) (second (fromList . reverse))) xs
  where
    step (s', p) (c1, c2, x2) =
      (s' & #cur .~ (x2 + s' ^. #cur) & #infoControl .~ (c2 + s' ^. #cur), (CubicI (c1 + s' ^. #cur) (c2 + s' ^. #cur), x2 + s' ^. #cur) : p)
toInfo s (SmoothCurveTo OriginAbsolute xs) =
  L.fold (L.Fold step (s, []) (second (fromList . reverse))) xs
  where
    step (s', p) (c2, x2) =
      (s' & #cur .~ x2, (CubicI (s' ^. #cur - (s' ^. #infoControl - s' ^. #cur)) c2, x2) : p)
toInfo s (SmoothCurveTo OriginRelative xs) =
  L.fold (L.Fold step (s, []) (second (fromList . reverse))) xs
  where
    step (s', p) (c2, x2) =
      ( s'
          & #cur .~ (x2 + s' ^. #cur)
          & #infoControl .~ (c2 + s' ^. #cur),
        (CubicI (s' ^. #cur - (s' ^. #infoControl - s' ^. #cur)) (c2 + s' ^. #cur), x2 + s' ^. #cur) : p
      )
toInfo s (QuadraticBezier OriginAbsolute xs) =
  L.fold (L.Fold step (s, []) (second (fromList . reverse))) xs
  where
    step (s', p) (c1, x2) =
      ( s'
          & #cur .~ x2
          & #infoControl .~ c1,
        (QuadI c1, x2) : p
      )
toInfo s (QuadraticBezier OriginRelative xs) =
  L.fold (L.Fold step (s, []) (second (fromList . reverse))) xs
  where
    step (s', p) (c1, x2) =
      (s' & #cur .~ x2 & #infoControl .~ (c1 + s' ^. #cur), (QuadI (c1 + s' ^. #cur), x2 + s' ^. #cur) : p)
toInfo s (SmoothQuadraticBezierCurveTo OriginAbsolute xs) =
  L.fold (L.Fold step (s, []) (second (fromList . reverse))) xs
  where
    step (s', p) x2 =
      ( s'
          & #cur .~ x2
          & #infoControl .~ (s' ^. #cur - (s' ^. #infoControl - s' ^. #cur)),
        (QuadI (s' ^. #cur - (s' ^. #infoControl - s' ^. #cur)), x2) : p
      )
toInfo s (SmoothQuadraticBezierCurveTo OriginRelative xs) =
  L.fold (L.Fold step (s, []) (second (fromList . reverse))) xs
  where
    step (s', p) x2 =
      ( s'
          & #cur .~ (x2 + s' ^. #cur)
          & #infoControl .~ (s' ^. #cur - (s' ^. #infoControl - s' ^. #cur)),
        (QuadI (s' ^. #cur - (s' ^. #infoControl - s' ^. #cur)), x2 + s' ^. #cur) : p
      )
toInfo s (EllipticalArc OriginAbsolute xs) =
  L.fold (L.Fold step (s, []) (second (fromList . reverse))) xs
  where
    step (s', p) a@(_, _, _, _, _, x2) =
      (s' & #cur .~ x2, (fromPathEllipticalArc' (s' ^. #cur) a, x2) : p)
toInfo s (EllipticalArc OriginRelative xs) =
  L.fold (L.Fold step (s, []) (second (fromList . reverse))) xs
  where
    step (s', p) a@(_, _, _, _, _, x2) =
      let x2' = x2 + s' ^. #cur
       in (s' & #cur .~ x2', (fromPathEllipticalArc' (s' ^. #cur) a, x2') : p)

fromPathEllipticalArc' :: Point a -> (a, a, a, Bool, Bool, Point a) -> PathInfo a
fromPathEllipticalArc' _ (x, y, r, l, s, _) = ArcI (ArcInfo (Point x y) r l s)

-- | Convert from a path command list to a PathA specification
toPathXYs :: NonEmpty PathCommand -> NonEmpty (PathInfo Double, Point Double)
toPathXYs (x:|xs) =
  snd (foldl' (\(x', l) a -> second (l <>) $ toInfo x' a) (toInfo stateInfo0 x) xs)

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
pathBoxes :: NonEmpty (PathInfo Double, Point Double) -> Rect Double
pathBoxes (x :| xs) =
  L.fold (L.Fold step begin snd) xs
  where
    begin :: (Point Double, Rect Double)
    begin = (snd x, singleton (snd x))
    step ::
      (Point Double, Rect Double) ->
      (PathInfo Double, Point Double) ->
      (Point Double, Rect Double)
    step (start', r) a = (snd a, pathBox start' a <> r)

-- | Bounding box for a path info, start and end Points.
pathBox :: Point Double -> (PathInfo Double, Point Double) -> Rect Double
pathBox start' (info, end) =
  case info of
    StartI -> singleton end
    LineI -> space1 ([start', end] :: NonEmpty (Point Double))
    CubicI c1 c2 -> cubicBox (CubicPosition start' end c1 c2)
    QuadI c -> quadBox (QuadPosition start' end c)
    ArcI i -> arcBox (ArcPosition start' end i)

-- | Bounding box for a list of path XYs.
pathBoxes_ :: NonEmpty (PathData Double) -> Rect Double
pathBoxes_ (x :| xs) =
  L.fold (L.Fold step begin snd) xs
  where
    begin :: (Point Double, Rect Double)
    begin = (pointPathData x, singleton (pointPathData x))
    step (start', r) a = (pointPathData a, pathBox_ start' a <> r)

-- | Bounding box for a path info, start and end Points.
pathBox_ :: Point Double -> PathData Double -> Rect Double
pathBox_ start' info =
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

projectControls' :: Foldable t =>
  Rect Double
  -> Rect Double
  -> t (PathInfo Double, Point Double)
  -> [(PathInfo Double, Point Double)]
projectControls' new old ps = (reverse . snd) (foldl' (\(prevp, l) (i, xy) -> (xy, projectControl new old prevp xy i : l)) (zero, []) ps)

projectControls_ :: Foldable t =>
  Rect Double
  -> Rect Double
  -> t (PathData Double)
  -> [PathData Double]
projectControls_ new old ps =
  (reverse . snd)
  (foldl' (\(prevp, l) i -> let d = projectControl_ new old prevp i in (pointPathData d, d:l))
   (zero, []) ps)

projectControl_
  :: Rect Double
  -> Rect Double
  -> Point Double
  -> PathData Double
  -> PathData Double
projectControl_ new old _ (CubicP c1 c2 p) =
      CubicP (projectOnP new old c1) (projectOnP new old c2) (projectOnP new old p)
projectControl_ new old _ (QuadP c p) =
      QuadP (projectOnP new old c) (projectOnP new old p)
projectControl_ new old p1 (ArcP ai p2) = ArcP (projectArcPosition new old (ArcPosition p1 p2 ai)) (projectOnP new old p2)
projectControl_ new old _ (LineP p) = LineP (projectOnP new old p)
projectControl_ new old _ (StartP p) = StartP (projectOnP new old p)

projectControl
  :: Rect Double
  -> Rect Double
  -> Point Double
  -> Point Double
  -> PathInfo Double
  -> (PathInfo Double, Point Double)
projectControl new old _ p (CubicI c1 c2) =
      (CubicI (projectOnP new old c1) (projectOnP new old c2), projectOnP new old p)
projectControl new old _ p (QuadI c) =
      (QuadI (projectOnP new old c), projectOnP new old p)
projectControl new old p1 p2 (ArcI ai) = (ArcI $ projectArcPosition new old (ArcPosition p1 p2 ai), projectOnP new old p2)
projectControl new old _ p LineI = (LineI, projectOnP new old p)
projectControl new old _ p StartI = (StartI, projectOnP new old p)

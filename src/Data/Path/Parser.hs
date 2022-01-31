{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | SVG path manipulation
module Data.Path.Parser
  ( -- * Parsing
    -- $parsing
    parsePath,
    svgToPathData,
    pathDataToSvg,
  )
where

import Data.Path
import Optics.Core hiding ((<|))
import qualified Data.Attoparsec.Text as A
import Data.Either
import Data.FormatN
import Data.Text (Text, pack)
import qualified Data.Text as Text
import GHC.Generics
import NumHask.Prelude
import Chart.Data
import Data.Functor
import Control.Applicative
import Data.Scientific (toRealFloat)
import GHC.OverloadedLabels
import Control.Monad.State.Lazy
import Data.Bifunctor
-- import qualified Data.List as List

-- $parsing
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
-- Right [MoveTo OriginAbsolute [Point -1.0 0.5],EllipticalArc OriginAbsolute [(0.5,0.5,0.0,True,True,Point 0.0 -1.2320508075688774),(1.0,1.0,0.0,False,False,Point -0.5 -0.3660254037844387),(1.0,1.0,0.0,False,False,Point -1.0 0.5)],EndPath]
--
-- https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/d
parsePath :: Text -> Either String [PathCommand]
parsePath t = A.parseOnly pathParser t

commaWsp :: A.Parser ()
commaWsp = A.skipSpace *> A.option () (A.string "," $> ()) <* A.skipSpace

point :: A.Parser (Point Double)
point = Point <$> num <* commaWsp <*> num

points :: A.Parser [Point Double]
points = fromList <$> point `A.sepBy1` commaWsp

pointPair :: A.Parser (Point Double, Point Double)
pointPair = (,) <$> point <* commaWsp <*> point

pointPairs :: A.Parser [(Point Double, Point Double)]
pointPairs = fromList <$> pointPair `A.sepBy1` commaWsp

pathParser :: A.Parser [PathCommand]
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

nums :: A.Parser [Double]
nums = num `A.sepBy1` commaWsp

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
    MoveTo !Origin ![Point Double]
  | -- | Line to, 'L' or 'l' Svg path command.
    LineTo !Origin ![Point Double]
  | -- | Equivalent to the 'H' or 'h' svg path command.
    HorizontalTo !Origin ![Double]
  | -- | Equivalent to the 'V' or 'v' svg path command.
    VerticalTo !Origin ![Double]
  | -- | Cubic bezier, 'C' or 'c' command
    CurveTo !Origin ![(Point Double, Point Double, Point Double)]
  | -- | Smooth cubic bezier, equivalent to 'S' or 's' command
    SmoothCurveTo !Origin ![(Point Double, Point Double)]
  | -- | Quadratic bezier, 'Q' or 'q' command
    QuadraticBezier !Origin ![(Point Double, Point Double)]
  | -- | Quadratic bezier, 'T' or 't' command
    SmoothQuadraticBezierCurveTo !Origin ![Point Double]
  | -- | Elliptical arc, 'A' or 'a' command.
    EllipticalArc !Origin ![(Double, Double, Double, Bool, Bool, Point Double)]
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

svgCoords :: PathData Double -> PathData Double
svgCoords (CubicP a b p) = CubicP (pointToSvgCoords a) (pointToSvgCoords b) (pointToSvgCoords p)
svgCoords (QuadP a p) = QuadP (pointToSvgCoords a) (pointToSvgCoords p)
svgCoords (StartP p) = StartP (pointToSvgCoords p)
svgCoords (LineP p) = LineP (pointToSvgCoords p)
svgCoords (ArcP i p) = ArcP i (pointToSvgCoords p)

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
  PathData Double ->
  -- | path text
  Text
toPathAbsolute (StartP p) = "M " <> pp p
toPathAbsolute (LineP p) = "L " <> pp p
toPathAbsolute (CubicP c1 c2 p) =
  "C "
    <> pp c1
    <> " "
    <> pp c2
    <> " "
    <> pp p
toPathAbsolute (QuadP control p) =
  "Q "
    <> pp control
    <> " "
    <> pp p
toPathAbsolute (ArcP (ArcInfo (Point x y) phi' l sw) x2) =
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

svgToPathData :: Text -> [PathData Double]
svgToPathData = toPathDatas . either error id . parsePath

-- | convert [PathData] to an svg d path text.
--
pathDataToSvg :: [PathData Double] -> Text
pathDataToSvg xs = Text.intercalate " " $ fmap toPathAbsolute xs

-- | Convert from a path command list to a PathA specification
toPathDatas :: [PathCommand] -> [PathData Double]
toPathDatas xs = fmap svgCoords $ mconcat $ flip evalState stateCur0 $ sequence $ toInfo <$> xs

-- | Convert relative points to absolute points
-- FIXME: does this need reversing?
relToAbs :: (Additive a) => a -> [a] -> [a]
relToAbs _ [] = []
relToAbs p (x:xs) = fmap (p+) $ foldr (\a ps -> a+head ps:ps) [x] xs

moveTo :: [Point Double] -> State PathCursor [PathData Double]
moveTo xs = do
  put (PathCursor (last xs) (head xs) Nothing)
  pure (StartP (head xs) : (LineP <$> tail xs))

lineTo :: [Point Double] -> State PathCursor [PathData Double]
lineTo xs = do
  modify ((#curPrevious .~ last xs) . (#curControl .~ Nothing))
  pure $ LineP <$> xs

horTo :: [Double] -> State PathCursor [PathData Double]
horTo xs = do
  (PathCursor (Point _ y) _ _) <- get
  lineTo (fmap (\x -> Point x y) xs)

verTo :: [Double] -> State PathCursor [PathData Double]
verTo ys = do
  (PathCursor (Point x _) _ _) <- get
  lineTo (fmap (\y -> Point x y) ys)

curveTo :: [(Point Double, Point Double, Point Double)] -> State PathCursor [PathData Double]
curveTo xs = do
  modify ((#curPrevious .~ (\(_,_,p) -> p) (last xs)) .
          (#curControl .~ Just ((\(_,c2,_) -> c2) (last xs))))
  pure $ (\(c1, c2, x2) -> CubicP c1 c2 x2) <$> xs

-- | Convert relative points to absolute points
-- FIXME: needs reversing?
relToAbs3 :: Additive a => a -> [(a,a,a)] -> [(a,a,a)]
relToAbs3 _ [] = []
relToAbs3 p (x:xs) = fmap (\(y,z,w) -> (p+y,p+z,p+w)) $ foldr (\(a,a',a'') ps -> (\(q,q',q'') -> (q+a,q'+a',q''+a'')) (head ps):ps) [x] xs

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

smoothCurveTo :: [(Point Double, Point Double)] -> State PathCursor [PathData Double]
smoothCurveTo xs = do
  sequence (smoothCurveToStep <$> xs)

-- | Convert relative points to absolute points
relToAbs2 :: Additive a => a -> [(a,a)] -> [(a,a)]
relToAbs2 _ [] = []
relToAbs2 p (x:xs) = fmap (bimap (p +) (p +)) $ foldr (\(a,a') ps -> (\(q,q') -> (q+a,q'+a')) (head ps):ps) [x] xs

quad :: [(Point Double, Point Double)] -> State PathCursor [PathData Double]
quad xs = do
  modify ((#curPrevious .~ snd (last xs)) .
          (#curControl .~ Just (fst (last xs))))
  pure $ uncurry QuadP <$> xs

smoothQuadStep :: Point Double -> State PathCursor (PathData Double)
smoothQuadStep x2 = do
  c1 <- reflControlPoint
  modify ((#curControl .~ Just c1) . (#curPrevious .~ x2))
  pure (QuadP c1 x2)

smoothQuad :: [Point Double] -> State PathCursor [PathData Double]
smoothQuad xs = do
  sequence (smoothQuadStep <$> xs)

arcTo :: [(Double, Double, Double, Bool, Bool, Point Double)] -> State PathCursor [PathData Double]
arcTo xs = do
  modify ((#curPrevious .~ (\(_,_,_,_,_,p) -> p) (last xs)) . (#curControl .~ Nothing))
  pure $ fromPathEllipticalArc <$> xs

fromPathEllipticalArc :: (a, a, a, Bool, Bool, Point a) -> PathData a
fromPathEllipticalArc (x, y, r, l, s, p) = ArcP (ArcInfo (Point x y) r l s) p

-- | Convert relative points to absolute points
relToAbsArc :: Additive a => Point a -> [(a,a,a,Bool,Bool,Point a)] -> [(a,a,a,Bool,Bool,Point a)]
relToAbsArc _ [] = []
relToAbsArc p (x:xs) =
  fmap (\(y0,y1,y2,y3,y4,y5) -> (y0,y1,y2,y3,y4,p+y5)) $
  foldr (\(_,_,_,_,_,a) ps -> (\(y0,y1,y2,y3,y4,y5) -> (y0,y1,y2,y3,y4,y5+a)) (head ps):ps) [x] xs

-- | Convert a path command fragment to PathData
--
-- flips the y-dimension of points.
--
toInfo :: PathCommand -> State PathCursor [PathData Double]
toInfo (MoveTo OriginAbsolute xs) = moveTo xs
toInfo (MoveTo OriginRelative xs) = do
  (PathCursor p _ _) <- get
  moveTo (relToAbs p xs)
toInfo EndPath = do
  (PathCursor _ s _) <- get
  pure [LineP s]
toInfo (LineTo OriginAbsolute xs) = lineTo xs
toInfo (LineTo OriginRelative xs) = do
  (PathCursor p _ _) <- get
  lineTo (relToAbs p xs)
toInfo (HorizontalTo OriginAbsolute xs) = horTo xs
toInfo (HorizontalTo OriginRelative xs) = do
  (PathCursor (Point x _) _ _) <- get
  horTo (relToAbs x xs)
toInfo (VerticalTo OriginAbsolute xs) = verTo xs
toInfo (VerticalTo OriginRelative ys) = do
  (PathCursor (Point _ y) _ _) <- get
  verTo (relToAbs y ys)
toInfo (CurveTo OriginAbsolute xs) = curveTo xs
toInfo (CurveTo OriginRelative xs) = do
  (PathCursor p _ _) <- get
  curveTo (relToAbs3 p xs)
toInfo (SmoothCurveTo OriginAbsolute xs) = smoothCurveTo xs
toInfo (SmoothCurveTo OriginRelative xs) = do
  (PathCursor p _ _) <- get
  smoothCurveTo (relToAbs2 p xs)
toInfo (QuadraticBezier OriginAbsolute xs) = quad xs
toInfo (QuadraticBezier OriginRelative xs) = do
  (PathCursor p _ _) <- get
  quad (relToAbs2 p xs)
toInfo (SmoothQuadraticBezierCurveTo OriginAbsolute xs) = smoothQuad xs
toInfo (SmoothQuadraticBezierCurveTo OriginRelative xs) = do
  (PathCursor p _ _) <- get
  smoothQuad (relToAbs p xs)
toInfo (EllipticalArc OriginAbsolute xs) = arcTo xs
toInfo (EllipticalArc OriginRelative xs) = do
  (PathCursor p _ _) <- get
  arcTo (relToAbsArc p xs)


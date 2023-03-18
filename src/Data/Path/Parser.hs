{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use unwords" #-}
{-# LANGUAGE TemplateHaskell #-}

-- | SVG path manipulation
module Data.Path.Parser
  ( -- * Parsing
    -- $parsing
    parsePath,
    svgToPathData,
    pathDataToSvg,
    PathCommand (..),
    Origin (..),
    toPathDatas,
  )
where

import Chart.Data
import Control.Applicative hiding (many, some, (<|>), optional)
import Control.Monad.State.Lazy
import Data.FormatN
import Data.Path ( PathData(..), ArcInfo(ArcInfo) )
import GHC.Generics
import GHC.OverloadedLabels
import NumHask.Prelude
import Optics.Core hiding ((<|))
import Data.ByteString (ByteString, intercalate)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Char8 (pack)
import FlatParse.Basic
import Chart.FlatParse hiding (point, points, pointPair, pointPairs)

-- $parsing
-- Every element of an svg path can be thought of as exactly two points in space, with instructions of how to draw a curve between them.  From this point of view, one which this library adopts, a path chart is thus very similar to a line chart.  There's just a lot more information about the style of this line to deal with.
--
-- References:
--
-- [SVG d attribute](https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/d)
--
-- [SVG Paths](https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths)

-- | Parse a raw path string.
--
-- >>> let outerseg1 = "M-1.0,0.5 A0.5 0.5 0.0 1 1 0.0,-1.2320508075688774 1.0 1.0 0.0 0 0 -0.5,-0.3660254037844387 1.0 1.0 0.0 0 0 -1.0,0.5 Z"
-- >>> parsePath outerseg1
-- Just [MoveTo OriginAbsolute [Point -1.0 0.5],EllipticalArc OriginAbsolute [(0.5,0.5,0.0,True,True,Point 0.0 -1.2320508075688774),(1.0,1.0,0.0,False,False,Point -0.5 -0.3660254037844387),(1.0,1.0,0.0,False,False,Point -1.0 0.5)],EndPath]
--
parsePath :: ByteString -> Maybe [PathCommand]
parsePath bs = case runParser pathParser bs of
  OK x _ -> Just x
  _ -> Nothing

isWs' :: Char -> Bool
isWs' x =
  (x == ' ') ||
  (x == '\n') ||
  (x == '\t') ||
  (x == '\r')

ws' :: Parser e Char
ws' = satisfy isWs'

comma' :: Parser e ()
comma' = $(char ',')

commaWsp :: Parser e (Maybe ())
commaWsp = many ws' *> optional comma' <* many ws'

num :: Parser e Double
num = signed double

point :: Parser e (Point Double)
point = Point <$> num <* commaWsp <*> num

numComma :: Parser e Double
numComma = num <* commaWsp

points :: Parser e [Point Double]
points = (:) <$> point <*> many (commaWsp *> point) <|> pure []

pointPair :: Parser e (Point Double, Point Double)
pointPair = (,) <$> point <* commaWsp <*> point

pointPairs :: Parser e [(Point Double, Point Double)]
pointPairs =(:) <$> pointPair <*> many (commaWsp *> pointPair) <|> pure []

nums :: Parser e [Double]
nums = (:) <$> num <*> many (commaWsp *> num) <|> pure []

flag :: Parser e Bool
flag = fmap (/= 0) digit

manyComma :: Parser e a -> Parser e [a]
manyComma a = (:) <$> a <*> many (commaWsp *> a) <|> pure []

flagComma :: Parser e Bool
flagComma = flag <* commaWsp

curveToArgs :: Parser
  e
  (Point Double, Point Double, Point Double)
curveToArgs =
      (,,)
        <$> (point <* commaWsp)
        <*> (point <* commaWsp)
        <*> point

ellipticalArgs :: Parser
  e
  (Double, Double, Double, Bool, Bool, Point Double)
ellipticalArgs =
      (,,,,,)
        <$> numComma
        <*> numComma
        <*> numComma
        <*> flagComma
        <*> flagComma
        <*> point

pathParser :: Parser e [PathCommand]
pathParser = many ws' *> manyComma command

command :: Parser e PathCommand
command =
  (MoveTo OriginAbsolute <$ $(char 'M') <*> points)
    <|> (MoveTo OriginRelative <$ $(char 'm') <*> points)
    <|> (LineTo OriginAbsolute <$ $(char 'L') <*> points)
    <|> (LineTo OriginRelative <$ $(char 'l') <*> points)
    <|> (HorizontalTo OriginAbsolute <$ $(char 'H') <*> nums)
    <|> (HorizontalTo OriginRelative <$ $(char 'h') <*> nums)
    <|> (VerticalTo OriginAbsolute <$ $(char 'V') <*> nums)
    <|> (VerticalTo OriginRelative <$ $(char 'v') <*> nums)
    <|> (CurveTo OriginAbsolute <$ $(char 'C') <*> manyComma curveToArgs)
    <|> (CurveTo OriginRelative <$ $(char 'c') <*> manyComma curveToArgs)
    <|> (SmoothCurveTo OriginAbsolute <$ $(char 'S') <*> pointPairs)
    <|> (SmoothCurveTo OriginRelative <$ $(char 's') <*> pointPairs)
    <|> (QuadraticBezier OriginAbsolute <$ $(char 'Q') <*> pointPairs)
    <|> (QuadraticBezier OriginRelative <$ $(char 'q') <*> pointPairs)
    <|> (SmoothQuadraticBezierCurveTo OriginAbsolute <$ $(char 'T') <*> points)
    <|> (SmoothQuadraticBezierCurveTo OriginRelative <$ $(char 't') <*> points)
    <|> (EllipticalArc OriginAbsolute <$ $(char 'A') <*> manyComma ellipticalArgs)
    <|> (EllipticalArc OriginRelative <$ $(char 'a') <*> manyComma ellipticalArgs)
    <|> (EndPath <$ $(char 'Z') <* commaWsp)
    <|> (EndPath <$ $(char 'z') <* commaWsp)

-- | Path command definition (ripped from reanimate-svg).
data PathCommand
  = -- | M or m command
    MoveTo !Origin ![Point Double]
  | -- | Line to, L or l Svg path command.
    LineTo !Origin ![Point Double]
  | -- | Equivalent to the H or h svg path command.
    HorizontalTo !Origin ![Double]
  | -- | Equivalent to the V or v svg path command.
    VerticalTo !Origin ![Double]
  | -- | Cubic bezier, C or c command
    CurveTo !Origin ![(Point Double, Point Double, Point Double)]
  | -- | Smooth cubic bezier, equivalent to S or s command
    SmoothCurveTo !Origin ![(Point Double, Point Double)]
  | -- | Quadratic bezier, Q or q command
    QuadraticBezier !Origin ![(Point Double, Point Double)]
  | -- | Quadratic bezier, T or t command
    SmoothQuadraticBezierCurveTo !Origin ![Point Double]
  | -- | Elliptical arc, A or a command.
    EllipticalArc !Origin ![(Double, Double, Double, Bool, Bool, Point Double)]
  | -- | Close the path, Z or z svg path command.
    EndPath
  deriving (Eq, Show, Generic)

-- | Tell if a path command is absolute (in the current
-- user coordiante) or relative to the previous point.
data Origin
  = -- | Next point in absolute coordinate
    OriginAbsolute
  | -- | Next point relative to the previous
    OriginRelative
  deriving (Eq, Show, Generic)

pointToSvgCoords :: Point Double -> Point Double
pointToSvgCoords (Point x y) = Point x (-y)

svgCoords :: PathData Double -> PathData Double
svgCoords (CubicP a b p) = CubicP (pointToSvgCoords a) (pointToSvgCoords b) (pointToSvgCoords p)
svgCoords (QuadP a p) = QuadP (pointToSvgCoords a) (pointToSvgCoords p)
svgCoords (StartP p) = StartP (pointToSvgCoords p)
svgCoords (LineP p) = LineP (pointToSvgCoords p)
svgCoords (ArcP i p) = ArcP i (pointToSvgCoords p)

-- | Convert from a path info, start point, end point triple to a path text clause.
--
-- Note that morally,
--
-- > toPathsAbsolute . toPathDatas . parsePath == id
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
  ByteString
toPathAbsolute (StartP p) = "M " <> pp' p
toPathAbsolute (LineP p) = "L " <> pp' p
toPathAbsolute (CubicP c1 c2 p) =
  "C "
    <> pp' c1
    <> " "
    <> pp' c2
    <> " "
    <> pp' p
toPathAbsolute (QuadP control p) =
  "Q "
    <> pp' control
    <> " "
    <> pp' p
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
    <> pp' x2

-- | Render a point (including conversion to SVG Coordinates).
pp' :: Point Double -> ByteString
pp' (Point x y) = encodeUtf8 $
  formatOrShow (FixedStyle 4) Nothing x <> ","
    <> formatOrShow (FixedStyle 4) Nothing (bool (-y) y (y == zero))

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

-- | Convert from an SVG d attribute text snippet to a [`PathData` `Double`]
svgToPathData :: ByteString -> [PathData Double]
svgToPathData = foldMap toPathDatas . parsePath

-- | Convert from [`PathData` `Double`] to an SVG d path text snippet.
pathDataToSvg :: [PathData Double] -> ByteString
pathDataToSvg xs = intercalate " " $ fmap toPathAbsolute xs

-- | Convert from a path command list to a PathA specification
toPathDatas :: [PathCommand] -> [PathData Double]
toPathDatas xs = fmap svgCoords $ mconcat $ flip evalState stateCur0 $ mapM toPathData xs

-- | Convert relative points to absolute points
relToAbs :: (Additive a) => a -> [a] -> [a]
relToAbs p xs = accsum (p : xs)

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
  lineTo (fmap (`Point` y) xs)

verTo :: [Double] -> State PathCursor [PathData Double]
verTo ys = do
  (PathCursor (Point x _) _ _) <- get
  lineTo (fmap (Point x) ys)

curveTo :: [(Point Double, Point Double, Point Double)] -> State PathCursor [PathData Double]
curveTo xs = do
  modify
    ( (#curPrevious .~ (\(_, _, p) -> p) (last xs))
        . (#curControl ?~ (\(_, c2, _) -> c2) (last xs))
    )
  pure $ (\(c1, c2, x2) -> CubicP c1 c2 x2) <$> xs

-- | Convert relative points to absolute points
relToAbs3 :: (Additive a) => a -> [(a, a, a)] -> [(a, a, a)]
relToAbs3 p xs = xs'
  where
    x1 = (\(x, _, _) -> x) <$> xs
    x2 = (\(_, x, _) -> x) <$> xs
    x3 = (\(_, _, x) -> x) <$> xs
    x1' = fmap (p +) (accsum x1)
    x2' = fmap (p +) (accsum x2)
    x3' = fmap (p +) (accsum x3)
    xs' = zip3 x1' x2' x3'

reflControlPoint :: State PathCursor (Point Double)
reflControlPoint = do
  (PathCursor p _ c) <- get
  case c of
    Nothing -> pure p
    Just c' -> pure (p - (c' - p))

smoothCurveToStep :: (Point Double, Point Double) -> State PathCursor (PathData Double)
smoothCurveToStep (c2, x2) = do
  c1 <- reflControlPoint
  modify ((#curControl ?~ c2) . (#curPrevious .~ x2))
  pure (CubicP c1 c2 x2)

smoothCurveTo :: [(Point Double, Point Double)] -> State PathCursor [PathData Double]
smoothCurveTo xs =
  mapM smoothCurveToStep xs

-- | Convert relative points to absolute points
relToAbs2 :: (Additive a) => a -> [(a, a)] -> [(a, a)]
relToAbs2 p xs = xs'
  where
    x1 = fst <$> xs
    x2 = snd <$> xs
    x1' = fmap (p +) (accsum x1)
    x2' = fmap (p +) (accsum x2)
    xs' = zip x1' x2'

quad :: [(Point Double, Point Double)] -> State PathCursor [PathData Double]
quad xs = do
  modify
    ( (#curPrevious .~ snd (last xs))
        . (#curControl ?~ fst (last xs))
    )
  pure $ uncurry QuadP <$> xs

smoothQuadStep :: Point Double -> State PathCursor (PathData Double)
smoothQuadStep x2 = do
  c1 <- reflControlPoint
  modify ((#curControl ?~ c1) . (#curPrevious .~ x2))
  pure (QuadP c1 x2)

smoothQuad :: [Point Double] -> State PathCursor [PathData Double]
smoothQuad xs =
  mapM smoothQuadStep xs

arcTo :: [(Double, Double, Double, Bool, Bool, Point Double)] -> State PathCursor [PathData Double]
arcTo xs = do
  modify ((#curPrevious .~ (\(_, _, _, _, _, p) -> p) (last xs)) . (#curControl .~ Nothing))
  pure $ fromPathEllipticalArc <$> xs

fromPathEllipticalArc :: (a, a, a, Bool, Bool, Point a) -> PathData a
fromPathEllipticalArc (x, y, r, l, s, p) = ArcP (ArcInfo (Point x y) r l s) p

-- | Convert relative points to absolute points
relToAbsArc :: (Additive a) => Point a -> [(a, a, a, Bool, Bool, Point a)] -> [(a, a, a, Bool, Bool, Point a)]
relToAbsArc p xs = xs'
  where
    ps = (\(_, _, _, _, _, pt) -> pt) <$> xs
    ps' = fmap (p +) (accsum ps)
    xs' = zipWith (\(x0, x1, x2, x3, x4, _) pt -> (x0, x1, x2, x3, x4, pt)) xs ps'

-- | Convert a path command fragment to PathData
--
-- flips the y-dimension of points.
toPathData :: PathCommand -> State PathCursor [PathData Double]
toPathData (MoveTo OriginAbsolute xs) = moveTo xs
toPathData (MoveTo OriginRelative xs) = do
  (PathCursor p _ _) <- get
  moveTo (relToAbs p xs)
toPathData EndPath = do
  (PathCursor _ s _) <- get
  pure [LineP s]
toPathData (LineTo OriginAbsolute xs) = lineTo xs
toPathData (LineTo OriginRelative xs) = do
  (PathCursor p _ _) <- get
  lineTo (relToAbs p xs)
toPathData (HorizontalTo OriginAbsolute xs) = horTo xs
toPathData (HorizontalTo OriginRelative xs) = do
  (PathCursor (Point x _) _ _) <- get
  horTo (relToAbs x xs)
toPathData (VerticalTo OriginAbsolute xs) = verTo xs
toPathData (VerticalTo OriginRelative ys) = do
  (PathCursor (Point _ y) _ _) <- get
  verTo (relToAbs y ys)
toPathData (CurveTo OriginAbsolute xs) = curveTo xs
toPathData (CurveTo OriginRelative xs) = do
  (PathCursor p _ _) <- get
  curveTo (relToAbs3 p xs)
toPathData (SmoothCurveTo OriginAbsolute xs) = smoothCurveTo xs
toPathData (SmoothCurveTo OriginRelative xs) = do
  (PathCursor p _ _) <- get
  smoothCurveTo (relToAbs2 p xs)
toPathData (QuadraticBezier OriginAbsolute xs) = quad xs
toPathData (QuadraticBezier OriginRelative xs) = do
  (PathCursor p _ _) <- get
  quad (relToAbs2 p xs)
toPathData (SmoothQuadraticBezierCurveTo OriginAbsolute xs) = smoothQuad xs
toPathData (SmoothQuadraticBezierCurveTo OriginRelative xs) = do
  (PathCursor p _ _) <- get
  smoothQuad (relToAbs p xs)
toPathData (EllipticalArc OriginAbsolute xs) = arcTo xs
toPathData (EllipticalArc OriginRelative xs) = do
  (PathCursor p _ _) <- get
  arcTo (relToAbsArc p xs)

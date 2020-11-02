{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedLabels #-}

module Data.Path
  ( -- writePath,
    -- writePaths,
    ArcStuff (..),
    PathInstruction (..),
    parsePath,
    toInstructions,
    toPathAbsolute,
    toPathAbsolutes,
  ) where

-- import Chart.Types
-- import Chart.Render
import Data.Colour
import qualified Graphics.SvgTree as SvgTree
import Graphics.SvgTree (PathCommand (..), Origin(..))
import Graphics.SvgTree.PathParser
import qualified Data.Attoparsec.Text as A
import Lucid
import NumHask.Space
import NumHask.Prelude
import qualified Data.Text.Lazy as Lazy
import Control.Lens
import Linear hiding (norm, angle, zero)
-- import Control.Monad.Trans.State.Lazy

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

data ArcStuff a = ArcStuff { rx :: a, ry :: a, rot :: a, sweep :: Bool, large :: Bool} deriving (Eq, Show, Generic)

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
toPathAbsolute (ArcI (ArcStuff x y r sw l), x2, x1) =
  "A " <>
  show (x * norm (x2 - x1)) <> " " <>
  show (y * norm (x2 - x1)) <> " " <>
  show r <> " " <>
  bool "0" "1" sw <> " " <>
  bool "0" "1" l <> " " <>
  pp x2

toPathAbsolutes :: [(PathInstruction Double, Point Double)] -> Text
toPathAbsolutes xs = snd $ foldl' (\(prev,t) (i, p) -> (p,t<>" "<>toPathAbsolute (i,p,prev))) (zero,mempty) xs

fromV2 :: V2 a -> Point a
fromV2 (V2 x y) = Point x y

fromPathCurve :: (ExpField a, TrigField a) => Point a -> (Point a, Point a, Point a) -> PathInstruction a
fromPathCurve x1 (c1, c2, x2) = CubicI (Polar mag1 angle1) (Polar mag2 angle2)
  where
    mag1 = norm (c1 - x1) / norm (x2 - x1)
    angle1 = angle (c1 - x2)
    mag2 = norm (c2 - x2) / norm (x1 - x2)
    angle2 = angle (c2 - x1)

fromPathEllipticalArc :: (ExpField a) => Point a -> (a, a, a, Bool, Bool, Point a) -> PathInstruction a
fromPathEllipticalArc x1 (x, y, r, l, s, x2) = ArcI (ArcStuff x' y' r l s)
  where
    x' = x / norm (x2 - x1)
    y' = y / norm (x2 - x1)

fromPathQuad :: (ExpField a, TrigField a) => Point a -> (Point a, Point a) -> PathInstruction a
fromPathQuad x1 (c1, x2) = QuadI (Polar mag1 angle1)
  where
    mag1 = norm (c1 - x1) / norm (x2 - x1)
    angle1 = angle (c1 - x2)

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

toInstructions :: [SvgTree.PathCommand] -> [(PathInstruction Double, Point Double)]
toInstructions [] = []
toInstructions xs = snd $ foldl' (\(x,l) a -> second (l<>) $ toInstruction x a) ((zero,zero),[]) xs


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

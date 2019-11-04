{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Chart.Svg
  ( aspect
  , ratio
  , ChartSvg(..)
  , treeRect
  , treeGlyph
  , treeLine
  , treeShape
  , treeText
  , tree
  , projectSpots
  , projectSpotsWith
  , chartSvg_
  , chartSvg
  , chartSvgWith
  , fittedSvg
  , pad
  , frame
  , defaultFrame
  , renderXml
  , renderXmlWith
  , xmlToText
  , write
  , writeWith
  , rotateDA
  , translateDA
  , rotateSvg
  , translateSvg
  , ScratchStyle(ScratchStyle)
  , defaultScratchStyle
  , clearScratchStyle
  , scratchSvgWith
  , scratch
  , scratchWith
  , scratchSvg
  , placedLabel
  , ChartSvgStyle(ChartSvgStyle)
  , defaultChartSvgStyle
  , defaultSvgFrame
  , renderChartSvg
  ) where

import Chart.Core
import Chart.Spot
import Codec.Picture.Types
import Graphics.Svg as Svg hiding (Point, toPoint)
import Graphics.Svg.CssTypes as Svg hiding (Point)
import Control.Lens hiding (transform)
import Linear.V2
import NumHask.Point
import NumHask.Rect
import Text.XML.Light.Output
import qualified Data.Map as Map
import qualified Data.Text as Text
import Protolude as P
import Control.Category (id)
import GHC.Exts
import Algebra.Lattice

-- * Svg

-- | convert a ratio of x-plane : y-plane to a ViewBox with a height of one.
aspect :: (Fractional a) => a -> Rect a
aspect a = Rect (a * (-0.5)) (a * 0.5) (-0.5) 0.5

-- | convert a Rect to a ratio
ratio :: (Fractional a) => Rect a -> a
ratio (Rect x z y w) = (z-x)/(w-y)

-- | Svg of a Chart consists of
-- An Svg `Tree` list and a Rect
data ChartSvg a = ChartSvg
  { vbox :: Rect a
  , chartTrees :: [Tree]
  } deriving (Eq, Show)

instance (Lattice a, Eq a) => Semigroup (ChartSvg a) where
  (ChartSvg a b) <> (ChartSvg a' b') = ChartSvg (a<>a') (b<>b')

instance (Chartable a) => Monoid (ChartSvg a) where
  mempty = ChartSvg unitRect mempty

-- * svg primitives
 
-- | convert a point to the svg co-ordinate system
-- The svg coordinate system has the y-axis going from top to bottom.
pointSvg :: (Real a) => Point a -> (Number, Number)
pointSvg (Point x y) = (Num (realToFrac x), Num (-(realToFrac y)))

-- | convert a Rect to the svg co-ordinate system
rectSvg :: (Real a, HasRectangle s) => Rect a -> s -> s
rectSvg r =
  (rectUpperLeftCorner .~ (Num x, Num (-w))) .
  (rectWidth .~ Num (z-x)) .
  (rectHeight .~ Num (w-y))
  where
    (Rect  x z y w) = realToFrac <$> r

-- | Rectange svg
treeRect :: (Real a) => Rect a -> Tree
treeRect a =
  RectangleTree $ rectSvg a defaultSvg

-- | Text svg
treeText :: (Chartable a) => TextStyle -> P.Text -> Point a -> Tree
treeText s t p =
  TextTree Nothing (textAt (pointSvg p) t) &
  maybe id (\x -> drawAttr %~ rotatePDA x p) (realToFrac <$> s ^. #rotation)

-- | GlyphShape to svg primitive
treeShape :: GlyphShape -> Double -> Point Double -> Tree
treeShape CircleGlyph s p =
  CircleTree $ Circle mempty (pointSvg p) (Num (realToFrac s/2))
treeShape SquareGlyph s p = treeRect (translateRect p ((s*) <$> unitRect))
treeShape (RectSharpGlyph x') s p =
  treeRect (translateRect p (scale (Point s (x'*s)) unitRect))
treeShape (RectRoundedGlyph x'' rx ry) s p  =
  RectangleTree $ rectSvg (addPoint p $ scale (Point s (x''*s)) unitRect) $
  rectCornerRadius .~ (Num (realToFrac rx), Num (realToFrac ry)) $
  defaultSvg
treeShape (TriangleGlyph (Point xa ya) (Point xb yb) (Point xc yc)) s p  =
  PolygonTree $
  (polygonPoints .~ rps) $
  (drawAttr %~ translateDA p)
  defaultSvg
  where
    rps =
      [ V2 (s*xa) (-s*ya)
      , V2 (s*xb) (-s*yb)
      , V2 (s*xc) (-s*yc)
      ]
treeShape (EllipseGlyph x') s p =
  EllipseTree $ Ellipse mempty (pointSvg p)
  (Num $ realToFrac s/2) (Num $ (realToFrac x'*realToFrac s)/2)
treeShape (VLineGlyph x') s (Point x y) =
  LineTree $ Line (mempty & strokeWidth .~ Last (Just (Num (realToFrac x'))))
  (pointSvg (Point x (y - s/2)))
  (pointSvg (Point x (y + s/2)))
treeShape (HLineGlyph x') s (Point x y) =
  LineTree $ Line (mempty & strokeWidth .~ Last (Just (Num $ realToFrac x')))
  (pointSvg (Point (x - s/2) y))
  (pointSvg (Point (x + s/2) y))
treeShape SmileyGlyph s' p =
  groupTrees mempty
  [ CircleTree
    $ defaultSvg
    & drawAttr . fillColor .~ (Last $ Just $ ColorRef $ PixelRGBA8 255 255 0 255)
    & circleCenter .~ (Num (0.5 * realToFrac s), Num (0.5 * realToFrac s))
    & circleRadius .~ Num (0.5 * realToFrac s)
  , EllipseTree
    $ defaultSvg
    & drawAttr . fillColor .~ (Last $ Just $ ColorRef $ PixelRGBA8 255 255 255 255)
    & ellipseCenter .~ (Num (0.35 * realToFrac s), Num (0.3 * realToFrac s))
    & ellipseXRadius .~ Num (0.05 * realToFrac s)
    & ellipseYRadius .~ Num (0.1 * realToFrac s)
  , EllipseTree
    $ defaultSvg
    & drawAttr . fillColor .~ (Last $ Just $ ColorRef $ PixelRGBA8 255 255 255 255)
    & ellipseCenter .~ (Num (0.65 * realToFrac s), Num (0.3 * realToFrac s))
    & ellipseXRadius .~ Num (0.05 * realToFrac s)
    & ellipseYRadius .~ Num (0.1 * realToFrac s)
  , GroupTree $
    defaultSvg &
    groupChildren .~
    [ PathTree $
      defaultSvg
      & pathDefinition .~
      [ MoveTo OriginAbsolute [V2 0 0]
      , EllipticalArc OriginAbsolute [(0.38*s,0.4*s,0.1*s,False,False, V2 (0.65*s) 0)]
      ]
      & drawAttr . fillColor .~ (Last $ Just FillNone)
      & drawAttr . strokeColor .~ (Last $ Just $ ColorRef $
                                   PixelRGBA8 0 0 0 255)
      & drawAttr . strokeWidth .~ Last (Just (Num (realToFrac s * 0.03)))
      & drawAttr . transform ?~ [Translate (0.18*s) (0.65*s)]
    ]
  ]
  & drawAttr . transform ?~ [Translate (x - s/2) (-y - s/2)]
  where
    s = realToFrac s'
    (Point x y) = realToFrac <$> p

-- | GlyphStyle to svg primitive
treeGlyph :: (Fractional a, Real a) => GlyphStyle -> Point a -> Tree
treeGlyph s p =
  treeShape (s ^. #shape) (s ^. #size) (realToFrac <$> p) &
  maybe id (\x -> drawAttr %~ rotatePDA x p) (realToFrac <$> s ^. #rotation)

-- | line svg
treeLine :: (Chartable a) => [Point a] -> Tree
treeLine xs =
  PolyLineTree $
  polyLinePoints .~ ((\(Point x y) -> V2 x (-y)) . fmap realToFrac <$> xs) $
  defaultSvg

-- | convert a Chart to svg
tree :: (Chartable a) => Chart a -> Tree
tree (Chart (TextA s ts) xs) =
  groupTrees (dagText s) (zipWith (treeText s) ts (toPoint <$> xs))
tree (Chart (GlyphA s) xs) =
  groupTrees (dagGlyph s) (treeGlyph s . toPoint <$> xs)
tree (Chart (LineA s) xs) =
  groupTrees (dagLine s) [treeLine (toPoint <$> xs)]
tree (Chart (RectA s) xs) =
  groupTrees (dagRect s) (treeRect <$> (toRect <$> xs))
tree (Chart BlankA _) =
  groupTrees mempty []

-- | add drawing attributes as a group svg wrapping a [Tree]
groupTrees :: DrawAttributes -> [Tree] -> Tree
groupTrees da' tree' =
  defaultSvg &
  drawAttr %~ (da'<>) &
  groupChildren .~ tree' &
  GroupTree

-- | create a ChartSvg from a [Chart] and a Rect
chartSvg_ :: (Chartable a) =>
  Rect a -> [Chart a] -> ChartSvg a
chartSvg_ a cs = ChartSvg a (tree <$> cs)

projectSpots :: (Chartable a) => Rect a -> [Chart a] -> [Chart a]
projectSpots a cs = cs'
  where
    xss = projectTo2 a (spots <$> cs)
    ss = annotation <$> cs
    cs' = zipWith Chart ss xss

projectSpotsWith :: (Chartable a) => Rect a -> Rect a -> [Chart a] -> [Chart a]
projectSpotsWith new old cs = cs'
  where
    xss = fmap (projectOn new old) . spots <$> cs
    ss = annotation <$> cs
    cs' = zipWith Chart ss xss

-- | convert a [Chart] to a ChartSvg, projecting Chart data to the supplied Rect, and expanding the Rect for chart style if necessary
chartSvg :: (Chartable a) =>
  Rect a -> [Chart a] -> ChartSvg a
chartSvg a cs = chartSvg_ (defRect $ styleBoxes cs') cs'
  where
    cs' = projectSpots a cs

-- | convert a [Chart] to a ChartSvg, projecting Chart data from a specified Rect range to the supplied Rect, and expanding the Rect for chart style if necessary
chartSvgWith :: (Chartable a) =>
  Rect a -> Rect a -> [Chart a] -> ChartSvg a
chartSvgWith new old cs = chartSvg_ (addToRect new (styleBoxes cs')) cs'
  where
    cs' = projectSpotsWith new old cs

-- | convert a [Chart] to a ChartSvg, setting the Rect equal to the Chart data area
fittedSvg :: (Chartable a) =>
  [Chart a] -> ChartSvg a
fittedSvg cs =
  chartSvg (defRect $ styleBoxes cs) cs

-- | widen a ChartSvg Rect by a fraction of the size.
pad :: (Chartable a) => a -> ChartSvg a -> ChartSvg a
pad p (ChartSvg vb s) = ChartSvg (widenProp p vb) s

-- | add an enclosing fitted frame to a ChartSvg
frame :: (Chartable a) => RectStyle -> ChartSvg a -> ChartSvg a
frame o (ChartSvg vb _) =
  ChartSvg
  vb
  ((:[]) . tree $ Chart (RectA o) [SpotRect vb])

-- | a default frame
defaultFrame :: (Chartable a) => ChartSvg a -> ChartSvg a
defaultFrame ch = frame (border 0.01 blue 1.0) ch <> ch

-- | render a ChartSvg to an xml Document with the supplied size and various bits and pieces
renderXmlWith :: (Real a) => Point a -> Map.Map Text.Text Element -> Text.Text -> [CssRule] -> FilePath -> ChartSvg a -> Document
renderXmlWith (Point wid hei) defs desc css fp (ChartSvg vb ts) =
  Document
  ((\(Rect x z y w) -> Just (x,-w,z-x,w-y)) $ realToFrac <$> vb)
  (Just (Num (realToFrac wid)))
  (Just (Num (realToFrac hei)))
  ts (Map.mapKeys Text.unpack defs) (Text.unpack desc) css fp

-- | render a ChartSvg to an xml Document with the supplied size
renderXml :: (Real a) => Point a -> ChartSvg a -> Document
renderXml p = renderXmlWith p Map.empty "" [] ""

-- | render an xml document to Text
xmlToText :: Document -> P.Text
xmlToText = Text.pack . ppcElement defaultConfigPP . xmlOfDocument

-- | write a ChartSvg to a svg file with various Document attributes.
writeWith :: (Real a) => FilePath -> Point a -> Map.Map Text.Text Element -> Text.Text -> [CssRule] -> ChartSvg a -> IO ()
writeWith fp p defs desc css c = saveXmlFile fp (renderXmlWith p defs desc css fp c)

-- | write a ChartSvg to an svg file.
write :: (Real a) => FilePath -> Point a -> ChartSvg a -> IO ()
write fp p = writeWith fp p Map.empty "" []

-- * transformations
-- | A DrawAttributes to rotate by x degrees.
rotateDA :: (Real a, HasDrawAttributes s) => a -> s -> s
rotateDA a s = s & transform %~ (Just . maybe r (<>r)) where
  r = [ Rotate (realToFrac a) Nothing ]

-- | A DrawAttributes to rotate around a point by x degrees.
rotatePDA :: (Real a, HasDrawAttributes s) => a -> Point a -> s -> s
rotatePDA a (Point x y) s = s & transform %~ (Just . maybe r (<>r)) where
  r = [ Rotate (realToFrac a) (Just (realToFrac x,-realToFrac y)) ]

-- | A DrawAttributes to translate by a Point.
translateDA :: (Real a, HasDrawAttributes s) => Point a -> s -> s
translateDA p = transform %~
  (\x -> Just $ maybe [ Translate x' (-y')] (<> [ Translate x' (-y')]) x)
  where
    Point x' y' = realToFrac <$> p

-- | Rotate a ChartSvg expanding the Rect as necessary.
-- Multiple rotations will expand the bounding box conservatively.
rotateSvg :: (Chartable a) => a -> ChartSvg a -> ChartSvg a
rotateSvg r (ChartSvg vb c) =
  ChartSvg
  (rotateRect r vb)
  [groupTrees (rotateDA r mempty) c]

-- | Translate a ChartSvg also moving the Rect
translateSvg :: (Chartable a) => Point a -> ChartSvg a -> ChartSvg a
translateSvg p (ChartSvg vb c) = 
  ChartSvg
  (translateRect p vb)
  [groupTrees (translateDA p mempty) c]

-- * development helpers

data ScratchStyle = ScratchStyle
  { fileName :: FilePath
  , size :: Double
  , ratioAspect :: Double
  , outerPad :: Double
  , innerPad :: Double
  , frame' :: ChartSvg Double -> ChartSvg Double
  , maybeOrig :: Maybe GlyphStyle
  } deriving (Generic)

defaultScratchStyle :: ScratchStyle
defaultScratchStyle  =
  ScratchStyle "other/scratchpad.svg" 200 1.5 1.05 1.05 defaultFrame Nothing

clearScratchStyle :: ScratchStyle
clearScratchStyle  = ScratchStyle "other/scratchpad.svg" 400 1 1 1 defaultFrame Nothing

scratchSvgWith :: ScratchStyle -> [ChartSvg Double] -> IO ()
scratchSvgWith s x =
  write
  (s ^. #fileName)
  (Point (s ^. #ratioAspect * s ^. #size) (s ^. #size)) $
  pad (s ^. #outerPad) $
  (s ^. #frame') $
  pad (s ^. #innerPad) $
  mconcat x

scratchSvg :: [ChartSvg Double] -> IO ()
scratchSvg = scratchSvgWith defaultScratchStyle

scratch :: [Chart Double] -> IO ()
scratch = scratchWith defaultScratchStyle

scratchWith :: ScratchStyle -> [Chart Double] -> IO ()
scratchWith s x =
  write
  (s ^. #fileName)
  (Point (s ^. #ratioAspect * s ^. #size) (s ^. #size)) $
  pad (s ^. #outerPad) $
  (s ^. #frame') $
  pad (s ^. #innerPad) $
  chartSvg
  (aspect (s ^. #ratioAspect))
  (orig' <> x) where
  orig' = case s ^. #maybeOrig of
    Nothing -> mempty
    Just g -> [showOriginWith g]

placedLabel :: (Chartable a) => Point a -> a -> Text.Text -> Chart a
placedLabel p d t =
  Chart (TextA (defaultTextStyle & #translate ?~ (realToFrac <$> p) &
                #rotation ?~ realToFrac d) [t])
  [SP 0 0]

data ChartSvgStyle = ChartSvgStyle
  { sizex :: Double
  , sizey :: Double
  , chartAspect :: Double
  , outerPad :: Maybe Double
  , innerPad :: Maybe Double
  , chartFrame :: Maybe RectStyle
  , orig :: Maybe GlyphStyle
  } deriving (Generic, Show)

defaultChartSvgStyle :: ChartSvgStyle
defaultChartSvgStyle = ChartSvgStyle 800 600 1.7 (Just 1.02) Nothing Nothing Nothing

defaultSvgFrame :: RectStyle
defaultSvgFrame = border 0.01 blue 1.0

renderChartSvg :: Double -> Double -> ChartSvg Double -> Text.Text
renderChartSvg x y =
  xmlToText . renderXml (Point x y)

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
  , Chart.Svg.rotate
  , translate
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
import Data.List (zipWith3)
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
-- | Rectange svg
treeRect :: (Real a) => Rect a -> Tree
treeRect a =
  RectangleTree $
  rectUpperLeftCorner .~ (Num x, Num (-w)) $
  rectWidth .~ Num (z-x) $
  rectHeight .~ Num (w-y) $
  defaultSvg
  where
    (Rect x z y w) = realToFrac <$> a

-- | Text svg
treeText :: (Chartable a) => P.Text -> Point a -> Tree
treeText t p =
  TextTree Nothing (textAt (Num x, Num (-y)) t)
  where
    (Point x y) = realToFrac <$> p

-- | Text svg with rotation
treeTextRotate :: (Chartable a) => P.Text -> a -> Point a -> Tree
treeTextRotate t rot p =
  TextTree Nothing (textAt (Num x, Num (-y)) t) &
  drawAttr .~ rotatePDA rot p
  where
    (Point x y) = realToFrac <$> p

-- | GlyphShape to svg primitive
treeShape :: GlyphShape -> Double -> Point Double -> Tree
treeShape CircleGlyph s p =
  CircleTree $ Circle mempty (Num x, Num (-y)) (Num (realToFrac s/2))
  where
    (Point x y) = realToFrac <$> p
treeShape SquareGlyph s p = treeRect (translateRect p ((s*) <$> unitRect))
treeShape (RectSharpGlyph x') s p =
  treeRect (translateRect p (scale (Point s (x'*s)) unitRect))
treeShape (RectRoundedGlyph x'' rx ry) s p  =
  RectangleTree $
  rectUpperLeftCorner .~ (Num (x+x'), Num (-(w+y'))) $
  rectWidth .~ Num (realToFrac z-realToFrac x) $
  rectHeight .~ Num (realToFrac w-realToFrac y) $
  rectCornerRadius .~ (Num (realToFrac rx), Num (realToFrac ry)) $
  defaultSvg
  where
    (Rect x z y w) = realToFrac <$> scale (Point s (x''*s)) unitRect
    (Point x' y') = realToFrac <$> p
treeShape (TriangleGlyph (Point xa ya) (Point xb yb) (Point xc yc)) s p  =
  PolygonTree $
  polygonPoints .~ rps $
  drawAttr . transform ?~ [Translate x' (-y')] $
  defaultSvg
  where
    rps =
      [ V2 (s*xa) (-s*ya)
      , V2 (s*xb) (-s*yb)
      , V2 (s*xc) (-s*yc)
      ]
    (Point x' y') = realToFrac <$> p
treeShape (EllipseGlyph x') s (Point x y) =
  EllipseTree $ Ellipse mempty (Num (realToFrac x), Num (-(realToFrac y)))
  (Num $ realToFrac s/2) (Num $ (realToFrac x'*realToFrac s)/2)
treeShape (VLineGlyph x') s (Point x y) =
  LineTree $ Line (mempty & strokeWidth .~ Last (Just (Num (realToFrac x'))))
  (Num (realToFrac x), Num (realToFrac $ y - s/2))
  (Num (realToFrac x), Num (realToFrac $ y + s/2))
treeShape (HLineGlyph x') s (Point x y) =
  LineTree $ Line (mempty & strokeWidth .~ Last (Just (Num $ realToFrac x')))
  (Num (realToFrac $ x - s/2), Num $ realToFrac y)
  (Num (realToFrac $ x + s/2), Num (realToFrac y))
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
treeGlyph :: GlyphStyle -> Point Double -> Tree
treeGlyph s =
  treeShape (s ^. #shape) (s ^. #size)

-- | line svg
treeLine :: (Chartable a) => [Point a] -> Tree
treeLine xs =
  PolyLineTree $
  polyLinePoints .~ ((\(Point x y) -> V2 x (-y)) . fmap realToFrac <$> xs) $
  defaultSvg

-- | convert a Chart to svg
tree :: (Chartable a) => Chart a -> Tree
tree (Chart (TextA s ts) das xs) =
  groupTrees (das <> daText s) (zipWith treeText' ts (toPoint <$> xs))
  where
    treeText' = maybe treeText (\r txt p -> treeTextRotate txt (realToFrac r) p) (s ^. #rotation)
tree (Chart (GlyphA s) das xs) =
  groupTrees (das <> daGlyph s) (treeGlyph s <$> (toPoint . fmap realToFrac <$> xs))
tree (Chart (LineA s) das xs) =
  groupTrees (das <> daLine s) [treeLine (toPoint <$> xs)]
tree (Chart (RectA s) das xs) =
  groupTrees (das <> daRect s) (treeRect <$> (toRect <$> xs))
tree (Chart BlankA das _) =
  groupTrees das []

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
    dass = drawatts <$> cs
    cs' = zipWith3 Chart ss dass xss

projectSpotsWith :: (Chartable a) => Rect a -> Rect a -> [Chart a] -> [Chart a]
projectSpotsWith new old cs = cs'
  where
    xss = fmap (projectOn new old) . spots <$> cs
    ss = annotation <$> cs
    dass = drawatts <$> cs
    cs' = zipWith3 Chart ss dass xss

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
  ((:[]) . tree $ Chart (RectA o) mempty [SpotRect vb])

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
rotateDA :: (Real a) => a -> DrawAttributes
rotateDA r = mempty & transform ?~ [Rotate (realToFrac r) Nothing]

-- | A DrawAttributes to rotate around a point by x degrees.
rotatePDA :: (Real a) => a -> Point a -> DrawAttributes
rotatePDA r p = mempty & transform ?~ [Rotate (realToFrac r) (Just (x,-y))]
  where
    (Point x y) = realToFrac <$> p

-- | A DrawAttributes to translate by a Point.
translateDA :: (Real a) => Point a -> DrawAttributes
translateDA (Point x y) = mempty & transform ?~
  [Translate (realToFrac x) (-realToFrac y)]

-- | Rotate a ChartSvg expanding the Rect as necessary.
-- Multiple rotations will expand the bounding box conservatively.
rotate :: (Chartable a, Floating a) => a -> ChartSvg a -> ChartSvg a
rotate r (ChartSvg vb c) = 
  ChartSvg
  (rotateRect r vb)
  [groupTrees (rotateDA r) c]

-- | Translate a ChartSvg also moving the Rect
translate :: (Chartable a) => Point a -> ChartSvg a -> ChartSvg a
translate p (ChartSvg vb c) = 
  ChartSvg
  (translateRect p vb)
  [groupTrees (translateDA p) c]

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
  Chart (TextA defaultTextStyle [t])
  (mempty <> translateDA p <> rotateDA d)
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

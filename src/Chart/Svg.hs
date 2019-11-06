{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
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
  ( treeRect
  , treeGlyph
  , treeLine
  , treeShape
  , treeText
  , tree
  , dagRect
  , dagText
  , dagGlyph
  , dagLine
  , rotateDA
  , translateDA
  , rotateSvg
  , translateSvg
  , renderToXml
  , renderToXmlWith
  , xmlToText
  , writeWithXml
  , writeChartSvg
  , renderChartSvg
  ) where

import Prelude hiding (writeFile)
import Chart.Types
import Codec.Picture.Types
import Graphics.Svg as Svg hiding (Point, toPoint, Text)
import Graphics.Svg.CssTypes as Svg hiding (Point)
import Control.Lens hiding (transform)
import Linear.V2
import NumHask.Space hiding (Element)
import Text.XML.Light.Output
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text (Text)
import GHC.Exts
import GHC.OverloadedLabels
import Data.Monoid
-- * Svg

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
treeText :: (Chartable a) => TextStyle -> Text -> Point a -> Tree
treeText s t p =
  TextTree Nothing (textAt (pointSvg p) t) &
  maybe id (\x -> drawAttr %~ rotatePDA x p) (realToFrac <$> s ^. #rotation)

-- | GlyphShape to svg Tree
treeShape :: GlyphShape -> Double -> Point Double -> Tree
treeShape CircleGlyph s p =
  CircleTree $ Circle mempty (pointSvg p) (Num (realToFrac s/2))
treeShape SquareGlyph s p = treeRect (move p ((s*) <$> unitRect))
treeShape (RectSharpGlyph x') s p =
  treeRect (move p (scale (Point s (x'*s)) unitRect))
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

-- | GlyphStyle to svg Tree
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

-- draw attribute computations
dagRect :: RectStyle -> DrawAttributes
dagRect o =
  mempty &
  (strokeWidth .~ Last (Just $ Num (realToFrac $ o ^. #borderSize))) .
  (strokeColor .~ Last (Just $ ColorRef (promotePixel $ o ^. #borderColor))) .
  (strokeOpacity ?~ realToFrac (o ^. #borderOpacity)) .
  (fillColor .~ Last (Just $ ColorRef (promotePixel $ o ^. #color))) .
  (fillOpacity ?~ realToFrac (o ^. #opacity)) 

-- | group draw attributes for TextStyle. rotation is defined by svg relative to a point (or to an origin) and so rotation needs to be dealth with separately.
dagText :: ( ) => TextStyle -> DrawAttributes
dagText o =
  mempty &
  (fontSize .~ Last (Just $ Num (o ^. #size))) &
  (strokeWidth .~ Last (Just $ Num 0)) &
  (strokeColor .~ Last (Just FillNone)) &
  (fillColor .~ Last (Just $ ColorRef (promotePixel $ o ^. #color))) &
  (fillOpacity ?~ realToFrac (o ^. #opacity)) &
  (textAnchor .~ Last (Just (toTextAnchor $ o ^. #anchor))) &
  maybe id (\(Point x y) -> transform ?~ [Translate (realToFrac x) (-realToFrac y)])
             (o ^. #translate)
  where
    toTextAnchor :: Anchor -> TextAnchor
    toTextAnchor AnchorMiddle = TextAnchorMiddle
    toTextAnchor AnchorStart = TextAnchorStart
    toTextAnchor AnchorEnd = TextAnchorEnd

dagGlyph :: GlyphStyle -> DrawAttributes
dagGlyph o =
  mempty &
  (strokeWidth .~ Last (Just $ Num (o ^. #borderSize))) &
  (strokeColor .~ Last (Just $ ColorRef (promotePixel $ o ^. #borderColor))) &
  (strokeOpacity ?~ realToFrac (o ^. #borderOpacity)) &
  (fillColor .~ Last (Just $ ColorRef (promotePixel $ o ^. #color))) &
  (fillOpacity ?~ realToFrac (o ^. #opacity)) &
  maybe id (\(Point x y) -> transform ?~ [Translate (realToFrac x) (-realToFrac y)])
             (o ^. #translate)

dagLine :: LineStyle -> DrawAttributes
dagLine o =
  mempty &
  (strokeWidth .~ Last (Just $ Num (o ^. #width))) .
  (strokeColor .~ Last (Just $ ColorRef (promotePixel $ o ^. #color))) .
  (strokeOpacity ?~ realToFrac (o ^. #opacity)) .
  (fillColor .~ Last (Just FillNone))

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
  (move p vb)
  [groupTrees (translateDA p mempty) c]

-- * rendering
-- | render a ChartSvg to an xml Document with the supplied size and various bits and pieces
renderToXmlWith :: (Real a) => Point a -> Map.Map Text.Text Element -> Text.Text -> [CssRule] -> FilePath -> ChartSvg a -> Document
renderToXmlWith (Point wid hei) defs desc css fp (ChartSvg vb ts) =
  Document
  ((\(Rect x z y w) -> Just (x,-w,z-x,w-y)) $ realToFrac <$> vb)
  (Just (Num (realToFrac wid)))
  (Just (Num (realToFrac hei)))
  ts (Map.mapKeys Text.unpack defs) (Text.unpack desc) css fp

-- | render a ChartSvg to an xml Document with the supplied size
renderToXml :: (Real a) => Point a -> ChartSvg a -> Document
renderToXml p = renderToXmlWith p Map.empty "" [] ""

-- | render an xml document to Text
xmlToText :: Document -> Text
xmlToText = Text.pack . ppcElement defaultConfigPP . xmlOfDocument

renderChartSvg :: Double -> Double -> ChartSvg Double -> Text.Text
renderChartSvg x y =
  xmlToText . renderToXml (Point x y)

-- | write a ChartSvg to a svg file with various Document attributes.
writeWithXml :: (Real a) => FilePath -> Point a -> Map.Map Text.Text Element -> Text.Text -> [CssRule] -> ChartSvg a -> IO ()
writeWithXml fp p defs desc css c = Text.writeFile fp (xmlToText $ renderToXmlWith p defs desc css fp c)

writeChartSvg :: FilePath -> Point Double -> ChartSvg Double -> IO ()
writeChartSvg fp (Point x y) cs = Text.writeFile fp (renderChartSvg x y cs)


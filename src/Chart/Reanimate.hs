{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | Integration of reanimate and chart-svg
module Chart.Reanimate
  ( fromHudOptionsChart,
    fromHudChart,
    fromChartsWith,
    renderToDocument,
    tree,
    toPixelRGBA8,
  )
where

import Chart
import Codec.Picture.Types
import Control.Lens hiding (transform)
import qualified Data.Attoparsec.Text as A
import Graphics.SvgTree.PathParser
import Graphics.SvgTree.Types hiding (Point, Text)
import Linear.V2
import NumHask.Prelude hiding (fold)
import qualified NumHask.Space as NH

-- | Convert to reanimate color primitive.
toPixelRGBA8 :: Colour -> PixelRGBA8
toPixelRGBA8 (Colour r g b o) =
  PixelRGBA8
    (fromIntegral $ (floor $ r * 256 :: Int))
    (fromIntegral $ (floor $ g * 256 :: Int))
    (fromIntegral $ (floor $ b * 256 :: Int))
    (fromIntegral $ (floor $ o * 256 :: Int))

-- | Render a chart to a 'Document' using the supplied svg and hud config.
fromHudOptionsChart :: SvgOptions -> HudOptions -> [Hud Double] -> [Chart Double] -> Document
fromHudOptionsChart so hc hs cs = fromHudChart so (hs <> hs') (cs <> cs')
  where
    (hs', cs') = makeHud (padBox $ dataBoxes cs) hc

-- | Render some huds and charts to a 'Document'.
fromHudChart :: SvgOptions -> [Hud Double] -> [Chart Double] -> Document
fromHudChart so hs cs = fromChartsWith so (runHud (initialCanvas (so ^. #chartAspect) cs) hs cs)

-- | render Charts with the supplied svg options.
fromChartsWith :: SvgOptions -> [Chart Double] -> Document
fromChartsWith so cs =
  (renderToDocument size' rect' cs')
  where
    rect' = styleBoxesS cs' & maybe id padRect (so ^. #outerPad)
    cs' =
      cs &
      runHud (styleBoxesS cs) [chartAspectHud (so ^. #chartAspect)] &
      maybe id (\x -> frameChart x (fromMaybe 0 (so ^. #innerPad)))
        (so ^. #chartFrame)
    Point w h = NH.width rect'
    size' = Point ((so ^. #svgHeight)/h*w) (so ^. #svgHeight)

-- | render Charts to a Document using the supplied size and viewbox.
renderToDocument :: Point Double -> Rect Double -> [Chart Double] -> Document
renderToDocument (Point w' h') vb cs =
  Document
    ((\(Rect x z y w) -> Just (x, - w, z - x, w - y)) $ vb)
    (Just (Num w'))
    (Just (Num h'))
    (tree <$> cs)
    (unpack "")
    ""
    (PreserveAspectRatio False AlignNone Nothing)

-- * svg primitives

-- | convert a point to the svg co-ordinate system
-- The svg coordinate system has the y-axis going from top to bottom.
pointSvg :: Point Double -> (Number, Number)
pointSvg (Point x y) = (Num x, Num (- y))

-- | A DrawAttributes to rotate around a point by x degrees.
rotatePDA :: (HasDrawAttributes s) => Double -> Point Double -> s -> s
rotatePDA a (Point x y) s = s & transform %~ (Just . maybe r (<> r))
  where
    r = [Rotate a (Just (x, -y))]

-- | A DrawAttributes to translate by a Point.
translateDA :: (HasDrawAttributes s) => Point Double -> s -> s
translateDA (Point x' y') =
  transform
    %~ (\x -> Just $ maybe [Translate x' (- y')] (<> [Translate x' (- y')]) x)

-- | convert a Rect to the svg co-ordinate system
rectSvg :: Rect Double -> Rectangle -> Rectangle
rectSvg (Rect x z y w) =
  (rectUpperLeftCorner .~ (Num x, Num (- w)))
    . (rectWidth .~ Just (Num (z - x)))
    . (rectHeight .~ Just (Num (w - y)))

-- | Rectange svg
treeRect :: Rect Double -> Tree
treeRect a =
  RectangleTree $ rectSvg a defaultSvg

-- | Text svg
treeText :: TextStyle -> Text -> Point Double -> Tree
treeText s t p =
  TextTree Nothing (textAt (pointSvg p) t)
    & maybe id (\x -> drawAttributes %~ rotatePDA x p) (s ^. #rotation)

-- | GlyphShape to svg Tree
treeShape :: GlyphShape -> Double -> Point Double -> Tree
treeShape CircleGlyph s p =
  CircleTree $ Circle mempty (pointSvg p) (Num (s / 2))
treeShape SquareGlyph s p = treeRect (move p ((s *) <$> one))
treeShape (RectSharpGlyph x') s p =
  treeRect (move p (scale (Point s (x' * s)) one))
treeShape (RectRoundedGlyph x'' rx ry) s p =
  RectangleTree
    . rectSvg (addPoint p $ scale (Point s (x'' * s)) one)
    . (rectCornerRadius .~ (Just $ Num rx, Just $ Num ry))
    $ defaultSvg
treeShape (TriangleGlyph (Point xa ya) (Point xb yb) (Point xc yc)) s p =
  PolygonTree
    . (polygonPoints .~ rps)
    $ (drawAttributes %~ translateDA p) defaultSvg
  where
    rps =
      [ V2 (s * xa) (- s * ya),
        V2 (s * xb) (- s * yb),
        V2 (s * xc) (- s * yc)
      ]
treeShape (EllipseGlyph x') s p =
  EllipseTree $
    Ellipse
      mempty
      (pointSvg p)
      (Num $ s / 2)
      (Num $ (x' * s) / 2)
treeShape (VLineGlyph x') s (Point x y) =
  LineTree $
    Line
      (mempty & strokeWidth .~ Last (Just (Num x')))
      (pointSvg (Point x (y - s / 2)))
      (pointSvg (Point x (y + s / 2)))
treeShape (HLineGlyph x') s (Point x y) =
  LineTree $
    Line
      (mempty & strokeWidth .~ Last (Just (Num x')))
      (pointSvg (Point (x - s / 2) y))
      (pointSvg (Point (x + s / 2) y))
treeShape (PathGlyph path) _ p =
  PathTree $ Path ((drawAttributes %~ translateDA p) defaultSvg) path'
  where
    path' = either mempty (: []) $ A.parseOnly command path

-- | GlyphStyle to svg Tree
treeGlyph :: GlyphStyle -> Point Double -> Tree
treeGlyph s p =
  treeShape (s ^. #shape) (s ^. #size) p
    & maybe id (\x -> drawAttributes %~ rotatePDA x p) (s ^. #rotation)

-- | line svg
treeLine :: [Point Double] -> Tree
treeLine xs =
  PolyLineTree
    . (polyLinePoints .~ ((\(Point x y) -> V2 x (- y)) <$> xs))
    $ defaultSvg

-- | convert a Chart to svg
--
-- FIXME: add PathA
tree :: Chart Double -> Tree
tree (Chart (TextA s ts) xs) =
  groupTrees (daText s) (zipWith (treeText s) ts (toPoint <$> xs))
tree (Chart (GlyphA s) xs) =
  groupTrees (daGlyph s) (treeGlyph s . toPoint <$> xs)
tree (Chart (LineA s) xs) =
  groupTrees (daLine s) [treeLine (toPoint <$> xs)]
tree (Chart (RectA s) xs) =
  groupTrees (daRect s) (treeRect <$> (toRect <$> xs))
tree (Chart BlankA _) =
  groupTrees mempty []

-- | add drawing attributes as a group svg wrapping a [Tree]
groupTrees :: DrawAttributes -> [Tree] -> Tree
groupTrees da' tree' =
  GroupTree (drawAttributes %~ (<> da') $ groupChildren .~ tree' $ defaultSvg)

-- * DrawAttribute computations
daRect :: RectStyle -> DrawAttributes
daRect o =
  mempty
    & (strokeWidth .~ Last (Just $ Num (o ^. #borderSize)))
    . (strokeColor .~ Last (Just $ ColorRef (toPixelRGBA8 $ o ^. #borderColor)))
    . (strokeOpacity ?~ (realToFrac $ opac $ o ^. #borderColor))
    . (fillColor .~ Last (Just $ ColorRef (toPixelRGBA8 $ o ^. #color)))
    . (fillOpacity ?~ (realToFrac $ opac $ o ^. #color))

daText :: () => TextStyle -> DrawAttributes
daText o =
  mempty
    & (fontSize .~ Last (Just $ Num (o ^. #size)))
    & (strokeWidth .~ Last (Just $ Num 0))
    & (strokeColor .~ Last (Just FillNone))
    & (fillColor .~ Last (Just $ ColorRef (toPixelRGBA8 $ o ^. #color)))
    & (fillOpacity ?~ realToFrac (opac $ o ^. #color))
    & (textAnchor .~ Last (Just (toTextAnchor $ o ^. #anchor)))
    & maybe
      id
      (\(Point x y) -> transform ?~ [Translate x (-y)])
      (o ^. #translate)
  where
    toTextAnchor :: Anchor -> TextAnchor
    toTextAnchor AnchorMiddle = TextAnchorMiddle
    toTextAnchor AnchorStart = TextAnchorStart
    toTextAnchor AnchorEnd = TextAnchorEnd

daGlyph :: GlyphStyle -> DrawAttributes
daGlyph o =
  mempty
    & (strokeWidth .~ Last (Just $ Num (o ^. #borderSize)))
    & (strokeColor .~ Last (Just $ ColorRef (toPixelRGBA8 $ o ^. #borderColor)))
    & (strokeOpacity ?~ realToFrac (opac $ o ^. #borderColor))
    & (fillColor .~ Last (Just $ ColorRef (toPixelRGBA8 $ o ^. #color)))
    & (fillOpacity ?~ realToFrac (opac $ o ^. #color))
    & maybe
      id
      (\(Point x y) -> transform ?~ [Translate x (-y)])
      (o ^. #translate)

daLine :: LineStyle -> DrawAttributes
daLine o =
  mempty
    & (strokeWidth .~ Last (Just $ Num (o ^. #width)))
    . (strokeColor .~ Last (Just $ ColorRef (toPixelRGBA8 $ o ^. #color)))
    . (strokeOpacity ?~ realToFrac (opac $ o ^. #color))
    . (fillColor .~ Last (Just FillNone))

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | Integration of reanimate and chart-svg
module Chart.Reanimate
  ( chartSvgRe,
    chartSvgTree,
    renderHudChartRe,
    renderChartsWithRe,
    renderHudChartTree,
    renderChartsWithTree,
    renderToDocument,
    renderToTrees,
    tree,
    toPixelRGBA8,
  )
where

import Chart as C hiding (transform, Line)
import Codec.Picture.Types
import Control.Lens hiding (transform)
import qualified Data.Attoparsec.Text as A
import Graphics.SvgTree.PathParser
import Graphics.SvgTree.Types as SvgTree hiding (Point, Text)
import Linear.V2
import NumHask.Prelude hiding (fold)
import qualified NumHask.Space as NH
import qualified Graphics.SvgTree.CssTypes as Css
import qualified Graphics.SvgTree as SvgTree

-- | Render a 'ChartSvg' to a 'Document'
chartSvgRe :: ChartSvg -> Document
chartSvgRe (ChartSvg so hc hs cs) = renderHudChartRe so (hs <> hs') (cs <> cs')
  where
    (hs', cs') = makeHud (padBox $ dataBoxes cs) hc

-- | Render a 'ChartSvg' to 'Tree's
chartSvgTree :: ChartSvg -> [Tree]
chartSvgTree (ChartSvg so hc hs cs) = renderHudChartTree so (hs <> hs') (cs <> cs')
  where
    (hs', cs') = makeHud (padBox $ dataBoxes cs) hc

-- | Render some huds and charts to a 'Document'.
renderHudChartRe :: SvgOptions -> [Hud Double] -> [Chart Double] -> Document
renderHudChartRe so hs cs =
  renderChartsWithRe so (runHud (initialCanvas (so ^. #chartAspect) cs) hs cs)

-- | Render some huds and charts to trees.
renderHudChartTree :: SvgOptions -> [Hud Double] -> [Chart Double] -> [Tree]
renderHudChartTree so hs cs =
  renderChartsWithTree so (runHud (initialCanvas (so ^. #chartAspect) cs) hs cs)

-- | render Charts with the supplied svg options.
renderChartsWithRe :: SvgOptions -> [Chart Double] -> Document
renderChartsWithRe so cs =
  renderToDocument (view #cssOptions so) size' rect' cs'
  where
    rect' = styleBoxesS cs' & maybe id padRect (so ^. #outerPad)
    cs' =
      cs &
      runHud (styleBoxesS cs) [chartAspectHud (so ^. #chartAspect)] &
      maybe id (\x -> frameChart x (fromMaybe 0 (so ^. #innerPad)))
        (so ^. #chartFrame)
    Point w h = NH.width rect'
    size' = Point ((so ^. #svgHeight)/h*w) (so ^. #svgHeight)

-- | render Charts with the supplied svg options.
renderChartsWithTree :: SvgOptions -> [Chart Double] -> [Tree]
renderChartsWithTree so cs =
  renderToTrees (view #cssOptions so) $
  cs &
  runHud (styleBoxesS cs) [chartAspectHud (so ^. #chartAspect)] &
  maybe id (\x -> frameChart x (fromMaybe 0 (so ^. #innerPad)))
    (so ^. #chartFrame)

-- | render Charts to a Document using the supplied size and viewbox.
renderToDocument :: CssOptions -> Point Double -> Rect Double -> [Chart Double] -> Document
renderToDocument csso (Point w' h') vb cs =
  Document
    ((\(Rect x z y w) -> Just (x, - w, z - x, w - y)) vb)
    (Just (Num w'))
    (Just (Num h'))
    (SvgTree.cssApply (cssRules csso) . tree <$> cs)
    (unpack "")
    ""
    (PreserveAspectRatio False AlignNone Nothing)

-- | render Charts to a Tree list (which is more conducive to downstream manipulation than a document).
renderToTrees :: CssOptions -> [Chart Double] -> [Tree]
renderToTrees csso cs = SvgTree.cssApply (cssRules csso) . tree <$> cs

cssRules :: CssOptions -> [Css.CssRule]
cssRules UseCssCrisp = [cssCrisp']
cssRules UseGeometricPrecision = [cssGeometricPrecision]
cssRules NoCssOptions = []

-- | crisp edges css
cssGeometricPrecision :: Css.CssRule
cssGeometricPrecision = Css.CssRule [] [Css.CssDeclaration "shape-rendering" [[Css.CssString "geometricPrecision"]]]

cssCrisp' :: Css.CssRule
cssCrisp' = Css.CssRule [] [Css.CssDeclaration "shape-rendering" [[Css.CssString "crispEdges"]]]

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

-- | GlyphStyle to svg Tree
treePath :: [PathInfo Double] -> [Point Double] -> Tree
treePath s p = PathTree $ Path mempty (zipWith (curry toPathCommand) s p)

-- | convert a 'Chart' to a 'Tree'
--
tree :: Chart Double -> Tree
tree (Chart (TextA s ts) xs) =
  groupTrees (daText s) (zipWith (treeText s) ts (toPoint <$> xs))
tree (Chart (GlyphA s) xs) =
  groupTrees (daGlyph s) (treeGlyph s . toPoint <$> xs)
tree (Chart (LineA s) xs) =
  groupTrees (daLine s) [treeLine (toPoint <$> xs)]
tree (Chart (RectA s) xs) =
  groupTrees (daRect s) (treeRect <$> (toRect <$> xs))
tree (Chart (PathA s pis) xs) =
  groupTrees (daPath s) [treePath pis (toPoint <$> xs)]
tree (Chart BlankA _) =
  groupTrees mempty []

-- | add drawing attributes as a group svg wrapping a [Tree]
groupTrees :: DrawAttributes -> [Tree] -> Tree
groupTrees da' tree' =
  GroupTree (drawAttributes %~ (<> da') $ groupChildren .~ tree' $ defaultSvg)

-- * DrawAttribute computations
daRect :: RectStyle -> DrawAttributes
daRect o =
  mempty &
  (strokeWidth .~ Last (Just $ Num (o ^. #borderSize))) &
  (strokeColor .~ Last (Just $ ColorRef (toPixelRGBA8 $ o ^. #borderColor))) &
  (strokeOpacity ?~ realToFrac (opac $ o ^. #borderColor)) &
  (fillColor .~ Last (Just $ ColorRef (toPixelRGBA8 $ o ^. #color))) &
  (fillOpacity ?~ realToFrac (opac $ o ^. #color))

daText :: () => TextStyle -> DrawAttributes
daText o =
  mempty &
  (fontSize .~ Last (Just $ Num (o ^. #size))) &
  (strokeWidth .~ Last (Just $ Num 0)) &
  (strokeColor .~ Last (Just FillNone)) &
  (fillColor .~ Last (Just $ ColorRef (toPixelRGBA8 $ o ^. #color))) &
  (fillOpacity ?~ realToFrac (opac $ o ^. #color)) &
  (textAnchor .~ Last (Just (toTextAnchor $ o ^. #anchor))) &
  maybe
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
  mempty &
  (strokeWidth .~ Last (Just $ Num (o ^. #borderSize))) &
  (strokeColor .~
   Last (Just $ ColorRef (toPixelRGBA8 $ o ^. #borderColor))) &
  (strokeOpacity ?~ realToFrac (opac $ o ^. #borderColor)) &
  (fillColor .~ Last (Just $ ColorRef (toPixelRGBA8 $ o ^. #color))) &
  (fillOpacity ?~ realToFrac (opac $ o ^. #color)) &
  maybe id (\(Point x y) -> transform ?~ [Translate x (-y)]) (o ^. #translate)

daLine :: LineStyle -> DrawAttributes
daLine o =
  mempty &
  (strokeWidth .~ Last (Just $ Num (o ^. #width))) &
  (strokeColor .~ Last (Just $ ColorRef (toPixelRGBA8 $ o ^. #color))) &
  (strokeOpacity ?~ realToFrac (opac $ o ^. #color)) &
  (fillColor .~ Last (Just FillNone)) &
  maybe id (\x -> strokeLineCap .~ Last (Just $ fromLineCap' x))
  (o ^. #linecap) &
  maybe id (\x -> strokeLineJoin .~ Last (Just $ fromLineJoin' x))
  (o ^. #linejoin) &
  maybe id (\x -> strokeOffset .~ Last (Just $ Num x))
  (o ^. #dashoffset) &
  maybe id (\xs -> strokeDashArray .~ Last (Just (Num <$> xs)))
  (o ^. #dasharray)

fromLineCap' :: LineCap -> Cap
fromLineCap' LineCapButt = CapButt
fromLineCap' LineCapRound = CapRound
fromLineCap' LineCapSquare = CapSquare

fromLineJoin' :: C.LineJoin -> SvgTree.LineJoin
fromLineJoin' LineJoinMiter = JoinMiter
fromLineJoin' LineJoinBevel = JoinBevel
fromLineJoin' LineJoinRound = JoinRound

daPath :: PathStyle -> DrawAttributes
daPath o =
  mempty &
  (strokeWidth .~ Last (Just $ Num (o ^. #borderSize))) &
  (strokeColor .~ Last
   (Just $ ColorRef (toPixelRGBA8 $ o ^. #borderColor))) &
  (strokeOpacity ?~ realToFrac (opac $ o ^. #borderColor)) &
  (fillColor .~ Last (Just $ ColorRef (toPixelRGBA8 $ o ^. #color))) &
  (fillOpacity ?~ realToFrac (opac $ o ^. #color))

-- * svg primitives

-- | Convert to reanimate color primitive.
toPixelRGBA8 :: Colour -> PixelRGBA8
toPixelRGBA8 (Colour r g b o) =
  PixelRGBA8
    (fromIntegral (floor $ r * 256 :: Int))
    (fromIntegral (floor $ g * 256 :: Int))
    (fromIntegral (floor $ b * 256 :: Int))
    (fromIntegral (floor $ o * 256 :: Int))

-- | convert a point to the svg co-ordinate system
-- The svg coordinate system has the y-axis going from top to bottom.
pointSvg :: Point Double -> (Number, Number)
pointSvg (Point x y) = (Num x, Num (- y))

-- | A DrawAttributes to rotate around a point by x degrees.
rotatePDA :: (HasDrawAttributes s) => Double -> Point Double -> s -> s
rotatePDA a (Point x y) s = s & transform %~ (Just . maybe r (<> r))
  where
    r = [Rotate (-a*180/pi) (Just (x, -y))]

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

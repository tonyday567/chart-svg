{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Chart.Svg
  ( treeRect,
    treeGlyph,
    treeLine,
    treeShape,
    treeText,
    tree,
    namedElements,
    groupTrees,
    strokeRect,
    transformRect,
    dagRect,
    dagText,
    dagGlyph,
    dagLine,
    dagPixel,
    pointSvg,
    rotateDA,
    rotatePDA,
    translateDA,
    rotateSvg,
    translateSvg,
    renderToXml,
    renderToXmlWith,
    xmlToText,
    writeWithXml,
    writeChartSvg,
    writeChartSvgUnsafe,
    renderChartSvg,
    renderChartSvgUnsafe,
  )
where

import Chart.Types
import Codec.Picture.Types
import Control.Lens hiding (transform)
import Data.Foldable
import Data.Generics.Labels ()
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.Exception
import Graphics.Svg as Svg hiding (Point, Text, toPoint)
import Graphics.Svg.CssTypes as Svg hiding (Point)
import Linear.V2
import NumHask.Space hiding (Element)
import Text.XML.Light.Output
import Prelude hiding (writeFile)
import Text.XML.Light.Types as XML
import Data.Bool

-- * Svg

-- * svg primitives

-- | convert a point to the svg co-ordinate system
-- The svg coordinate system has the y-axis going from top to bottom.
pointSvg :: (Real a) => Point a -> (Number, Number)
pointSvg (Point x y) = (Num (realToFrac x), Num (- (realToFrac y)))

-- | convert a Rect to the svg co-ordinate system
rectSvg :: (Real a, HasRectangle s) => Rect a -> s -> s
rectSvg r =
  (rectUpperLeftCorner .~ (Num x, Num (- w)))
    . (rectWidth .~ Num (z - x))
    . (rectHeight .~ Num (w - y))
  where
    (Rect x z y w) = realToFrac <$> r

-- | Rectange svg
treeRect :: (Real a) => Rect a -> Tree
treeRect a =
  RectangleTree $ rectSvg a defaultSvg

-- | Text svg
treeText :: (Chartable a) => TextStyle -> Text -> Point a -> Tree
treeText s t p =
  bool id (groupTrees (mempty & attrClass .~ ["hasmathjax"]) . (:[])) (s ^. #hasMathjax) $
  TextTree Nothing (textAt (pointSvg p) t)
    & maybe id (\x -> drawAttr %~ rotatePDA x p) (realToFrac <$> s ^. #rotation)

-- | GlyphShape to svg Tree
treeShape :: GlyphShape -> Double -> Point Double -> Tree
treeShape CircleGlyph s p =
  CircleTree $ Circle mempty (pointSvg p) (Num (realToFrac s / 2))
treeShape SquareGlyph s p = treeRect (move p ((s *) <$> unitRect))
treeShape (RectSharpGlyph x') s p =
  treeRect (move p (scale (Point s (x' * s)) unitRect))
treeShape (RectRoundedGlyph x'' rx ry) s p =
  RectangleTree
    . rectSvg (addPoint p $ scale (Point s (x'' * s)) unitRect)
    . (rectCornerRadius .~ (Num (realToFrac rx), Num (realToFrac ry)))
    $ defaultSvg
treeShape (TriangleGlyph (Point xa ya) (Point xb yb) (Point xc yc)) s p =
  PolygonTree
    . (polygonPoints .~ rps)
    $ (drawAttr %~ translateDA p) defaultSvg
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
      (Num $ realToFrac s / 2)
      (Num $ (realToFrac x' * realToFrac s) / 2)
treeShape (VLineGlyph x') s (Point x y) =
  LineTree $
    Line
      (mempty & strokeWidth .~ Last (Just (Num (realToFrac x'))))
      (pointSvg (Point x (y - s / 2)))
      (pointSvg (Point x (y + s / 2)))
treeShape (HLineGlyph x') s (Point x y) =
  LineTree $
    Line
      (mempty & strokeWidth .~ Last (Just (Num $ realToFrac x')))
      (pointSvg (Point (x - s / 2) y))
      (pointSvg (Point (x + s / 2) y))
treeShape SmileyGlyph s' p =
  groupTrees
    mempty
    [ CircleTree $
        defaultSvg
          & drawAttr . fillColor .~ (Last . Just . ColorRef $ PixelRGBA8 255 255 0 255)
          & circleCenter .~ (Num (0.5 * realToFrac s), Num (0.5 * realToFrac s))
          & circleRadius .~ Num (0.5 * realToFrac s),
      EllipseTree $
        defaultSvg
          & drawAttr . fillColor
            .~ (Last . Just . ColorRef $ PixelRGBA8 255 255 255 255)
          & ellipseCenter .~ (Num (0.35 * realToFrac s), Num (0.3 * realToFrac s))
          & ellipseXRadius .~ Num (0.05 * realToFrac s)
          & ellipseYRadius .~ Num (0.1 * realToFrac s),
      EllipseTree $
        defaultSvg
          & drawAttr . fillColor
            .~ (Last . Just . ColorRef $ PixelRGBA8 255 255 255 255)
          & ellipseCenter .~ (Num (0.65 * realToFrac s), Num (0.3 * realToFrac s))
          & ellipseXRadius .~ Num (0.05 * realToFrac s)
          & ellipseYRadius .~ Num (0.1 * realToFrac s),
      GroupTree $
        defaultSvg
          & groupChildren
          .~ [ PathTree $
                 defaultSvg
                   & pathDefinition
                     .~ [ MoveTo OriginAbsolute [V2 0 0],
                          EllipticalArc OriginAbsolute [(0.38 * s, 0.4 * s, 0.1 * s, False, False, V2 (0.65 * s) 0)]
                        ]
                   & drawAttr . fillColor .~ (Last $ Just FillNone)
                   & drawAttr . strokeColor
                     .~ ( Last . Just . ColorRef $
                            PixelRGBA8 0 0 0 255
                        )
                   & drawAttr . strokeWidth .~ Last (Just (Num (realToFrac s * 0.03)))
                   & drawAttr . transform ?~ [Translate (0.18 * s) (0.65 * s)]
             ]
    ]
    & drawAttr . transform ?~ [Translate (x - s / 2) (- y - s / 2)]
  where
    s = realToFrac s'
    (Point x y) = realToFrac <$> p
treeShape (PathGlyph pcs) _ _ =
  PathTree $ Path mempty pcs

-- | GlyphStyle to svg Tree
treeGlyph :: (Fractional a, Real a) => GlyphStyle -> Point a -> Tree
treeGlyph s p =
  treeShape (s ^. #shape) (s ^. #size) (realToFrac <$> p)
    & maybe id (\x -> drawAttr %~ rotatePDA x p) (realToFrac <$> s ^. #rotation)

-- | line svg
treeLine :: (Chartable a) => [Point a] -> Tree
treeLine xs =
  PolyLineTree
    . (polyLinePoints .~ ((\(Point x y) -> V2 x (- y)) . fmap realToFrac <$> xs))
    $ defaultSvg

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
tree (Chart (PixelA s) xs) =
  groupTrees (dagPixel s) (treeRect <$> (toRect <$> xs))
tree (Chart BlankA _) =
  groupTrees mempty []

-- | convert a Chart to named elements
namedElements :: Chart a -> Map.Map Text.Text Svg.Element
namedElements (Chart (PixelA s) _) =
  Map.fromList [lgPixel s]
namedElements _ = Map.empty

-- | calculate the linear gradient to shove in defs
-- FIXME: Only works for #pixelGradient = 0 or pi//2. Can do much better with something like https://stackoverflow.com/questions/9025678/how-to-get-a-rotated-linear-gradient-svg-for-use-as-a-background-image
lgPixel :: PixelStyle -> (Text, Svg.Element)
lgPixel o =
  (o ^. #pixelTextureId, ElementLinearGradient $ defaultSvg &
    linearGradientStart .~ (Num x0, Num y0) &
    linearGradientStop .~ (Num x1, Num y1) &
    linearGradientStops .~
    [defaultSvg & gradientColor .~ promotePixel (o ^. #pixelColorMin) & gradientOpacity .~ Just (realToFrac $ o ^. #pixelOpacityMin) & gradientOffset .~ 0,
    defaultSvg & gradientColor .~ promotePixel (o ^. #pixelColorMax) & gradientOpacity .~ Just (realToFrac $ o ^. #pixelOpacityMax)  & gradientOffset .~ 1
    ]
  )
  where
    x0 = min 0 (cos (o ^. #pixelGradient))
    x1 = max 0 (cos (o ^. #pixelGradient))
    y0 = max 0 (sin (o ^. #pixelGradient))
    y1 = min 0 (sin (o ^. #pixelGradient))

-- | add drawing attributes as a group svg wrapping a [Tree]
groupTrees :: DrawAttributes -> [Tree] -> Tree
groupTrees da' tree' =
  GroupTree (drawAttr %~ (<> da') $ groupChildren .~ tree' $ defaultSvg)

-- * DrawAttribute computations

-- | the extra Rect from the stroke element of an svg style attribute
strokeRect :: (Fractional a) => DrawAttributes -> Rect a -> Rect a
strokeRect das r = r `addRect` (realToFrac <$> Rect (- x / 2) (x / 2) (- x / 2) (x / 2))
  where
    x = case das ^. Svg.strokeWidth & getLast of
      Just (Num x') -> x'
      _ -> 0

transformRect :: (Chartable a) => Spot a -> DrawAttributes -> Rect a -> Rect a
transformRect sp da r =
  realToFrac
    <$> foldl'
      addtr
      (realToFrac <$> r)
      (fromMaybe [] (da ^. transform))
  where
    (Point x y) = realToFrac <$> toPoint sp
    addtr r' t = case t of
      Translate x' y' -> move (Point x' (- y')) r'
      TransformMatrix {} ->
        throw NotYetImplementedException
      Scale s Nothing -> (s *) <$> r'
      Scale sx (Just sy) -> scale (Point sx sy) r'
      Rotate d Nothing ->
        rotateRect
          d
          ( realToFrac
              <$> move
                (Point (- x) (- y))
                (realToFrac <$> r')
          )
      Rotate d (Just (x', y')) -> rotateRect d (move (Point (x' - x) (y' - y)) (realToFrac <$> r'))
      SkewX _ -> throw NotYetImplementedException
      SkewY _ -> throw NotYetImplementedException
      TransformUnknown -> r'

dagRect :: RectStyle -> DrawAttributes
dagRect o =
  mempty
    & (strokeWidth .~ Last (Just $ Num (realToFrac $ o ^. #borderSize)))
    . (strokeColor .~ Last (Just $ ColorRef (promotePixel $ o ^. #borderColor)))
    . (strokeOpacity ?~ realToFrac (o ^. #borderOpacity))
    . (fillColor .~ Last (Just $ ColorRef (promotePixel $ o ^. #color)))
    . (fillOpacity ?~ realToFrac (o ^. #opacity))

dagPixel :: PixelStyle -> DrawAttributes
dagPixel o =
  mempty
    & (strokeWidth .~ Last (Just $ Num (realToFrac $ o ^. #pixelRectStyle . #borderSize)))
    . (strokeColor .~ Last (Just $ ColorRef (promotePixel $ o ^. #pixelRectStyle . #borderColor)))
    . (strokeOpacity ?~ realToFrac (o ^. #pixelRectStyle . #borderOpacity))
    . (fillColor .~ Last (Just $ TextureRef (Text.unpack $ o ^. #pixelTextureId)))

-- | group draw attributes for TextStyle. rotation is defined by svg relative to a point (or to an origin) and so rotation needs to be dealth with separately.
dagText :: () => TextStyle -> DrawAttributes
dagText o =
  mempty
    & (fontSize .~ Last (Just $ Num (o ^. #size)))
    & (strokeWidth .~ Last (Just $ Num 0))
    & (strokeColor .~ Last (Just FillNone))
    & (fillColor .~ Last (Just $ ColorRef (promotePixel $ o ^. #color)))
    & (fillOpacity ?~ realToFrac (o ^. #opacity))
    & (textAnchor .~ Last (Just (toTextAnchor $ o ^. #anchor)))
    & maybe
      id
      (\(Point x y) -> transform ?~ [Translate (realToFrac x) (- realToFrac y)])
      (o ^. #translate)
  where
    toTextAnchor :: Anchor -> TextAnchor
    toTextAnchor AnchorMiddle = TextAnchorMiddle
    toTextAnchor AnchorStart = TextAnchorStart
    toTextAnchor AnchorEnd = TextAnchorEnd

dagGlyph :: GlyphStyle -> DrawAttributes
dagGlyph o =
  mempty
    & (strokeWidth .~ Last (Just $ Num (o ^. #borderSize)))
    & (strokeColor .~ Last (Just $ ColorRef (promotePixel $ o ^. #borderColor)))
    & (strokeOpacity ?~ realToFrac (o ^. #borderOpacity))
    & (fillColor .~ Last (Just $ ColorRef (promotePixel $ o ^. #color)))
    & (fillOpacity ?~ realToFrac (o ^. #opacity))
    & maybe
      id
      (\(Point x y) -> transform ?~ [Translate (realToFrac x) (- realToFrac y)])
      (o ^. #translate)

dagLine :: LineStyle -> DrawAttributes
dagLine o =
  mempty
    & (strokeWidth .~ Last (Just $ Num (o ^. #width)))
    . (strokeColor .~ Last (Just $ ColorRef (promotePixel $ o ^. #color)))
    . (strokeOpacity ?~ realToFrac (o ^. #opacity))
    . (fillColor .~ Last (Just FillNone))

-- * transformations

-- | A DrawAttributes to rotate by x degrees.
rotateDA :: (Real a, HasDrawAttributes s) => a -> s -> s
rotateDA a s = s & transform %~ (Just . maybe r (<> r))
  where
    r = [Rotate (realToFrac a) Nothing]

-- | A DrawAttributes to rotate around a point by x degrees.
rotatePDA :: (Real a, HasDrawAttributes s) => a -> Point a -> s -> s
rotatePDA a (Point x y) s = s & transform %~ (Just . maybe r (<> r))
  where
    r = [Rotate (realToFrac a) (Just (realToFrac x, - realToFrac y))]

-- | A DrawAttributes to translate by a Point.
translateDA :: (Real a, HasDrawAttributes s) => Point a -> s -> s
translateDA p =
  transform
    %~ (\x -> Just $ maybe [Translate x' (- y')] (<> [Translate x' (- y')]) x)
  where
    Point x' y' = realToFrac <$> p

-- | Rotate a ChartSvg expanding the Rect as necessary.
-- Multiple rotations will expand the bounding box conservatively.
rotateSvg :: (Chartable a) => a -> ChartSvg a -> ChartSvg a
rotateSvg r (ChartSvg vb c defs) =
  ChartSvg
    (rotateRect r vb)
    [groupTrees (rotateDA r mempty) c]
    defs

-- | Translate a ChartSvg also moving the Rect
translateSvg :: (Chartable a) => Point a -> ChartSvg a -> ChartSvg a
translateSvg p (ChartSvg vb c defs) =
  ChartSvg
    (move p vb)
    [groupTrees (translateDA p mempty) c]
    defs

-- * rendering

-- | render a ChartSvg to an xml Document with the supplied size and various bits and pieces
-- note that no attempt is made to resolve name collisions in definitions of named elements
renderToXmlWith :: (Real a) => Point a -> Map.Map Text.Text Svg.Element -> Text.Text -> [CssRule] -> FilePath -> ChartSvg a -> Document
renderToXmlWith (Point wid hei) defs desc css fp (ChartSvg vb ts defs') =
  Document
    ((\(Rect x z y w) -> Just (x, - w, z - x, w - y)) $ realToFrac <$> vb)
    (Just (Num (realToFrac wid)))
    (Just (Num (realToFrac hei)))
    ts
    (Map.mapKeys Text.unpack (defs `Map.union` defs'))
    (Text.unpack desc)
    css
    fp

cssCrisp :: CssRule
cssCrisp = CssRule [] [CssDeclaration "shape-rendering" [[CssString "crispEdges"]]]

-- | render a ChartSvg to an xml Document with the supplied size
renderToXml :: (Real a) => Point a -> Bool -> ChartSvg a -> Document
renderToXml p crisp = renderToXmlWith p Map.empty "" (bool [] [cssCrisp] crisp) ""

-- | render an xml document to Text
xmlToText :: Document -> Text
xmlToText = Text.pack . ppcElement defaultConfigPP . xmlOfDocument

-- | render an xml document to Text
xmlToTextUnsafe :: Document -> Text
xmlToTextUnsafe = Text.pack . ppcElement defaultConfigPP . unescapeTextElements . xmlOfDocument

unescapeTextElements :: XML.Element -> XML.Element
unescapeTextElements e = e { elContent = unescape <$> elContent e}

unescape :: Content -> Content
unescape (Elem e) = Elem (unescapeTextElements e)
unescape (Text t) = Text (t { cdVerbatim = CDataRaw})
unescape x = x

renderChartSvg :: Point Double -> Bool -> ChartSvg Double -> Text.Text
renderChartSvg p c =
  xmlToText . renderToXml p c

renderChartSvgUnsafe :: Point Double -> Bool -> ChartSvg Double -> Text.Text
renderChartSvgUnsafe p c =
  xmlToTextUnsafe . renderToXml p c

-- | write a ChartSvg to a svg file with various Document attributes.
writeWithXml :: (Real a) => FilePath -> Point a -> Map.Map Text.Text Svg.Element -> Text.Text -> [CssRule] -> ChartSvg a -> IO ()
writeWithXml fp p defs desc css c = Text.writeFile fp (xmlToText $ renderToXmlWith p defs desc css fp c)

writeChartSvg :: FilePath -> Point Double -> Bool -> ChartSvg Double -> IO ()
writeChartSvg fp p c cs = Text.writeFile fp (renderChartSvg p c cs)

writeChartSvgUnsafe :: FilePath -> Point Double -> Bool -> ChartSvg Double -> IO ()
writeChartSvgUnsafe fp p c cs = Text.writeFile fp (renderChartSvgUnsafe p c cs)


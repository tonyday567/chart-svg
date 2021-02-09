{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | Integration of reanimate and chart-svg
module Chart.Reanimate
  ( ReanimateConfig(..),
    defaultReanimateConfig,
    animChartSvg,
    ChartReanimate(..),
    chartReanimate,
    toTreeA,
    tree,
    treeFromFile,
  )
where

import Chart as C hiding (transform, Line, renderChartsWith)
import Codec.Picture.Types
import Control.Lens hiding (transform)
import qualified Data.Attoparsec.Text as A
import Linear.V2
import NumHask.Prelude hiding (fold)
import Reanimate as Re
import qualified Graphics.SvgTree.PathParser as Svg
import Graphics.SvgTree as Svg hiding (Text)

-- | global reanimate configuration.
--
-- >>> defaultReanimateConfig
-- ReanimateConfig {duration = 5.0, background = Just "black", globalFontFamily = Just ["Arial","Helvetica","sans-serif"], globalFontStyle = Just FontStyleNormal, globalAlignment = AlignxMinYMin}
data ReanimateConfig = ReanimateConfig
  { duration :: Double,
    background :: Maybe Text,
    globalFontFamily :: Maybe [Text],
    globalFontStyle :: Maybe Svg.FontStyle,
    globalAlignment :: Svg.Alignment
  } deriving (Eq, Show, Generic)

-- |
defaultReanimateConfig :: ReanimateConfig
defaultReanimateConfig = ReanimateConfig 5 (Just "black") (Just ["Arial", "Helvetica", "sans-serif"]) (Just FontStyleNormal) AlignxMinYMin

-- | Animate a ChartSvg animation.
animChartSvg :: ReanimateConfig -> (Double -> ChartSvg) -> Animation
animChartSvg cfg cs =
  mkAnimation (view #duration cfg) $ toTreeA cfg cs

globalAtts :: ReanimateConfig -> Svg.DrawAttributes
globalAtts cfg =
  mempty &
    maybe id (\x -> fontFamily .~ Last (Just (fmap unpack x)))
     (view #globalFontFamily cfg) .
    maybe id (\x -> fontStyle .~ Last (Just x))
     (view #globalFontStyle cfg)

-- | The output of the raw translation of ChartSvg to a reanimate svg tree.
data ChartReanimate =
  ChartReanimate
  { trees :: [Tree],
    box :: Rect Double,
    size :: C.Point Double
  } deriving (Eq, Show, Generic)

-- | Render a 'ChartSvg' to 'Tree's, the fitted chart viewbox, and the suggested SVG dimensions
--
chartReanimate :: ChartSvg -> ChartReanimate
chartReanimate cs = ChartReanimate ts rect' size'
  where
    (cl'', rect', size') = renderToCRS so cl'
    so = view #svgOptions cs
    cl' = renderToCharts cs
    ts = tree <$> cl''

-- | convert a ChartSvg animation to a Tree animation.
toTreeA :: ReanimateConfig -> (Double -> ChartSvg) -> Double -> Tree
toTreeA cfg cs x =
  reCss (cs x & view (#svgOptions . #cssOptions)) $
  mkGroup $ (mkBackground . unpack <$> maybeToList (view #background cfg)) <>
  [ (\cr ->
       let (Rect x z y w) =
             view #box cr in
         withViewBox' (x,y,z-x,w-y)
         (PreserveAspectRatio False (view #globalAlignment cfg) Nothing) $
         flipYAxis $
         groupTrees (globalAtts cfg) $ view #trees cr) $
    chartReanimate
    (cs x)
  ]

reCss :: CssOptions -> (Tree -> Tree)
reCss NoCssOptions = id
reCss UseCssCrisp = Svg.cssApply (Svg.cssRulesOfText "* { shape-rendering: crispEdges; }")
reCss UseGeometricPrecision = Svg.cssApply (Svg.cssRulesOfText "* { shape-rendering: geometricPrecision; }")

withViewBox' :: (Double, Double, Double, Double) -> Svg.PreserveAspectRatio -> Tree -> Tree
withViewBox' vbox par child = Re.translate (-screenWidth/2) (-screenHeight/2) $
  svgTree Document
  { _documentViewBox = Just vbox
  , _documentWidth = Just (Num screenWidth)
  , _documentHeight = Just (Num screenHeight)
  , _documentElements = [child]
  , _documentDescription = ""
  , _documentLocation = ""
  , _documentAspectRatio = par
  }

-- | Rectange svg
treeRect :: Rect Double -> Tree
treeRect a =
  RectangleTree $ rectSvg a defaultSvg

-- | Text svg
treeText :: TextStyle -> Text -> C.Point Double -> Tree
treeText s t p =
  TextTree Nothing (textAt (pointSvg p) t)
    & maybe id (\x -> drawAttributes %~ rotatePDA x p) (s ^. #rotation)

-- | GlyphShape to svg Tree
treeShape :: GlyphShape -> Double -> C.Point Double -> Tree
treeShape CircleGlyph s p =
  CircleTree $ Circle mempty (pointSvg p) (Num (s / 2))
treeShape SquareGlyph s p = treeRect (move p ((s *) <$> one))
treeShape (RectSharpGlyph x') s p =
  treeRect (move p (C.scale (C.Point s (x' * s)) one))
treeShape (RectRoundedGlyph x'' rx ry) s p =
  RectangleTree
    . rectSvg (addPoint p $ C.scale (C.Point s (x'' * s)) one)
    . (rectCornerRadius .~ (Just $ Num rx, Just $ Num ry))
    $ defaultSvg
treeShape (TriangleGlyph (C.Point xa ya) (C.Point xb yb) (C.Point xc yc)) s p =
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
treeShape (VLineGlyph x') s (C.Point x y) =
  LineTree $
    Line
      (mempty & strokeWidth .~ Last (Just (Num x')))
      (pointSvg (C.Point x (y - s / 2)))
      (pointSvg (C.Point x (y + s / 2)))
treeShape (HLineGlyph x') s (C.Point x y) =
  LineTree $
    Line
      (mempty & strokeWidth .~ Last (Just (Num x')))
      (pointSvg (C.Point (x - s / 2) y))
      (pointSvg (C.Point (x + s / 2) y))
treeShape (PathGlyph path) s p =
  Svg.PathTree
  (Svg.Path
   (Svg.defaultSvg &
    (Svg.drawAttributes %~ scaleDA (C.Point s s) . translateDA p))
    (either mempty id $ A.parseOnly Svg.pathParser path))

-- | GlyphStyle to svg Tree
treeGlyph :: GlyphStyle -> C.Point Double -> Tree
treeGlyph s p =
  treeShape (s ^. #shape) (s ^. #size) p
    & maybe id (\x -> drawAttributes %~ rotatePDA x p) (s ^. #rotation)

-- | line svg
treeLine :: [C.Point Double] -> Tree
treeLine xs =
  PolyLineTree
    . (polyLinePoints .~ ((\(C.Point x y) -> V2 x (- y)) <$> xs))
    $ defaultSvg

-- | GlyphStyle to svg Tree
treePath :: [PathInfo Double] -> [C.Point Double] -> Tree
treePath s p =
  PathTree $
  Path mempty (zipWith
               (curry toPathCommand)
               s
               (fmap (\(C.Point x y) -> C.Point x (-y)) p))

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
  (textAnchor .~ Last (Just (toTextAnchor $ o ^. #anchor)))
  where
    toTextAnchor :: Anchor -> Svg.TextAnchor
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
  maybe id (\(C.Point x y) -> transform ?~ [Translate x (-y)]) (o ^. #translate)

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

fromLineCap' :: LineCap -> Svg.Cap
fromLineCap' LineCapButt = CapButt
fromLineCap' LineCapRound = CapRound
fromLineCap' LineCapSquare = CapSquare

fromLineJoin' :: C.LineJoin -> Svg.LineJoin
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
pointSvg :: C.Point Double -> (Svg.Number, Svg.Number)
pointSvg (C.Point x y) = (Num x, Num (- y))

-- | A DrawAttributes to rotate around a point by x degrees.
rotatePDA :: (HasDrawAttributes s) => Double -> C.Point Double -> s -> s
rotatePDA a (C.Point x y) s = s & transform %~ (Just . maybe r (<> r))
  where
    r = [Rotate (-a*180/pi) (Just (x, -y))]

-- | A DrawAttributes to translate by a Point.
translateDA :: (HasDrawAttributes s) => C.Point Double -> s -> s
translateDA (C.Point x' y') =
  transform
    %~ (\x -> Just $ maybe [Translate x' (- y')] (<> [Translate x' (- y')]) x)

-- | A DrawAttributes to translate by a Point.
scaleDA :: (HasDrawAttributes s) => C.Point Double -> s -> s
scaleDA (C.Point x' y') =
  transform
    %~ (\x -> Just $ maybe [Scale x' (Just y')] (<> [Scale x' (Just y')]) x)

-- | convert a Rect to the svg co-ordinate system
rectSvg :: Rect Double -> Svg.Rectangle -> Svg.Rectangle
rectSvg (Rect x z y w) =
  (rectUpperLeftCorner .~ (Num x, Num (- w)))
    . (rectWidth .~ Just (Num (z - x)))
    . (rectHeight .~ Just (Num (w - y)))

-- | import a Tree from a file
treeFromFile :: FilePath -> IO Tree
treeFromFile fp = do
  t <- Svg.loadSvgFile fp
  pure $ maybe Svg.None Re.unbox t

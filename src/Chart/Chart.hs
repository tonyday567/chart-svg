{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Chart API
module Chart.Chart where

import Data.Colour
import Chart.Style
-- import Chart.Types (HudOptions(..), Hud, makeHud)
-- import qualified Chart.Render as R
import Data.Bifunctor
import Data.Generics.Labels ()
import Data.Path
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import NumHask.Prelude
import NumHask.Space as NH hiding (Element)
import Lucid
import Data.List.NonEmpty as NonEmpty
import GHC.OverloadedLabels
import Control.Lens
-- import Data.Tree
import Text.HTML.TagSoup (maybeTagText, parseTags)
import Lucid.Base
import NeatInterpolation
import qualified Data.Text.Lazy as Lazy
import Control.Monad.State.Lazy

data BlankStyle

data Chart a =
  RectChart RectStyle (NonEmpty (Rect a)) |
  TextChart TextStyle (NonEmpty (Text, Point a)) |
  LineChart LineStyle (NonEmpty (Point a)) |
  GlyphChart GlyphStyle (NonEmpty (Point a)) |
  PathChart PathStyle (NonEmpty (PathInfo a, Point a)) |
  BlankChart (NonEmpty (Rect a))

box_ :: Chart a -> Rect a
box_ (RectChart _ a) = foldRectUnsafe a
box_ (TextChart _ a) = space1 $ snd <$> a
box_ (LineChart _ a) = space1 a
box_ (GlyphChart _ a) = space1 a
box_ (PathChart _ a) = pathBoxes' a
box_ (BlankChart a) = foldRectUnsafe a

sbox_ :: Chart a -> Rect a
sbox_ (RectChart s a) = foldRectUnsafe $ padRect (0.5 * view #borderSize s) <$> a
sbox_ (TextChart s a) = foldRectUnsafe $ uncurry (styleBoxText_ s) <$> a
sbox_ (LineChart s a) = foldRectUnsafe $ padRect (0.5 * Chart.Style.width s) . singleton <$> a
sbox_ (GlyphChart s a) = foldRectUnsafe $ (\p -> addPoint p (styleBoxGlyph_ s)) <$> a
sbox_ (PathChart s a) = padRect (0.5 * view #borderSize s) (pathBoxes' a)
sbox_ (BlankChart a) = foldRectUnsafe a

move_ :: Point a -> Chart a -> Chart a
move_ p (RectChart s a) = RectChart s (addPoint p <$> a)
move_ p (TextChart s a) = TextChart s (second (p+) <$> a)
move_ p (LineChart s a) = LineChart s ((p+) <$> a)
move_ p (GlyphChart s a) = GlyphChart s ((p+) <$> a)
move_ p (PathChart s a) = PathChart s (second (p+) <$> a)
move_ p (BlankChart a) = BlankChart (addPoint p <$> a)

draw_ :: Chart a -> Html ()
draw_ (RectChart _ a) = sconcat $ svgRect_ <$> a
draw_ (TextChart s a) = sconcat $ uncurry (svgText_ s) <$> a
draw_ (LineChart _ a) = svgLine_ (NumHask.Prelude.toList a)
draw_ (GlyphChart s a) = sconcat $ svgGlyph_ s <$> a
draw_ (PathChart _ a) = svgPath_ (NonEmpty.toList $ fst <$> a) (NonEmpty.toList $ snd <$> a)
draw_ (BlankChart _) = mempty

atts_ :: Chart a -> [Attribute]
atts_ (RectChart s _) = attsRect s
atts_ (TextChart s _) = attsText s
atts_ (LineChart s _) = attsLine s
atts_ (GlyphChart s _) = attsGlyph s
atts_ (PathChart s _) = attsPath s
atts_ (BlankChart _) = mempty

-- | the extra area from text styling
styleBoxText_ ::
  TextStyle ->
  Text ->
  Point Double ->
  Rect Double
styleBoxText_ o t p = move p $ maybe flat (`rotationBound` flat) (o ^. #rotation)
  where
    flat = Rect ((-x' / 2.0) + x' * a') (x' / 2 + x' * a') ((-y' / 2) - n1') (y' / 2 - n1')
    s = o ^. #size
    h = o ^. #hsize
    v = o ^. #vsize
    n1 = o ^. #nudge1
    x' = s * h * fromIntegral (sum $ maybe 0 Text.length . maybeTagText <$> parseTags t)
    y' = s * v
    n1' = s * n1
    a' = case o ^. #anchor of
      AnchorStart -> 0.5
      AnchorEnd -> -0.5
      AnchorMiddle -> 0.0

-- | the extra area from glyph styling
styleBoxGlyph_ :: GlyphStyle -> Rect Double
styleBoxGlyph_ s = move p' $
  sw $ case sh of
    CircleGlyph -> (sz *) <$> one
    SquareGlyph -> (sz *) <$> one
    EllipseGlyph a -> NH.scale (Point sz (a * sz)) one
    RectSharpGlyph a -> NH.scale (Point sz (a * sz)) one
    RectRoundedGlyph a _ _ -> NH.scale (Point sz (a * sz)) one
    VLineGlyph _ -> NH.scale (Point ((s ^. #borderSize) * sz) sz) one
    HLineGlyph _ -> NH.scale (Point sz ((s ^. #borderSize) * sz)) one
    TriangleGlyph a b c -> (sz *) <$> sconcat (toRect . PointXY <$> (a :| [b, c]) :: NonEmpty (Rect Double))
    PathGlyph path' -> (sz *) <$> fromMaybe one (pathBoxes . toPathXYs . parsePath $ path')
  where
    sh = s ^. #shape
    sz = s ^. #size
    sw = padRect (0.5 * s ^. #borderSize)
    p' = fromMaybe (Point 0.0 0.0) (s ^. #translate)


-- ** ChartSvg

-- | Specification of a chart for rendering to SVG
data ChartSvg = ChartSvg
  { svgOptions :: SvgOptions,
    -- hudOptions :: HudOptions,
    hudList :: [Hud Double],
    chartTree :: [Chart Double]
  }
  deriving (Generic)
instance Semigroup ChartSvg where
  (<>) (ChartSvg _ h c) (ChartSvg s' h' c') =
    ChartSvg s' (h <> h') (c <> c')

instance Monoid ChartSvg where
  mempty = ChartSvg defaultSvgOptions [] []

-- * rendering

-- | @svg@ element + svg 2 attributes
svg2Tag :: Term [Attribute] (s -> t) => s -> t
svg2Tag m =
  svg_
    [ makeAttribute "xmlns" "http://www.w3.org/2000/svg",
      makeAttribute "xmlns:xlink" "http://www.w3.org/1999/xlink"
    ]
    m

renderToSvg :: CssOptions -> Point Double -> Rect Double -> [Chart a] -> Html ()
renderToSvg csso (Point w' h') (Rect x z y w) cs =
  with
    ( svg2Tag
        ( cssText csso
            <> mconcat (svg <$> cs)
        )
    )
    [ width_ (pack $ show w'),
      height_ (pack $ show h'),
      makeAttribute "viewBox" (pack $ show x <> " " <> show (-w) <> " " <> show (z - x) <> " " <> show (w - y))
    ]

-- | Low-level conversion of a Chart to svg
svg :: Chart a -> Lucid.Html ()
svg c = term "g" (atts_ c) (draw_ c)

cssText :: CssOptions -> Html ()
cssText csso = style_ [] $
  cssShapeRendering (csso ^. #shapeRendering) <>
  cssPreferColorScheme (light, dark) (csso ^. #preferColorScheme)

cssShapeRendering :: CssShapeRendering -> Text
cssShapeRendering UseGeometricPrecision = "svg { shape-rendering: geometricPrecision; }"
cssShapeRendering UseCssCrisp = "svg { shape-rendering: crispEdges; }"
cssShapeRendering NoShapeRendering = mempty

cssPreferColorScheme :: (Colour, Colour) -> CssPreferColorScheme -> Text
cssPreferColorScheme (bglight, _) PreferLight =
  [trimming|
    svg {
      color-scheme: light dark;
    }
    @media (prefers-color-scheme:dark) {
      svg {
        background-color: $c;
      }
    }
  |]
    where c = hex bglight
cssPreferColorScheme (_, bgdark) PreferDark =
  [trimming|
    svg {
      color-scheme: light dark;
    }
    @media (prefers-color-scheme:light) {
      svg {
        background-color: $c;
      }
    }
  |] where c = hex bgdark
cssPreferColorScheme _ PreferNormal = mempty

{-
makeCharts :: ChartAspect -> HudOptions -> [Chart a] -> [Chart a]
makeCharts asp ho cs =
  let (hs', hc') = makeHud (padBox $ dataBoxes cs) ho
   in runHud (initialCanvas asp (cs <> hc')) hs' (cs <> hc')
-}

boxes :: (Foldable f, Functor f) => f (Chart a) -> Rect Double
boxes cs = foldRectUnsafe $ box_ <$> cs

sboxes :: (Foldable f, Functor f) => f (Chart a) -> Rect Double
sboxes cs = foldRectUnsafe $ sbox_ <$> cs

renderToCRS :: SvgOptions -> [Chart Double] -> ([Chart Double], Rect Double, Point Double)
renderToCRS so cs = (cs', rect', size')
  where
    rect' = sboxes cs' & maybe id padRect (so ^. #outerPad)
    cs' =
      cs
        & runHud penult [chartAspectHud (so ^. #chartAspect)]
        & maybe
          id
          (\x -> frameChart x (fromMaybe 0 (so ^. #innerPad)))
          (so ^. #chartFrame)
    Point w h = NH.width rect'
    size' = Point ((so ^. #svgHeight) / h * w) (so ^. #svgHeight)
    penult = case so ^. #chartAspect of
      FixedAspect _ -> sboxes cs
      CanvasAspect _ -> boxes cs
      ChartAspect -> sboxes cs
      UnadjustedAspect -> boxes cs

{-
-- | Consume the ChartSvg and produce the combined huds and charts as a chart list.
renderToCharts :: ChartSvg -> [Chart a]
renderToCharts cs = makeCharts (view (#svgOptions . #chartAspect) cs) (view #hudOptions cs) (view #chartList cs)
-}

-- | render Charts with the supplied options.
renderChartsWith :: SvgOptions -> [Chart Double] -> Text
renderChartsWith so cs =
  Lazy.toStrict $ renderText (renderToSvg (so ^. #cssOptions) size' rect' cs'')
  where
    rect' = sboxes cs' & maybe id padRect (so ^. #outerPad)
    cs' =
      cs
        & runHud penult [chartAspectHud (so ^. #chartAspect)]
        & maybe
          id
          (\x -> frameChart x (fromMaybe 0 (so ^. #innerPad)))
          (so ^. #chartFrame)
    cs'' =
      foldMap (\c -> [RectChart (blob c) (rect' :| [])]) (so ^. #background) <> cs'
    Point w h = NH.width rect'
    size' = Point ((so ^. #svgHeight) / h * w) (so ^. #svgHeight)
    penult = case so ^. #chartAspect of
      FixedAspect _ -> sboxes cs
      CanvasAspect _ -> boxes cs
      ChartAspect -> sboxes cs
      UnadjustedAspect -> boxes cs

-- | render charts with the supplied svg options and huds
renderHudChart :: SvgOptions -> [Hud Double] -> [Chart Double] -> Text
renderHudChart so hs cs = renderChartsWith so (runHud (initialCanvas (so ^. #chartAspect) cs) hs cs)

-- | calculation of the canvas given the 'ChartAspect'
initialCanvas :: ChartAspect -> [Chart Double] -> Rect Double
initialCanvas (FixedAspect a) _ = aspect a
initialCanvas (CanvasAspect a) _ = aspect a
initialCanvas ChartAspect cs = aspect $ ratio $ sboxes cs
initialCanvas UnadjustedAspect cs = boxes cs

-- | Render a chart using the supplied svg and hud config.
--
-- >>> chartSvg mempty
-- "<svg height=\"300.0\" width=\"300.0\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"-0.52 -0.52 1.04 1.04\"></svg>"
chartSvg :: ChartSvg -> Text
chartSvg (ChartSvg so hs cs) = renderHudChart so hs cs
  where
    -- (hs', cs') = makeHud (padBox $ foldRectUnsafe $ box_ <$> cs) ho

-- | Render a chart using the default svg options and no hud.
--
-- >>> chartSvgDefault [] == chartSvg mempty
-- True
chartSvgDefault :: [Chart a] -> Text
chartSvgDefault cs = chartSvg $ mempty & #chartTree .~ cs

{-

-- | Render a chart using default svg and hud options.
--
-- >>> chartSvgHud [] == (chartSvg $ mempty & #hudOptions .~ defaultHudOptions)
-- True
chartSvgHud :: [Chart a] -> Text
chartSvgHud cs =
  chartSvg $
    mempty
      & #hudOptions .~ defaultHudOptions
      & #chartTree .~ cs


-}

-- | Write to a file.
writeChartSvg :: FilePath -> ChartSvg -> IO ()
writeChartSvg fp cs =
  writeFile fp (unpack $ chartSvg cs)

-- | Write a chart to a file with default svg options and no hud.
writeChartSvgDefault :: FilePath -> [Chart a] -> IO ()
writeChartSvgDefault fp cs = writeChartSvg fp (mempty & #chartTree .~ cs)

{-
-- | Write a chart to a file with default svg and hud options.
writeChartSvgHud :: FilePath -> [Chart a] -> IO ()
writeChartSvgHud fp cs =
  writeChartSvg
    fp
    ( mempty
        & #chartTree .~ cs
        & #hudOptions .~ defaultHudOptions
    )


-}

-- * Rendering

-- | Make Lucid Html given term and attributes
terms :: Text -> [Lucid.Attribute] -> Lucid.Html ()
terms t = with $ makeXmlElementNoEnd t

-- | Rectangle svg
svgRect_ :: Rect Double -> Lucid.Html ()
svgRect_ (Rect x z y w) =
  terms
    "rect"
    [ width_ (pack $ show $ z - x),
      height_ (pack $ show $ w - y),
      term "x" (pack $ show x),
      term "y" (pack $ show $ -w)
    ]

-- | Text svg
svgText_ :: TextStyle -> Text -> Point Double -> Lucid.Html ()
svgText_ s t p@(Point x y) =
  term
    "text"
    ( [ term "x" (pack $ show x),
        term "y" (pack $ show $ -y)
      ]
        <> foldMap (\x' -> [term "transform" (toRotateText x' p)]) (s ^. #rotation)
    )
    (toHtmlRaw t)

-- | line svg
svgLine_ :: [Point Double] -> Lucid.Html ()
svgLine_ [] = mempty
svgLine_ xs = terms "polyline" [term "points" (toPointsText xs)]
  where
    toPointsText xs' = Text.intercalate "\n" $ (\(Point x y) -> pack (show x <> "," <> show (-y))) <$> xs'

-- | GlyphShape to svg Tree
svgShape_ :: GlyphShape -> Double -> Point Double -> Lucid.Html ()
svgShape_ CircleGlyph s (Point x y) =
  terms
    "circle"
    [ term "cx" (pack $ show x),
      term "cy" (pack $ show $ -y),
      term "r" (pack $ show $ 0.5 * s)
    ]
svgShape_ SquareGlyph s p =
  svgRect_ (move p ((s *) <$> one))
svgShape_ (RectSharpGlyph x') s p =
  svgRect_ (move p (NH.scale (Point s (x' * s)) one))
svgShape_ (RectRoundedGlyph x' rx ry) s p =
  terms
    "rect"
    [ term "width" (pack $ show $ z - x),
      term "height" (pack $ show $ w - y),
      term "x" (pack $ show x),
      term "y" (pack $ show $ -w),
      term "rx" (pack $ show rx),
      term "ry" (pack $ show ry)
    ]
  where
    (Rect x z y w) = move p (NH.scale (Point s (x' * s)) one)
svgShape_ (TriangleGlyph (Point xa ya) (Point xb yb) (Point xc yc)) s p =
  terms
    "polygon"
    [ term "transform" (toTranslateText p),
      term "points" (pack $ show (s * xa) <> "," <> show (-(s * ya)) <> " " <> show (s * xb) <> "," <> show (-(s * yb)) <> " " <> show (s * xc) <> "," <> show (-(s * yc)))
    ]
svgShape_ (EllipseGlyph x') s (Point x y) =
  terms
    "ellipse"
    [ term "cx" ((pack . show) x),
      term "cy" ((pack . show) $ -y),
      term "rx" ((pack . show) $ 0.5 * s),
      term "ry" ((pack . show) $ 0.5 * s * x')
    ]
svgShape_ (VLineGlyph _) s (Point x y) =
  terms "polyline" [term "points" (pack $ show x <> "," <> show (-(y - s / 2)) <> "\n" <> show x <> "," <> show (-(y + s / 2)))]
svgShape_ (HLineGlyph _) s (Point x y) =
  terms "polyline" [term "points" (pack $ show (x - s / 2) <> "," <> show (-y) <> "\n" <> show (x + s / 2) <> "," <> show (-y))]
svgShape_ (PathGlyph path) s p =
  terms "path" [term "d" path, term "transform" (toTranslateText p <> " " <> toScaleText s)]

-- | GlyphStyle to svg Tree
svgGlyph_ :: GlyphStyle -> Point Double -> Lucid.Html ()
svgGlyph_ s p =
  svgShape_ (s ^. #shape) (s ^. #size) p
    & maybe id (\r -> term "g" [term "transform" (toRotateText r p)]) (s ^. #rotation)

-- | Path svg
svgPath_ :: [PathInfo Double] -> [Point Double] -> Lucid.Html ()
svgPath_ _ [] = mempty
svgPath_ _ [_] = mempty
svgPath_ infos ps =
  terms "path" [term "d" (toPathAbsolutes (NumHask.Prelude.zip infos ps))]

-- | RectStyle to Attributes
attsRect :: RectStyle -> [Lucid.Attribute]
attsRect o =
  [ term "stroke-width" (pack $ show $ o ^. #borderSize),
    term "stroke" (hex $ o ^. #borderColor),
    term "stroke-opacity" (pack $ show $ opac $ o ^. #borderColor),
    term "fill" (hex $ o ^. #color),
    term "fill-opacity" (pack $ show $ opac $ o ^. #color)
  ]

-- | TextStyle to Attributes
attsText :: TextStyle -> [Lucid.Attribute]
attsText o =
  [ term "stroke-width" "0.0",
    term "stroke" "none",
    term "fill" (toHex $ o ^. #color),
    term "fill-opacity" (pack $ show $ opac $ o ^. #color),
    term "font-size" (pack $ show $ o ^. #size),
    term "text-anchor" (toTextAnchor $ o ^. #anchor)
  ]
  where
    toTextAnchor :: Anchor -> Text
    toTextAnchor AnchorMiddle = "middle"
    toTextAnchor AnchorStart = "start"
    toTextAnchor AnchorEnd = "end"

-- | GlyphStyle to Attributes
attsGlyph :: GlyphStyle -> [Lucid.Attribute]
attsGlyph o =
  [ term "stroke-width" (pack $ show $ o ^. #borderSize),
    term "stroke" (toHex $ o ^. #borderColor),
    term "stroke-opacity" (pack $ show $ opac $ o ^. #borderColor),
    term "fill" (toHex $ o ^. #color),
    term "fill-opacity" (pack $ show $ opac $ o ^. #color)
  ]
    <> foldMap ((: []) . term "transform" . toTranslateText) (o ^. #translate)

-- | LineStyle to Attributes
attsLine :: LineStyle -> [Lucid.Attribute]
attsLine o =
  [ term "stroke-width" (pack $ show $ o ^. #width),
    term "stroke" (toHex $ o ^. #color),
    term "stroke-opacity" (pack $ show $ opac $ o ^. #color),
    term "fill" "none"
  ]
    <> foldMap (\x -> [term "stroke-linecap" (fromLineCap x)]) (o ^. #linecap)
    <> foldMap (\x -> [term "stroke-linejoin" (fromLineJoin x)]) (o ^. #linejoin)
    <> foldMap (\x -> [term "stroke-dasharray" (fromDashArray x)]) (o ^. #dasharray)
    <> foldMap (\x -> [term "stroke-dashoffset" (pack $ show x)]) (o ^. #dashoffset)

-- | PathStyle to Attributes
attsPath :: PathStyle -> [Lucid.Attribute]
attsPath o =
  [ term "stroke-width" (pack $ show $ o ^. #borderSize),
    term "stroke" (hex $ o ^. #borderColor),
    term "stroke-opacity" (pack $ show $ opac $ o ^. #borderColor),
    term "fill" (hex $ o ^. #color),
    term "fill-opacity" (pack $ show $ opac $ o ^. #color)
  ]

-- | includes a flip of the y dimension.
toTranslateText :: Point Double -> Text
toTranslateText (Point x y) =
  pack $
    "translate(" <> show x <> ", " <> show (-y) <> ")"

-- | includes reference changes:
--
-- - from radians to degrees
--
-- - from counter-clockwise is a positive rotation to clockwise is positive
--
-- - flip y dimension
toRotateText :: Double -> Point Double -> Text
toRotateText r (Point x y) =
  pack $
    "rotate(" <> show (-r * 180 / pi) <> ", " <> show x <> ", " <> show (-y) <> ")"

toScaleText :: Double -> Text
toScaleText x =
  pack $
    "scale(" <> show x <> ")"

-- | additive padding
padRect :: (Subtractive a) => a -> Rect a -> Rect a
padRect p (Rect x z y w) = Rect (x - p) (z + p) (y - p) (w + p)

-- | pad a Rect to remove singleton dimensions
padBox :: Rect Double -> Rect Double
padBox (Rect x z y w)
      | x == z && y == w = Rect (x - 0.5) (x + 0.5) (y - 0.5) (y + 0.5)
      | x == z = Rect (x - 0.5) (x + 0.5) y w
      | y == w = Rect x z (y - 0.5) (y + 0.5)
      | otherwise = Rect x z y w

-- | overlay a frame on some charts with some additive padding between
--
-- >>> frameChart defaultRectStyle 0.1 [Chart BlankA []]
-- [Chart {annotation = RectA (RectStyle {borderSize = 1.0e-2, borderColor = Colour 0.65 0.81 0.89 1.00, color = Colour 0.12 0.47 0.71 1.00}), xys = []},Chart {annotation = BlankA, xys = []}]
frameChart :: RectStyle -> Double -> [Chart a] -> [Chart a]
frameChart rs p cs = RectChart rs (padRect p (sboxes cs):|[]):cs


-- * Examples

-- | unit example
--
-- ![unit example](other/unit.svg)
unitExample :: ChartSvg
unitExample = mempty & #chartTree .~ [RectChart defaultRectStyle (one:|[])]

-- | rect example
--
-- ![rect example](other/rect.svg)
rectExample :: ChartSvg
rectExample =
  mempty
{-
    & #hudOptions .~ (defaultHudOptions &
                      #hudAxes .~ [defaultAxisOptions & #axisTick . #ltick .~ Nothing] &
                      #hudCanvas .~ Nothing)
-}
  & #chartTree .~ NumHask.Prelude.zipWith RectChart ropts rss

rss :: [NonEmpty (Rect Double)]
rss =
  [ NonEmpty.fromList $ gridR (\x -> exp (-(x ** 2) / 2)) (Range -5 5) 50,
    NonEmpty.fromList $ gridR (\x -> 0.5 * exp (-(x ** 2) / 8)) (Range -5 5) 50
  ]

ropts :: [RectStyle]
ropts =
  [ blob (setOpac 0.3 (palette1 3)),
    blob (setOpac 0.3 (palette1 5))
  ]


-- * Hud
-- | Dimensions that are tracked in the 'HudT':
--
-- - chartDim: the rectangular dimension of the physical representation of a chart on the screen so that new hud elements can be appended. Adding a hud piece tends to expand the chart dimension.
--
-- - canvasDim: the rectangular dimension of the canvas on which data will be represented. At times appending a hud element will cause the canvas dimension to shift.
--
-- - dataDim: the rectangular dimension of the data being represented. Adding hud elements can cause this to change.
data ChartDims a = ChartDims
  { chartDim :: Rect a,
    canvasDim :: Rect a,
    dataDim :: Rect a
  }
  deriving (Eq, Show, Generic)

-- | Hud monad transformer
newtype HudT m a = Hud {unhud :: [Chart a] -> StateT (ChartDims a) m [Chart a]}

-- | Heads-Up-Display for a 'Chart'
type Hud = HudT Identity

instance (Monad m) => Semigroup (HudT m a) where
  (<>) (Hud h1) (Hud h2) = Hud $ h1 >=> h2

instance (Monad m) => Monoid (HudT m a) where
  mempty = Hud pure

-- | run two hud's simultaneously (using the same original ChartDims state) rather than sequentially (which is the <> operation).
simulHud :: (Ord a, Monad m) => HudT m a -> HudT m a -> HudT m a
simulHud (Hud fa) (Hud fb) = Hud $ \cs -> do
  s <- get
  (cs', ChartDims ch ca d) <- lift $ runStateT (fa cs) s
  (cs'', ChartDims ch' ca' d') <- lift $ runStateT (fb cs') s
  put (ChartDims (ch <> ch') (ca <> ca') (d <> d'))
  pure cs''

-- | Project the chart data given the ChartAspect
chartAspectHud :: (Monad m) => ChartAspect -> HudT m Double
chartAspectHud fa = Hud $ \cs -> do
  canvasd <- use #canvasDim
  chartd <- use #chartDim
  case fa of
    FixedAspect a -> pure $ project_ (aspect a) cs
    CanvasAspect a ->
      pure $
        project_ (aspect (a * ratio canvasd / ratio chartd)) cs
    ChartAspect -> pure $ project_ (aspect $ ratio chartd) cs
    UnadjustedAspect -> pure cs

-- | Combine huds and charts to form a new Chart using the supplied initial canvas and data dimensions. Note that chart data is transformed by this computation (and the use of a linear type is an open question).
runHudWith ::
  -- | initial canvas dimension
  Rect Double ->
  -- | initial data dimension
  Rect Double ->
  -- | huds to add
  [Hud Double] ->
  -- | underlying chart
  [Chart Double] ->
  -- | integrated chart list
  [Chart Double]
runHudWith ca xs hs cs =
  evalState
    ((unhud $ mconcat hs) cs')
    (ChartDims ca' da' xs)
  where
    da' = fromMaybe one $ dataBoxes cs'
    ca' = fromMaybe one $ styleBoxes cs'
    cs' = projectXYsWith ca xs cs

-- | Combine huds and charts to form a new [Chart] using the supplied canvas and the actual data dimension.
--
-- Note that the original chart data are transformed and irrevocably lost by this computation.
runHud ::
  -- | initial canvas dimension
  Rect Double ->
  -- | huds
  [Hud Double] ->
  -- | underlying charts
  [Chart Double] ->
  -- | integrated chart list
  [Chart Double]
runHud ca hs cs = runHudWith ca (padBox $ dataBoxes cs) hs cs

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

-- | Rendering charts to SVG.
--
-- Note that type signatures are tightened to Double as sane SVG rendering suggests.
module Chart.Render
  ( ChartSvg (..),
    chartSvg,
    chartSvgDefault,
    chartSvgHud,
    writeChartSvg,
    writeChartSvgDefault,
    writeChartSvgHud,
    svg2Tag,
    cssCrisp,
    geometricPrecision,
    svg,
    svgt,
    getSize,
    getViewbox,
    scaleCharts,
    attsRect,
    terms,
    makeAttribute,
  )
where

import Chart.Types
import Data.Colour
import Data.Path
import Control.Lens hiding (transform)
import Data.Generics.Labels ()
import qualified Data.Text.Lazy as Lazy
import Lucid
import qualified Lucid.Base as Lucid
import Lucid.Base
import NumHask.Prelude
import NumHask.Space as NH hiding (Element)
import qualified Data.Text as Text

-- | Specification of a chart for rendering to SVG
data ChartSvg
  = ChartSvg
      { svgOptions :: SvgOptions,
        hudOptions :: HudOptions,
        hudList :: [Hud Double],
        chartList :: [Chart Double]
      }
  deriving (Generic)

instance Semigroup ChartSvg where
  (<>) (ChartSvg _ o h c) (ChartSvg s' o' h' c') =
    ChartSvg s' (o <> o') (h <> h') (c <> c')

instance Monoid ChartSvg where
  mempty = ChartSvg defaultSvgOptions mempty [] []

-- | scale chart data, projecting to the supplied Rect, and expanding the resultant Rect for chart style if necessary.
--
-- Note that this modifies the underlying chart data.
-- FIXME: do a divide to make an exact fit
scaleCharts ::
  Rect Double ->
  [Chart Double] ->
  (Rect Double, [Chart Double])
scaleCharts cs r = (fromMaybe one $ styleBoxes cs', cs')
  where
    cs' = projectXYs cs r

-- | get SVG size from options and chart details.
getSize :: SvgOptions -> [Chart Double] -> Point Double
getSize o cs = case view #svgAspect o of
  ManualAspect a -> (view #svgHeight o *) <$> Point a 1
  ChartAspect -> (\(Rect x z y w) -> Point (view #svgHeight o * (z - x)) (view #svgHeight o * (w - y))) . fromMaybe one $ styleBoxes cs
  DataAspect -> (\(Rect x z y w) -> Point (view #svgHeight o * (z - x)) (view #svgHeight o * (w - y))) . fromMaybe one $ dataBoxes cs

-- | Get SVG viewbox as a Rect from options and chart details.
getViewbox :: SvgOptions -> [Chart Double] -> Rect Double
getViewbox o cs =
  bool asp (fromMaybe one $ styleBoxes cs) (NoScaleCharts == view #scaleCharts' o)
  where
    asp =
      case view #svgAspect o of
        ManualAspect a -> Rect (a * -0.5) (a * 0.5) -0.5 0.5
        ChartAspect -> fromMaybe one $ styleBoxes cs
        DataAspect -> fromMaybe one $ dataBoxes cs

-- * rendering

-- | @svg@ element + svg 2 attributes
svg2Tag :: Term [Attribute] (s -> t) => s -> t
svg2Tag m =
  svg_
    [ Lucid.makeAttribute "xmlns" "http://www.w3.org/2000/svg",
      Lucid.makeAttribute "xmlns:xlink" "http://www.w3.org/1999/xlink"
    ]
    m

renderToSvg :: CssOptions -> Point Double -> Rect Double -> [Chart Double] -> Html ()
renderToSvg csso (Point w' h') (Rect x z y w) cs =
  with
    ( svg2Tag
        ( cssText csso <>
            mconcat (svg <$> cs)
        )
    )
    [ width_ (show w'),
      height_ (show h'),
      makeAttribute "viewBox" (show x <> " " <> show (- w) <> " " <> show (z - x) <> " " <> show (w - y))
    ]

cssText :: CssOptions -> Html ()
cssText UseCssCrisp = cssCrisp
cssText UseGeometricPrecision = geometricPrecision
cssText NoCssOptions = mempty

-- | crisp edges css
cssCrisp :: Html ()
cssCrisp = style_ [type_ "text/css"] ("* { shape-rendering: crispEdges; }" :: Text)

-- | crisp edges css
geometricPrecision :: Html ()
geometricPrecision = style_ [type_ "text/css"] ("* { shape-rendering: geometricPrecision; }" :: Text)

-- | render Charts with the supplied options.
renderChartsWith :: SvgOptions -> [Chart Double] -> Text
renderChartsWith so cs =
  Lazy.toStrict $ renderText (renderToSvg (so ^. #useCssCrisp) (getSize so cs'') r' cs'')
  where
    r' = r & maybe id padRect (so ^. #outerPad)
    cs'' =
      cs'
        & maybe id (\x -> frameChart x (fromMaybe 0 (so ^. #innerPad))) (so ^. #chartFrame)
    (r, cs') =
      bool
        (getViewbox so cs, cs)
        (scaleCharts (getViewbox so cs) cs)
        (ScaleCharts == so ^. #scaleCharts')

renderHudChart :: SvgOptions -> [Hud Double] -> [Chart Double] -> Text
renderHudChart so hs cs = renderChartsWith so (runHud (getViewbox so cs) hs cs)

-- | Render a chart using the supplied svg and hud config.
-- FIXME: fixRect usage
--
-- >>> chartSvg mempty
-- "<svg xmlns=\"http://www.w3.org/2000/svg\" height=\"300.0\" viewBox=\"-0.52 -0.52 1.04 1.04\" width=\"450.0\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"></svg>"
chartSvg :: ChartSvg -> Text
chartSvg (ChartSvg so ho hs cs) = renderHudChart so (hs <> hs') (cs <> cs')
  where
    (hs', cs') = makeHud (fixRect $ dataBoxes cs) ho

-- | Render a chart using the default svg options and no hud.
--
-- >>> chartSvgDefault [] == chartSvg mempty
-- True
chartSvgDefault :: [Chart Double] -> Text
chartSvgDefault cs = chartSvg $ mempty & #chartList .~ cs

-- | Render a chart using default svg and hud options.
--
-- >>> chartSvgHud [] == (chartSvg $ mempty & #hudOptions .~ defaultHudOptions)
-- True
chartSvgHud :: [Chart Double] -> Text
chartSvgHud cs =
  chartSvg $
    mempty
      & #hudOptions .~ defaultHudOptions
      & #chartList .~ cs

-- | Write to a file.
writeChartSvg :: FilePath -> ChartSvg -> IO ()
writeChartSvg fp cs =
  writeFile fp (chartSvg cs)

-- | Write a chart to a file with default svg options and no hud.
writeChartSvgDefault :: FilePath -> [Chart Double] -> IO ()
writeChartSvgDefault fp cs = writeChartSvg fp (mempty & #chartList .~ cs)

-- | Write a chart to a file with default svg and hud options.
writeChartSvgHud :: FilePath -> [Chart Double] -> IO ()
writeChartSvgHud fp cs =
  writeChartSvg
    fp
    ( mempty
        & #chartList .~ cs
        & #hudOptions .~ defaultHudOptions
    )

-- | Rectangle svg
svgRect :: Rect Double -> Lucid.Html ()
svgRect (Rect x z y w) =
  terms
    "rect"
    [ width_ (show $ z - x),
      height_ (show $ w - y),
      term "x" (show x),
      term "y" (show $ - w)
    ]

-- | Text svg
svgText :: TextStyle -> Text -> Point Double -> Lucid.Html ()
svgText s t p@(Point x y) =
  term
    "text"
    ( [ term "x" (show x),
        term "y" (show $ - y)
      ]
        <> maybe [] (\x' -> [term "transform" (toRotateText x' p)]) (s ^. #rotation)
    )
    (toHtmlRaw t)

-- | line svg
svgLine :: [Point Double] -> Lucid.Html ()
svgLine [] = mempty
svgLine xs = terms "polyline" [term "points" (toPointsText xs)]
  where
    toPointsText xs' = Text.intercalate "\n" $ (\(Point x y) -> show x <> "," <> show (- y)) <$> xs'

-- | GlyphShape to svg Tree
svgShape :: GlyphShape -> Double -> Point Double -> Lucid.Html ()
svgShape CircleGlyph s (Point x y) =
  terms
    "circle"
    [ term "cx" (show x),
      term "cy" (show $ - y),
      term "r" (show $ 0.5 * s)
    ]
svgShape SquareGlyph s p =
  svgRect (move p ((s *) <$> one))
svgShape (RectSharpGlyph x') s p =
  svgRect (move p (NH.scale (Point s (x' * s)) one))
svgShape (RectRoundedGlyph x' rx ry) s p =
  terms
    "rect"
    [ term "width" (show $ z - x),
      term "height" (show $ w - y),
      term "x" (show x),
      term "y" (show $ - w),
      term "rx" (show rx),
      term "ry" (show ry)
    ]
  where
    (Rect x z y w) = move p (NH.scale (Point s (x' * s)) one)
svgShape (TriangleGlyph (Point xa ya) (Point xb yb) (Point xc yc)) s p =
  terms
    "polygon"
    [ term "transform" (toTranslateText p),
      term "points" (show (s * xa) <> "," <> show (- (s * ya)) <> " " <> show (s * xb) <> "," <> show (- (s * yb)) <> " " <> show (s * xc) <> "," <> show (- (s * yc)))
    ]
svgShape (EllipseGlyph x') s (Point x y) =
  terms
    "ellipse"
    [ term "cx" (show x),
      term "cy" (show $ - y),
      term "rx" (show $ 0.5 * s),
      term "ry" (show $ 0.5 * s * x')
    ]
svgShape (VLineGlyph _) s (Point x y) =
  terms "polyline" [term "points" (show x <> "," <> show (- (y - s / 2)) <> "\n" <> show x <> "," <> show (- (y + s / 2)))]
svgShape (HLineGlyph _) s (Point x y) =
  terms "polyline" [term "points" (show (x - s / 2) <> "," <> show (- y) <> "\n" <> show (x + s / 2) <> "," <> show (- y))]
svgShape (PathGlyph path) s p =
  terms "path" [term "d" path, term "transform" (toTranslateText p <> " " <> toScaleText s)]

-- | GlyphStyle to svg Tree
svgGlyph :: GlyphStyle -> Point Double -> Lucid.Html ()
svgGlyph s p =
  svgShape (s ^. #shape) (s ^. #size) p
    & maybe id (\r -> term "g" [term "transform" (toRotateText r p)]) (s ^. #rotation)

-- | Path svg
svgPath :: PathStyle -> [Point Double] -> Lucid.Html ()
svgPath _ [] = mempty
svgPath _ [_] = mempty
svgPath pstyle ps =
  terms "path" [term "d" (toPathAbsolutes (zip (pstyle ^. #pathInfo) ps))]

-- | Low-level conversion of a Chart to svg
-- FIXME: abstract extra html content into here...
svg :: Chart Double -> Lucid.Html ()
svg (Chart (TextA s ts) xs) =
  term "g" (attsText s) (mconcat $ zipWith (\t p -> svgText s t (toPoint p)) ts xs)
svg (Chart (GlyphA s) xs) =
  term "g" (attsGlyph s) (mconcat $ svgGlyph s . toPoint <$> xs)
svg (Chart (LineA s) xs) =
  term "g" (attsLine s) (svgLine $ toPoint <$> xs)
svg (Chart (RectA s) xs) =
  term "g" (attsRect s) (mconcat $ svgRect . toRect <$> xs)
svg (Chart (PathA s) xs) =
  term "g" (attsPath s) (svgPath s $ toPoint <$> xs)
svg (Chart BlankA _) = mempty

-- | Add a tooltip as part of a chart to svg conversion.
--
-- FIXME: remove redundancy.
svgt :: Chart Double -> (TextStyle, Text) -> Lucid.Html ()
svgt (Chart (TextA s ts) xs) (s', ts') =
  term "g" (attsText s) (Lucid.title_ (attsText s') (Lucid.toHtml ts') <> mconcat (zipWith (\t p -> svgText s t (toPoint p)) ts xs))
svgt (Chart (GlyphA s) xs) (s', ts') =
  term "g" (attsGlyph s)
  (Lucid.title_ (attsText s') (Lucid.toHtml ts') <>
   mconcat (svgGlyph s . toPoint <$> xs))
svgt (Chart (LineA s) xs) (s', ts') =
  term "g" (attsLine s)
  (Lucid.title_ (attsText s') (Lucid.toHtml ts') <>
   svgLine (toPoint <$> xs))
svgt (Chart (RectA s) xs) (s', ts') =
  term "g" (attsRect s)
  (Lucid.title_ (attsText s') (Lucid.toHtml ts') <>
   mconcat (svgRect . toRect <$> xs))
svgt (Chart (PathA s) xs) (s', ts') =
  term "g" (attsPath s)
  (Lucid.title_ (attsText s') (Lucid.toHtml ts') <>
   (svgPath s $ toPoint <$> xs))
svgt (Chart BlankA _) _ = mempty

terms :: Text -> [Lucid.Attribute] -> Lucid.Html ()
terms t = with $ makeXmlElementNoEnd t

-- * Style to Attributes
attsRect :: RectStyle -> [Lucid.Attribute]
attsRect o =
  [ term "stroke-width" (show $ o ^. #borderSize),
    term "stroke" (hex $ o ^. #borderColor),
    term "stroke-opacity" (show $ opac $ o ^. #borderColor),
    term "fill" (hex $ o ^. #color),
    term "fill-opacity" (show $ opac $ o ^. #color)
  ]

attsText :: TextStyle -> [Lucid.Attribute]
attsText o =
  [ term "stroke-width" "0.0",
    term "stroke" "none",
    term "fill" (toHex $ o ^. #color),
    term "fill-opacity" (show $ opac $ o ^. #color),
    term "font-size" (show $ o ^. #size),
    term "text-anchor" (toTextAnchor $ o ^. #anchor)
  ]
    <> maybe [] ((: []) . term "transform" . toTranslateText) (o ^. #translate)
  where
    toTextAnchor :: Anchor -> Text
    toTextAnchor AnchorMiddle = "middle"
    toTextAnchor AnchorStart = "start"
    toTextAnchor AnchorEnd = "end"

attsGlyph :: GlyphStyle -> [Lucid.Attribute]
attsGlyph o =
  [ term "stroke-width" (show $ o ^. #borderSize),
    term "stroke" (toHex $ o ^. #borderColor),
    term "stroke-opacity" (show $ opac $ o ^. #borderColor),
    term "fill" (toHex $ o ^. #color),
    term "fill-opacity" (show $ opac $ o ^. #color)
  ]
    <> maybe [] ((: []) . term "transform" . toTranslateText) (o ^. #translate)

attsLine :: LineStyle -> [Lucid.Attribute]
attsLine o =
  [ term "stroke-width" (show $ o ^. #width),
    term "stroke" (toHex $ o ^. #color),
    term "stroke-opacity" (show $ opac $ o ^. #color),
    term "fill" "none"
  ]

attsPath :: PathStyle -> [Lucid.Attribute]
attsPath o =
  [ term "stroke-width" (show $ o ^. #borderSize),
    term "stroke" (hex $ o ^. #borderColor),
    term "stroke-opacity" (show $ opac $ o ^. #borderColor),
    term "fill" (hex $ o ^. #color),
    term "fill-opacity" (show $ opac $ o ^. #color)
  ] <>
  fmap (\(p,t) -> case p of
           MarkerStart -> term "marker-start" ("url(#" <> t <> ")")
           MarkerMid -> term "marker-mid" ("url(#" <> t <> ")")
           MarkerEnd -> term "marker-end" ("url(#" <> t <> ")")
       ) (o ^. #pathMarkers)

toTranslateText :: Point Double -> Text
toTranslateText (Point x y) =
  "translate(" <> show x <> ", " <> show (- y) <> ")"

toRotateText :: Double -> Point Double -> Text
toRotateText r (Point x y) =
  "rotate(" <> show r <> ", " <> show x <> ", " <> show (- y) <> ")"

toScaleText :: Double -> Text
toScaleText x =
  "scale(" <> show x <> ")"

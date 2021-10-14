{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wall #-}

-- | Rendering charts to SVG.
--
-- Note that type signatures are tightened to Double as sane SVG rendering suggests.
module Chart.Render
  ( ChartSvg (..),
    renderToCharts,
    renderToCRS,
    chartSvg,
    chartSvgDefault,
    chartSvgHud,
    renderChartsWith,
    renderHudChart,
    writeChartSvg,
    writeChartSvgDefault,
    writeChartSvgHud,
    svg2Tag,
    svg,
    svgRect,
    svgText,
    svgLine,
    svgGlyph,
    svgPath,
    terms,
    makeAttribute,

    -- * low-level conversions
    attsRect,
    attsText,
    attsGlyph,
    attsLine,
    attsPath,
    svgShape,
  )
where

import Chart.Types
import Control.Lens hiding (transform)
import Data.Colour
import Data.Generics.Labels ()
import Data.Path
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import Lucid
import Lucid.Base
import qualified Lucid.Base as Lucid
import NumHask.Prelude
import NumHask.Space as NH hiding (Element, singleton)
import NeatInterpolation

-- $setup
-- >>> import Control.Lens
-- >>> import Chart

-- | Specification of a chart for rendering to SVG
data ChartSvg = ChartSvg
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
        ( cssText csso
            <> mconcat (svg <$> cs)
        )
    )
    [ width_ (pack $ show w'),
      height_ (pack $ show h'),
      makeAttribute "viewBox" (pack $ show x <> " " <> show (-w) <> " " <> show (z - x) <> " " <> show (w - y))
    ]

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

makeCharts :: ChartAspect -> HudOptions -> [Chart Double] -> [Chart Double]
makeCharts asp ho cs =
  let (hs', hc') = makeHud (padBox $ dataBoxes cs) ho
   in runHud (initialCanvas asp (cs <> hc')) hs' (cs <> hc')

renderToCRS :: SvgOptions -> [Chart Double] -> ([Chart Double], Rect Double, Point Double)
renderToCRS so cs = (cs', rect', size')
  where
    rect' = styleBoxesS cs' & maybe id padRect (so ^. #outerPad)
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
      FixedAspect _ -> styleBoxesS cs
      CanvasAspect _ -> dataBoxesS cs
      ChartAspect -> styleBoxesS cs
      UnadjustedAspect -> dataBoxesS cs

-- | Consume the ChartSvg and produce the combined huds and charts as a chart list.
renderToCharts :: ChartSvg -> [Chart Double]
renderToCharts cs = makeCharts (view (#svgOptions . #chartAspect) cs) (view #hudOptions cs) (view #chartList cs)

-- | render Charts with the supplied options.
renderChartsWith :: SvgOptions -> [Chart Double] -> Text
renderChartsWith so cs =
  Lazy.toStrict $ renderText (renderToSvg (so ^. #cssOptions) size' rect' cs'')
  where
    rect' = styleBoxesS cs' & maybe id padRect (so ^. #outerPad)
    cs' =
      cs
        & runHud penult [chartAspectHud (so ^. #chartAspect)]
        & maybe
          id
          (\x -> frameChart x (fromMaybe 0 (so ^. #innerPad)))
          (so ^. #chartFrame)
    cs'' =
      foldMap (\c -> [Chart (RectA (blob c)) (singleton [RectXY rect'])]) (so ^. #background) <> cs'
    Point w h = NH.width rect'
    size' = Point ((so ^. #svgHeight) / h * w) (so ^. #svgHeight)
    penult = case so ^. #chartAspect of
      FixedAspect _ -> styleBoxesS cs
      CanvasAspect _ -> dataBoxesS cs
      ChartAspect -> styleBoxesS cs
      UnadjustedAspect -> dataBoxesS cs

-- | render charts with the supplied svg options and huds
renderHudChart :: SvgOptions -> [Hud Double] -> [Chart Double] -> Text
renderHudChart so hs cs = renderChartsWith so (runHud (initialCanvas (so ^. #chartAspect) cs) hs cs)

-- | Render a chart using the supplied svg and hud config.
--
-- >>> chartSvg mempty
-- "<svg height=\"300.0\" width=\"300.0\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"-0.52 -0.52 1.04 1.04\"></svg>"
chartSvg :: ChartSvg -> Text
chartSvg (ChartSvg so ho hs cs) = renderHudChart so (hs <> hs') (cs <> cs')
  where
    (hs', cs') = makeHud (padBox $ dataBoxes cs) ho

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
  writeFile fp (unpack $ chartSvg cs)

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
    [ width_ (pack $ show $ z - x),
      height_ (pack $ show $ w - y),
      term "x" (pack $ show x),
      term "y" (pack $ show $ -w)
    ]

-- | Text svg
svgText :: TextStyle -> Text -> Point Double -> Lucid.Html ()
svgText s t p@(Point x y) =
  term
    "text"
    ( [ term "x" (pack $ show x),
        term "y" (pack $ show $ -y)
      ]
        <> foldMap (\x' -> [term "transform" (toRotateText x' p)]) (s ^. #rotation)
    )
    (toHtmlRaw t)

-- | line svg
svgLine :: [Point Double] -> Lucid.Html ()
svgLine [] = mempty
svgLine xs = terms "polyline" [term "points" (toPointsText xs)]
  where
    toPointsText xs' = Text.intercalate "\n" $ (\(Point x y) -> pack (show x <> "," <> show (-y))) <$> xs'

-- | GlyphShape to svg Tree
svgShape :: GlyphShape -> Double -> Point Double -> Lucid.Html ()
svgShape CircleGlyph s (Point x y) =
  terms
    "circle"
    [ term "cx" (pack $ show x),
      term "cy" (pack $ show $ -y),
      term "r" (pack $ show $ 0.5 * s)
    ]
svgShape SquareGlyph s p =
  svgRect (move p ((s *) <$> one))
svgShape (RectSharpGlyph x') s p =
  svgRect (move p (NH.scale (Point s (x' * s)) one))
svgShape (RectRoundedGlyph x' rx ry) s p =
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
svgShape (TriangleGlyph (Point xa ya) (Point xb yb) (Point xc yc)) s p =
  terms
    "polygon"
    [ term "transform" (toTranslateText p),
      term "points" (pack $ show (s * xa) <> "," <> show (-(s * ya)) <> " " <> show (s * xb) <> "," <> show (-(s * yb)) <> " " <> show (s * xc) <> "," <> show (-(s * yc)))
    ]
svgShape (EllipseGlyph x') s (Point x y) =
  terms
    "ellipse"
    [ term "cx" ((pack . show) x),
      term "cy" ((pack . show) $ -y),
      term "rx" ((pack . show) $ 0.5 * s),
      term "ry" ((pack . show) $ 0.5 * s * x')
    ]
svgShape (VLineGlyph _) s (Point x y) =
  terms "polyline" [term "points" (pack $ show x <> "," <> show (-(y - s / 2)) <> "\n" <> show x <> "," <> show (-(y + s / 2)))]
svgShape (HLineGlyph _) s (Point x y) =
  terms "polyline" [term "points" (pack $ show (x - s / 2) <> "," <> show (-y) <> "\n" <> show (x + s / 2) <> "," <> show (-y))]
svgShape (PathGlyph path) s p =
  terms "path" [term "d" path, term "transform" (toTranslateText p <> " " <> toScaleText s)]

-- | GlyphStyle to svg Tree
svgGlyph :: GlyphStyle -> Point Double -> Lucid.Html ()
svgGlyph s p =
  svgShape (s ^. #shape) (s ^. #size) p
    & maybe id (\r -> term "g" [term "transform" (toRotateText r p)]) (s ^. #rotation)

-- | Path svg
svgPath :: [PathInfo Double] -> [Point Double] -> Lucid.Html ()
svgPath _ [] = mempty
svgPath _ [_] = mempty
svgPath infos ps =
  terms "path" [term "d" (toPathAbsolutes (zip infos ps))]

svgAtts :: Annotation -> [Attribute]
svgAtts (TextA s _) = attsText s
svgAtts (GlyphA s) = attsGlyph s
svgAtts (LineA s) = attsLine s
svgAtts (RectA s) = attsRect s
svgAtts (PathA s _) = attsPath s
svgAtts BlankA = mempty

svgHtml :: Chart Double -> Lucid.Html ()
svgHtml (Chart (TextA s ts) xss) =
  mconcat $ mconcat $ fmap (\xs -> zipWith (\t p -> svgText s t (toPoint p)) ts xs) xss
svgHtml (Chart (GlyphA s) xs) =
  mconcat $ mconcat $ fmap (svgGlyph s . toPoint) <$> xs
svgHtml (Chart (LineA _) xs) =
  mconcat $ fmap svgLine $ fmap toPoint <$> xs
svgHtml (Chart (RectA _) xs) =
  mconcat $ mconcat $ fmap (svgRect . toRect) <$> xs
svgHtml (Chart (PathA _ infos) xs) =
  mconcat $ fmap (svgPath infos) $ fmap toPoint <$> xs
svgHtml (Chart BlankA _) = mempty

-- | Low-level conversion of a Chart to svg
svg :: Chart Double -> Lucid.Html ()
svg c = term "g" (svgAtts $ c ^. #annotation) (svgHtml c)

-- | Make Lucid Html given term and attributes
terms :: Text -> [Lucid.Attribute] -> Lucid.Html ()
terms t = with $ makeXmlElementNoEnd t

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

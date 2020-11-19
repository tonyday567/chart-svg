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
    renderChartsWith,
    renderHudChart,
    writeChartSvg,
    writeChartSvgDefault,
    writeChartSvgHud,
    svg2Tag,
    cssCrisp,
    geometricPrecision,
    svg,
    attsRect,
    terms,
    makeAttribute,

    -- * Augmentation
    ChartExtra (..),
    toChartExtra,
    renderChartExtrasWith,

    attsText,
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

-- | Specification of a chart for rendering to SVG
data ChartSvgExtra
  = ChartSvgExtra
      { svgOptionsExtra :: SvgOptions,
        hudOptionsExtra :: HudOptions,
        hudListExtra :: [Hud Double],
        chartExtraList :: [ChartExtra Double]
      }
  deriving (Generic)

instance Semigroup ChartSvgExtra where
  (<>) (ChartSvgExtra _ o h c) (ChartSvgExtra s' o' h' c') =
    ChartSvgExtra s' (o <> o') (h <> h') (c <> c')

instance Monoid ChartSvgExtra where
  mempty = ChartSvgExtra defaultSvgOptions mempty [] []

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

renderToSvgExtra :: CssOptions -> Point Double -> Rect Double -> [ChartExtra Double] -> Html ()
renderToSvgExtra csso (Point w' h') (Rect x z y w) cs =
  with
    ( svg2Tag
        ( cssText csso <>
            mconcat (svgExtra <$> cs)
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
  Lazy.toStrict $ renderText (renderToSvg (so ^. #cssOptions) size' rect' cs')
  where
    rect' = styleBoxesS cs' & maybe id padRect (so ^. #outerPad)
    cs' =
      cs &
      runHud penult [chartAspectHud (so ^. #chartAspect)] &
      maybe id (\x -> frameChart x (fromMaybe 0 (so ^. #innerPad)))
        (so ^. #chartFrame)
    Point w h = NH.width rect'
    size' = Point ((so ^. #svgHeight)/h*w) (so ^. #svgHeight)
    penult = case so ^. #chartAspect of
      FixedAspect _ -> styleBoxesS cs
      CanvasAspect _ -> dataBoxesS cs
      ChartAspect -> styleBoxesS cs
      UnadjustedAspect -> dataBoxesS cs

-- | render ChartExtras with the supplied options.
renderChartExtrasWith :: SvgOptions -> [ChartExtra Double] -> Text
renderChartExtrasWith so cs =
  Lazy.toStrict $ renderText (renderToSvgExtra (so ^. #cssOptions) size' rect' cs')
  where
    cs' = zipWith (\(ChartExtra _ l a h') c' -> ChartExtra c' l a h') cs (csFinalize csa)
    rect' = styleBoxesS (csFinalize csa) & maybe id padRect (so ^. #outerPad)
    csFinalize =
      maybe id (\x -> frameChart x (fromMaybe 0 (so ^. #innerPad)))
        (so ^. #chartFrame) .
      runHud penult [chartAspectHud (so ^. #chartAspect)]
    Point w h = NH.width rect'
    size' = Point ((so ^. #svgHeight)/h*w) (so ^. #svgHeight)
    csa = fmap (view #chartActual) cs
    penult = case so ^. #chartAspect of
      FixedAspect _ -> styleBoxesS csa
      CanvasAspect _ -> dataBoxesS csa
      ChartAspect -> styleBoxesS csa
      UnadjustedAspect -> dataBoxesS csa

-- | render charts with the supplied svg options and huds
renderHudChart :: SvgOptions -> [Hud Double] -> [Chart Double] -> Text
renderHudChart so hs cs = renderChartsWith so (runHud (initialCanvas (so ^. #chartAspect) cs) hs cs)

-- | Render a chart using the supplied svg and hud config.
--
-- >>> chartSvg mempty
-- "<svg xmlns=\"http://www.w3.org/2000/svg\" height=\"300.0\" viewBox=\"-0.52 -0.52 1.04 1.04\" width=\"450.0\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"></svg>"
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

svgAtts :: Annotation -> [Attribute]
svgAtts (TextA s _) = attsText s
svgAtts (GlyphA s) = attsGlyph s
svgAtts (LineA s) = attsLine s
svgAtts (RectA s) = attsRect s
svgAtts (PathA s) = attsPath s
svgAtts BlankA = mempty

svgHtml :: Chart Double -> Lucid.Html ()
svgHtml (Chart (TextA s ts) xs) =
  mconcat $ zipWith (\t p -> svgText s t (toPoint p)) ts xs
svgHtml (Chart (GlyphA s) xs) =
  mconcat $ svgGlyph s . toPoint <$> xs
svgHtml (Chart (LineA _) xs) =
  svgLine $ toPoint <$> xs
svgHtml (Chart (RectA _) xs) =
  mconcat $ svgRect . toRect <$> xs
svgHtml (Chart (PathA s) xs) =
  svgPath s $ toPoint <$> xs
svgHtml (Chart BlankA _) = mempty

-- | Low-level conversion of a Chart to svg
svg :: Chart Double -> Lucid.Html ()
svg c = term "g" (svgAtts $ c ^. #annotation) (svgHtml c)

-- | render extra attributes and html
svgExtra :: ChartExtra Double -> Lucid.Html ()
svgExtra (ChartExtra c l' as h) =
  case l' of
    Nothing -> x
    Just l ->
       term "a" [term "xlink:href" l :: Lucid.Attribute] x
  where
    x = term "g" ((svgAtts $ c ^. #annotation) <> as) (svgHtml c <> h)

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
  ] <>
  (maybe [] (\x -> [term "stroke-linecap" (fromLineCap x)]) (o ^. #linecap)) <>
  (maybe [] (\x -> [term "stroke-dasharray" (fromDashArray x)]) (o ^. #dasharray))

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


-- * augmentation

data ChartExtra a =
  ChartExtra
  { chartActual :: Chart a,
    chartLink :: Maybe Text,
    chartAttributes :: [Attribute],
    chartContent :: Html ()
  } deriving (Show, Generic)

toChartExtra :: Chart a -> ChartExtra a
toChartExtra c = ChartExtra c Nothing mempty mempty

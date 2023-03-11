{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall #-}

-- | Conversion from a chart to SVG.
module Chart.Svg
  ( -- * ChartSvg
    ChartSvg (..),
    toChartTree,
    writeChartSvg,
    chartSvg,
    initialCanvas,

    -- * SVG Options
    SvgOptions (..),
    defaultSvgOptions,

    -- * SVG Style primitives
    CssOptions (..),
    defaultCssOptions,
    CssShapeRendering (..),
    CssPreferColorScheme (..),
    cssShapeRendering,
    cssPreferColorScheme,
  )
where

import Chart.Data
import Chart.Hud
import Chart.Primitive
import Chart.Style
import Data.Colour
import Data.Maybe
import Data.Path
import Data.Path.Parser
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import Data.Tree
import GHC.Generics
import Lucid
import Lucid.Base
import NeatInterpolation
import Optics.Core
import Prelude
import FlatParse.Basic

-- $setup
--
-- >>> :set -XOverloadedLabels
-- >>> :set -XOverloadedStrings
-- >>> import Chart
-- >>> import Optics.Core

-- helpers
--
draw :: Chart -> Html ()
draw (RectChart _ a) = mconcat $ svgRect_ <$> a
draw (TextChart s a) = mconcat $ uncurry (svgText_ s) <$> a
draw (LineChart _ as) = svgLine_ as
draw (GlyphChart s a) = mconcat $ svgGlyph_ s <$> a
draw (PathChart _ a) = svgPath_ a
draw (BlankChart _) = mempty

atts :: Chart -> [Attribute]
atts (RectChart s _) = attsRect s
atts (TextChart s _) = attsText s
atts (LineChart s _) = attsLine s
atts (GlyphChart s _) = attsGlyph s
atts (PathChart s _) = attsPath s
atts (BlankChart _) = mempty

svgChartTree :: ChartTree -> Lucid.Html ()
svgChartTree cs
  | isNothing label && null cs' = mconcat $ svgChartTree . ChartTree <$> xs
  | otherwise = term "g" (foldMap (\x -> [term "class" x]) label) content'
  where
    (ChartTree (Node (label, cs') xs)) = filterChartTree (not . isEmptyChart) cs
    content' = (mconcat $ svg <$> cs') <> (mconcat $ svgChartTree . ChartTree <$> xs)

-- ** ChartSvg

-- | Specification of a chart ready to be rendered to SVG includes:
--
-- - svg options
--
-- - hud options
--
-- - any extra hud elements beyond the usual options
--
-- - an underlying chart tree.
--
-- See Data.Examples for usage.
data ChartSvg = ChartSvg
  { svgOptions :: SvgOptions,
    hudOptions :: HudOptions,
    extraHuds :: [Hud],
    charts :: ChartTree
  }
  deriving (Generic)

instance Semigroup ChartSvg where
  (<>) (ChartSvg _ o h c) (ChartSvg s' o' h' c') =
    ChartSvg s' (o <> o') (h <> h') (c <> c')

instance Monoid ChartSvg where
  mempty = ChartSvg defaultSvgOptions mempty mempty mempty

-- * rendering

-- | @svg@ element + svg 2 attributes
svg2Tag :: (Term [Attribute] (s -> t)) => s -> t
svg2Tag m =
  svg_
    [ makeAttribute "xmlns" "http://www.w3.org/2000/svg",
      makeAttribute "xmlns:xlink" "http://www.w3.org/1999/xlink"
    ]
    m

renderToText :: Html () -> Text
renderToText = Lazy.toStrict . renderText

renderToSvg :: SvgOptions -> ChartTree -> Html ()
renderToSvg so cs =
  with
    (svg2Tag (cssText (view #cssOptions so) <> svgChartTree cs))
    [ width_ (pack $ show w''),
      height_ (pack $ show h''),
      makeAttribute "viewBox" (pack $ show x <> " " <> show (-w) <> " " <> show (z - x) <> " " <> show (w - y))
    ]
  where
    r@(Rect x z y w) = singletonGuard (view styleBox' cs)
    Point w' h' = width r
    Point w'' h'' = Point ((so ^. #svgHeight) / h' * w') (so ^. #svgHeight)

-- | Low-level conversion of a Chart to svg
svg :: Chart -> Lucid.Html ()
svg (BlankChart _) = mempty
svg c = term "g" (atts c) (draw c)

cssText :: CssOptions -> Html ()
cssText csso =
  style_ [] $
    cssShapeRendering (csso ^. #shapeRendering)
      <> cssPreferColorScheme (light, dark) (csso ^. #preferColorScheme)
      <> csso ^. #cssExtra

-- | CSS shape rendering text snippet
cssShapeRendering :: CssShapeRendering -> Text
cssShapeRendering UseGeometricPrecision = "svg { shape-rendering: geometricPrecision; }"
cssShapeRendering UseCssCrisp = "svg { shape-rendering: crispEdges; }"
cssShapeRendering NoShapeRendering = mempty

-- | CSS prefer-color-scheme text snippet
cssPreferColorScheme :: (Colour, Colour) -> CssPreferColorScheme -> Text
cssPreferColorScheme (cl, cd) PreferHud =
  [trimming|
svg {
  color-scheme: light dark;
}
{
  .canvas g, .title g, .axisbar g, .ticktext g, .tickglyph g, .ticklines g, .legendContent g text {
    fill: $hexDark;
  }
  .ticklines g, .tickglyph g, .legendBorder g {
    stroke: $hexDark;
  }
  .legendBorder g {
    fill: $hexLight;
  }
}
@media (prefers-color-scheme:dark) {
  .canvas g, .title g, .axisbar g, .ticktext g, .tickglyph g, .ticklines g, .legendContent g text {
    fill: $hexLight;
  }
  .ticklines g, .tickglyph g, .legendBorder g {
    stroke: $hexLight;
  }
  .legendBorder g {
    fill: $hexDark;
  }
}
|]
  where
    hexLight = hex cl
    hexDark = hex cd
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
  where
    c = hex bglight
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
  |]
  where
    c = hex bgdark
cssPreferColorScheme _ PreferNormal = mempty

-- | consume the huds transforming a 'ChartSvg' to a 'ChartTree'
toChartTree :: ChartSvg -> ChartTree
toChartTree cs =
  runHudWith
    (initialCanvas (view (#hudOptions % #chartAspect) cs) (view #charts cs))
    db'
    hs'
    (view #charts cs <> blank db')
  where
    (hs, db') = toHuds (view #hudOptions cs) (singletonGuard $ view (#charts % box') cs)
    hs' =
      hs
        <> view #extraHuds cs

-- | The initial canvas before applying Huds
--
-- >>> initialCanvas (FixedAspect 1.5) (unnamed [RectChart defaultRectStyle [one]])
-- Rect -0.75 0.75 -0.5 0.5
initialCanvas :: ChartAspect -> ChartTree -> CanvasBox
initialCanvas (FixedAspect a) _ = aspect a
initialCanvas (CanvasAspect a) _ = aspect a
initialCanvas ChartAspect cs = singletonGuard $ view box' cs

-- | Render a chart using the supplied svg and hud config.
--
-- >>> chartSvg mempty
-- "<svg width=\"450.0\" height=\"300.0\" viewBox=\"-0.75 -0.5 1.5 1.0\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"><style>svg {\n  color-scheme: light dark;\n}\n{\n  .canvas g, .title g, .axisbar g, .ticktext g, .tickglyph g, .ticklines g, .legendContent g text {\n    fill: #0d0d0d;\n  }\n  .ticklines g, .tickglyph g, .legendBorder g {\n    stroke: #0d0d0d;\n  }\n  .legendBorder g {\n    fill: #f0f0f0;\n  }\n}\n@media (prefers-color-scheme:dark) {\n  .canvas g, .title g, .axisbar g, .ticktext g, .tickglyph g, .ticklines g, .legendContent g text {\n    fill: #f0f0f0;\n  }\n  .ticklines g, .tickglyph g, .legendBorder g {\n    stroke: #f0f0f0;\n  }\n  .legendBorder g {\n    fill: #0d0d0d;\n  }\n}</style><g class=\"chart\"></g><g class=\"hud\"></g></svg>"
chartSvg :: ChartSvg -> Text
chartSvg cs = renderToText (renderToSvg (view #svgOptions cs) (toChartTree cs))

-- | Write to a file.
writeChartSvg :: FilePath -> ChartSvg -> IO ()
writeChartSvg fp cs =
  writeFile fp (unpack $ chartSvg cs)

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
    <> case view #frame s of
      Nothing -> mempty
      Just f -> svg (RectChart (f & over #borderSize (* view #size s)) [styleBoxText s t p])

-- | line svg
svgLine_ :: [[Point Double]] -> Lucid.Html ()
svgLine_ xss =
  mconcat $
    (\xs -> terms "polyline" [term "points" (toPointsText xs)]) <$> xss
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
  svgRect_ (move p (scale (Point s (x' * s)) one))
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
    (Rect x z y w) = move p (scale (Point s (x' * s)) one)
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
svgShape_ VLineGlyph s (Point x y) =
  terms "polyline" [term "points" (pack $ show x <> "," <> show (-(y - s / 2)) <> "\n" <> show x <> "," <> show (-(y + s / 2)))]
svgShape_ HLineGlyph s (Point x y) =
  terms "polyline" [term "points" (pack $ show (x - s / 2) <> "," <> show (-y) <> "\n" <> show (x + s / 2) <> "," <> show (-y))]
svgShape_ (PathGlyph path _) s p =
  terms "path" [term "d" (pack $ utf8ToStr path), term "transform" (toTranslateText p <> " " <> toScaleText s)]

-- | GlyphStyle to svg Tree
svgGlyph_ :: GlyphStyle -> Point Double -> Lucid.Html ()
svgGlyph_ s p =
  svgShape_ (s ^. #shape) (s ^. #size) p
    & maybe id (\r -> term "g" [term "transform" (toRotateText r p)]) (s ^. #rotation)

-- | Path svg
svgPath_ :: [PathData Double] -> Lucid.Html ()
svgPath_ ps =
  terms "path" [term "d" (pack $ utf8ToStr $ pathDataToSvg ps)]

-- | RectStyle to Attributes
attsRect :: RectStyle -> [Lucid.Attribute]
attsRect o =
  [ term "stroke-width" (pack $ show $ o ^. #borderSize),
    term "stroke" (showRGB $ o ^. #borderColor),
    term "stroke-opacity" (pack $ show $ opac $ o ^. #borderColor),
    term "fill" (showRGB $ o ^. #color),
    term "fill-opacity" (pack $ show $ opac $ o ^. #color)
  ]

-- | TextStyle to Attributes
attsText :: TextStyle -> [Lucid.Attribute]
attsText o =
  [ term "stroke-width" "0.0",
    term "stroke" "none",
    term "fill" (showRGB $ o ^. #color),
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
  [ term "stroke-width" (pack $ show sw),
    term "stroke" (showRGB $ o ^. #borderColor),
    term "stroke-opacity" (pack $ show $ opac $ o ^. #borderColor),
    term "fill" (showRGB $ o ^. #color),
    term "fill-opacity" (pack $ show $ opac $ o ^. #color)
  ]
    <> foldMap ((: []) . term "transform" . toTranslateText) (o ^. #translate)
  where
    sw = case o ^. #shape of
      PathGlyph _ NoScaleBorder -> o ^. #borderSize
      PathGlyph _ ScaleBorder -> min 0.2 (o ^. #borderSize / o ^. #size)
      _ -> o ^. #borderSize

-- | LineStyle to Attributes
attsLine :: LineStyle -> [Lucid.Attribute]
attsLine o =
  [ term "stroke-width" (pack $ show $ o ^. #size),
    term "stroke" (showRGB $ o ^. #color),
    term "stroke-opacity" (pack $ show $ opac $ o ^. #color),
    term "fill" "none"
  ]
    <> foldMap (\x -> [term "stroke-linecap" (fromLineCap x)]) (o ^. #linecap)
    <> foldMap (\x -> [term "stroke-linejoin" (fromLineJoin x)]) (o ^. #linejoin)
    <> foldMap (\x -> [term "stroke-dasharray" (fromDashArray x)]) (o ^. #dasharray)
    <> foldMap (\x -> [term "stroke-dashoffset" (pack $ show x)]) (o ^. #dashoffset)

-- | Convert a dash representation from a list to text
fromDashArray :: [Double] -> Text
fromDashArray xs = Text.intercalate " " $ pack . show <$> xs

-- | PathStyle to Attributes
attsPath :: PathStyle -> [Lucid.Attribute]
attsPath o =
  [ term "stroke-width" (pack $ show $ o ^. #borderSize),
    term "stroke" (showRGB $ o ^. #borderColor),
    term "stroke-opacity" (pack $ show $ opac $ o ^. #borderColor),
    term "fill" (showRGB $ o ^. #color),
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

-- | SVG tag options.
--
-- >>> defaultSvgOptions
-- SvgOptions {svgHeight = 300.0, cssOptions = CssOptions {shapeRendering = NoShapeRendering, preferColorScheme = PreferHud, cssExtra = ""}}
data SvgOptions = SvgOptions
  { svgHeight :: Double,
    cssOptions :: CssOptions
  }
  deriving (Eq, Show, Generic)

-- | The official svg options
defaultSvgOptions :: SvgOptions
defaultSvgOptions = SvgOptions 300 defaultCssOptions

-- | CSS shape rendering options
data CssShapeRendering = UseGeometricPrecision | UseCssCrisp | NoShapeRendering deriving (Show, Eq, Generic)

-- | CSS prefer-color-scheme options
data CssPreferColorScheme
  = -- | includes css that switches approriate hud elements between light and dark.
    PreferHud
  | PreferDark
  | PreferLight
  | PreferNormal
  deriving (Show, Eq, Generic)

-- | css options
--
-- >>> defaultCssOptions
-- CssOptions {shapeRendering = NoShapeRendering, preferColorScheme = PreferHud, cssExtra = ""}
data CssOptions = CssOptions {shapeRendering :: CssShapeRendering, preferColorScheme :: CssPreferColorScheme, cssExtra :: Text} deriving (Show, Eq, Generic)

-- | No special shape rendering and default hud responds to user color scheme preferences.
defaultCssOptions :: CssOptions
defaultCssOptions = CssOptions NoShapeRendering PreferHud mempty

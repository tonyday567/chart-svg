{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall #-}

-- | Chart API
module Chart.Svg
  ( ChartSvg(..),
    writeChartSvg,
    chartSvg,
  ) where

import Chart.Primitive
import Data.Colour
import Chart.Style
import Chart.Hud
import Data.Path
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import Prelude
import Lucid
import Data.List.NonEmpty (NonEmpty(..))
import Optics.Core
import Lucid.Base
import NeatInterpolation
import qualified Data.Text.Lazy as Lazy
import Chart.Data
import GHC.Generics
import Data.Semigroup
import Data.Maybe
import Data.Foldable
import Data.Path.Parser

draw :: Chart Double -> Html ()
draw (RectChart _ a) = sconcat $ svgRect_ <$> a
draw (TextChart s a) = sconcat $ uncurry (svgText_ s) <$> a
draw (LineChart _ as) = svgLine_ as
draw (GlyphChart s a) = sconcat $ svgGlyph_ s <$> a
draw (PathChart _ a) = svgPath_ a
draw (BlankChart _) = mempty

atts :: Chart a -> [Attribute]
atts (RectChart s _) = attsRect s
atts (TextChart s _) = attsText s
atts (LineChart s _) = attsLine s
atts (GlyphChart s _) = attsGlyph s
atts (PathChart s _) = attsPath s
atts (BlankChart _) = mempty

-- ** ChartSvg

-- | Specification of a chart for rendering to SVG
data ChartSvg = ChartSvg
  { svgOptions :: SvgOptions,
    hudOptions :: HudOptions,
    extraHuds :: Huds,
    chartTree :: [Chart Double]
  }
  deriving (Generic)
instance Semigroup ChartSvg where
  (<>) (ChartSvg _ o h c) (ChartSvg s' o' h' c') =
    ChartSvg s' (o <> o') (h <> h') (c <> c')

instance Monoid ChartSvg where
  mempty = ChartSvg defaultSvgOptions mempty mempty []

-- * rendering

-- | @svg@ element + svg 2 attributes
svg2Tag :: Term [Attribute] (s -> t) => s -> t
svg2Tag m =
  svg_
    [ makeAttribute "xmlns" "http://www.w3.org/2000/svg",
      makeAttribute "xmlns:xlink" "http://www.w3.org/1999/xlink"
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

-- | Low-level conversion of a Chart to svg
svg :: Chart Double -> Lucid.Html ()
svg (BlankChart _) = mempty
svg c = term "g" (atts c) (draw c)

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

-- | render Charts with the supplied options.
renderChartsWith :: SvgOptions -> [Chart Double] -> Text
renderChartsWith so cs =
  Lazy.toStrict $ renderText (renderToSvg (so ^. #cssOptions) size' rect' cs')
  where
    rect' = styleBoxes cs' & maybe id padRect (so ^. #outerPad)
    cs' =
      cs
        & maybe
          id
          (\x -> frameChart x (fromMaybe 0 (so ^. #innerPad)))
          (so ^. #chartFrame)
    Point w h = width rect'
    size' = Point ((so ^. #svgHeight) / h * w) (so ^. #svgHeight)

-- | render charts with the supplied svg options and hud
renderHudChart :: SvgOptions -> Huds -> [Chart Double] -> Text
renderHudChart so hs cs =
  renderChartsWith so (runHud (initialCanvas (so ^. #chartAspect) cs) hs' cs)
  where
    hs' = hs <> Huds [chartAspectHud maxBound (so ^. #chartAspect)]

-- | calculation of the canvas given the 'ChartAspect'
initialCanvas :: ChartAspect -> [Chart Double] -> Rect Double
initialCanvas (FixedAspect a) _ = aspect a
initialCanvas (CanvasAspect a) _ = aspect a
initialCanvas ChartAspect cs = aspect $ ratio $ styleBoxes cs
initialCanvas UnadjustedAspect cs = boxes cs

-- | Render a chart using the supplied svg and hud config.
--
-- >>> chartSvg mempty
-- "<svg height=\"300.0\" width=\"300.0\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"-0.52 -0.52 1.04 1.04\"></svg>"
chartSvg :: ChartSvg -> Text
chartSvg (ChartSvg so ho hs cs) = renderHudChart so (hs <> fromHudOptions ho) cs

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

-- | line svg
svgLine_ :: NonEmpty (NonEmpty (Point Double)) -> Lucid.Html ()
svgLine_ xss = sconcat $
  (\xs -> terms "polyline" [term "points" (toPointsText (toList xs))]) <$> xss
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
  terms "path" [term "d" path, term "transform" (toTranslateText p <> " " <> toScaleText s)]

-- | GlyphStyle to svg Tree
svgGlyph_ :: GlyphStyle -> Point Double -> Lucid.Html ()
svgGlyph_ s p =
  svgShape_ (s ^. #shape) (s ^. #size) p
    & maybe id (\r -> term "g" [term "transform" (toRotateText r p)]) (s ^. #rotation)

-- | Path svg
svgPath_ :: NonEmpty (PathData Double) -> Lucid.Html ()
svgPath_ ps =
  terms "path" [term "d" (pathDataToSvg ps)]

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
  [ term "stroke-width" (pack $ show sw),
    term "stroke" (toHex $ o ^. #borderColor),
    term "stroke-opacity" (pack $ show $ opac $ o ^. #borderColor),
    term "fill" (toHex $ o ^. #color),
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


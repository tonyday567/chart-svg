{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Conversion between Chart and Markup representations.
module Chart.Markup
  ( Markup (..),
    ChartOptions (..),
    markupChartOptions,
    markupChartTree,
    markupChart,
    header,
    renderChartOptions,
    encodeChartOptions,
    writeChartOptions,
    CssOptions (..),
    defaultCssOptions,
    CssPreferColorScheme (..),
    cssPreferColorScheme,
    fillSwitch,
    CssShapeRendering (..),
    markupCssOptions,
    MarkupOptions (..),
    defaultMarkupOptions,
    encodeNum,
    encodePx,
  )
where

import Chart.Data
import Chart.Hud
import Chart.Primitive hiding (tree)
import Chart.Style
import Data.Bool
import Data.ByteString (ByteString, intercalate, writeFile)
import Data.Colour
import Data.FormatN
import Data.Maybe
import Data.Path
import Data.Path.Parser
import Data.String.Interpolate
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics
import MarkupParse
import Optics.Core hiding (element)
import Prelude

-- $setup
--
-- >>> :set -XOverloadedLabels
-- >>> :set -XOverloadedStrings
-- >>> import Chart
-- >>> import Optics.Core
-- >>> let c0 = ChartOptions (defaultMarkupOptions & #cssOptions % #preferColorScheme .~ PreferNormal) mempty mempty
-- >>> import Chart.Examples
-- >>> import MarkupParse

-- | Show a Double, or rounded to 4 decimal places if this is shorter.
--
-- >>> encodeNum 1
-- "1.0"
--
-- >>> encodeNum 1.23456
-- "1.2346"
encodeNum :: Double -> ByteString
encodeNum = encodeUtf8 . formatOrShow (FixedStyle 4) Nothing

-- | SVG width and height, without any unit suffix, are defined as pixels, which are Integers
--
-- >>> encodePx 300.0
-- "300"
encodePx :: Double -> ByteString
encodePx = strToUtf8 . show . (floor :: Double -> Int)

-- | Convert a ChartTree to markup
--
-- >>> lineExample & view #charts & markupChartTree & markdown_ Compact Xml
-- "<g class=\"line\"><g stroke-width=\"0.0150\" stroke=\"rgb(2%, 73%, 80%)\" stroke-opacity=\"1.0\" fill=\"none\"><polyline points=\"0,-1.0 1.0,-1.0 2.0,-5.0\"/></g><g stroke-width=\"0.0150\" stroke=\"rgb(2%, 29%, 48%)\" stroke-opacity=\"1.0\" fill=\"none\"><polyline points=\"0,0 2.8,-3.0\"/></g><g stroke-width=\"0.0150\" stroke=\"rgb(66%, 7%, 55%)\" stroke-opacity=\"1.0\" fill=\"none\"><polyline points=\"0.5,-4.0 0.5,0\"/></g></g>"
markupChartTree :: ChartTree -> Markup
markupChartTree cs =
  maybe xs' (\l -> element "g" [Attr "class" (encodeUtf8 l)] xs') label
  where
    (ChartTree (Node (label, cs') xs)) = filterChartTree (not . isEmptyChart) cs
    xs' = mconcat $ fmap markupChart cs' <> (markupChartTree . ChartTree <$> xs)

markupText :: TextStyle -> Text -> Point Double -> Markup
markupText s t p@(Point x y) = frame' <> element "text" as (content c)
  where
    as =
      uncurry Attr
        <$> [ ("x", encodeNum x),
              ("y", encodeNum $ -y)
            ]
          <> maybeToList ((\x' -> ("transform", toRotateText x' p)) <$> (s ^. #rotation))
    frame' = case view #frame s of
      Nothing -> Markup mempty
      Just f -> markupChart (RectChart (f & over #borderSize (* view #size s)) [styleBoxText s t p])
    c = encodeUtf8 t

-- | Markup a text rotation about a point in radians.
--
-- includes reference changes:
--
-- - from radians to degrees
--
-- - from counter-clockwise is a positive rotation to clockwise is positive
--
-- - flip y dimension
toRotateText :: Double -> Point Double -> ByteString
toRotateText r (Point x y) =
  "rotate(" <> encodeNum (-r * 180 / pi) <> ", " <> encodeNum x <> ", " <> encodeNum (-y) <> ")"

toScaleText :: Double -> ByteString
toScaleText x =
  "scale(" <> encodeNum x <> ")"

-- | Convert a Rect to Markup
markupRect :: Rect Double -> Markup
markupRect (Rect x z y w) =
  emptyElem "rect" as
  where
    as =
      uncurry Attr
        <$> [ ("width", encodeNum (z - x)),
              ("height", encodeNum (w - y)),
              ("x", encodeNum x),
              ("y", encodeNum (-w))
            ]

-- | Convert a Chart to Markup
--
-- >>> lineExample & view #charts & foldOf charts' & head & markupChart & markdown_ Compact Xml
-- "<g stroke-width=\"0.0150\" stroke=\"rgb(2%, 73%, 80%)\" stroke-opacity=\"1.0\" fill=\"none\"><polyline points=\"0,-1.0 1.0,-1.0 2.0,-5.0\"/></g>"
markupChart :: Chart -> Markup
markupChart = uncurry (element "g") . f
  where
    f (RectChart s xs) = (attsRect s, mconcat (markupRect <$> xs))
    f (TextChart s xs) = (attsText s, mconcat (uncurry (markupText s) <$> xs))
    f (GlyphChart s xs) = (attsGlyph s, mconcat (markupGlyph s <$> xs))
    f (PathChart s xs) = (attsPath s, markupPath xs)
    f (LineChart s xs) = (attsLine s, markupLine xs)
    f (BlankChart _) = ([], mempty)

markupLine :: [[Point Double]] -> Markup
markupLine lss =
  mconcat $ emptyElem "polyline" . (: []) . Attr "points" . toPointsText <$> lss

toPointsText :: [Point Double] -> ByteString
toPointsText xs = intercalate " " $ (\(Point x y) -> encodeNum x <> "," <> encodeNum (-y)) <$> xs

-- | Path markup
markupPath :: [PathData Double] -> Markup
markupPath ps =
  emptyElem "path" [Attr "d" (pathDataToSvg ps)]

-- | GlyphStyle to markup Tree
-- Note rotation on the outside not the inside.
markupGlyph :: GlyphStyle -> Point Double -> Markup
markupGlyph s p =
  case view #rotation s of
    Nothing -> gl
    Just r -> element "g" [Attr "transform" (toRotateText r p)] gl
  where
    gl = markupShape_ (s ^. #shape) (s ^. #size) p

-- | Convert a dash representation from a list to text
fromDashArray :: [Double] -> ByteString
fromDashArray xs = intercalate " " $ encodeNum <$> xs

fromDashOffset :: Double -> ByteString
fromDashOffset x = encodeNum x

attsLine :: LineStyle -> [Attr]
attsLine o =
  uncurry Attr
    <$> [ ("stroke-width", encodeNum $ o ^. #size),
          ("stroke", showRGB $ o ^. #color),
          ("stroke-opacity", showOpacity $ o ^. #color),
          ("fill", "none")
        ]
      <> catMaybes
        [(\x -> ("stroke-linecap", fromLineCap x)) <$> (o ^. #linecap)]
      <> foldMap (\x -> [("stroke-linejoin", fromLineJoin x)]) (o ^. #linejoin)
      <> foldMap (\x -> [("stroke-dasharray", fromDashArray x)]) (o ^. #dasharray)
      <> foldMap (\x -> [("stroke-dashoffset", fromDashOffset x)]) (o ^. #dashoffset)

attsRect :: RectStyle -> [Attr]
attsRect o =
  uncurry Attr
    <$> [ ("stroke-width", encodeNum $ o ^. #borderSize),
          ("stroke", showRGB $ o ^. #borderColor),
          ("stroke-opacity", showOpacity $ o ^. #borderColor),
          ("fill", showRGB $ o ^. #color),
          ("fill-opacity", showOpacity $ o ^. #color)
        ]

-- | TextStyle to [Attr]
attsText :: TextStyle -> [Attr]
attsText o =
  uncurry Attr
    <$> [ ("stroke-width", "0.0"),
          ("stroke", "none"),
          ("fill", showRGB $ o ^. #color),
          ("fill-opacity", showOpacity $ o ^. #color),
          ("font-size", encodeNum $ o ^. #size),
          ("text-anchor", toTextAnchor $ o ^. #anchor)
        ]
  where
    toTextAnchor :: Anchor -> ByteString
    toTextAnchor AnchorMiddle = "middle"
    toTextAnchor AnchorStart = "start"
    toTextAnchor AnchorEnd = "end"

-- | GlyphStyle to [Attr]
attsGlyph :: GlyphStyle -> [Attr]
attsGlyph o =
  uncurry Attr
    <$> [ ("stroke-width", encodeNum sw),
          ("stroke", showRGB $ o ^. #borderColor),
          ("stroke-opacity", showOpacity $ o ^. #borderColor),
          ("fill", showRGB $ o ^. #color),
          ("fill-opacity", showOpacity $ o ^. #color)
        ]
      <> foldMap ((: []) . (,) "transform" . toTranslateText) (o ^. #translate)
  where
    sw = case o ^. #shape of
      PathGlyph _ NoScaleBorder -> o ^. #borderSize
      PathGlyph _ ScaleBorder -> min 0.2 (o ^. #borderSize / o ^. #size)
      _ -> o ^. #borderSize

-- | PathStyle to [Attr]
attsPath :: PathStyle -> [Attr]
attsPath o =
  uncurry Attr
    <$> [ ("stroke-width", encodeNum $ o ^. #borderSize),
          ("stroke", showRGB $ o ^. #borderColor),
          ("stroke-opacity", showOpacity $ o ^. #borderColor),
          ("fill", showRGB $ o ^. #color),
          ("fill-opacity", showOpacity $ o ^. #color)
        ]

-- | includes a flip of the y dimension.
toTranslateText :: Point Double -> ByteString
toTranslateText (Point x y) =
  "translate(" <> encodeNum x <> ", " <> encodeNum (-y) <> ")"

-- | GlyphShape to markup Tree
markupShape_ :: GlyphShape -> Double -> Point Double -> Markup
markupShape_ CircleGlyph s (Point x y) = emptyElem "circle" as
  where
    as =
      uncurry Attr
        <$> [ ("cx", encodeNum x),
              ("cy", encodeNum $ -y),
              ("r", encodeNum $ 0.5 * s)
            ]
markupShape_ SquareGlyph s p =
  markupRect (move p ((s *) <$> one :: Rect Double))
markupShape_ (RectSharpGlyph x') s p =
  markupRect (move p (scale (Point s (x' * s)) one :: Rect Double))
markupShape_ (RectRoundedGlyph x' rx ry) s p = emptyElem "rect" as
  where
    as =
      uncurry Attr
        <$> [ ("width", encodeNum $ z - x),
              ("height", encodeNum $ w - y),
              ("x", encodeNum x),
              ("y", encodeNum $ -w),
              ("rx", encodeNum rx),
              ("ry", encodeNum ry)
            ]
    (Rect x z y w) = move p (scale (Point s (x' * s)) one)
markupShape_ (TriangleGlyph (Point xa ya) (Point xb yb) (Point xc yc)) s p =
  emptyElem "polygon" as
  where
    as =
      uncurry Attr
        <$> [ ("transform", toTranslateText p),
              ("points", encodeNum (s * xa) <> "," <> encodeNum (-(s * ya)) <> " " <> encodeNum (s * xb) <> "," <> encodeNum (-(s * yb)) <> " " <> encodeNum (s * xc) <> "," <> encodeNum (-(s * yc)))
            ]
markupShape_ (EllipseGlyph x') s (Point x y) =
  emptyElem "ellipse" as
  where
    as =
      uncurry Attr
        <$> [ ("cx", (strToUtf8 . show) x),
              ("cy", (strToUtf8 . show) $ -y),
              ("rx", (strToUtf8 . show) $ 0.5 * s),
              ("ry", (strToUtf8 . show) $ 0.5 * s * x')
            ]
markupShape_ VLineGlyph s (Point x y) =
  emptyElem "polyline" [Attr "points" $ encodeNum x <> "," <> encodeNum (-(y - s / 2)) <> "\n" <> encodeNum x <> "," <> encodeNum (-(y + s / 2))]
markupShape_ HLineGlyph s (Point x y) =
  emptyElem "polyline" [Attr "points" $ encodeNum (x - s / 2) <> "," <> encodeNum (-y) <> "\n" <> encodeNum (x + s / 2) <> "," <> encodeNum (-y)]
markupShape_ (PathGlyph path _) s p =
  emptyElem "path" (uncurry Attr <$> [("d", path), ("transform", toTranslateText p <> " " <> toScaleText s)])

-- | Create the classic SVG element
--
-- >>> header (Just 300) (Rect (-0.75) 0.75 (-0.5) 0.5) (element_ "foo" []) & markdown_ Compact Xml
-- "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"450\" height=\"300\" viewBox=\"-0.75 -0.5 1.5 1.0\"><foo></foo></svg>"
header :: Maybe Double -> Rect Double -> Markup -> Markup
header markupheight viewbox content' =
  element
    "svg"
    ( uncurry Attr
        <$> ( [ ("xmlns", "http://www.w3.org/2000/svg"),
              ("xmlns:xlink", "http://www.w3.org/1999/xlink")] <>
            widthAndHeight <>
            [ ("viewBox", encodeNum x <> " " <> encodeNum (-w) <> " " <> encodeNum (z - x) <> " " <> encodeNum (w - y))
            ] )
    )
    content'
  where
    (Rect x z y w) = viewbox
    Point w' h = width viewbox
    widthAndHeight = case markupheight of
      Nothing -> []
      Just h' -> [ ("width", encodePx w''),
                  ("height", encodePx h')]
        where
          w'' = h' / h * w'

-- | CSS prefer-color-scheme text snippet
--
-- >>> cssPreferColorScheme (light, dark) PreferHud
-- "svg {\n  color-scheme: light dark;\n}\n{\n  .canvas g, .title g, .axisbar g, .ticktext g, .tickglyph g, .ticklines g, .legendContent g text {\n    fill: rgb(5%, 5%, 5%);\n  }\n  .ticklines g, .tickglyph g, .legendBorder g {\n    stroke: rgb(5%, 5%, 5%);\n  }\n  .legendBorder g {\n    fill: rgb(94%, 94%, 94%);\n  }\n}\n@media (prefers-color-scheme:dark) {\n  .canvas g, .title g, .axisbar g, .ticktext g, .tickglyph g, .ticklines g, .legendContent g text {\n    fill: rgb(94%, 94%, 94%);\n  }\n  .ticklines g, .tickglyph g, .legendBorder g {\n    stroke: rgb(94%, 94%, 94%);\n  }\n  .legendBorder g {\n    fill: rgb(5%, 5%, 5%);\n  }\n}"
cssPreferColorScheme :: (Colour, Colour) -> CssPreferColorScheme -> ByteString
cssPreferColorScheme (cl, cd) PreferHud =
  [i|svg {
  color-scheme: light dark;
}
{
  .canvas g, .title g, .axisbar g, .ticktext g, .tickglyph g, .ticklines g, .legendContent g text {
    fill: #{showRGB cd};
  }
  .ticklines g, .tickglyph g, .legendBorder g {
    stroke: #{showRGB cd};
  }
  .legendBorder g {
    fill: #{showRGB cl};
  }
}
@media (prefers-color-scheme:dark) {
  .canvas g, .title g, .axisbar g, .ticktext g, .tickglyph g, .ticklines g, .legendContent g text {
    fill: #{showRGB cl};
  }
  .ticklines g, .tickglyph g, .legendBorder g {
    stroke: #{showRGB cl};
  }
  .legendBorder g {
    fill: #{showRGB cd};
  }
}|]
cssPreferColorScheme (cl, _) PreferLight =
  [i|svg {
      color-scheme: light dark;
    }
    @media (prefers-color-scheme:dark) {
      markup {
        background-color: #{showRGB cl};
      }
    }|]
cssPreferColorScheme (_, cd) PreferDark =
  [i|svg {
      color-scheme: light dark;
    }
    @media (prefers-color-scheme:light) {
      markup {
        background-color: #{showRGB cd};
      }
    }|]
cssPreferColorScheme _ PreferNormal = mempty

-- | CSS snippet to switch between dark and light mode
--
-- > fillSwitch (color1, color2) "dark" "stuff"
--
-- ... will default to color1 for elements of the "stuff" class, but switch to color2 if "dark" mode is preferred by the user.
fillSwitch :: (Colour, Colour) -> ByteString -> ByteString -> ByteString
fillSwitch (colorNormal, colorPrefer) prefer item =
  [i|
{
  .#{item} g {
    fill: #{showRGB colorNormal};
  }
}
@media (prefers-color-scheme:#{prefer}) {
  .#{item} g {
    fill: #{showRGB colorPrefer};
  }
}
|]

-- | Markup options.
--
-- >>> defaultMarkupOptions
-- MarkupOptions {markupHeight = Just 300.0, chartAspect = FixedAspect 1.5, cssOptions = CssOptions {shapeRendering = NoShapeRendering, preferColorScheme = PreferHud, cssExtra = ""}, renderStyle = Compact}
data MarkupOptions = MarkupOptions
  { markupHeight :: Maybe Double,
    chartAspect :: ChartAspect,
    cssOptions :: CssOptions,
    renderStyle :: RenderStyle
  }
  deriving (Eq, Show, Generic)

-- | The official markup options
defaultMarkupOptions :: MarkupOptions
defaultMarkupOptions = MarkupOptions (Just 300) (FixedAspect 1.5) defaultCssOptions Compact

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
data CssOptions = CssOptions {shapeRendering :: CssShapeRendering, preferColorScheme :: CssPreferColorScheme, cssExtra :: ByteString} deriving (Show, Eq, Generic)

-- | No special shape rendering and default hud responds to user color scheme preferences.
defaultCssOptions :: CssOptions
defaultCssOptions = CssOptions NoShapeRendering PreferHud mempty

-- | Convert CssOptions to Markup
markupCssOptions :: CssOptions -> Markup
markupCssOptions css =
  elementc "style" [] $
    cssPreferColorScheme (light, dark) (view #preferColorScheme css)
      <> markupShapeRendering (view #shapeRendering css)
      <> view #cssExtra css

-- | CSS shape rendering text snippet
markupShapeRendering :: CssShapeRendering -> ByteString
markupShapeRendering UseGeometricPrecision = "svg { shape-rendering: geometricPrecision; }"
markupShapeRendering UseCssCrisp = "svg { shape-rendering: crispEdges; }"
markupShapeRendering NoShapeRendering = mempty

-- | A product type representing charts, hud options and markup options, which can be transformed into 'Markup'.
data ChartOptions = ChartOptions
  { markupOptions :: MarkupOptions,
    hudOptions :: HudOptions,
    charts :: ChartTree
  }
  deriving (Generic, Eq, Show)

-- | Convert ChartOptions to Markup
--
-- >>> markupChartOptions (ChartOptions (defaultMarkupOptions & #cssOptions % #preferColorScheme .~ PreferNormal) mempty mempty) & markdown_ Compact Xml
-- "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"450\" height=\"300\" viewBox=\"-0.75 -0.5 1.5 1.0\"><style></style><g class=\"chart\"></g><g class=\"hud\"></g></svg>"
markupChartOptions :: ChartOptions -> Markup
markupChartOptions co =
  header
    (view (#markupOptions % #markupHeight) co)
    viewbox
    ( markupCssOptions (view (#markupOptions % #cssOptions) co)
        <> markupChartTree csAndHud
    )
  where
    asp = view (#markupOptions % #chartAspect) co
    viewbox = maybe (initialCanvas asp mempty) padSingletons (view styleBox' csAndHud)
    csAndHud = addHud (view #hudOptions co) asp (view #charts co)

-- | Render ChartOptions to an SVG ByteString
--
-- >>> encodeChartOptions (ChartOptions (defaultMarkupOptions & #cssOptions % #preferColorScheme .~ PreferNormal) mempty mempty)
-- "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"450\" height=\"300\" viewBox=\"-0.75 -0.5 1.5 1.0\"><style></style><g class=\"chart\"></g><g class=\"hud\"></g></svg>"
encodeChartOptions :: ChartOptions -> ByteString
encodeChartOptions co = markdown_ (view (#markupOptions % #renderStyle) co) Xml $ markupChartOptions co

-- | Render ChartOptions to an SVG Text snippet
--
-- >>> renderChartOptions (ChartOptions (defaultMarkupOptions & #cssOptions % #preferColorScheme .~ PreferNormal) mempty mempty)
-- "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"450\" height=\"300\" viewBox=\"-0.75 -0.5 1.5 1.0\"><style></style><g class=\"chart\"></g><g class=\"hud\"></g></svg>"
renderChartOptions :: ChartOptions -> Text
renderChartOptions = decodeUtf8 . encodeChartOptions

instance Semigroup ChartOptions where
  (<>) (ChartOptions _ h c) (ChartOptions s' h' c') =
    ChartOptions s' (h <> h') (c <> c')

instance Monoid ChartOptions where
  mempty = ChartOptions defaultMarkupOptions mempty mempty

-- | Convert ChartOptions to an SVG ByteString and save to a file
writeChartOptions :: FilePath -> ChartOptions -> IO ()
writeChartOptions fp co = Data.ByteString.writeFile fp (encodeChartOptions co)

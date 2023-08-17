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
import Data.ByteString.Char8 (pack)
import Data.Colour
import Data.FormatN
import Data.Maybe
import Data.Path
import Data.Path.Parser
import Data.String.Interpolate
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Tree (Tree (..))
import GHC.Generics
import Optics.Core hiding (element)
import Prelude
import MarkupParse
import FlatParse.Basic (utf8ToStr)

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
encodePx = pack . show . (floor :: Double -> Int)

-- | Convert a ChartTree to markup
--
-- >>> lineExample & view #charts & markupChartTree
-- [Node {rootLabel = StartTag "g" [Attr "class" "line"], subForest = [Node {rootLabel = StartTag "g" [Attr "stroke-width" "0.0150",Attr "stroke" "rgb(2%, 73%, 80%)",Attr "stroke-opacity" "1.0",Attr "fill" "none"], subForest = [Node {rootLabel = EmptyElemTag "polyline" [Attr "points" "0,-1.0 1.0,-1.0 2.0,-5.0"], subForest = []}]},Node {rootLabel = StartTag "g" [Attr "stroke-width" "0.0150",Attr "stroke" "rgb(2%, 29%, 48%)",Attr "stroke-opacity" "1.0",Attr "fill" "none"], subForest = [Node {rootLabel = EmptyElemTag "polyline" [Attr "points" "0,0 2.8,-3.0"], subForest = []}]},Node {rootLabel = StartTag "g" [Attr "stroke-width" "0.0150",Attr "stroke" "rgb(66%, 7%, 55%)",Attr "stroke-opacity" "1.0",Attr "fill" "none"], subForest = [Node {rootLabel = EmptyElemTag "polyline" [Attr "points" "0.5,-4.0 0.5,0"], subForest = []}]}]}]
markupChartTree :: ChartTree -> [Tree Token]
markupChartTree cs =
  case (xs', label) of
    ([], Nothing) -> mempty
    (xs'', Nothing) -> xs''
    (xs'', Just l) -> [Node (StartTag "g" [Attr "class" (encodeUtf8 l)]) xs'']
  where
    (ChartTree (Node (label, cs') xs)) = filterChartTree (not . isEmptyChart) cs
    xs' = mconcat $ fmap markupChart cs' <> (markupChartTree . ChartTree <$> xs)

markupText :: TextStyle -> Text -> Point Double -> Tree Token
markupText s t p@(Point x y) = Node (StartTag "text" as) (xs <> [pure (Content c)])
  where
    as =
      uncurry Attr <$>
          [ ("x", encodeNum x),
            ("y", encodeNum $ -y)
          ]
            <> maybeToList ((\x' -> ("transform", toRotateText x' p)) <$> (s ^. #rotation))
    xs = case view #frame s of
      Nothing -> []
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
markupRect :: Rect Double -> Token
markupRect (Rect x z y w) =
  EmptyElemTag "rect" as
  where
    as =
      uncurry Attr <$>
          [ ("width", encodeNum (z - x)),
            ("height", encodeNum (w - y)),
            ("x", encodeNum x),
            ("y", encodeNum (-w))
          ]

-- | Convert a Chart to Markup
--
-- >>> lineExample & view #charts & foldOf charts' & head & markupChart
--[Node {rootLabel = StartTag "g" [Attr "stroke-width" "0.0150",Attr "stroke" "rgb(2%, 73%, 80%)",Attr "stroke-opacity" "1.0",Attr "fill" "none"], subForest = [Node {rootLabel = EmptyElemTag "polyline" [Attr "points" "0,-1.0 1.0,-1.0 2.0,-5.0"], subForest = []}]}]
markupChart :: Chart -> [Tree Token]
markupChart (RectChart s xs) =
  [Node (StartTag "g" (attsRect s)) (pure . markupRect <$> xs)]
markupChart (TextChart s xs) =
  [Node (StartTag "g" (attsText s)) ((uncurry (markupText s)) <$> xs)]
markupChart (GlyphChart s xs) =
  [Node (StartTag "g" (attsGlyph s)) (fmap (markupGlyph s) xs)]
markupChart (PathChart s xs) =
  [Node (StartTag "g" (attsPath s)) [pure $ markupPath xs]]
markupChart (LineChart s xs) =
  [Node (StartTag "g" (attsLine s)) (pure <$> markupLine xs)]
markupChart (BlankChart _) = []

markupLine :: [[Point Double]] -> [Token]
markupLine lss =
  (EmptyElemTag "polyline" . (:[]) . Attr "points" . toPointsText) <$> lss

toPointsText :: [Point Double] -> ByteString
toPointsText xs = intercalate " " $ (\(Point x y) -> encodeNum x <> "," <> encodeNum (-y)) <$> xs

-- | Path markup
markupPath :: [PathData Double] -> Token
markupPath ps =
  (EmptyElemTag "path" [Attr "d" (pathDataToSvg ps)])

-- | GlyphStyle to markup Tree
-- Note rotation on the outside not the inside.
markupGlyph :: GlyphStyle -> Point Double -> Tree Token
markupGlyph s p =
  case view #rotation s of
    Nothing -> pure gl
    Just r -> Node (StartTag "g" [Attr "transform" (toRotateText r p)]) [pure gl]
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
  uncurry Attr <$>
  [ ("stroke-width", encodeNum $ o ^. #borderSize),
      ("stroke", showRGB $ o ^. #borderColor),
      ("stroke-opacity", showOpacity $ o ^. #borderColor),
      ("fill", showRGB $ o ^. #color),
      ("fill-opacity", showOpacity $ o ^. #color)
    ]

-- | TextStyle to [Attr]
attsText :: TextStyle -> [Attr]
attsText o =
  uncurry Attr <$>
  [ ("stroke-width", "0.0"),
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
  uncurry Attr <$>
      [ ("stroke-width", encodeNum sw),
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
  uncurry Attr <$>
      [ ("stroke-width", encodeNum $ o ^. #borderSize),
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
markupShape_ :: GlyphShape -> Double -> Point Double -> Token
markupShape_ CircleGlyph s (Point x y) = EmptyElemTag "circle" as
  where
    as =
      uncurry Attr <$>
          [ ("cx", encodeNum x),
            ("cy", encodeNum $ -y),
            ("r", encodeNum $ 0.5 * s)
          ]
markupShape_ SquareGlyph s p =
  markupRect (move p ((s *) <$> one :: Rect Double))
markupShape_ (RectSharpGlyph x') s p =
  markupRect (move p (scale (Point s (x' * s)) one :: Rect Double))
markupShape_ (RectRoundedGlyph x' rx ry) s p = EmptyElemTag "rect" as
  where
    as =
      uncurry Attr <$>
          [ ("width", encodeNum $ z - x),
            ("height", encodeNum $ w - y),
            ("x", encodeNum x),
            ("y", encodeNum $ -w),
            ("rx", encodeNum rx),
            ("ry", encodeNum ry)
          ]
    (Rect x z y w) = move p (scale (Point s (x' * s)) one)
markupShape_ (TriangleGlyph (Point xa ya) (Point xb yb) (Point xc yc)) s p =
  EmptyElemTag "polygon" as
  where
    as =
      uncurry Attr <$>
          [ ("transform", toTranslateText p),
            ("points", encodeNum (s * xa) <> "," <> encodeNum (-(s * ya)) <> " " <> encodeNum (s * xb) <> "," <> encodeNum (-(s * yb)) <> " " <> encodeNum (s * xc) <> "," <> encodeNum (-(s * yc)))
          ]
markupShape_ (EllipseGlyph x') s (Point x y) =
  EmptyElemTag "ellipse" as
  where
    as =
      uncurry Attr <$>
          [ ("cx", (pack . show) x),
            ("cy", (pack . show) $ -y),
            ("rx", (pack . show) $ 0.5 * s),
            ("ry", (pack . show) $ 0.5 * s * x')
          ]
markupShape_ VLineGlyph s (Point x y) =
  EmptyElemTag "polyline" [Attr "points" $ encodeNum x <> "," <> encodeNum (-(y - s / 2)) <> "\n" <> encodeNum x <> "," <> encodeNum (-(y + s / 2))]
markupShape_ HLineGlyph s (Point x y) =
  EmptyElemTag "polyline" [Attr "points" $ encodeNum (x - s / 2) <> "," <> encodeNum (-y) <> "\n" <> encodeNum (x + s / 2) <> "," <> encodeNum (-y)]
markupShape_ (PathGlyph path _) s p =
  EmptyElemTag "path" (uncurry Attr <$> [("d", path), ("transform", toTranslateText p <> " " <> toScaleText s)])

-- | Create the classic SVG element
--
-- >>> header 100 one [pure (StartTag "foo" [])]
-- Node {rootLabel = StartTag "svg" [Attr "xmlns" "http://www.w3.org/2000/svg",Attr "xmlns:xlink" "http://www.w3.org/1999/xlink",Attr "width" "100",Attr "height" "100",Attr "viewBox" "-0.5 -0.5 1.0 1.0"], subForest = [Node {rootLabel = StartTag "foo" [], subForest = []}]}
header :: Double -> Rect Double -> [Tree Token] -> Tree Token
header markupheight viewbox content' =
  Node (StartTag
    "svg"
    ( uncurry Attr <$>
        [ ("xmlns", "http://www.w3.org/2000/svg"),
          ("xmlns:xlink", "http://www.w3.org/1999/xlink"),
          ("width", encodePx w''),
          ("height", encodePx h'),
          ("viewBox", encodeNum x <> " " <> encodeNum (-w) <> " " <> encodeNum (z - x) <> " " <> encodeNum (w - y))
        ]
    ))
    content'
  where
    (Rect x z y w) = viewbox
    Point w' h = width viewbox
    Point w'' h' = Point (markupheight / h * w') markupheight

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
-- MarkupOptions {markupHeight = 300.0, cssOptions = CssOptions {shapeRendering = NoShapeRendering, preferColorScheme = PreferHud, cssExtra = ""}}
data MarkupOptions = MarkupOptions
  { markupHeight :: Double,
    cssOptions :: CssOptions
  }
  deriving (Eq, Show, Generic)

-- | The official markup options
defaultMarkupOptions :: MarkupOptions
defaultMarkupOptions = MarkupOptions 300 defaultCssOptions

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
--
-- >>> markupCssOptions defaultCssOptions
-- Node {rootLabel = StartTag "style" [], subForest = [Node {rootLabel = Content "svg {\n  color-scheme: light dark;\n}\n{\n  .canvas g, .title g, .axisbar g, .ticktext g, .tickglyph g, .ticklines g, .legendContent g text {\n    fill: rgb(5%, 5%, 5%);\n  }\n  .ticklines g, .tickglyph g, .legendBorder g {\n    stroke: rgb(5%, 5%, 5%);\n  }\n  .legendBorder g {\n    fill: rgb(94%, 94%, 94%);\n  }\n}\n@media (prefers-color-scheme:dark) {\n  .canvas g, .title g, .axisbar g, .ticktext g, .tickglyph g, .ticklines g, .legendContent g text {\n    fill: rgb(94%, 94%, 94%);\n  }\n  .ticklines g, .tickglyph g, .legendBorder g {\n    stroke: rgb(94%, 94%, 94%);\n  }\n  .legendBorder g {\n    fill: rgb(5%, 5%, 5%);\n  }\n}", subForest = []}]}
markupCssOptions :: CssOptions -> Tree Token
markupCssOptions css =
  Node (StartTag "style" [])
    [ pure $ Content $
        cssPreferColorScheme (light, dark) (view #preferColorScheme css)
          <> markupShapeRendering (view #shapeRendering css)
          <> view #cssExtra css
    ]

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
-- >>> markupChartOptions (ChartOptions (defaultMarkupOptions & #cssOptions % #preferColorScheme .~ PreferNormal) mempty mempty)
-- Markup {standard = Xml, markupTree = [Node {rootLabel = StartTag "svg" [Attr "xmlns" "http://www.w3.org/2000/svg",Attr "xmlns:xlink" "http://www.w3.org/1999/xlink",Attr "width" "450",Attr "height" "300",Attr "viewBox" "-0.75 -0.5 1.5 1.0"], subForest = [Node {rootLabel = StartTag "style" [], subForest = [Node {rootLabel = Content "", subForest = []}]},Node {rootLabel = StartTag "g" [Attr "class" "chart"], subForest = []},Node {rootLabel = StartTag "g" [Attr "class" "hud"], subForest = []}]}]}
markupChartOptions :: ChartOptions -> Markup
markupChartOptions co = Markup Xml . (:[]) $
  header
    (view (#markupOptions % #markupHeight) co)
    viewbox
    ( [markupCssOptions (view (#markupOptions % #cssOptions) co)]
        <> markupChartTree csAndHud
    )
  where
    viewbox = singletonGuard (view styleBox' csAndHud)
    csAndHud = addHud (view #hudOptions co) (view #charts co)

-- | Render ChartOptions to an SVG ByteString
--
-- >>> encodeChartOptions (ChartOptions (defaultMarkupOptions & #cssOptions % #preferColorScheme .~ PreferNormal) mempty mempty)
-- "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"450\" height=\"300\" viewBox=\"-0.75 -0.5 1.5 1.0\"><style></style><g class=\"chart\"></g><g class=\"hud\"></g></svg>"
encodeChartOptions :: ChartOptions -> ByteString
encodeChartOptions = markdown Compact . markupChartOptions

-- | Render ChartOptions to an SVG Text snippet
--
-- >>> renderChartOptions (ChartOptions (defaultMarkupOptions & #cssOptions % #preferColorScheme .~ PreferNormal) mempty mempty)
-- "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"450\" height=\"300\" viewBox=\"-0.75 -0.5 1.5 1.0\"><style></style><g class=\"chart\"></g><g class=\"hud\"></g></svg>"
renderChartOptions :: ChartOptions -> Text
renderChartOptions = Text.pack . utf8ToStr . markdown Compact . markupChartOptions

instance Semigroup ChartOptions where
  (<>) (ChartOptions _ h c) (ChartOptions s' h' c') =
    ChartOptions s' (h <> h') (c <> c')

instance Monoid ChartOptions where
  mempty = ChartOptions defaultMarkupOptions mempty mempty

-- | Convert ChartOptions to an SVG ByteString and save to a file
writeChartOptions :: FilePath -> ChartOptions -> IO ()
writeChartOptions fp co = Data.ByteString.writeFile fp (encodeChartOptions co)

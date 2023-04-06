{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

-- | An intermediary representation not unlike SVG or XML but only forming a subset of these standards.
module Chart.Markup
  ( Attributes (..),
    attribute,
    Markup (..),
    Content (..),
    renderMarkup,
    encodeMarkup,
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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Path
import Data.Path.Parser
import Data.String.Interpolate
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Tree (Tree (..))
import Data.TreeDiff
import GHC.Generics
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

-- | A collection of attributes as a ByteString key-value map.
newtype Attributes = Attributes {attMap :: Map ByteString ByteString} deriving (Eq, Show, Generic)

instance ToExpr Attributes

-- Like Last for most attributes but concatenates the "class" attribute.
instance Semigroup Attributes where
  (<>) (Attributes m) (Attributes m') =
    Attributes $
      Map.unionWithKey
        ( \k a b ->
            case k of
              "class" -> a <> " " <> b
              _ -> b
        )
        m
        m'

instance Monoid Attributes where
  mempty = Attributes Map.empty

-- | Create a singleton Attributes
attribute :: (ByteString, ByteString) -> Attributes
attribute (k, v) = Attributes $ Map.singleton k v

-- | A representation of SVG (and XML) markup with no specific knowledge of SVG or XML syntax rules.
--
-- >>> let c0 = ChartOptions (defaultMarkupOptions & #cssOptions % #preferColorScheme .~ PreferNormal) mempty mempty
-- >>> markupChartOptions c0
-- Markup {tag = "svg", atts = Attributes {attMap = fromList [("height","300"),("viewBox","-0.75 -0.5 1.5 1.0"),("width","450"),("xmlns","http://www.w3.org/2000/svg"),("xmlns:xlink","http://www.w3.org/1999/xlink")]}, contents = [MarkupLeaf (Markup {tag = "style", atts = Attributes {attMap = fromList []}, contents = [Content ""]}),MarkupLeaf (Markup {tag = "g", atts = Attributes {attMap = fromList [("class","chart")]}, contents = []}),MarkupLeaf (Markup {tag = "g", atts = Attributes {attMap = fromList [("class","hud")]}, contents = []})]}
data Markup = Markup
  { tag :: ByteString,
    atts :: Attributes,
    contents :: [Content]
  }
  deriving (Eq, Show, Generic)

instance ToExpr Markup

-- | The things that can be inside (form the Content of) a Markup element, especially in a DOM context. Comments are unused by the library representation of a chart and are here to help with parsing arbitrary svg in the wild.
--
-- >>> contents (markupChartOptions c0)
-- [MarkupLeaf (Markup {tag = "style", atts = Attributes {attMap = fromList []}, contents = [Content ""]}),MarkupLeaf (Markup {tag = "g", atts = Attributes {attMap = fromList [("class","chart")]}, contents = []}),MarkupLeaf (Markup {tag = "g", atts = Attributes {attMap = fromList [("class","hud")]}, contents = []})]
data Content = Content ByteString | Comment ByteString | MarkupLeaf Markup deriving (Eq, Show, Generic)

instance ToExpr Content

-- | render markup to Text compliant with being an SVG object (and XML element)
--
-- >>> renderMarkup (markupChartOptions c0)
-- "<svg height=\"300\" viewBox=\"-0.75 -0.5 1.5 1.0\" width=\"450\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"><style></style><g class=\"chart\"/><g class=\"hud\"/></svg>"
renderMarkup :: Markup -> Text
renderMarkup (Markup n as xs) =
  bool [i|<#{na}>#{ls}</#{n}>|] [i|<#{na}/>|] (xs == mempty)
  where
    na = intercalate " " ([n] <> (uncurry encodeAttribute <$> Map.toList (attMap as)))
    ls = mconcat (encodeContent <$> xs)

-- | render markup to a ByteString compliant with being an SVG object (and XML element)
--
-- >>> encodeMarkup (markupChartOptions c0)
-- "<svg height=\"300\" viewBox=\"-0.75 -0.5 1.5 1.0\" width=\"450\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"><style></style><g class=\"chart\"/><g class=\"hud\"/></svg>"
encodeMarkup :: Markup -> ByteString
encodeMarkup (Markup n as xs) =
  bool [i|<#{na}>#{ls}</#{n}>|] [i|<#{na}/>|] (xs == mempty)
  where
    na = intercalate " " ([n] <> (uncurry encodeAttribute <$> Map.toList (attMap as)))
    ls = mconcat (encodeContent <$> xs)

encodeContent :: Content -> ByteString
encodeContent (Content c) = c
encodeContent (Comment c) = encodeComment c
encodeContent (MarkupLeaf x) = encodeMarkup x

encodeComment :: ByteString -> ByteString
encodeComment c = "<!--" <> c <> "-->"

encodeAttribute :: ByteString -> ByteString -> ByteString
encodeAttribute a b = [i|#{a}="#{b}"|]

-- | Convert a ChartTree to markup
--
-- >>> lineExample & view #charts & markupChartTree
-- [Markup {tag = "g", atts = Attributes {attMap = fromList [("class","line")]}, contents = [MarkupLeaf (Markup {tag = "g", atts = Attributes {attMap = fromList [("fill","none"),("stroke","rgb(2%, 73%, 80%)"),("stroke-opacity","1.0"),("stroke-width","0.0150")]}, contents = [MarkupLeaf (Markup {tag = "polyline", atts = Attributes {attMap = fromList [("points","0,-1.0 1.0,-1.0 2.0,-5.0")]}, contents = []})]}),MarkupLeaf (Markup {tag = "g", atts = Attributes {attMap = fromList [("fill","none"),("stroke","rgb(2%, 29%, 48%)"),("stroke-opacity","1.0"),("stroke-width","0.0150")]}, contents = [MarkupLeaf (Markup {tag = "polyline", atts = Attributes {attMap = fromList [("points","0,0 2.8,-3.0")]}, contents = []})]}),MarkupLeaf (Markup {tag = "g", atts = Attributes {attMap = fromList [("fill","none"),("stroke","rgb(66%, 7%, 55%)"),("stroke-opacity","1.0"),("stroke-width","0.0150")]}, contents = [MarkupLeaf (Markup {tag = "polyline", atts = Attributes {attMap = fromList [("points","0.5,-4.0 0.5,0")]}, contents = []})]})]}]
markupChartTree :: ChartTree -> [Markup]
markupChartTree cs =
  case (xs', label) of
    ([], Nothing) -> mempty
    (xs'', Nothing) -> xs''
    (xs'', Just l) -> [Markup "g" (Attributes . Map.singleton "class" . encodeUtf8 $ l) (MarkupLeaf <$> xs'')]
  where
    (ChartTree (Node (label, cs') xs)) = filterChartTree (not . isEmptyChart) cs
    xs' = mapMaybe markupChart cs' <> (mconcat $ markupChartTree . ChartTree <$> xs)

markupText :: TextStyle -> Text -> Point Double -> Markup
markupText s t p@(Point x y) = Markup "text" as ((MarkupLeaf <$> xs) <> [Content c])
  where
    as =
      Attributes $
        Map.fromList $
          [ ("x", encodeNum x),
            ("y", encodeNum $ -y)
          ]
            <> maybeToList ((\x' -> ("transform", toRotateText x' p)) <$> (s ^. #rotation))
    xs = case view #frame s of
      Nothing -> []
      Just f -> maybeToList $ markupChart (RectChart (f & over #borderSize (* view #size s)) [styleBoxText s t p])
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
  Markup "rect" as mempty
  where
    as =
      Attributes $
        Map.fromList
          [ ("width", encodeNum (z - x)),
            ("height", encodeNum (w - y)),
            ("x", encodeNum x),
            ("y", encodeNum (-w))
          ]

-- | Convert a Chart to Markup
--
-- >>> lineExample & view #charts & foldOf charts' & head & markupChart
-- Just (Markup {tag = "g", atts = Attributes {attMap = fromList [("fill","none"),("stroke","rgb(2%, 73%, 80%)"),("stroke-opacity","1.0"),("stroke-width","0.0150")]}, contents = [MarkupLeaf (Markup {tag = "polyline", atts = Attributes {attMap = fromList [("points","0,-1.0 1.0,-1.0 2.0,-5.0")]}, contents = []})]})
markupChart :: Chart -> Maybe Markup
markupChart (RectChart s xs) = Just $ Markup "g" (attsRect s) (MarkupLeaf . markupRect <$> xs)
markupChart (TextChart s xs) = Just $ Markup "g" (attsText s) (MarkupLeaf . uncurry (markupText s) <$> xs)
markupChart (GlyphChart s xs) = Just $ Markup "g" (attsGlyph s) (MarkupLeaf <$> fmap (markupGlyph s) xs)
markupChart (PathChart s xs) = Just $ Markup "g" (attsPath s) [MarkupLeaf $ markupPath xs]
markupChart (LineChart s xs) = Just $ Markup "g" (attsLine s) (MarkupLeaf <$> markupLine xs)
markupChart (BlankChart _) = Nothing

markupLine :: [[Point Double]] -> [Markup]
markupLine lss =
  fmap (($ mempty) . Markup "polyline" . attribute . ("points",) . toPointsText) lss

toPointsText :: [Point Double] -> ByteString
toPointsText xs = intercalate " " $ (\(Point x y) -> encodeNum x <> "," <> encodeNum (-y)) <$> xs

-- | Path markup
markupPath :: [PathData Double] -> Markup
markupPath ps =
  Markup "path" (foldMap attribute [("d", pathDataToSvg ps)]) mempty

-- | GlyphStyle to markup Tree
-- Note rotation on the outside not the inside.
markupGlyph :: GlyphStyle -> Point Double -> Markup
markupGlyph s p =
  case view #rotation s of
    Nothing -> gl
    Just r -> Markup "g" (foldMap attribute [("transform", toRotateText r p)]) [MarkupLeaf gl]
  where
    gl = markupShape_ (s ^. #shape) (s ^. #size) p

-- | Convert a dash representation from a list to text
fromDashArray :: [Double] -> ByteString
fromDashArray xs = intercalate " " $ encodeNum <$> xs

fromDashOffset :: Double -> ByteString
fromDashOffset x = encodeNum x

attsLine :: LineStyle -> Attributes
attsLine o =
  mconcat $
    attribute
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

attsRect :: RectStyle -> Attributes
attsRect o =
  foldMap
    attribute
    [ ("stroke-width", encodeNum $ o ^. #borderSize),
      ("stroke", showRGB $ o ^. #borderColor),
      ("stroke-opacity", showOpacity $ o ^. #borderColor),
      ("fill", showRGB $ o ^. #color),
      ("fill-opacity", showOpacity $ o ^. #color)
    ]

-- | TextStyle to Attributes
attsText :: TextStyle -> Attributes
attsText o =
  Attributes $
    Map.fromList
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

-- | GlyphStyle to Attributes
attsGlyph :: GlyphStyle -> Attributes
attsGlyph o =
  Attributes $
    Map.fromList $
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

-- | PathStyle to Attributes
attsPath :: PathStyle -> Attributes
attsPath o =
  Attributes $
    Map.fromList
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
markupShape_ :: GlyphShape -> Double -> Point Double -> Markup
markupShape_ CircleGlyph s (Point x y) = Markup "circle" as mempty
  where
    as =
      Attributes $
        Map.fromList
          [ ("cx", encodeNum x),
            ("cy", encodeNum $ -y),
            ("r", encodeNum $ 0.5 * s)
          ]
markupShape_ SquareGlyph s p =
  markupRect (move p ((s *) <$> one :: Rect Double))
markupShape_ (RectSharpGlyph x') s p =
  markupRect (move p (scale (Point s (x' * s)) one :: Rect Double))
markupShape_ (RectRoundedGlyph x' rx ry) s p = Markup "rect" as mempty
  where
    as =
      Attributes $
        Map.fromList
          [ ("width", encodeNum $ z - x),
            ("height", encodeNum $ w - y),
            ("x", encodeNum x),
            ("y", encodeNum $ -w),
            ("rx", encodeNum rx),
            ("ry", encodeNum ry)
          ]
    (Rect x z y w) = move p (scale (Point s (x' * s)) one)
markupShape_ (TriangleGlyph (Point xa ya) (Point xb yb) (Point xc yc)) s p =
  Markup "polygon" as mempty
  where
    as =
      Attributes $
        Map.fromList
          [ ("transform", toTranslateText p),
            ("points", encodeNum (s * xa) <> "," <> encodeNum (-(s * ya)) <> " " <> encodeNum (s * xb) <> "," <> encodeNum (-(s * yb)) <> " " <> encodeNum (s * xc) <> "," <> encodeNum (-(s * yc)))
          ]
markupShape_ (EllipseGlyph x') s (Point x y) =
  Markup "ellipse" as mempty
  where
    as =
      Attributes $
        Map.fromList
          [ ("cx", (pack . show) x),
            ("cy", (pack . show) $ -y),
            ("rx", (pack . show) $ 0.5 * s),
            ("ry", (pack . show) $ 0.5 * s * x')
          ]
markupShape_ VLineGlyph s (Point x y) =
  Markup "polyline" (foldMap attribute [("points", encodeNum x <> "," <> encodeNum (-(y - s / 2)) <> "\n" <> encodeNum x <> "," <> encodeNum (-(y + s / 2)))]) mempty
markupShape_ HLineGlyph s (Point x y) =
  Markup "polyline" (foldMap attribute [("points", encodeNum (x - s / 2) <> "," <> encodeNum (-y) <> "\n" <> encodeNum (x + s / 2) <> "," <> encodeNum (-y))]) mempty
markupShape_ (PathGlyph path _) s p =
  Markup "path" (foldMap attribute [("d", path), ("transform", toTranslateText p <> " " <> toScaleText s)]) mempty

-- | Create the classic SVG element
--
-- >>> header 100 one [Markup "foo" mempty mempty]
-- Markup {tag = "svg", atts = Attributes {attMap = fromList [("height","100"),("viewBox","-0.5 -0.5 1.0 1.0"),("width","100"),("xmlns","http://www.w3.org/2000/svg"),("xmlns:xlink","http://www.w3.org/1999/xlink")]}, contents = [MarkupLeaf (Markup {tag = "foo", atts = Attributes {attMap = fromList []}, contents = []})]}
header :: Double -> Rect Double -> [Markup] -> Markup
header markupheight viewbox content' =
  Markup
    "svg"
    ( foldMap
        attribute
        [ ("xmlns", "http://www.w3.org/2000/svg"),
          ("xmlns:xlink", "http://www.w3.org/1999/xlink"),
          ("width", encodePx w''),
          ("height", encodePx h'),
          ("viewBox", encodeNum x <> " " <> encodeNum (-w) <> " " <> encodeNum (z - x) <> " " <> encodeNum (w - y))
        ]
    )
    (MarkupLeaf <$> content')
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
--
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
-- Markup {tag = "style", atts = Attributes {attMap = fromList []}, contents = [Content "svg {\n  color-scheme: light dark;\n}\n{\n  .canvas g, .title g, .axisbar g, .ticktext g, .tickglyph g, .ticklines g, .legendContent g text {\n    fill: rgb(5%, 5%, 5%);\n  }\n  .ticklines g, .tickglyph g, .legendBorder g {\n    stroke: rgb(5%, 5%, 5%);\n  }\n  .legendBorder g {\n    fill: rgb(94%, 94%, 94%);\n  }\n}\n@media (prefers-color-scheme:dark) {\n  .canvas g, .title g, .axisbar g, .ticktext g, .tickglyph g, .ticklines g, .legendContent g text {\n    fill: rgb(94%, 94%, 94%);\n  }\n  .ticklines g, .tickglyph g, .legendBorder g {\n    stroke: rgb(94%, 94%, 94%);\n  }\n  .legendBorder g {\n    fill: rgb(5%, 5%, 5%);\n  }\n}"]}
markupCssOptions :: CssOptions -> Markup
markupCssOptions css =
  Markup
    "style"
    mempty
    [ Content $
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
-- Markup {tag = "svg", atts = Attributes {attMap = fromList [("height","300"),("viewBox","-0.75 -0.5 1.5 1.0"),("width","450"),("xmlns","http://www.w3.org/2000/svg"),("xmlns:xlink","http://www.w3.org/1999/xlink")]}, contents = [MarkupLeaf (Markup {tag = "style", atts = Attributes {attMap = fromList []}, contents = [Content ""]}),MarkupLeaf (Markup {tag = "g", atts = Attributes {attMap = fromList [("class","chart")]}, contents = []}),MarkupLeaf (Markup {tag = "g", atts = Attributes {attMap = fromList [("class","hud")]}, contents = []})]}
markupChartOptions :: ChartOptions -> Markup
markupChartOptions co =
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
-- "<svg height=\"300\" viewBox=\"-0.75 -0.5 1.5 1.0\" width=\"450\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"><style></style><g class=\"chart\"/><g class=\"hud\"/></svg>"
encodeChartOptions :: ChartOptions -> ByteString
encodeChartOptions = encodeMarkup . markupChartOptions

-- | Render ChartOptions to an SVG Text snippet
--
-- >>> renderChartOptions (ChartOptions (defaultMarkupOptions & #cssOptions % #preferColorScheme .~ PreferNormal) mempty mempty)
-- "<svg height=\"300\" viewBox=\"-0.75 -0.5 1.5 1.0\" width=\"450\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"><style></style><g class=\"chart\"/><g class=\"hud\"/></svg>"
renderChartOptions :: ChartOptions -> Text
renderChartOptions = renderMarkup . markupChartOptions

instance Semigroup ChartOptions where
  (<>) (ChartOptions _ h c) (ChartOptions s' h' c') =
    ChartOptions s' (h <> h') (c <> c')

instance Monoid ChartOptions where
  mempty = ChartOptions defaultMarkupOptions mempty mempty

-- | Convert ChartOptions to an SVG ByteString and save to a file
writeChartOptions :: FilePath -> ChartOptions -> IO ()
writeChartOptions fp co = Data.ByteString.writeFile fp (encodeChartOptions co)

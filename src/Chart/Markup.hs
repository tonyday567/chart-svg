{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TupleSections #-}

-- | An intermediary representation not unlike SVG or XML but only forming a subset of these standards.
module Chart.Markup
  ( Attribute (..),
    inject,
    eject,
    Attributes (..),
    singleAtt,
    Markup(..),
    Content(..),
    printMarkup,
    printContent,
    printAttribute,
    ChartOptions(..),
    markupChartOptions,
    markupChartTree,
    markupChart,
    header,
    printChartOptions,
    writeChartOptions,
    CssOptions(..),
    defaultCssOptions,
    CssPreferColorScheme(..),
    cssPreferColorScheme,
    CssShapeRendering(..),
    MarkupOptions(..),
    defaultMarkupOptions,
  ) where


import Chart.Data
import Prelude
import Data.ByteString ( intercalate, ByteString, writeFile )
import GHC.Generics
import Data.String.Interpolate
import Data.ByteString.Char8 (pack)
import Optics.Core hiding (element)
import Data.Maybe
import Data.Text.Encoding (encodeUtf8)
-- import Data.Text hiding (unpack, pack, filter, intercalate, empty)
import Chart.Primitive hiding (tree)
import Chart.Style
import Data.Colour
import Data.Text (Text)
import Data.Path
import Chart.Hud
import Data.Tree (Tree(..))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Bool
import Data.Bifunctor
-- import GHC.Exts
import Data.Path.Parser
import Data.TreeDiff

-- $setup
--
-- >>> :set -XOverloadedLabels
-- >>> :set -XOverloadedStrings
-- >>> import Chart
-- >>> import Optics.Core
-- >>> let c0 = ChartOptions (defaultMarkupOptions & #cssOptions % #preferColorScheme .~ PreferNormal) mempty mempty

-- | Allows class to be concatenated in monadic appends, with:
--
-- -- Class attributes concatenated (as a string of space separated names)
--
-- -- ID treadted as a special Attribute, and
--
-- -- All other attributes treated as the Last semigroup.
--
data Attribute =
  Class |
  ID |
  Attribute ByteString deriving (Eq, Show, Generic, Ord)

instance ToExpr Attribute

-- | convert a ByteString to an Attribute tag
inject :: ByteString -> Attribute
inject "class" = Class
inject "id" = ID
inject a = Attribute a

-- | convert an Attribute to a ByteString
eject :: Attribute -> ByteString
eject Class = "class"
eject ID = "id"
eject (Attribute a) = a

newtype Attributes = Attributes { attMap :: Map Attribute ByteString } deriving (Eq, Show, Generic)

instance ToExpr Attributes

instance Semigroup Attributes
  where
    (<>) (Attributes m) (Attributes m') =
      Attributes $ Map.unionWithKey
      (\k a b ->
        case k of
          Class -> a <> " " <> b
          ID -> b
          Attribute _ -> b

      ) m m'

instance Monoid Attributes
  where
    mempty = Attributes Map.empty

singleAtt :: (ByteString, ByteString) -> Attributes
singleAtt ("class",b) = Attributes $ Map.singleton Class b
singleAtt ("id",b) = Attributes $ Map.singleton ID b
singleAtt (a,b) = Attributes $ Map.singleton (Attribute a) b

-- | A representation of SVG (and XML) markup with no specific knowledge of SVG or XML syntax rules.
--
-- >>> markupChartOptions c0
--
data Markup = Markup {
  tag :: ByteString,
  atts :: Attributes,
  contents :: [Content]
  } deriving (Eq, Show, Generic)

instance ToExpr Markup

-- | The things that can be inside (form the Content of) a Markup element, especially in a DOM context. Comments are unused by the library representation of a chart and are here to help with parsing arbitrary svg in the wild.
--
-- >>> contents (markupChartOptions c0)
data Content = Content ByteString | Comment ByteString | MarkupLeaf Markup deriving (Eq, Show, Generic)

instance ToExpr Content

-- | render markup to a ByteString compliant with an SVG object (and XML element)
--
-- >>> printMarkup (markupChartOptions c0)
printMarkup :: Markup -> ByteString
printMarkup (Markup n as xs) =
  bool [i|<#{na}>#{ls}</#{n}>|] [i|<#{na}/>|] (xs==mempty)
    where
      na = intercalate " " ([n] <> (uncurry printAttribute <$> Map.toList (attMap as)))
      ls = mconcat (printContent <$> xs)

-- | render Content to SVG
--
-- >>> printContent <$> contents $ markupChartOptions c0)
printContent :: Content -> ByteString
printContent (Content c) = c
printContent (Comment c) = printComment c
printContent (MarkupLeaf x) = printMarkup x

-- | Render a comment
--
-- >>> printComment "comment"
-- "<!--comment-->"
printComment :: ByteString -> ByteString
printComment c = "<!--" <> c <> "-->"

-- | render an Attribute with a value
--
-- >>> printAttribute Class "a"
-- "class = \"a\""
printAttribute :: Attribute -> ByteString -> ByteString
printAttribute a b = [i|#{eject a}="#{b}"|]

-- | Convert a ChartTree to markup
--
-- >>> lineExample & view #charts & markupChartTree
--
markupChartTree :: ChartTree -> [Markup]
markupChartTree cs =
  case (xs', label) of
    ([], Nothing) -> mempty
    (xs'', Nothing) -> xs''
    (xs'', Just l) -> [Markup "g" (Attributes . Map.singleton Class . encodeUtf8 $ l) (MarkupLeaf <$> xs'')]
    where
      (ChartTree (Node (label, cs') xs)) = filterChartTree (not . isEmptyChart) cs
      xs' = mapMaybe markupChart cs' <> (mconcat $ markupChartTree . ChartTree <$> xs)

-- | Text markup
--
-- >>> markupText defaultTextStyle "example" (Point 0 0)
--
markupText :: TextStyle -> Text -> Point Double -> Markup
markupText s t p@(Point x y) = Markup "text" as ((MarkupLeaf <$> xs) <> [Content c])
  where
    as = Attributes $ Map.fromList $
      [ (Attribute "x", pack $ show x),
        (Attribute "y", pack $ show $ -y)
      ] <>
      maybeToList ((\x' -> (Attribute "transform", toRotateText x' p)) <$> (s ^. #rotation))
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
--
-- >>> toRotateText 0.5 (Point 0 1)
toRotateText :: Double -> Point Double -> ByteString
toRotateText r (Point x y) =
    pack $ "rotate(" <> show (-r * 180 / pi) <> ", " <> show x <> ", " <> show (-y) <> ")"

toScaleText :: Double -> ByteString
toScaleText x =
  pack $
    "scale(" <> show x <> ")"

-- | Convert a Rect to Markup
--
-- >>> markupRect one
--
markupRect :: (Show a, Num a) => Rect a -> Markup
markupRect (Rect x z y w) =
    Markup "rect" as mempty
    where
      as = Attributes $ Map.fromList $ first Attribute <$>
        [ ("width", pack $ show $ z - x),
          ("height", pack $ show $ w - y),
          ("x", pack $ show x),
          ("y", pack $ show $ -w)
        ]

-- | Convert a Chart to Markup
--
-- >>> lineExample & view #charts & markupChart
markupChart :: Chart -> Maybe Markup
markupChart (RectChart s xs) = Just $ Markup "g" (attsRect s) (MarkupLeaf . markupRect <$> xs)
markupChart (TextChart s xs) = Just $ Markup "g" (attsText s) (MarkupLeaf . uncurry (markupText s) <$> xs)
markupChart (GlyphChart s xs) = Just $ Markup "g" (attsGlyph s) (MarkupLeaf <$> fmap (markupGlyph s) xs)
markupChart (PathChart s xs) = Just $ Markup "g" (attsPath s) [MarkupLeaf $ markupPath xs]
markupChart (LineChart s xs) = Just $ Markup "g" (attsLine s) (MarkupLeaf <$> markupLine xs)
markupChart (BlankChart _) = Nothing

markupLine :: [[Point Double]] -> [Markup]
markupLine lss =
  fmap (($ mempty) . Markup "polyline" . singleAtt . ("points",) . toPointsText) lss

toPointsText :: [Point Double] -> ByteString
toPointsText xs = intercalate " " $ (\(Point x y) -> pack (show x <> "," <> show (-y))) <$> xs

-- | Path markup
--
-- >>> markupPath $ toPathDatas [MoveTo OriginAbsolute [Point (-1.0) 0.5],EllipticalArc OriginAbsolute [(0.5,0.5,0.0,True,True,Point 0.0 (-1.2320508075688774)),(1.0,1.0,0.0,False,False,Point (-0.5) (-0.3660254037844387)),(1.0,1.0,0.0,False,False,Point (-1.0) 0.5)],EndPath]
-- Markup {tag = "path", atts = Attributes {attMap = fromList [(Attribute "d","M -1.0,0.5 A 0.5 0.5 -0.0 1 1 0,-1.2321 A 1.0 1.0 -0.0 0 0 -0.5,-0.3660 A 1.0 1.0 -0.0 0 0 -1.0,0.5 L -1.0,0.5")]}, contents = []}
markupPath :: [PathData Double] -> Markup
markupPath ps =
  Markup "path" (foldMap singleAtt [("d", pathDataToSvg ps)]) mempty

-- | GlyphStyle to markup Tree
-- Note rotation on the outside not the inside.
markupGlyph :: GlyphStyle -> Point Double -> Markup
markupGlyph s p =
  case view #rotation s of
    Nothing -> gl
    Just r -> Markup "g" (foldMap singleAtt [("transform", toRotateText r p)]) [MarkupLeaf gl]
  where
    gl = markupShape_ (s ^. #shape) (s ^. #size) p

-- | Convert a dash representation from a list to text
fromDashArray :: [Double] -> ByteString
fromDashArray xs = intercalate " " $ pack . show <$> xs

fromDashOffset :: Double -> ByteString
fromDashOffset x = pack (show x)

attsLine :: LineStyle -> Attributes
attsLine o = mconcat $ singleAtt <$>
  [ ("stroke-width", pack $ show $ o ^. #size),
    ("stroke", showRGBA $ o ^. #color),
    ("fill", "none")
  ] <>
  catMaybes
  [(\x -> ("stroke-linecap", fromLineCap x)) <$> (o ^. #linecap)]
    <> foldMap (\x -> [("stroke-linejoin", fromLineJoin x)]) (o ^. #linejoin)
    <> foldMap (\x -> [("stroke-dasharray", fromDashArray x)]) (o ^. #dasharray)
    <> foldMap (\x -> [("stroke-dashoffset", fromDashOffset x)]) (o ^. #dashoffset)

attsRect :: RectStyle -> Attributes
attsRect o = foldMap singleAtt
  [ ("stroke-width", pack $ show $ o ^. #borderSize),
    ("stroke", showRGBA $ o ^. #borderColor),
    ("fill", showRGBA $ o ^. #color)
  ]

-- | TextStyle to Attributes
attsText :: TextStyle -> Attributes
attsText o = Attributes $ Map.fromList $ fmap (first Attribute)
  [ ("stroke-width","0.0"),
    ("stroke", "none"),
    ("fill", showRGBA $ o ^. #color),
    ("font-size", pack $ show $ o ^. #size),
    ("text-anchor", toTextAnchor $ o ^. #anchor)
  ]
  where
    toTextAnchor :: Anchor -> ByteString
    toTextAnchor AnchorMiddle = "middle"
    toTextAnchor AnchorStart = "start"
    toTextAnchor AnchorEnd = "end"

-- | GlyphStyle to Attributes
attsGlyph :: GlyphStyle -> Attributes
attsGlyph o = Attributes $ Map.fromList $ fmap (first Attribute) $
  [ ("stroke-width", pack $ show sw),
    ("stroke", showRGBA $ o ^. #borderColor),
    ("fill", showRGBA $ o ^. #color)
  ]
  <> foldMap ((: []) . (,) "transform" . toTranslateText) (o ^. #translate)
  where
    sw = case o ^. #shape of
      PathGlyph _ NoScaleBorder -> o ^. #borderSize
      PathGlyph _ ScaleBorder -> min 0.2 (o ^. #borderSize / o ^. #size)
      _ -> o ^. #borderSize

-- | PathStyle to Attributes
attsPath :: PathStyle -> Attributes
attsPath o = Attributes $ Map.fromList $ fmap (first Attribute)
  [ ("stroke-width", pack $ show $ o ^. #borderSize),
    ("stroke", showRGBA $ o ^. #borderColor),
    ("fill", showRGBA $ o ^. #color)
  ]

-- | includes a flip of the y dimension.
toTranslateText :: Point Double -> ByteString
toTranslateText (Point x y) =
  pack $
    "translate(" <> show x <> ", " <> show (-y) <> ")"

-- | GlyphShape to markup Tree
markupShape_ :: GlyphShape -> Double -> Point Double -> Markup
markupShape_ CircleGlyph s (Point x y) = Markup "circle" as mempty where
  as = Attributes $ Map.fromList  $ fmap (first Attribute)
    [ ("cx", pack $ show x),
      ("cy", pack $ show $ -y),
      ("r", pack $ show $ 0.5 * s)
    ]
markupShape_ SquareGlyph s p =
  markupRect (move p ((s *) <$> one :: Rect Double))
markupShape_ (RectSharpGlyph x') s p =
  markupRect (move p (scale (Point s (x' * s)) one :: Rect Double))
markupShape_ (RectRoundedGlyph x' rx ry) s p = Markup "rect" as mempty
  where
    as = Attributes $ Map.fromList $ fmap (first Attribute)
      [ ("width", pack $ show $ z - x),
        ("height", pack $ show $ w - y),
        ("x", pack $ show x),
        ("y", pack $ show $ -w),
        ("rx", pack $ show rx),
        ("ry", pack $ show ry)
      ]
    (Rect x z y w) = move p (scale (Point s (x' * s)) one)
markupShape_ (TriangleGlyph (Point xa ya) (Point xb yb) (Point xc yc)) s p =
  Markup "polygon" as mempty
  where
    as = Attributes $ Map.fromList $ fmap (first Attribute)
      [ ("transform", toTranslateText p),
        ("points", pack $ show (s * xa) <> "," <> show (-(s * ya)) <> " " <> show (s * xb) <> "," <> show (-(s * yb)) <> " " <> show (s * xc) <> "," <> show (-(s * yc)))
      ]
markupShape_ (EllipseGlyph x') s (Point x y) =
  Markup "ellipse" as mempty
  where
    as = Attributes $ Map.fromList $ fmap (first Attribute)
      [ ("cx", (pack . show) x),
        ("cy", (pack . show) $ -y),
        ("rx", (pack . show) $ 0.5 * s),
        ("ry", (pack . show) $ 0.5 * s * x')
      ]
markupShape_ VLineGlyph s (Point x y) =
  Markup "polyline" (foldMap singleAtt [("points", pack $ show x <> "," <> show (-(y - s / 2)) <> "\n" <> show x <> "," <> show (-(y + s / 2)))]) mempty
markupShape_ HLineGlyph s (Point x y) =
  Markup "polyline" (foldMap singleAtt [("points", pack $ show (x - s / 2) <> "," <> show (-y) <> "\n" <> show (x + s / 2) <> "," <> show (-y))]) mempty
markupShape_ (PathGlyph path _) s p =
  Markup "path" (foldMap singleAtt [("d", path), ("transform", toTranslateText p <> " " <> toScaleText s)]) mempty

-- | Create the classic SVG element
--
-- >>> header 100 one [Markup "foo" mempty mempty]
--
header :: Double -> Rect Double -> [Markup] -> Markup
header markupheight viewbox content' =
  Markup
  "svg"
   (foldMap singleAtt
    [("xmlns", "http://www.w3.org/2000/svg"),
     ("xmlns:xlink", "http://www.w3.org/1999/xlink"),
     ("width", pack $ show w''),
     ("height", pack $ show h'),
     ("viewBox", pack $ show x <> " " <> show (-w) <> " " <> show (z - x) <> " " <> show (w - y))
    ]) (MarkupLeaf <$> content')
  where
    (Rect x z y w) = viewbox
    Point w' h = width viewbox
    Point w'' h' = Point (markupheight / h * w') markupheight

-- | CSS prefer-color-scheme text snippet
--
-- >>> cssPreferColorScheme (light, dark) PreferHud
--
cssPreferColorScheme :: (Colour, Colour) -> CssPreferColorScheme -> ByteString
cssPreferColorScheme (cl, cd) PreferHud =
  [i|svg {
  color-scheme: light dark;
}
{
  .canvas g, .title g, .axisbar g, .ticktext g, .tickglyph g, .ticklines g, .legendContent g text {
    fill: #{showRGBA cd};
  }
  .ticklines g, .tickglyph g, .legendBorder g {
    stroke: #{showRGBA cd};
  }
  .legendBorder g {
    fill: #{showRGBA cl};
  }
}
@media (prefers-color-scheme:dark) {
  .canvas g, .title g, .axisbar g, .ticktext g, .tickglyph g, .ticklines g, .legendContent g text {
    fill: #{showRGBA cl};
  }
  .ticklines g, .tickglyph g, .legendBorder g {
    stroke: #{showRGBA cl};
  }
  .legendBorder g {
    fill: #{showRGBA cd};
  }
}|]
cssPreferColorScheme (cl, _) PreferLight =
  [i|svg {
      color-scheme: light dark;
    }
    @media (prefers-color-scheme:dark) {
      markup {
        background-color: #{showRGBA cl};
      }
    }|]
cssPreferColorScheme (_, cd) PreferDark =
  [i|svg {
      color-scheme: light dark;
    }
    @media (prefers-color-scheme:light) {
      markup {
        background-color: #{showRGBA cd};
      }
    }|]
cssPreferColorScheme _ PreferNormal = mempty

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
--
markupCssOptions :: CssOptions -> Markup
markupCssOptions css = Markup "style" mempty
  [Content $
   cssPreferColorScheme (light, dark) (view #preferColorScheme css) <>
   markupShapeRendering (view #shapeRendering css) <>
   view #cssExtra css
  ]

-- | CSS shape rendering text snippet
markupShapeRendering :: CssShapeRendering -> ByteString
markupShapeRendering UseGeometricPrecision = "svg { shape-rendering: geometricPrecision; }"
markupShapeRendering UseCssCrisp = "svg { shape-rendering: crispEdges; }"
markupShapeRendering NoShapeRendering = mempty

-- | A sum type representing charts, hud options and markup options, which can be transformed into SVG.
--
-- >>> unitExample
--
data ChartOptions = ChartOptions {
  markupOptions :: MarkupOptions,
  hudOptions :: HudOptions,
  charts :: ChartTree } deriving (Generic, Eq, Show)

-- | Convert ChartOptions to Markup
--
-- >>> markupChartOptions (ChartOptions (defaultMarkupOptions & #cssOptions % #preferColorScheme .~ PreferNormal) mempty mempty)
-- Markup {tag = "svg", atts = Attributes {attMap = fromList [(Attribute "height","300.0"),(Attribute "viewBox","-0.75 -0.5 1.5 1.0"),(Attribute "width","450.0"),(Attribute "xmlns","http://www.w3.org/2000/svg"),(Attribute "xmlns:xlink","http://www.w3.org/1999/xlink")]}, contents = [MarkupLeaf (Markup {tag = "style", atts = Attributes {attMap = fromList []}, contents = [Content ""]}),MarkupLeaf (Markup {tag = "g", atts = Attributes {attMap = fromList [(Class,"chart")]}, contents = []}),MarkupLeaf (Markup {tag = "g", atts = Attributes {attMap = fromList [(Class,"hud")]}, contents = []})]}
markupChartOptions :: ChartOptions -> Markup
markupChartOptions co = header (view (#markupOptions % #markupHeight) co) viewbox
  ([markupCssOptions (view (#markupOptions % #cssOptions) co)] <>
   markupChartTree csAndHud)
  where
    viewbox = singletonGuard (view styleBox' csAndHud)
    csAndHud = addHud (view #hudOptions co) (view #charts co)

-- | Render ChartOptions to an SVG ByteString
--
-- >>> printChartOptions (ChartOptions (defaultMarkupOptions & #cssOptions % #preferColorScheme .~ PreferNormal) mempty mempty)
-- Markup {tag = "svg", atts = Attributes {attMap = fromList [(Attribute "height","300.0"),(Attribute "viewBox","-0.75 -0.5 1.5 1.0"),(Attribute "width","450.0"),(Attribute "xmlns","http://www.w3.org/2000/svg"),(Attribute "xmlns:xlink","http://www.w3.org/1999/xlink")]}, contents = [MarkupLeaf (Markup {tag = "style", atts = Attributes {attMap = fromList []}, contents = [Content ""]}),MarkupLeaf (Markup {tag = "g", atts = Attributes {attMap = fromList [(Class,"chart")]}, contents = []}),MarkupLeaf (Markup {tag = "g", atts = Attributes {attMap = fromList [(Class,"hud")]}, contents = []})]}
printChartOptions :: ChartOptions -> ByteString
printChartOptions = printMarkup . markupChartOptions

instance Semigroup ChartOptions where
  (<>) (ChartOptions _ h c) (ChartOptions s' h' c') =
    ChartOptions s' (h <> h') (c <> c')

instance Monoid ChartOptions where
  mempty = ChartOptions defaultMarkupOptions mempty mempty

-- | Convert ChartOptions to an SVG ByteString and save to a file
writeChartOptions :: FilePath -> ChartOptions -> IO ()
writeChartOptions fp co = Data.ByteString.writeFile fp (printChartOptions co)

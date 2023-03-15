{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use unwords" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE TupleSections #-}
{-# HLINT ignore "Use foldMap" #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# HLINT ignore "Use isAsciiLower" #-}
{-# HLINT ignore "Use isAsciiUpper" #-}
{-# LANGUAGE MagicHash #-}
{-# HLINT ignore "Use isDigit" #-}
{-# HLINT ignore "Use void" #-}
{-# HLINT ignore "Use <$" #-}
{-# HLINT ignore "Use infix" #-}
-- https://www.w3.org/TR/SVG2/text.html#TextElement

module Chart.Markup where

import Prelude
import Data.ByteString ( intercalate, ByteString, writeFile )
import GHC.Generics
import Data.String.Interpolate
import Chart.Data hiding (Element)
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

-- $setup
--
-- >>> :set -XOverloadedLabels
-- >>> :set -XOverloadedStrings
-- >>> import Chart
-- >>> import Optics.Core

-- | Allows class to be concatenated in monadic appends.
--
-- Isolates ID as special.
data Attribute =
  Class |
  ID |
  Attribute ByteString deriving (Eq, Show, Generic, Ord)

newtype Attributes = Attributes { attMap :: Map Attribute ByteString } deriving (Eq, Show, Generic)

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

-- TODO: refactor singleton out
inject :: (ByteString, ByteString) -> Attributes
inject ("class",b) = Attributes $ Map.singleton Class b
inject ("id",b) = Attributes $ Map.singleton ID b
inject (a,b) = Attributes $ Map.singleton (Attribute a) b

eject :: Attribute -> ByteString
eject Class = "class"
eject ID = "id"
eject (Attribute a) = a

data Content = Content ByteString | CDSect ByteString | Comment ByteString | MarkupLeaf Markup deriving (Eq, Show, Generic)

data Markup = Markup {
  tag :: ByteString,
  atts :: Attributes,
  contents :: [Content]
  } deriving (Eq, Show, Generic)

-- * printing markup to bytestring

printMarkup :: Markup -> ByteString
printMarkup (Markup n as xs) =
  bool [i|<#{na}>#{ls}</#{n}>|] [i|<#{na}/>|] (xs==mempty)
    where
      na = intercalate " " ([n] <> (uncurry printAttribute <$> Map.toList (attMap as)))
      ls = mconcat (printContent <$> xs)

printContent :: Content -> ByteString
printContent (Content c) = c
printContent (MarkupLeaf x) = printMarkup x

printAttribute :: Attribute -> ByteString -> ByteString
printAttribute a b = [i|#{eject a}="#{b}"|]

-- * conversion to markup

data ChartOptions = ChartOptions {
  markupOptions :: MarkupOptions,
  hudOptions :: HudOptions,
  charts :: ChartTree } deriving (Generic, Eq, Show)

-- | Text markup
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

-- | includes reference changes:
--
-- - from radians to degrees
--
-- - from counter-clockwise is a positive rotation to clockwise is positive
--
-- - flip y dimension
toRotateText :: Double -> Point Double -> ByteString
toRotateText r (Point x y) =
    pack $ "rotate(" <> show (-r * 180 / pi) <> ", " <> show x <> ", " <> show (-y) <> ")"

toScaleText :: Double -> ByteString
toScaleText x =
  pack $
    "scale(" <> show x <> ")"

markupRect (Rect x z y w) =
    Markup "rect" as mempty
    where
      as = Attributes $ Map.fromList $ first Attribute <$>
        [ ("width", pack $ show $ z - x),
          ("height", pack $ show $ w - y),
          ("x", pack $ show x),
          ("y", pack $ show $ -w)
        ]

markupChart (RectChart s xs) = Just $ Markup "g" (attsRect s) (MarkupLeaf . markupRect <$> xs)
markupChart (TextChart s xs) = Just $ Markup "g" (attsText s) (MarkupLeaf . uncurry (markupText s) <$> xs)
markupChart (GlyphChart s xs) = Just $ Markup "g" (attsGlyph s) (MarkupLeaf <$> fmap (markupGlyph s) xs)
markupChart (PathChart s xs) = Just $ Markup "g" (attsPath s) [MarkupLeaf $ markupPath xs]
markupChart (LineChart s xs) = Just $ Markup "g" (attsLine s) (MarkupLeaf <$> markupLine xs)
markupChart (BlankChart _) = Nothing

-- | Path markup
markupLine :: [[Point Double]] -> [Markup]
markupLine lss =
  fmap (($ mempty) . Markup "polyline" . inject . ("points",) . toPointsText) lss

-- FIXME: should be space separator
toPointsText :: [Point Double] -> ByteString
toPointsText xs = intercalate "\n" $ (\(Point x y) -> pack (show x <> "," <> show (-y))) <$> xs

-- | Path markup
markupPath :: [PathData Double] -> Markup
markupPath ps =
  Markup "path" (foldMap inject [("d", pathDataToSvg ps)]) mempty

-- | GlyphStyle to markup Tree
-- Note rotation on the outside not the inside.
markupGlyph :: GlyphStyle -> Point Double -> Markup
markupGlyph s p =
  case view #rotation s of
    Nothing -> gl
    Just r -> Markup "g" (foldMap inject [("transform", toRotateText r p)]) [MarkupLeaf gl]
  where
    gl = markupShape_ (s ^. #shape) (s ^. #size) p

-- | Convert a dash representation from a list to text
fromDashArray :: [Double] -> ByteString
fromDashArray xs = intercalate " " $ pack . show <$> xs

fromDashOffset :: Double -> ByteString
fromDashOffset x = pack (show x)

attsLine :: LineStyle -> Attributes
attsLine o = mconcat $ inject <$>
  [ ("stroke-width", pack $ show $ o ^. #size),
    ("stroke", showRGBbs $ o ^. #color),
    ("stroke-opacity", pack $ show $ opac $ o ^. #color),
    ("fill", "none")
  ] <>
  catMaybes
  [(\x -> ("stroke-linecap", fromLineCap x)) <$> (o ^. #linecap)]
    <> foldMap (\x -> [("stroke-linejoin", fromLineJoin x)]) (o ^. #linejoin)
    <> foldMap (\x -> [("stroke-dasharray", fromDashArray x)]) (o ^. #dasharray)
    <> foldMap (\x -> [("stroke-dashoffset", fromDashOffset x)]) (o ^. #dashoffset)

attsRect :: RectStyle -> Attributes
attsRect o = foldMap inject
  [ ("stroke-width", pack $ show $ o ^. #borderSize),
    ("stroke", showRGBbs $ o ^. #borderColor),
    ("stroke-opacity", pack $ show $ opac $ o ^. #borderColor),
    ("fill", showRGBbs $ o ^. #color),
    ("fill-opacity", pack $ show $ opac $ o ^. #color)
  ]

-- | TextStyle to Attributes
attsText :: TextStyle -> Attributes
attsText o = Attributes $ Map.fromList $ fmap (first Attribute)
  [ ("stroke-width","0.0"),
    ("stroke", "none"),
    ("fill", showRGBbs $ o ^. #color),
    ("fill-opacity", pack $ show $ opac $ o ^. #color),
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
    ("stroke", showRGBbs $ o ^. #borderColor),
    ("stroke-opacity", pack $ show $ opac $ o ^. #borderColor),
    ("fill", showRGBbs $ o ^. #color),
    ("fill-opacity", pack $ show $ opac $ o ^. #color)
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
    ("stroke", showRGBbs $ o ^. #borderColor),
    ("stroke-opacity", pack $ show $ opac $ o ^. #borderColor),
    ("fill", showRGBbs $ o ^. #color),
    ("fill-opacity", pack $ show $ opac $ o ^. #color)
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
  Markup "polyline" (foldMap inject [("points", pack $ show x <> "," <> show (-(y - s / 2)) <> "\n" <> show x <> "," <> show (-(y + s / 2)))]) mempty
markupShape_ HLineGlyph s (Point x y) =
  Markup "polyline" (foldMap inject [("points", pack $ show (x - s / 2) <> "," <> show (-y) <> "\n" <> show (x + s / 2) <> "," <> show (-y))]) mempty
markupShape_ (PathGlyph path _) s p =
  Markup "path" (foldMap inject [("d", pack $ show path), ("transform", toTranslateText p <> " " <> toScaleText s)]) mempty

-- | @markup@ element + markup 2 attributes
header :: Double -> Rect Double -> [Markup] -> Markup
header markupheight viewbox content' =
  Markup
  "svg"
   (foldMap inject
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
cssPreferColorScheme :: (Text, Text) -> CssPreferColorScheme -> ByteString
cssPreferColorScheme (cl, cd) PreferHud =
  [i|svg {
  color-scheme: light dark;
}
{
  .canvas g, .title g, .axisbar g, .ticktext g, .tickglyph g, .ticklines g, .legendContent g text {
    fill: #{cd};
  }
  .ticklines g, .tickglyph g, .legendBorder g {
    stroke: #{cd};
  }
  .legendBorder g {
    fill: #{cl};
  }
}
@media (prefers-color-scheme:dark) {
  .canvas g, .title g, .axisbar g, .ticktext g, .tickglyph g, .ticklines g, .legendContent g text {
    fill: #{cl};
  }
  .ticklines g, .tickglyph g, .legendBorder g {
    stroke: #{cl};
  }
  .legendBorder g {
    fill: #{cd};
  }
}|]
cssPreferColorScheme (cl, _) PreferLight =
  [i|svg {
      color-scheme: light dark;
    }
    @media (prefers-color-scheme:dark) {
      markup {
        background-color: #{cl};
      }
    }|]
cssPreferColorScheme (_, cd) PreferDark =
  [i|svg {
      color-scheme: light dark;
    }
    @media (prefers-color-scheme:light) {
      markup {
        background-color: #{cd};
      }
    }|]
cssPreferColorScheme _ PreferNormal = mempty

-- | MARKUP tag options.
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

addHud :: HudOptions -> ChartTree -> ChartTree
addHud ho cs =
  runHudWith
    (initialCanvas (view #chartAspect ho) cs)
    db'
    hs
    (cs <> blank db')
  where
    (hs, db') = toHuds ho (singletonGuard $ view box' cs)

-- | The initial canvas before applying Huds
--
-- >>> initialCanvas (FixedAspect 1.5) (unnamed [RectChart defaultRectStyle [one]])
-- Rect -0.75 0.75 -0.5 0.5
initialCanvas :: ChartAspect -> ChartTree -> Rect Double
initialCanvas (FixedAspect a) _ = aspect a
initialCanvas (CanvasAspect a) _ = aspect a
initialCanvas ChartAspect cs = singletonGuard $ view box' cs

-- | css options
--
-- >>> defaultCssOptions
-- CssOptions {shapeRendering = NoShapeRendering, preferColorScheme = PreferHud, cssExtra = ""}
data CssOptions = CssOptions {shapeRendering :: CssShapeRendering, preferColorScheme :: CssPreferColorScheme, cssExtra :: ByteString} deriving (Show, Eq, Generic)

-- | No special shape rendering and default hud responds to user color scheme preferences.
defaultCssOptions :: CssOptions
defaultCssOptions = CssOptions NoShapeRendering PreferHud mempty

-- FIXME: remove hex
markupCssOptions :: CssOptions -> Markup
markupCssOptions css = Markup "style" mempty
  [Content $
   cssPreferColorScheme (hex light, hex dark) (view #preferColorScheme css) <>
   markupShapeRendering (view #shapeRendering css) <>
   view #cssExtra css
  ]

-- | CSS shape rendering text snippet
markupShapeRendering :: CssShapeRendering -> ByteString
markupShapeRendering UseGeometricPrecision = "svg { shape-rendering: geometricPrecision; }"
markupShapeRendering UseCssCrisp = "svg { shape-rendering: crispEdges; }"
markupShapeRendering NoShapeRendering = mempty

markupChartOptions :: ChartOptions -> Markup
markupChartOptions co = header (view (#markupOptions % #markupHeight) co) viewbox
  ([markupCssOptions (view (#markupOptions % #cssOptions) co)] <>
   markupChartTree csAndHud)
  where
    viewbox = singletonGuard (view styleBox' csAndHud)
    csAndHud = addHud (view #hudOptions co) (view #charts co)

printChartOptions :: ChartOptions -> ByteString
printChartOptions = printMarkup . markupChartOptions

instance Semigroup ChartOptions where
  (<>) (ChartOptions _ h c) (ChartOptions s' h' c') =
    ChartOptions s' (h <> h') (c <> c')

instance Monoid ChartOptions where
  mempty = ChartOptions defaultMarkupOptions mempty mempty

markupChartTree :: ChartTree -> [Markup]
markupChartTree cs =
  case (xs', label) of
    ([], Nothing) -> mempty
    (xs'', Nothing) -> xs''
    (xs'', Just l) -> [Markup "g" (Attributes . Map.singleton Class . encodeUtf8 $ l) (MarkupLeaf <$> xs'')]
    where
      (ChartTree (Node (label, cs') xs)) = filterChartTree (not . isEmptyChart) cs
      xs' = mapMaybe markupChart cs' <> (mconcat $ markupChartTree . ChartTree <$> xs)

writeChartOptions :: FilePath -> ChartOptions -> IO ()
writeChartOptions fp co = Data.ByteString.writeFile fp (printChartOptions co)

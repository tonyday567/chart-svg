{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
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

module Chart.Xml where

import Prelude
import Data.ByteString ( intercalate, ByteString, writeFile )
import GHC.Generics
import Data.String.Interpolate
import Chart.Data hiding (Element)
import Data.ByteString.Char8 (pack, unpack)
import Optics.Core hiding (element)
import Data.Maybe
import Data.Text.Encoding (encodeUtf8)
-- import Data.Text hiding (unpack, pack, filter, intercalate, empty)
import Chart.Primitive hiding (tree)
import Chart.Style
import Data.Colour
import Data.Text (Text)
import Data.Path
import Data.Path.Parser
import Chart.Hud
import Data.Tree (Tree(..))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Bool
import Data.Bifunctor
-- import GHC.Exts

-- $setup
--
-- >>> :set -XOverloadedLabels
-- >>> :set -XOverloadedStrings
-- >>> import Chart
-- >>> import Optics.Core

data Attribute =
  Class |
  ID |
  Attribute ByteString deriving (Eq, Show, Generic, Ord)

newtype Attributes = Attributes { atts :: Map Attribute ByteString } deriving (Eq, Show, Generic)

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

inject :: (ByteString, ByteString) -> Attributes
inject ("class",b) = Attributes $ Map.singleton Class b
inject ("id",b) = Attributes $ Map.singleton ID b
inject (a,b) = Attributes $ Map.singleton (Attribute a) b

eject :: Attribute -> ByteString
eject Class = "class"
eject ID = "id"
eject (Attribute a) = a


-- https://jwatt.org/svg/authoring/

-- xml-conduit sees this as:
-- Attributes as Map Text Text
-- forest as [Node] with Node as
-- NodeElement
-- NodeInstruction
-- NodeContent
-- NodeComment
--
-- xml gives:
-- name
-- [Att]
-- [Content] (Element, CData, CRef String)
-- Maybe Line (Int)
--
data Svg = Svg {
  name :: ByteString,
  attributes :: Attributes,
  forest :: [Svg],
  content :: ByteString
  } deriving (Eq, Show, Generic)

class ToSvg a where
  svg :: a -> Maybe Svg

-- | Text svg
svgText :: TextStyle -> Text -> Point Double -> Svg
svgText s t p@(Point x y) = Svg "text" as xs (encodeUtf8 t)
  where
    as = Attributes $ Map.fromList $
      [ (Attribute "x", pack $ show x),
        (Attribute "y", pack $ show $ -y)
      ] <>
      maybeToList ((\x' -> (Attribute "transform", toRotateText x' p)) <$> (s ^. #rotation))
    xs = case view #frame s of
      Nothing -> []
      Just f -> maybeToList $ svg (RectChart (f & over #borderSize (* view #size s)) [styleBoxText s t p])

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

instance (Num a, Show a) => ToSvg (Rect a) where
  svg (Rect x z y w) =
    Just $ Svg "rect" as mempty mempty
    where
      as = Attributes $ Map.fromList $ first Attribute <$>
        [ ("width", pack $ show $ z - x),
          ("height", pack $ show $ w - y),
          ("x", pack $ show x),
          ("y", pack $ show $ -w)
        ]

instance ToSvg Chart where
  svg (RectChart s xs) = Just $ Svg "g" (attsRect s) (mapMaybe svg xs) mempty
  svg (TextChart s xs) = Just $ Svg "g" (attsText s) (uncurry (svgText s) <$> xs) mempty
  svg (GlyphChart s xs) = Just $ Svg "g" (attsGlyph s) (mapMaybe (svgGlyph s) xs) mempty
  svg (PathChart s xs) = Just $ Svg "g" (attsPath s) [svgPath xs] mempty
  svg (LineChart s xs) = Just $ Svg "g" (attsLine s) (svgLine xs) mempty
  svg (BlankChart _) = Nothing

-- | Path svg
svgLine :: [[Point Double]] -> [Svg]
svgLine lss =
  [Svg "polyline" (foldMap (inject . ("points",) . toPointsText) lss) mempty mempty]

toPointsText :: [Point Double] -> ByteString
toPointsText xs = intercalate "\n" $ (\(Point x y) -> pack (show x <> "," <> show (-y))) <$> xs

-- | Path svg
svgPath :: [PathData Double] -> Svg
svgPath ps =
  Svg "path" (foldMap inject [("d", pathDataToSvg ps)]) mempty mempty

-- | GlyphStyle to svg Tree
svgGlyph :: GlyphStyle -> Point Double -> Maybe Svg
svgGlyph s p =
  svgShape_ (s ^. #shape) (s ^. #size) p & fmap (#forest %~ (<> rot))
  where
    rot =
      maybeToList $
      (\r -> Svg "g" (foldMap inject [("transform", toRotateText r p)]) mempty mempty) <$>
      (s ^. #rotation)

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

-- | GlyphShape to svg Tree
svgShape_ :: GlyphShape -> Double -> Point Double -> Maybe Svg
svgShape_ CircleGlyph s (Point x y) = Just $ Svg "circle" as mempty mempty where
  as = Attributes $ Map.fromList  $ fmap (first Attribute)
    [ ("cx", pack $ show x),
      ("cy", pack $ show $ -y),
      ("r", pack $ show $ 0.5 * s)
    ]
svgShape_ SquareGlyph s p =
  svg (move p ((s *) <$> one :: Rect Double))
svgShape_ (RectSharpGlyph x') s p =
  svg (move p (scale (Point s (x' * s)) one :: Rect Double))
svgShape_ (RectRoundedGlyph x' rx ry) s p = Just $ Svg "rect" as mempty mempty
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
svgShape_ (TriangleGlyph (Point xa ya) (Point xb yb) (Point xc yc)) s p =
  Just $ Svg "polygon" as mempty mempty
  where
    as = Attributes $ Map.fromList $ fmap (first Attribute)
      [ ("transform", toTranslateText p),
        ("points", pack $ show (s * xa) <> "," <> show (-(s * ya)) <> " " <> show (s * xb) <> "," <> show (-(s * yb)) <> " " <> show (s * xc) <> "," <> show (-(s * yc)))
      ]
svgShape_ (EllipseGlyph x') s (Point x y) =
  Just $ Svg "ellipse" as mempty mempty
  where
    as = Attributes $ Map.fromList $ fmap (first Attribute)
      [ ("cx", (pack . show) x),
        ("cy", (pack . show) $ -y),
        ("rx", (pack . show) $ 0.5 * s),
        ("ry", (pack . show) $ 0.5 * s * x')
      ]
svgShape_ VLineGlyph s (Point x y) =
  Just $ Svg "polyline" (foldMap inject [("points", pack $ show x <> "," <> show (-(y - s / 2)) <> "\n" <> show x <> "," <> show (-(y + s / 2)))]) mempty mempty
svgShape_ HLineGlyph s (Point x y) =
  Just $ Svg "polyline" (foldMap inject [("points", pack $ show (x - s / 2) <> "," <> show (-y) <> "\n" <> show (x + s / 2) <> "," <> show (-y))]) mempty mempty
svgShape_ (PathGlyph path _) s p =
  Just $ Svg "path" (foldMap inject [("d", pack $ show path), ("transform", toTranslateText p <> " " <> toScaleText s)]) mempty mempty

getAtt :: (Read a) => ByteString -> [(ByteString, ByteString)] -> Maybe a
getAtt n as = listToMaybe $ fmap (read . unpack . snd) $ filter ((==n) . fst) as

-- | @svg@ element + svg 2 attributes
header :: Double -> Rect Double -> [Svg] -> Svg
header svgheight viewbox content' =
  Svg
  "svg"
   (foldMap inject
    [("xmlns", "http://www.w3.org/2000/svg"),
     ("xmlns:xlink", "http://www.w3.org/1999/xlink"),
     ("width", pack $ show w''),
     ("height", pack $ show h'),
     ("viewbox", pack $ show x <> " " <> show (-w) <> " " <> show (z - x) <> " " <> show (w - y))
    ]) content' mempty
  where
    (Rect x z y w) = viewbox
    Point w' h = width viewbox
    Point w'' h' = Point (svgheight / h * w') svgheight

-- | CSS prefer-color-scheme text snippet
cssPreferColorScheme :: (Colour, Colour) -> CssPreferColorScheme -> ByteString
cssPreferColorScheme (cl, cd) PreferHud =
  [i|
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
    fill: #{hex cl};
  }
  .ticklines g, .tickglyph g, .legendBorder g {
    stroke: #{hex cl};
  }
  .legendBorder g {
    fill: #{hex cd};
  }
}
|]
cssPreferColorScheme (bglight, _) PreferLight =
  [i|
    svg {
      color-scheme: light dark;
    }
    @media (prefers-color-scheme:dark) {
      svg {
        background-color: #{hex bglight};
      }
    }
  |]
cssPreferColorScheme (_, bgdark) PreferDark =
  [i|
    svg {
      color-scheme: light dark;
    }
    @media (prefers-color-scheme:light) {
      svg {
        background-color: #{hex bgdark};
      }
    }
  |]
cssPreferColorScheme _ PreferNormal = mempty

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

renderToSvg :: ChartOptions -> Svg
renderToSvg co = header (view (#svgOptions % #svgHeight) co) viewbox (maybeToList $ svg (addHud (view #hudOptions co) cs))
  where
    viewbox = singletonGuard (view styleBox' cs)
    cs = view #charts co

chartTreeToSvg :: ChartTree -> Svg
chartTreeToSvg cs = renderToSvg (ChartOptions defaultSvgOptions mempty cs)

renderToByteString :: ChartOptions -> ByteString
renderToByteString = svgPrinter . renderToSvg

svgPrinter :: Svg -> ByteString
svgPrinter (Svg n as xs c) =
  bool [i|<#{spaced}>#{ls}#{c}</#{n}>|] [i|<#{spaced}/>|] (xs==mempty && c == mempty)
    where
      spaced = intercalate " " ([n] <> (uncurry attPrint <$> Map.toList (atts as)))
      ls = mconcat (svgPrinter <$> xs)

attPrint :: Attribute -> ByteString -> ByteString
attPrint a b = [i|#{eject a}="#{b}"|]
data ChartOptions = ChartOptions {
  svgOptions :: SvgOptions,
  hudOptions :: HudOptions,
  charts :: ChartTree
       } deriving (Generic, Eq, Show)

instance Semigroup ChartOptions where
  (<>) (ChartOptions _ h c) (ChartOptions s' h' c') =
    ChartOptions s' (h <> h') (c <> c')

instance Monoid ChartOptions where
  mempty = ChartOptions defaultSvgOptions mempty mempty

instance ToSvg ChartTree where
  svg cs = Just $ Svg "g" (maybe mempty (Attributes . Map.singleton Class . encodeUtf8) label) (content' <> svgs) mempty
    where
      (ChartTree (Node (label, cs') xs)) = filterChartTree (not . isEmptyChart) cs
      content' = mconcat (maybeToList . svg <$> cs')
      svgs = mconcat (maybeToList . svg . ChartTree <$> xs)

instance ToSvg ChartOptions where
  svg = Just . renderToSvg

writeChartOptions :: FilePath -> ChartOptions -> IO ()
writeChartOptions fp co = Data.ByteString.writeFile fp (renderToByteString co)


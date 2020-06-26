{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module Chart.Svg
  ( svg,
    svgt,
    chartDef,
    chartDefs,
    styleBox,
    styleBoxes,
    noStyleBoxes,
    styleBoxText,
    styleBoxGlyph,
  )
where

import Chart.Color
import Chart.Types
import Control.Category (id)
import Control.Lens hiding (transform)
import Data.Generics.Labels ()
import Data.Maybe
import Data.Monoid
import qualified Data.Text as Text
import Lucid
import Lucid.Base
import NumHask.Space as NH hiding (Element)
import Protolude hiding (writeFile)
import Text.HTML.TagSoup hiding (Attribute)

terms :: Text -> [Attribute] -> Html ()
terms t = with $ makeXmlElementNoEnd t

-- | the extra area from text styling
styleBoxText ::
  TextStyle ->
  Text.Text ->
  Point Double ->
  Rect Double
styleBoxText o t p = move (p + p') $ maybe flat (`rotateRect` flat) (o ^. #rotation)
  where
    flat = Rect ((- x' / 2.0) + x' * a') (x' / 2 + x' * a') ((- y' / 2) - n1') (y' / 2 - n1')
    s = o ^. #size
    h = o ^. #hsize
    v = o ^. #vsize
    n1 = o ^. #nudge1
    x' = s * h * fromIntegral (Protolude.sum $ maybe 0 Text.length . maybeTagText <$> parseTags t)
    y' = s * v
    n1' = s * n1
    a' = case o ^. #anchor of
      AnchorStart -> 0.5
      AnchorEnd -> -0.5
      AnchorMiddle -> 0.0
    p' = fromMaybe (Point 0.0 0.0) (o ^. #translate)

-- | the extra area from glyph styling
styleBoxGlyph :: GlyphStyle -> Rect Double
styleBoxGlyph s = move p' $ sw $ case sh of
  EllipseGlyph a -> NH.scale (Point sz (a * sz)) unitRect
  RectSharpGlyph a -> NH.scale (Point sz (a * sz)) unitRect
  RectRoundedGlyph a _ _ -> NH.scale (Point sz (a * sz)) unitRect
  VLineGlyph _ -> NH.scale (Point ((s ^. #borderSize) * sz) sz) unitRect
  HLineGlyph _ -> NH.scale (Point sz ((s ^. #borderSize) * sz)) unitRect
  TriangleGlyph a b c -> (sz *) <$> sconcat (toRect . SpotPoint <$> (a :| [b, c]) :: NonEmpty (Rect Double))
  _ -> (sz *) <$> unitRect
  where
    sh = s ^. #shape
    sz = s ^. #size
    sw = padRect (0.5 * s ^. #borderSize)
    p' = fromMaybe (Point 0.0 0.0) (s ^. #translate)

-- | the geometric dimensions of a Chart inclusive of style geometry
styleBox :: Chart Double -> Maybe (Rect Double)
styleBox (Chart (TextA s ts) xs) = foldRect $ zipWith (\t x -> styleBoxText s t (toPoint x)) ts xs
styleBox (Chart (GlyphA s) xs) = foldRect $ (\x -> move (toPoint x) (styleBoxGlyph s)) <$> xs
styleBox (Chart (RectA s) xs) = foldRect (padRect (0.5 * s ^. #borderSize) . toRect <$> xs)
styleBox (Chart (LineA s) xs) = foldRect (padRect (0.5 * s ^. #width) . toRect <$> xs)
styleBox (Chart BlankA xs) = foldRect (toRect <$> xs)
styleBox (Chart (PixelA s) xs) = foldRect (padRect (0.5 * s ^. #pixelRectStyle . #borderSize) . toRect <$> xs)

-- | the extra geometric dimensions of a [Chart]
styleBoxes :: [Chart Double] -> Maybe (Rect Double)
styleBoxes xss = foldRect $ catMaybes (styleBox <$> xss)

-- | geometric dimensions of a [Chart] not including style
noStyleBoxes :: [Chart Double] -> Maybe (Rect Double)
noStyleBoxes cs = foldRect $ toRect <$> mconcat (view #spots <$> cs)

-- | calculate the linear gradient to shove in defs
-- FIXME: Only works for #pixelGradient = 0 or pi//2. Can do much better with something like https://stackoverflow.com/questions/9025678/how-to-get-a-rotated-linear-gradient-svg-for-use-as-a-background-image
lgPixel :: PixelStyle -> Html ()
lgPixel o =
  term "linearGradient"
    [ id_ (o ^. #pixelTextureId),
      makeAttribute "x1" (show x0),
      makeAttribute "y1" (show y0),
      makeAttribute "x2" (show x1),
      makeAttribute "y2" (show y1)
    ]
    ( mconcat
        [ terms "stop"
            [ makeAttribute "stop_opacity" (show $ opac $ o ^. #pixelColorMin),
              makeAttribute "stop_color" (toHex (o ^. #pixelColorMin)),
              makeAttribute "offset" "0"
            ],
          terms "stop"
            [ makeAttribute "stop_opacity" (show $ opac $ o ^. #pixelColorMax),
              makeAttribute "stop_color" (toHex (o ^. #pixelColorMax)),
              makeAttribute "offset" "1"
            ]
        ]
    )
  where
    x0 = min 0 (cos (o ^. #pixelGradient))
    x1 = max 0 (cos (o ^. #pixelGradient))
    y0 = max 0 (sin (o ^. #pixelGradient))
    y1 = min 0 (sin (o ^. #pixelGradient))

-- | get chart definitions
chartDefs :: [Chart a] -> Html ()
chartDefs cs = bool (term "defs" (mconcat ds)) mempty (0 == length ds)
  where
    ds = mconcat $ chartDef <$> cs

chartDef :: Chart a -> [Html ()]
chartDef c = case c of
  (Chart (PixelA s) _) -> [lgPixel s]
  _ -> []

-- | Rectangle svg
svgRect :: Rect Double -> Html ()
svgRect (Rect x z y w) =
  terms "rect"
    [ width_ (show $ z - x),
      height_ (show $ w - y),
      term "x" (show x),
      term "y" (show $ - w)
    ]

-- | Text svg
svgText :: TextStyle -> Text -> Point Double -> Html ()
svgText s t p@(Point x y) =
  bool id (term "g" [class_ "hasmathjax"]) (s ^. #hasMathjax) $
    term "text"
      ( [ term "x" (show x),
          term "y" (show $ - y)
        ]
          <> maybe [] (\x' -> [term "transform" (toRotateText x' p)]) (s ^. #rotation)
      )
      (toHtmlRaw t)

-- | line svg
svgLine :: [Point Double] -> Html ()
svgLine [] = mempty
svgLine xs = terms "polyline" [term "points" (toPointsText xs)]
  where
    toPointsText xs' = Text.intercalate "\n" $ (\(Point x y) -> show x <> "," <> show (- y)) <$> xs'

-- | GlyphShape to svg Tree
svgShape :: GlyphShape -> Double -> Point Double -> Html ()
svgShape CircleGlyph s (Point x y) =
  terms "circle"
    [ term "cx" (show x),
      term "cy" (show $ - y),
      term "r" (show $ 0.5 * s)
    ]
svgShape SquareGlyph s p =
  svgRect (move p ((s *) <$> unitRect))
svgShape (RectSharpGlyph x') s p =
  svgRect (move p (NH.scale (Point s (x' * s)) unitRect))
svgShape (RectRoundedGlyph x' rx ry) s p =
  terms "rect"
    [ term "width" (show $ z - x),
      term "height" (show $ w - y),
      term "x" (show x),
      term "y" (show $ - w),
      term "rx" (show rx),
      term "ry" (show ry)
    ]
  where
    (Rect x z y w) = move p (NH.scale (Point s (x' * s)) unitRect)
svgShape (TriangleGlyph (Point xa ya) (Point xb yb) (Point xc yc)) s p =
  terms "polygon"
    [ term "transform" (toTranslateText p),
      term "points" (show (s * xa) <> "," <> show (- (s * ya)) <> " " <> show (s * xb) <> "," <> show (- (s * yb)) <> " " <> show (s * xc) <> "," <> show (- (s * yc)))
    ]
svgShape (EllipseGlyph x') s (Point x y) =
  terms "ellipse"
    [ term "cx" (show x),
      term "cy" (show $ - y),
      term "rx" (show $ 0.5 * s),
      term "ry" (show $ 0.5 * s * x')
    ]
svgShape (VLineGlyph _) s (Point x y) =
  terms "polyline" [term "points" (show x <> "," <> show (- (y - s / 2)) <> "\n" <> show x  <> "," <> show (- (y + s / 2)))]
svgShape (HLineGlyph _) s (Point x y) =
  terms "polyline" [term "points" (show (x - s / 2) <> "," <> show (- y) <> "\n" <> show (x + s / 2) <> "," <> show (- y))]
svgShape (PathGlyph path) _ p =
  terms "path" [term "d" path, term "transform" (toTranslateText p)]

-- | GlyphStyle to svg Tree
svgGlyph :: GlyphStyle -> Point Double -> Html ()
svgGlyph s p =
  svgShape (s ^. #shape) (s ^. #size) (realToFrac <$> p)
    & maybe id (\r -> term "g" [term "transform" (toRotateText r p)]) (s ^. #rotation)

-- | convert a Chart to svg
svg :: Chart Double -> Html ()
svg (Chart (TextA s ts) xs) =
  term "g" (attsText s) (mconcat $ zipWith (\t p -> svgText s t (toPoint p)) ts xs)
svg (Chart (GlyphA s) xs) =
  term "g" (attsGlyph s) (mconcat $ svgGlyph s . toPoint <$> xs)
svg (Chart (LineA s) xs) =
  term "g" (attsLine s) (svgLine $ toPoint <$> xs)
svg (Chart (RectA s) xs) =
  term "g" (attsRect s) (mconcat $ svgRect . toRect <$> xs)
svg (Chart (PixelA s) xs) =
  term "g" (attsPixel s) (mconcat $ svgRect . toRect <$> xs)
svg (Chart BlankA _) = mempty

-- | add a tooltip to a chart
svgt :: Chart Double -> (TextStyle, Text) -> Html ()
svgt (Chart (TextA s ts) xs) (s', ts') =
  term "g" (attsText s) (title_ (attsText s') (Lucid.toHtml ts') <> mconcat (zipWith (\t p -> svgText s t (toPoint p)) ts xs))
svgt (Chart (GlyphA s) xs) (s', ts') =
  term "g" (attsGlyph s) (title_ (attsText s') (Lucid.toHtml ts') <> mconcat (svgGlyph s . toPoint <$> xs))
svgt (Chart (LineA s) xs) (s', ts') =
  term "g" (attsLine s) (title_ (attsText s') (Lucid.toHtml ts') <> svgLine (toPoint <$> xs))
svgt (Chart (RectA s) xs) (s', ts') =
  term "g" (attsRect s) (title_ (attsText s') (Lucid.toHtml ts') <> mconcat (svgRect . toRect <$> xs))
svgt (Chart (PixelA s) xs) (s', ts') =
  term "g" (attsPixel s) (title_ (attsText s') (Lucid.toHtml ts') <> mconcat (svgRect . toRect <$> xs))
svgt (Chart BlankA _) _ = mempty

-- * Style to Attributes

attsRect :: RectStyle -> [Attribute]
attsRect o =
  [ term "stroke_width" (show $ o ^. #borderSize),
    term "stroke" (hex $ o ^. #borderColor),
    term "stroke_opacity" (show $ opac $ o ^. #borderColor),
    term "fill" (hex $ o ^. #color),
    term "fill_opacity" (show $ opac $ o ^. #color)
  ]

attsPixel :: PixelStyle -> [Attribute]
attsPixel o =
  [ term "stroke_width" (show $ o ^. #pixelRectStyle . #borderSize),
    term "stroke" (toHex $ o ^. #pixelRectStyle . #borderColor),
    term "stroke_opacity" (show $ opac $ o ^. #pixelRectStyle . #borderColor),
    term "fill" ("url(#" <> (o ^. #pixelTextureId) <> ")")
  ]

attsText :: TextStyle -> [Attribute]
attsText o =
  [ term "stroke_width" "0.0",
    term "stroke" "none",
    term "fill" (toHex $ o ^. #color),
    term "fill_opacity" (show $ opac $ o ^. #color),
    term "font_size" (show $ o ^. #size),
    term "text_anchor" (toTextAnchor $ o ^. #anchor)
  ]
    <> maybe [] ((: []) . term "transform" . toTranslateText) (o ^. #translate)
  where
    toTextAnchor :: Anchor -> Text
    toTextAnchor AnchorMiddle = "middle"
    toTextAnchor AnchorStart = "start"
    toTextAnchor AnchorEnd = "end"

attsGlyph :: GlyphStyle -> [Attribute]
attsGlyph o =
  [ term "stroke_width" (show $ o ^. #borderSize),
    term "stroke" (toHex $ o ^. #borderColor),
    term "stroke_opacity" (show $ opac $ o ^. #borderColor),
    term "fill" (toHex $ o ^. #color),
    term "fill_opacity" (show $ opac $ o ^. #color)
  ]
    <> maybe [] ((: []) . term "transform" . toTranslateText) (o ^. #translate)

attsLine :: LineStyle -> [Attribute]
attsLine o =
  [ term "stroke_width" (show $ o ^. #width),
    term "stroke" (toHex $ o ^. #color),
    term "stroke_opacity" (show $ opac $ o ^. #color),
    term "fill" "none"
  ]

toTranslateText :: Point Double -> Text
toTranslateText (Point x y) =
  "translate(" <> show x <> ", " <> show (- y) <> ")"

toRotateText :: Double -> Point Double -> Text
toRotateText r (Point x y) =
  "rotate(" <> show r <> ", " <> show x <> ", " <> show (- y) <> ")"

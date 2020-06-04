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
import qualified Lucid
import Lucid.Svg hiding (z)
import NumHask.Space as NH hiding (Element)
import Protolude hiding (writeFile)
import Text.HTML.TagSoup hiding (Attribute)

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

-- | calculate the linear gradient to shove in defs
-- FIXME: Only works for #pixelGradient = 0 or pi//2. Can do much better with something like https://stackoverflow.com/questions/9025678/how-to-get-a-rotated-linear-gradient-svg-for-use-as-a-background-image
lgPixel :: PixelStyle -> Svg ()
lgPixel o =
  linearGradient_
    [ id_ (o ^. #pixelTextureId),
      x1_ (show x0),
      y1_ (show y0),
      x2_ (show x1),
      y2_ (show y1)
    ]
    ( mconcat
        [ stop_
            [ stop_opacity_ (show $ opac $ o ^. #pixelColorMin),
              stop_color_ (toHex (o ^. #pixelColorMin)),
              offset_ "0"
            ],
          stop_
            [ stop_opacity_ (show $ opac $ o ^. #pixelColorMax),
              stop_color_ (toHex (o ^. #pixelColorMax)),
              offset_ "1"
            ]
        ]
    )
  where
    x0 = min 0 (cos (o ^. #pixelGradient))
    x1 = max 0 (cos (o ^. #pixelGradient))
    y0 = max 0 (sin (o ^. #pixelGradient))
    y1 = min 0 (sin (o ^. #pixelGradient))

-- | get chart definitions
chartDefs :: [Chart a] -> Svg ()
chartDefs cs = bool (defs_ (mconcat ds)) mempty (0 == length ds)
  where
    ds = mconcat $ chartDef <$> cs

chartDef :: Chart a -> [Svg ()]
chartDef c = case c of
  (Chart (PixelA s) _) -> [lgPixel s]
  _ -> []

-- | Rectangle svg
svgRect :: Rect Double -> Svg ()
svgRect (Rect x z y w) =
  rect_
    [ width_ (show $ z - x),
      height_ (show $ w - y),
      x_ (show x),
      y_ (show $ - w)
    ]

-- | Text svg
svgText :: TextStyle -> Text -> Point Double -> Svg ()
svgText s t p@(Point x y) =
  bool id (g_ [class_ "hasmathjax"]) (s ^. #hasMathjax) $
    text_
      ( [ x_ (show x),
          y_ (show $ - y)
        ]
          <> maybe [] (\x' -> [transform_ (toRotateText x' p)]) (s ^. #rotation)
      )
      (toHtmlRaw t)

-- | line svg
svgLine :: [Point Double] -> Svg ()
svgLine [] = mempty
svgLine xs = polyline_ [points_ (toPointsText xs)]
  where
    toPointsText xs' = Text.intercalate "\n" $ (\(Point x y) -> show x <> "," <> show (- y)) <$> xs'

-- | GlyphShape to svg Tree
svgShape :: GlyphShape -> Double -> Point Double -> Svg ()
svgShape CircleGlyph s (Point x y) =
  circle_
    [ cx_ (show x),
      cy_ (show $ - y),
      r_ (show $ 0.5 * s)
    ]
svgShape SquareGlyph s p =
  svgRect (move p ((s *) <$> unitRect))
svgShape (RectSharpGlyph x') s p =
  svgRect (move p (NH.scale (Point s (x' * s)) unitRect))
svgShape (RectRoundedGlyph x' rx ry) s p =
  rect_
    [ width_ (show $ z - x),
      height_ (show $ w - y),
      x_ (show x),
      y_ (show $ - w),
      rx_ (show rx),
      ry_ (show ry)
    ]
  where
    (Rect x z y w) = move p (NH.scale (Point s (x' * s)) unitRect)
svgShape (TriangleGlyph (Point xa ya) (Point xb yb) (Point xc yc)) s p =
  polygon_
    [ transform_ (toTranslateText p),
      points_ (show (s * xa) <> "," <> show (- (s * ya)) <> " " <> show (s * xb) <> "," <> show (- (s * yb)) <> " " <> show (s * xc) <> "," <> show (- (s * yc)))
    ]
svgShape (EllipseGlyph x') s (Point x y) =
  ellipse_
    [ cx_ (show x),
      cy_ (show $ - y),
      rx_ (show $ 0.5 * s),
      ry_ (show $ 0.5 * s * x')
    ]
svgShape (VLineGlyph _) s (Point x y) =
  polyline_ [points_ (show x <> "," <> show (- (y - s / 2)) <> "\n" <> show x  <> "," <> show (- (y + s / 2)))]
svgShape (HLineGlyph _) s (Point x y) =
  polyline_ [points_ (show (x - s / 2) <> "," <> show (- y) <> "\n" <> show (x + s / 2) <> "," <> show (- y))]
svgShape (PathGlyph path) _ p =
  path_ [d_ path, transform_ (toTranslateText p)]

-- | GlyphStyle to svg Tree
svgGlyph :: GlyphStyle -> Point Double -> Svg ()
svgGlyph s p =
  svgShape (s ^. #shape) (s ^. #size) (realToFrac <$> p)
    & maybe id (\r -> g_ [transform_ (toRotateText r p)]) (s ^. #rotation)

-- | convert a Chart to svg
svg :: Chart Double -> Svg ()
svg (Chart (TextA s ts) xs) =
  g_ (attsText s) (mconcat $ zipWith (\t p -> svgText s t (toPoint p)) ts xs)
svg (Chart (GlyphA s) xs) =
  g_ (attsGlyph s) (mconcat $ svgGlyph s . toPoint <$> xs)
svg (Chart (LineA s) xs) =
  g_ (attsLine s) (svgLine $ toPoint <$> xs)
svg (Chart (RectA s) xs) =
  g_ (attsRect s) (mconcat $ svgRect . toRect <$> xs)
svg (Chart (PixelA s) xs) =
  g_ (attsPixel s) (mconcat $ svgRect . toRect <$> xs)
svg (Chart BlankA _) = mempty

-- | add a tooltip to a chart
svgt :: Chart Double -> (TextStyle, Text) -> Svg ()
svgt (Chart (TextA s ts) xs) (s', ts') =
  g_ (attsText s) (title_ (attsText s') (Lucid.toHtml ts') <> mconcat (zipWith (\t p -> svgText s t (toPoint p)) ts xs))
svgt (Chart (GlyphA s) xs) (s', ts') =
  g_ (attsGlyph s) (title_ (attsText s') (Lucid.toHtml ts') <> mconcat (svgGlyph s . toPoint <$> xs))
svgt (Chart (LineA s) xs) (s', ts') =
  g_ (attsLine s) (title_ (attsText s') (Lucid.toHtml ts') <> svgLine (toPoint <$> xs))
svgt (Chart (RectA s) xs) (s', ts') =
  g_ (attsRect s) (title_ (attsText s') (Lucid.toHtml ts') <> mconcat (svgRect . toRect <$> xs))
svgt (Chart (PixelA s) xs) (s', ts') =
  g_ (attsPixel s) (title_ (attsText s') (Lucid.toHtml ts') <> mconcat (svgRect . toRect <$> xs))
svgt (Chart BlankA _) _ = mempty

-- * Style to Attributes

attsRect :: RectStyle -> [Attribute]
attsRect o =
  [ stroke_width_ (show $ o ^. #borderSize),
    stroke_ (hex $ o ^. #borderColor),
    stroke_opacity_ (show $ opac $ o ^. #borderColor),
    fill_ (hex $ o ^. #color),
    fill_opacity_ (show $ opac $ o ^. #color)
  ]

attsPixel :: PixelStyle -> [Attribute]
attsPixel o =
  [ stroke_width_ (show $ o ^. #pixelRectStyle . #borderSize),
    stroke_ (toHex $ o ^. #pixelRectStyle . #borderColor),
    stroke_opacity_ (show $ opac $ o ^. #pixelRectStyle . #borderColor),
    fill_ ("url(#" <> (o ^. #pixelTextureId) <> ")")
  ]

attsText :: TextStyle -> [Attribute]
attsText o =
  [ stroke_width_ "0.0",
    stroke_ "none",
    fill_ (toHex $ o ^. #color),
    fill_opacity_ (show $ opac $ o ^. #color),
    font_size_ (show $ o ^. #size),
    text_anchor_ (toTextAnchor $ o ^. #anchor)
  ]
    <> maybe [] ((: []) . transform_ . toTranslateText) (o ^. #translate)
  where
    toTextAnchor :: Anchor -> Text
    toTextAnchor AnchorMiddle = "middle"
    toTextAnchor AnchorStart = "start"
    toTextAnchor AnchorEnd = "end"

attsGlyph :: GlyphStyle -> [Attribute]
attsGlyph o =
  [ stroke_width_ (show $ o ^. #borderSize),
    stroke_ (toHex $ o ^. #borderColor),
    stroke_opacity_ (show $ opac $ o ^. #borderColor),
    fill_ (toHex $ o ^. #color),
    fill_opacity_ (show $ opac $ o ^. #color)
  ]
    <> maybe [] ((: []) . transform_ . toTranslateText) (o ^. #translate)

attsLine :: LineStyle -> [Attribute]
attsLine o =
  [ stroke_width_ (show $ o ^. #width),
    stroke_ (toHex $ o ^. #color),
    stroke_opacity_ (show $ opac $ o ^. #color),
    fill_ "none"
  ]

toTranslateText :: Point Double -> Text
toTranslateText (Point x y) =
  "translate(" <> show x <> ", " <> show (- y) <> ")"

toRotateText :: Double -> Point Double -> Text
toRotateText r (Point x y) =
  "rotate(" <> show r <> ", " <> show x <> ", " <> show (- y) <> ")"

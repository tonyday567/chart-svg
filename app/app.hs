{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}
  
import Chart.Svg
import NumHask.Prelude hiding (Text, rotate)
import Lens.Micro 
import Codec.Picture.Types
import qualified Data.Text as Text
import Data.List ((!!))
import Data.Generics.Product (field)
import Chart.Hud
import Chart.Core
import Chart.Spot
import Graphics.Svg.CssTypes hiding (Point)
import qualified Data.Map as Map

ropts :: [RectStyle]
ropts =
  [ blob (PixelRGBA8 93 165 218 255) 0.5
  , blob (PixelRGBA8 120 80 60 255) 0.5
  ]

rss :: [[Area Double]]
rss =
  [ areaXY (\x -> exp (-(x ** 2) / 2)) (Range -5 5) 50
  , areaXY (\x -> 0.5 * exp (-(x ** 2) / 8)) (Range -5 5) 50
  ]

rs :: RectStyle
rs = RectStyle 0.1 (PixelRGBA8 102 102 102 127) 0.5 (PixelRGBA8 102 5 102 127) 0.5

rs' :: RectStyle
rs' = rs &  field @"opacity" .~ 0.1 &  field @"borderOpacity" .~ 0.1

oneChart :: Chart Double
oneChart = Chart (RectA rs) mempty [one]

oneChart' :: Chart Double
oneChart' = Chart (RectA rs') mempty [one]

rotateOne :: ChartSvg Double
rotateOne = defaultFrame $
  chartSvg one [showOrigin] <>
  chartSvg one [oneChart'] <>
  rotate 30 (chartSvg one [oneChart])

translateOne :: ChartSvg Double
translateOne = defaultFrame $
  chartSvg one [showOrigin] <>
  chartSvg one [oneChart'] <>
  translate (Point 1 1) (rotate 30 (chartSvg one [oneChart]))

rectChart :: Chart Double
rectChart = Chart (RectA $ ropts!!0) mempty (SpotArea <$> rss!!0)

rectCharts :: [Chart Double]
rectCharts =
  zipWith (\s xs -> Chart (RectA s) mempty (SpotArea <$> xs)) ropts rss

-- * text
ts :: [(Text.Text, Point Double)]
ts = zip
  (map Text.singleton ['a' .. 'y'])
  [Point (sin (x * 0.1)) x | x <- [0 .. 25]]

textChart :: Chart Double
textChart =
  Chart
  (TextA (defaultTextStyle & field @"size" .~ (1.0 :: Double)) ["abcdefghij"])
  mempty
  [zero]

textsChart :: Chart Double
textsChart =
  Chart
  ( TextA
    (defaultTextStyle &
    field @"size" .~ 0.2)
    (fst <$> ts)
  )
  mempty
  (SpotPoint . snd <$> ts)

circle' :: Chart Double
circle' =
     Chart
      ( GlyphA (defaultGlyphStyle &
        field @"size" .~ 1 &
        field @"borderSize" .~ 0.2))
      mempty
      [zero]

smiley :: Chart Double
smiley =
     Chart
      ( GlyphA (defaultGlyphStyle &
        field @"size" .~ 1 &
        field @"borderSize" .~ (0.02 :: Double) &
        field @"shape" .~ SmileyGlyph))
      mempty
      [zero]

glyphs :: [Chart Double]
glyphs = zipWith
     (\(sh, bs) p ->
         Chart
         ( GlyphA (defaultGlyphStyle &
                    field @"size" .~ (0.2 :: Double) &
                    field @"borderSize" .~ bs &
                    field @"shape" .~ sh))
         mempty
         [p])
     [ (CircleGlyph, 0.01 :: Double)
     , (SquareGlyph, 0.01)
     , (RectSharpGlyph 0.75, 0.01)
     , (RectRoundedGlyph 0.75 0.01 0.01, 0.01)
     , (EllipseGlyph 0.75, 0)
     , (VLineGlyph 0.02, 0)
     , (HLineGlyph 0.02, 0)
     , (SmileyGlyph, 0.01)
     ]
     [SP x 0 | x <- [0..7]]


gdata :: [[Point Double]]
gdata =
  [ dataXY sin (Range 0 (2*pi)) 30
  , dataXY cos (Range 0 (2*pi)) 30
  ]

gopts :: [GlyphStyle]
gopts =
  [ field @"borderSize" .~ 0.001 $
    field @"size" .~ 0.1 $
    defaultGlyphStyle
  , field @"borderSize" .~ 0.001 $
    field @"size" .~ 0.1 $
    field @"color" .~ PixelRGBA8 100 30 30 100 $
    field @"shape" .~ RectRoundedGlyph 1.5 0.01 (0.01 :: Double) $
    defaultGlyphStyle
  ]

glyphsChart :: [Chart Double]
glyphsChart = zipWith (\d s -> Chart (GlyphA s) mempty (SpotPoint <$> d)) gdata gopts

-- textual
boundText :: [Chart Double]
boundText =
  [ Chart (RectA defaultRectStyle) mempty (SpotArea . styleBox <$> cs)
  , Chart a1 mempty ps
  ]
  where
  t1 = fst <$> ts
  ps = projectTo (vbArea $ aspect 3) $ SpotPoint . snd <$> ts
  t1s = TextA (defaultTextStyle & field @"size" .~ 0.2) . (:[]) <$> t1
  cs = zipWith (\x y -> Chart x mempty y) t1s ((:[]) <$> ps)
  a1 = TextA (defaultTextStyle & field @"size" .~ 0.2) t1

pixel' :: (Point Double -> Double) -> [Chart Double]
pixel' f =
  (\(r,c) -> Chart (RectA (RectStyle 0 black 0 c 1)) mempty [SpotArea r]) <$>
  pixelate f (fmap (pi*) one) (Point 100 100) blue grey

f1 :: TrigField a => Point a -> a
f1 (Point x y) = sin (cos (tan x)) * sin (cos (tan y))

cssCrisp :: CssRule
cssCrisp = CssRule [] [CssDeclaration "shape-rendering" [[CssString "crispEdges"]]]

label :: [Chart Double]
label =
  [placedLabel (Point (1.0 :: Double) 1.0) (45.0 :: Double) "text at (1,1) rotated by 45 degrees"]

-- * lines
ls :: [[Point Double]]
ls =
  map (uncurry Point) <$>
  [ [(0.0, 1.0), (1.0, 1.0), (2.0, 5.0)]
  , [(0.0, 0.0), (3.0, 3.0)]
  , [(0.5, 4.0), (0.5, 0)]
  ]

lopts :: [LineStyle]
lopts =
  zipWith (\w c -> defaultLineStyle & field @"color" .~ c & field @"width" .~ w)
  [0.015, 0.03, 0.01]
  [ PixelRGBA8 197 140 75 153
  , PixelRGBA8 60 127 43 153
  , PixelRGBA8 56 42 140 255
  ]

lines :: [Chart Double]
lines = zipWith (\d s -> Chart (LineA s) mempty (SpotPoint <$> d)) ls lopts

-- gline
gopts3 :: [GlyphStyle]
gopts3 =
  zipWith
  (\x y ->
     field @"color" .~ x $
     field @"borderColor" .~ x $
     field @"borderSize" .~ 0.005 $
     field @"shape" .~ y $
     field @"size" .~ 0.08 $
     defaultGlyphStyle)
  [ PixelRGBA8 120 67 30 120
  , PixelRGBA8 30 48 130 120
  , PixelRGBA8 60 60 60 120
  ]
  [EllipseGlyph 1.5, SquareGlyph, CircleGlyph]

glines :: [Chart Double]
glines = cs <> gs where
  cs = zipWith (\d s -> Chart (LineA s) mempty (SpotPoint <$> d)) ls lopts
  gs = zipWith (\d s -> Chart (GlyphA s) mempty (SpotPoint <$> d)) ls gopts3

lgdata :: [(Text.Text, Point Double)]
lgdata =
  (\(p@(Point x y)) -> (show x <> "," <> show y, fromIntegral <$> p)) <$>
    (Point <$> [0 .. 5] <*> [0 .. 5] :: [Point Int])

lglyph :: [Chart Double]
lglyph = txt <> gly where
  txt = (\(t, p) -> Chart (TextA
    ( defaultTextStyle &
      field @"opacity" .~ 0.2) [t]) (translateDA (Point ( 0 :: Double) 0.04))
    (SpotPoint <$> [p]))
    <$> lgdata
  gly = (\d -> Chart (GlyphA
    ( defaultGlyphStyle &
      field @"size" .~ 0.01 &
      field @"borderSize" .~ 0 &
      field @"color" .~ black))
      mempty
      (SpotPoint <$> [d])) <$> (snd <$> lgdata)

code :: (Semigroup a, IsString a) => a -> a
code x = "\n```\n" <> x <> "\n```\n"

main :: IO ()
main = do
  writeFile "other/mempty.md" (code $ xmlToText $ renderXml (Point (200.0 :: Double) 200.0) mempty)

  write "other/one.svg" (Point 200 200)
    (pad 1.1 $ chartSvg (one :: ViewBox Double) [Chart (RectA defaultRectStyle) mempty [one]])
  write "other/rotateOne.svg" (Point 200.0 200.0) rotateOne
  write "other/translateOne.svg" (Point 200.0 200.0) translateOne
  scratchWith (defaultScratchStyle & field @"fileName" .~ "other/rectChart.svg")
    [rectChart]
  scratchWith (defaultScratchStyle & field @"fileName" .~ "other/rectCharts.svg")
    rectCharts
  writeWith "other/pixel.svg" (Point 200.0 200.0) Map.empty "" [cssCrisp] (chartSvg one (pixel' f1))
  scratchWith
    (defaultScratchStyle &
     field @"fileName" .~ "other/textChart.svg" &
     field @"ratioAspect" .~ 3
    )
    [textChart]
  scratchWith
    (defaultScratchStyle &
     field @"fileName" .~ "other/textsChart.svg" &
     field @"ratioAspect" .~ 3
    )
    [textsChart]
  scratchWith
    (defaultScratchStyle &
     field @"fileName" .~ "other/boundText.svg" &
     field @"ratioAspect" .~ 3
    )
    boundText
  scratchWith
    (defaultScratchStyle &
     field @"fileName" .~ "other/label.svg" &
     field @"ratioAspect" .~ 1
    )
    label
  scratchWith
    (defaultScratchStyle &
     field @"fileName" .~ "other/circle.svg" &
     field @"ratioAspect" .~ 1
    )
    [circle']
  scratchWith
    (defaultScratchStyle &
     field @"fileName" .~ "other/glyphs.svg" &
     field @"ratioAspect" .~ 3
    )
    glyphs
  scratchWith
    (defaultScratchStyle &
     field @"fileName" .~ "other/smiley.svg" &
     field @"ratioAspect" .~ 1
    )
    [smiley]
  scratchWith
    (defaultScratchStyle &
     field @"fileName" .~ "other/glyphsChart.svg" &
     field @"ratioAspect" .~ 3
    )
    glyphsChart
  scratchWith
    (defaultScratchStyle &
     field @"fileName" .~ "other/lglyph.svg" &
     field @"ratioAspect" .~ 1.5
    )
    lglyph
  scratchWith
    (defaultScratchStyle &
     field @"fileName" .~ "other/lines.svg" &
     field @"ratioAspect" .~ 1.5
    )
    lines
  scratchWith
    (defaultScratchStyle &
     field @"fileName" .~ "other/glines.svg" &
     field @"ratioAspect" .~ 1.5
    )
    glines
  scratchWith
    (defaultScratchStyle &
     field @"fileName" .~ "other/compound.svg" &
     field @"ratioAspect" .~ 1.5
    )
    (lglyph <> glines)
  putStrLn (" 👍" :: Text.Text)


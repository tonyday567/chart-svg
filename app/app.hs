{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}
 
import Chart.Backend.Svg
import NumHask.Prelude hiding (Text)
import NumHask.Pair
import NumHask.Range
import NumHask.Rect
-- import Graphics.Svg.Types
import Lens.Micro 
import Codec.Picture.Types
import qualified Data.Text as Text
import Data.List ((!!))
import Data.Generics.Product (field)
import Data.Generics.Sum

-- import qualified Data.Text as Text
-- import qualified Data.Text.Lazy.IO as Lazy

dir :: FilePath
dir = "./other/"

ropts :: [RectStyle]
ropts =
  [ blob (PixelRGBA8 93 165 218 255) 0.2
  , blob (PixelRGBA8 120 80 60 255) 0.2
  ]

rss :: [[Rect Double]]
rss =
  [ rectXY (\x -> exp (-(x ** 2) / 2)) (Range -5 5) 50
  , rectXY (\x -> 0.5 * exp (-(x ** 2) / 8)) (Range -5 5) 50
  ]

one' :: Chart
one' =
  Chart
  (RectA $ RectStyle 0.1 (PixelRGBA8 102 102 102 127) 0.5 (PixelRGBA8 102 5 102 127)
   0.5 mempty)
  [SpotRect one]

one'' :: Chart
one'' =
  Chart
  (RectA $ RectStyle 0.1 (PixelRGBA8 102 102 102 127) 0.1 (PixelRGBA8 102 5 102 127)
   0.1 mempty)
  [SpotRect one]

rotateOne :: ChartSvg
rotateOne =
  multiSvg_ (aspect one) [showOrigin, one''] <>
  rotateSvg 30 (multiSvg_ (aspect one) [one'])

translateOne :: ChartSvg
translateOne = -- ChartSvg svg' vb' where
  multiSvg_ (aspect one) [showOrigin, one''] <>
  (translateSvg (Pair 1 1) $ rotateSvg 30 (multiSvg_ (aspect one) [one']))

  --(ChartSvg svg' vb) = translateSvg (Pair 1 1) rotateOne
  --vb' = vb <> vbox rotateOne

rectChart_Example :: ChartSvg
rectChart_Example =
  chartSvg_ (aspect 3) $ Chart (RectA $ ropts!!0) (SpotRect <$> rss!!0)

rectMulti_Example :: ChartSvg
rectMulti_Example = multiSvg_ (aspect 3) $
  zipWith (\s xs -> Chart (RectA s) (SpotRect <$> xs)) ropts rss

ts :: [(Text.Text, Pair Double)]
ts = zip
  (map Text.singleton ['a' .. 'y'])
  [Pair (sin (x * 0.1)) x | x <- [0 .. 25]]

textExample :: ChartSvg
textExample =
  chartSvg_ (ViewBox $ styleBox t1) t1
  where
    t1 = Chart
      (TextA (defaultTextStyle & field @"size" .~ 1) ["abcdefghij"])
      [SpotPoint $ Pair 0 0]

textChart_Example :: ChartSvg
textChart_Example =
  chartSvg_
  (aspect 3) $
  Chart
  ( TextA
    (defaultTextStyle &
    field @"size" .~ 0.2)
    (fst <$> ts)
  )
  (SpotPoint . snd <$> ts)

circleExample :: ChartSvg
circleExample =
  chartSvg_ (ViewBox $ styleBox c) c
  where
    c =
     Chart
      ( GlyphA (defaultGlyphStyle &
        field @"size" .~ 1 &
        field @"borderSize" .~ 0.2))
      [SpotPoint $ Pair 0 0]

smileyExample :: ChartSvg
smileyExample =
  chartSvg_ (ViewBox $ styleBox c) c
  where
    c =
     Chart
      ( GlyphA (defaultGlyphStyle &
        field @"size" .~ 1 &
        field @"borderSize" .~ 0.02 &
        field @"shape" .~ SmileyGlyph))
      [SpotPoint $ Pair 0 0]

cs1 :: [Chart]
cs1 = zipWith
     (\(sh, bs) p ->
         Chart
         ( GlyphA (defaultGlyphStyle &
                    field @"size" .~ 1 &
                    field @"borderSize" .~ bs &
                    field @"shape" .~ sh))
         [SpotPoint p])
     [ (CircleGlyph, 0.1)
     , (SquareGlyph, 0.1)
     , (RectSharpGlyph 0.75, 0.1)
     , (RectRoundedGlyph 0.75 0.1 0.1, 0.1)
     , (EllipseGlyph 0.75, 0)
     , (VLineGlyph 0.2, 0)
     , (HLineGlyph 0.2, 0)
     , (SmileyGlyph, 0.1)
     ]
     [Pair x 0 | x <- [0..7]]


gdata :: [[Pair Double]]
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
    field @"shape" .~ (RectRoundedGlyph 1.5 0.01 0.01) $
    defaultGlyphStyle
  ]

glyphExample :: ChartSvg
glyphExample =
  multiSvg_ (aspect 3) cs
  where
    cs = zipWith (\d s -> Chart (GlyphA s) (SpotPoint <$> d)) gdata gopts

-- rendering

fitted :: [Chart] -> ChartSvg
fitted cs =
  multiSvg_ (ViewBox $ styleBoxes cs) cs

decoration :: ChartSvg -> ChartSvg
decoration ch = frame (border 0.01 ublue 1) ch <> ch

rend :: Pair Double -> FilePath -> ChartSvg -> IO ()
rend asp f ch = save (dir<>f) asp (decoration ch)


-- textual

boundText :: ChartSvg
boundText = multiSvg_ (aspect 3)  [cr, ct]
  where
  t1 = fst <$> ts
  ps = projectTo (aspect 3) $ SpotPoint . snd <$> ts
  t1s = TextA (defaultTextStyle & field @"size" .~ 0.2) . (:[]) <$> t1
  cs = zipWith Chart t1s ((:[]) <$> ps)
  r1 = styleBox <$> cs
  r1' = (\(Rect x z y w) -> Rect (x - (z-x)/2) (z - (z-x)/2) y w) <$> r1
  a1 = TextA (defaultTextStyle & field @"size" .~ 0.2) t1
  cr = ( (Chart (RectA defaultRectStyle) (SpotRect <$> r1)))
  ct = ((Chart a1 (ps)))

-- TODO: shortcut the svg boiler-plate
pixelExample :: ChartSvg
pixelExample = multiSvg_ (aspect 1) $
  (\(r,c) -> Chart (RectA (RectStyle 0 ublack 0 c 1 mempty)) ([SpotRect r])) <$>
  pixelate (\(Pair x y) -> (x+y)*(x+y)) one (Pair 40 40) ured ublack


labelExample :: ChartSvg
labelExample =
    multiSvg (ViewBox $ Rect -5 5 -5 5)
    [ nudgeLE (Pair 1 1) 45
    , nudgeLE (Pair 0 0) 0 & field @"ann" . _As @"TextA" %~ _1 . field @"opacity" .~ 0.2
    , Chart (RectA defaultRectStyle) [SpotRect (rotatedRect 0 (styleBox $ nudgeLE (Pair 1 1) 45))]
    , Chart
      ( GlyphA (defaultGlyphStyle &
        field @"size" .~ 0.4 &
        field @"borderSize" .~ 0.02 &
        field @"color" .~ ured))
      [SpotPoint $ Pair 0 0]]
  where
    labelChart = Chart (TextA (defaultTextStyle & field @"size" .~ 1 & field @"textDA" %~ (<> translateDA (Pair 0 0)) ) (["translated text" :: Text.Text])) ([SpotPoint $ Pair 0 0])
    nudgeLE p d = labelChart & field @"ann" . _As @"TextA" %~ _1 . field @"textDA" %~ (<> translateDA p <> rotateDA d)


-- fabulous lines
ls :: [[Pair Double]]
ls =
  map (uncurry Pair) <$>
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

lineExample :: ChartSvg
lineExample =
  multiSvg_ (aspect 1.5) cs
  where
    cs = zipWith (\d s -> Chart (LineA s) (SpotPoint <$> d)) ls lopts

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



glineChart = cs <> gs where
  cs = zipWith (\d s -> Chart (LineA s) (SpotPoint <$> d)) ls lopts
  gs = zipWith (\d s -> Chart (GlyphA s) (SpotPoint <$> d)) ls gopts3

glineExample :: ChartSvg
glineExample =
  multiSvg_ (aspect 1.5) glineChart

lgdata :: [(Text.Text, Pair Double)]
lgdata =
  (\(p@(Pair x y)) -> (show x <> "," <> show y, fromIntegral <$> p)) <$>
    (Pair <$> [0 .. 5] <*> [0 .. 5] :: [Pair Int])

lglyphExample :: ChartSvg
lglyphExample = multiSvg_ (aspect 1) lglyphChart

lglyphChart :: [Chart]
lglyphChart = (ts <> gs)
  where
    ts = (\(t, p) ->
            Chart
            (TextA
             ( defaultTextStyle &
               field @"opacity" .~ 0.2 &
               field @"textDA" %~ (<> translateDA (Pair 0 0.04))) [t]) (SpotPoint <$> [p]))
         <$> lgdata
    gs = (\d ->
            Chart
            (GlyphA
             (defaultGlyphStyle &
              field @"size" .~ 0.01 &
              field @"borderSize" .~ 0 &
              field @"color" .~ ublack))
            (SpotPoint <$> [d])) <$> (snd <$> lgdata)

compoundExample :: ChartSvg
compoundExample = multiSvg_ (aspect 1.5) (lglyphChart <> glineChart)

main :: IO ()
main = do
  rend (Pair 200.0 200.0) "zero.svg" mempty
  rend (Pair 200.0 200.0) "one'.svg" (chartSvg_ (aspect one) one')
  rend (Pair 200.0 200.0) "rotateOne.svg" rotateOne
  rend (Pair 200.0 200.0) "translateOne.svg" translateOne
  rend (Pair 600.0 200.0) "rectChart_Example.svg" rectChart_Example
  rend (Pair 600.0 200.0) "rectMulti_Example.svg" rectMulti_Example
  rend (Pair 600.0 200.0) "textExample.svg" textExample
  rend (Pair 600.0 200.0) "textChart_Example.svg" textChart_Example
  rend (Pair 200.0 200.0) "circleExample.svg" circleExample
  rend (Pair 200.0 200.0) "glyphsExample.svg" (fitted cs1)
  rend (Pair 600.0 200.0) "glyphExample.svg" glyphExample
  rend (Pair 200.0 200.0) "smileyExample.svg" smileyExample
  rend (Pair 600 200) "boundText.svg" boundText
  rend (Pair 400.0 400.0) "pixelExample.svg" pixelExample
  rend (Pair 600.0 200.0) "labelExample.svg" labelExample
  rend (Pair 400.0 400.0) "lglyphExample.svg" lglyphExample
  rend (Pair 600.0 400.0) "lineExample.svg" lineExample
  rend (Pair 600.0 400.0) "glineExample.svg" glineExample
  rend (Pair 600.0 400.0) "compoundExample.svg" compoundExample
  putStrLn (" 👍" :: Text.Text)


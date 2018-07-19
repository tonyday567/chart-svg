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

cs1 :: [Chart]
cs1 = zipWith
     (\(sh, bs) p ->
         Chart
         ( GlyphA (defaultGlyphStyle &
                    field @"size" .~ 0.5 &
                    field @"borderSize" .~ bs &
                    field @"shape" .~ sh))
         [SpotPoint p])
     [ (CircleGlyph, 0.1)
     , (SquareGlyph, 0.1)
     , (RectSharpGlyph 1.5, 0)
     , (RectRoundedGlyph 1.5 0.02, 0)
     , (EllipseGlyph 1.5, 0)
     , (VLineGlyph 0.2, 0)
     , (HLineGlyph 0.2, 0)
     , (SmileyGlyph, 0)
     ]
     [Pair x 0 | x <- [0..7]]

fitted :: [Chart] -> ChartSvg
fitted cs =
  multiSvg_ (ViewBox $ styleBoxes cs) cs

decoration :: ChartSvg -> ChartSvg
decoration ch = frame (border 0.01 ublue 1) ch <> ch

rend :: Pair Double -> FilePath -> ChartSvg -> IO ()
rend asp f ch = save (dir<>f) asp (decoration ch)

exp1 :: IO ()
exp1 = do
  let t1 = fst <$> ts
  let ps = projectTo (aspect 3) $ SpotPoint . snd <$> ts
  let t1s = TextA (defaultTextStyle & field @"size" .~ 0.2) . (:[]) <$> t1
  let cs = zipWith Chart t1s ((:[]) <$> ps)
  let r1 = styleBox <$> cs
  let a1 = TextA (defaultTextStyle & field @"size" .~ 0.2) t1
  let cr = (chartSvg_ (aspect 3) (Chart (RectA defaultRectStyle) (SpotRect <$> r1)))
  let ct = (chartSvg_ (aspect 3) (Chart a1 (ps)))
  rend (Pair 600 200) "exp1.svg" $ ct <> cr

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
  exp1
  putStrLn (" 👍" :: Text.Text)

{-

pixelChart_Example =
  pixelChart_ asquare
  [(\(r,c) ->
      Pixel r
      (ucolor $ blend c
       (acolor $ UColor 0.47 0.73 0.86 1)
       (acolor $ UColor 0.01 0.06 0.22 1)
      )) <$>
   rectF (\(Pair x y) -> (x+y)*(x+y))
   one (Pair 40 40)]


boxedLabelledExample =
  labelled (LabelOptions
    ( field @"rotation" .~ (Just (-45)) $
      field @"alignH" .~ AlignLeft $
      defaultTextOptions)
    (Pair 1 1) 0.1)
  "a label"
  (glyph_ (field @"shape" .~ Octagaon $ defaultGlyphOptions))

ls :: [[Pair Double]]
ls =
  map (uncurry Pair) <$>
  [ [(0.0, 1.0), (1.0, 1.0), (2.0, 5.0)]
  , [(0.0, 0.0), (3.0, 3.0)]
  , [(0.5, 4.0), (0.5, 0)]
  ]

lopts :: [LineOptions]
lopts =
  zipWith LineOptions
  [0.015, 0.03, 0.01]
  [ UColor 0.773 0.510 0.294 0.6
  , UColor 0.235 0.498 0.169 0.6
  , UColor 0.204 0.161 0.537 1.0
  ]

lineChart_Example = lineChart_ lopts sixbyfour ls

lgdata :: [(Text, Pair Double)]
lgdata =
  (\(p@(Pair x y)) -> (show x <> "," <> show y, fromIntegral <$> p)) <$>
    (Pair <$> [0 .. 5] <*> [0 .. 5] :: [Pair Int])

gopts :: [GlyphOptions]
gopts =
  [ field @"borderSize" .~ 0.001 $
    field @"size" .~ 0.1 $
    defaultGlyphOptions
  , field @"borderSize" .~ 0.001 $
    field @"size" .~ 0.1 $
    field @"color" .~ ucolor (rybColor 7 `withOpacity` 0.4) $
    field @"shape" .~ (RectRounded 1.5 0.01) $
    defaultGlyphOptions
  ]

gdata :: [[Pair Double]]
gdata =
  [ dataXY sin (Range 0 (2*pi)) 30
  , dataXY cos (Range 0 (2*pi)) 30
  ]

glyphChart_Example = glyphChart_ gopts widescreen gdata

lglyphChart_Example =
  lglyphChart_
  [ field @"gap" .~ 0.015 $
    field @"text" . field @"size" .~ 0.12 $
    defaultLabelOptions]
  [field @"color" .~ ublack $
   field @"borderSize" .~ 0 $
   field @"size" .~ 0.01 $
   defaultGlyphOptions]
  sixbyfour
  [lgdata]

labelledExample =
  labelled (LabelOptions
    ( field @"rotation" .~ (Just (-45)) $
      field @"alignH" .~ AlignLeft $
      defaultTextOptions)
    (Pair 1 1) 0.1)
  "a label"
  (glyph_ defaultGlyphOptions)

gopts3 :: [GlyphOptions]
gopts3 =
  zipWith
  (\x y ->
     field @"color" .~ ucolor (withOpacity (d3Colors1 x) 0.2) $
     field @"borderColor" .~ ucolor (withOpacity (d3Colors1 x) 1) $
     field @"borderSize" .~ 0.005 $
     field @"shape" .~ y $
     field @"size" .~ 0.08 $
     defaultGlyphOptions)
  [6,8,2]
  [Ellipse 1.5, Square, Circle]

glineChart_Example = glineChart_ lopts gopts3 sixbyfour ls

titles' :: [(TitleOptions, Text)]
titles' =
  [ (defaultTitleOptions, "Example Chart")
  , ( field @"text" . field @"size" .~ 0.08 $
      field @"align" .~ AlignRight $
      field @"place" .~ PlaceBottom $
      defaultTitleOptions
    , "an example chart for chart-unit")
  ]

legends' :: [(LegendType, Text)]
legends' =
  zipWith
    (\x y -> (LegendLine x 0.1, y))
    lopts
    ["hockey stick", "diagonal line", "verticle line"]

hopts = 
     field @"titles" .~ titles' $
     field @"canvas" . field @"color" .~ UColor 0.3 0.3 0.3 0.3 $
     field @"axes" %~ map (field @"outerPad" .~ 1) $
     field @"legends" .~ [ field @"chartType" .~ legends' $
                           defaultLegendOptions] $
     defaultHudOptions

hud_Example = hud_ hopts sixbyfour one

pad' :: Double -> Rect Double -> Rect Double
pad' p (Rect x z y w) = Rect (x-wid) (z+wid) (y-hei) (w+hei)
  where
    wid = (p - 1) * (z - x)
    hei = (p - 1) * (w-y)

-}

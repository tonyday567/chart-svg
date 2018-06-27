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

rectExample :: Chart
rectExample =
  chart_ (aspect one) $
  RectChart
  (RectStyle 0.1 (PixelRGBA8 102 102 102 127) 0.3 (PixelRGBA8 102 5 102 127)
   0.3 mempty)
  [one]

rotateExample :: Chart
rotateExample = rotateChart 30 rectExample

translateExample :: Chart
translateExample = Chart svg' vb' where
  (Chart svg' vb) = translateChart (Pair 1 1) rotateExample
  vb' = vb <> vbox rotateExample

rectChart_Example :: Chart
rectChart_Example =
  chart_ (aspect 3) $ RectChart (ropts!!0) (rss!!0)

rectMulti_Example :: Chart
rectMulti_Example = multi_ (aspect 3) $ zipWith RectChart ropts rss

ts :: [(Text.Text, Pair Double)]
ts = zip
  (map Text.singleton ['a' .. 'y'])
  [Pair (sin (x * 0.1)) x | x <- [0 .. 25]]

textExample :: Chart
textExample =
  chart_ (ViewBox $ styleBox t1) t1
  where
    t1 = TextChart
      (defaultTextStyle & field @"size" .~ 1)
      [("hello jane",Pair 0 0)]

textChart_Example :: Chart
textChart_Example =
  chart_
  (aspect 3) $
  TextChart
  ( defaultTextStyle &
    field @"size" .~ 0.2
  )
  ts

circleExample :: Chart
circleExample =
  chart_ (ViewBox $ styleBox c) c
  where
    c =
      GlyphChart
      ( defaultGlyphStyle &
        field @"size" .~ 1 &
        field @"borderSize" .~ 0.2)
      [Pair 0 0]

frameStyle :: RectStyle
frameStyle = border 0.01 ublue 1

frame :: Chart -> Chart
frame c = c <> frameChart frameStyle c

main :: IO ()
main = do
  save (dir<>"mempty.svg") (Pair 200.0 200.0) (frame mempty)
  save (dir<>"rectExample.svg") (Pair 200.0 200.0)
    (padChart 1.1 $ frame $ rectExample)
  save (dir<>"rotateExample.svg") (Pair 200.0 200.0)
    (padChart 1.1 $ frame $ rotateExample)
  save (dir<>"translateExample.svg") (Pair 200.0 200.0)
    (padChart 1.1 $ frame $ translateExample)
  save (dir<>"rectChart_Example.svg") (Pair 600.0 200.0) $
    frame rectChart_Example
  save (dir<>"rectMulti_Example.svg") (Pair 600.0 200.0) $
    frame rectMulti_Example
  save (dir<>"textExample.svg") (Pair 600.0 200.0) $ frame textExample
  save (dir<>"textChart_Example.svg") (Pair 600.0 200.0) $ frame textChart_Example
  save (dir<>"circleExample.svg") (Pair 200.0 200.0) $ frame circleExample
  putStrLn (" 👍" :: Text.Text)

{-
showOrigin a svg = svg <> (glyph_ (field @"borderSize" .~ 0 $ field @"size" .~ a $ field @"color" .~ UColor 0 0 0 1 $ defaultGlyphOptions))

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

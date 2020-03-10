{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Chart.Examples where

import Chart
import Control.Applicative
import Control.Lens
import Data.Maybe
import qualified Data.Text as Text
import Data.Text (Text)
import GHC.Generics
import Protolude

data Ex
  = Ex
      { excss :: SvgOptions,
        exhc :: HudOptions,
        exmaxcs :: Int,
        exanns :: [Annotation],
        exspots :: [[Spot Double]]
      }
  deriving (Eq, Show, Generic)

makeExample :: HudOptions -> [Chart Double] -> Ex
makeExample hs cs = Ex defaultSvgOptions hs (length cs) (view #annotation <$> cs) (fmap (fmap realToFrac) . view #spots <$> cs)

exampleHudOptions :: Text -> Maybe Text -> Maybe (LegendOptions, [(Annotation, Text)]) -> HudOptions
exampleHudOptions t1 t2 legends' =
  defaultHudOptions
    & #hudTitles
      .~ ( [ defaultTitle t1
               & #style . #size .~ 0.08
               & #style . #opacity .~ 0.6
           ]
             <> maybe
               []
               ( \x ->
                   [ defaultTitle x
                       & #style . #size .~ 0.05
                       & #style . #opacity .~ 0.6
                       & #place .~ PlaceBottom
                       & #anchor .~ AnchorEnd
                   ]
               )
               t2
         )
    & #hudLegend .~ legends'

-- | line example
hockey :: Ex
hockey =
  Ex
    defaultSvgOptions
    ( exampleHudOptions
        "Example Chart"
        (Just "An example from chart-svg")
        (Just (legopts, zip (LineA <$> lopts) ["hockey", "line", "vertical"]))
    )
    3
    (LineA <$> lopts)
    (fmap SpotPoint <$> ls)

ls :: [[Point Double]]
ls =
  fmap (uncurry Point)
    <$> [ [(0.0, 1.0), (1.0, 1.0), (2.0, 5.0)],
          [(0.0, 0.0), (2.8, 3.0)],
          [(0.5, 4.0), (0.5, 0)]
        ]

lopts :: [LineStyle]
lopts =
  [ defaultLineStyle & #color .~ PixelRGB8 197 130 75 & #width .~ 0.015
      & #opacity .~ 0.6,
    defaultLineStyle & #color .~ PixelRGB8 60 127 43 & #width .~ 0.03
      & #opacity .~ 0.6,
    defaultLineStyle & #color .~ PixelRGB8 52 41 137 & #width .~ 0.01
      & #opacity .~ 1.0
  ]

legopts :: LegendOptions
legopts =
  defaultLegendOptions
    & #lsize .~ 0.2
    & #ltext . #size .~ 0.25
    & #innerPad .~ 0.05
    & #lscale .~ 0.25
    & #lplace .~ PlaceAbsolute (Point 0.5 (-0.3))

-- rects
ropts :: [RectStyle]
ropts =
  [ blob (PixelRGB8 93 165 218) 0.5,
    blob (PixelRGB8 120 80 60) 0.5
  ]

normExample :: Ex
normExample =
  Ex
    defaultSvgOptions
    mempty
    2
    (RectA <$> ropts)
    (fmap SpotRect <$> rss)

rss :: [[Rect Double]]
rss =
  [ gridR (\x -> exp (- (x ** 2) / 2)) (Range (-5) 5) 50,
    gridR (\x -> 0.5 * exp (- (x ** 2) / 8)) (Range (-5) 5) 50
  ]

rs :: RectStyle
rs = RectStyle 0.1 (PixelRGB8 102 102 102) 0.5 (PixelRGB8 102 5 102) 0.5

rs' :: RectStyle
rs' = rs & #opacity .~ 0.1 & #borderOpacity .~ 0.1

oneChart :: Chart Double
oneChart = Chart (RectA rs) [SpotRect unitRect]

onePixel :: Chart Double
onePixel = Chart (PixelA defaultPixelStyle) [SpotRect unitRect]

oneExample :: Ex
oneExample =
  Ex
    defaultSvgOptions
    mempty
    1
    [RectA rs]
    [[SpotRect unitRect]]

oneChart' :: Chart Double
oneChart' = Chart (RectA rs') [SpotRect unitRect]

rectChart :: [Chart Double]
rectChart = take 1 rectCharts

rectCharts :: [Chart Double]
rectCharts =
  zipWith (\s xs -> Chart (RectA s) (SpotRect <$> xs)) ropts rss

-- * text

ts :: [(Text.Text, Point Double)]
ts =
  zip
    (fmap Text.singleton ['a' .. 'y'])
    [Point (sin (x * 0.1)) x | x <- [0 .. 25]]

ts12 :: [(Text.Text, Point Double)]
ts12 =
  zip
    (fmap (Text.pack . replicate 12) ['m', 'o', 'n', 'a', 'd'])
    [Point (sin (x * 0.1)) x | x <- [0 .. 25]]

textChart :: Chart Double
textChart =
  Chart
    (TextA (defaultTextStyle & #size .~ (1.0 :: Double)) ["abcdefghij"])
    [SP 0 0]

textsChart :: Chart Double
textsChart =
  Chart
    ( TextA
        ( defaultTextStyle
            & #size .~ 0.1
        )
        (fst <$> ts)
    )
    (SpotPoint . snd <$> ts)

textExample :: Ex
textExample =
  Ex
    defaultSvgOptions
    defaultHudOptions
    26
    (TextA (defaultTextStyle & (#size .~ (0.05 :: Double))) . (: []) . fst <$> ts)
    ((: []) . SpotPoint . snd <$> ts)

circle' :: Chart Double
circle' =
  Chart
    ( GlyphA
        ( defaultGlyphStyle
            & #size .~ 1
            & #borderSize .~ 0.2
        )
    )
    [SP 0 0]

glyphs :: [Chart Double]
glyphs =
  zipWith
    ( \(sh, bs) p ->
        Chart
          ( GlyphA
              ( defaultGlyphStyle
                  & #size .~ (0.1 :: Double)
                  & #borderSize .~ bs
                  & #shape .~ sh
              )
          )
          [p]
    )
    [ (CircleGlyph, 0.01 :: Double),
      (SquareGlyph, 0.01),
      (RectSharpGlyph 0.75, 0.01),
      (RectRoundedGlyph 0.75 0.01 0.01, 0.01),
      (EllipseGlyph 0.75, 0),
      (VLineGlyph, 0.01),
      (HLineGlyph, 0.01),
      (TriangleGlyph (Point 0.0 0.0) (Point 1 1) (Point 1 0), 0.01)
    ]
    [SP x 0 | x <- [0 .. (7 :: Double)]]

glyphsExample :: Ex
glyphsExample = makeExample mempty glyphs

gdata :: [[Point Double]]
gdata =
  [ gridP sin (Range 0 (2 * pi)) 30,
    gridP cos (Range 0 (2 * pi)) 30
  ]

gopts :: [GlyphStyle]
gopts =
  [ (#borderSize .~ 0.001)
      . (#size .~ 0.1)
      $ defaultGlyphStyle,
    (#borderSize .~ 0.001)
      . (#size .~ 0.1)
      . (#color .~ PixelRGB8 100 30 30)
      . (#shape .~ RectRoundedGlyph 1.5 0.01 (0.01 :: Double))
      $ defaultGlyphStyle
  ]

glyphsChart :: [Chart Double]
glyphsChart = zipWith (\d s -> Chart (GlyphA s) (SpotPoint <$> d)) gdata ((#size .~ 0.02) <$> gopts)

-- textual
boundText :: [Chart Double]
boundText =
  [ t1,
    t2,
    Chart BlankA [SpotRect (Rect 0 0.1 (-0.5) 0.5)],
    Chart (RectA defaultRectStyle) [SpotRect (defRectS $ styleBox t1)],
    Chart (RectA defaultRectStyle) [SpotRect (defRectS $ styleBox t2)]
  ]
  where
    t1 =
      Chart
        ( TextA
            (defaultTextStyle & #anchor .~ AnchorStart & #hsize .~ 0.45 & #size .~ 0.08)
            ["a pretty long piece of text"]
        )
        [SP 0.0 0.0]
    t2 =
      Chart
        ( TextA
            (defaultTextStyle & #anchor .~ AnchorStart & #hsize .~ 0.45 & #size .~ 0.08)
            ["another pretty long piece of text"]
        )
        [SP 1 1]

pixelOptions :: PixelOptions
pixelOptions =
  defaultPixelOptions & #poRange .~ fmap (pi *) unitRect & #poGrain .~ Point 100 100

f1 :: (Floating a) => Point a -> a
f1 (Point x y) = sin (cos (tan x)) * sin (cos (tan y))

-- | pixel example
pixelEx :: ([Chart Double], [Hud Double])
pixelEx = pixelfl f1 (defaultPixelOptions & #poGrain .~ Point 100 100 & #poRange .~ Rect 1 2 1 2) (defaultPixelLegendOptions "pixel test")

placedLabel :: (Chartable a) => Point a -> a -> Text.Text -> Chart a
placedLabel p d t =
  Chart ( TextA ( defaultTextStyle & #rotation ?~ realToFrac d ) [t] ) [SpotPoint p]

label :: [Chart Double]
label =
  [placedLabel (Point (1.0 :: Double) 1.0) (45.0 :: Double) "text at (1,1) rotated by 45 degrees"]

lines' :: [Chart Double]
lines' = zipWith (\d s -> Chart (LineA s) (SpotPoint <$> d)) ls lopts

-- gline
gopts3 :: [GlyphStyle]
gopts3 =
  zipWith
    ( \x y ->
        (#color .~ x)
          . (#borderColor .~ x)
          . (#borderSize .~ 0.005)
          . (#shape .~ y)
          . (#size .~ 0.08)
          $ defaultGlyphStyle
    )
    [ PixelRGB8 120 67 30,
      PixelRGB8 30 48 130,
      PixelRGB8 60 60 60
    ]
    [EllipseGlyph 1.5, SquareGlyph, CircleGlyph]

glines :: [Chart Double]
glines = cs <> gs
  where
    cs = zipWith (\d s -> Chart (LineA s) (SpotPoint <$> d)) ls lopts
    gs = zipWith (\d s -> Chart (GlyphA s) (SpotPoint <$> d)) ls gopts3

lgdata :: [(Text.Text, Point Double)]
lgdata =
  (\p@(Point x y) -> (Text.pack (show x <> "," <> show y), fromIntegral <$> p))
    <$> (Point <$> [0 .. 5] <*> [0 .. 5] :: [Point Int])

lglyph :: [Chart Double]
lglyph = txt <> gly
  where
    txt =
      ( \(t, p) ->
          Chart
            ( TextA
                ( defaultTextStyle
                    & #opacity .~ 0.2
                    & #translate ?~ Point 0 0.04
                )
                [t]
            )
            (SpotPoint <$> [p])
      )
        <$> lgdata
    gly =
      ( \d ->
          Chart
            ( GlyphA
                ( defaultGlyphStyle
                    & #size .~ 0.01
                    & #borderSize .~ 0
                    & #color .~ black
                )
            )
            (SpotPoint <$> [d])
      )
        <$> (snd <$> lgdata)

defExample :: Ex
defExample =
  makeExample
    defaultHudOptions
    [Chart (GlyphA defaultGlyphStyle) (SpotPoint <$> gridP sin (Range 0 (2 * pi)) 30)]

legExample :: Ex
legExample =
  makeExample (mempty & #hudLegend .~ Just (defaultLegendOptions, l1))
    [Chart (LineA defaultLineStyle) [SP 0 0, SP 1 1]]
  where
    l1 =
      [ (GlyphA defaultGlyphStyle, "glyph"),
        (RectA defaultRectStyle, "rect"),
        (TextA (defaultTextStyle & #anchor .~ AnchorStart) ["content"], "text"),
        (LineA defaultLineStyle, "line"),
        (BlankA, "blank")
      ]

corners' :: Rect Double -> Double -> [Chart Double]
corners' (Rect x z y w) s =
  [ Chart
      ( GlyphA
          . (#borderSize .~ 0)
          . (#size .~ s)
          $ defaultGlyphStyle
      )
      [SP x y, SP x w, SP z y, SP z w]
  ]

hud1 :: [Hud Double]
hud1 =
  [ tick PlaceBottom defaultTick
      <> tick PlaceLeft defaultTick
  ]

euclid :: Integer -> [(Integer, Integer)]
euclid x = filter (\(a, b) -> a /= 0 && b /= 0) $ (\m n -> (m * m - n * n, 2 * m * n)) <$> [1 .. x] <*> [1 .. x] :: [(Integer, Integer)]

legendTest :: HudOptions
legendTest =
  defaultHudOptions
    & #hudLegend
    .~ Just
      ( defaultLegendOptions
          & #lscale .~ 0.3
          & #lplace .~ PlaceAbsolute (Point 0.0 0.0)
          & #lsize .~ 0.12
          & #ltext . #size .~ 0.16,
        l1
      )
  where
    l1 =
      [ (GlyphA defaultGlyphStyle, "glyph"),
        (RectA defaultRectStyle, "rect"),
        (TextA (defaultTextStyle & #anchor .~ AnchorStart) ["content"], "text"),
        (LineA defaultLineStyle, "line"),
        (GlyphA defaultGlyphStyle, "abcdefghijklmnopqrst"),
        (BlankA, "blank")
      ]

writeChartExample :: FilePath -> Ex -> IO ()
writeChartExample fp (Ex css' hc' _ anns' spots') =
  writeHudOptionsChart fp css' hc' [] (zipWith Chart anns' spots')

barDataExample :: BarData
barDataExample =
  BarData
    [[1, 2, 3, 5, 8, 0, -2, 11, 2, 1], [1 .. 10]]
    (Just (("row " <>) . Text.pack . show <$> [1 .. 11]))
    (Just (("column " <>) . Text.pack . show <$> [1 .. 2]))

barExample :: Ex
barExample = makeExample hc cs
  where
    (hc, cs) = barChart defaultBarOptions barDataExample

writeAllExamples :: IO ()
writeAllExamples = do
  writeCharts "other/mempty.svg" []
  writeCharts "other/one.svg" [Chart (RectA defaultRectStyle) [SpotRect (unitRect :: Rect Double)]]
  writeHudOptionsChart "other/hud.svg" defaultSvgOptions defaultHudOptions [] []
  writeCharts "other/rectChart.svg" rectChart
  writeCharts "other/rectCharts.svg" rectCharts
  writeHudOptionsChart
    "other/pixel.svg"
    defaultSvgOptions
    mempty
    (snd pixelEx)
    (fst pixelEx)
  writeCharts "other/textChart.svg" [textChart]
  writeCharts "other/textsChart.svg" [textsChart]
  writeCharts "other/boundText.svg" boundText
  writeCharts "other/label.svg" label
  writeCharts "other/circle.svg" [circle']
  writeCharts "other/glyphs.svg" glyphs
  writeCharts "other/glyphsChart.svg" glyphsChart
  writeCharts "other/lglyph.svg" lglyph
  writeCharts "other/lines.svg" lines'
  writeCharts "other/glines.svg" glines
  writeCharts "other/compound.svg" (lglyph <> glines)
  writeChartExample "other/def.svg" defExample
  writeChartExample "other/leg.svg" legExample
  writeChartExample "other/hockey.svg" hockey
  writeChartExample "other/bar.svg" barExample
  putStrLn (" 👍" :: Text)

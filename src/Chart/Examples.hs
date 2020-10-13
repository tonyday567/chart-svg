{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Chart.Examples where

import Chart
import Control.Lens
import Data.List ((!!))
import qualified Data.Text as Text
import NumHask.Prelude
import Web.Rep

data Ex
  = Ex
      { excss :: SvgOptions,
        exhc :: HudOptions,
        exmaxcs :: Int,
        exanns :: [Annotation],
        exxys :: [[XY Double]]
      }
  deriving (Eq, Show, Generic)

makeExample :: HudOptions -> [Chart Double] -> Ex
makeExample hs cs = Ex defaultSvgOptions hs (length cs) (view #annotation <$> cs) (fmap (fmap realToFrac) . view #xys <$> cs)

repEx :: (Monad m) => Ex -> SharedRep m (Text, Text)
repEx (Ex css hc maxcs anns xs) =
  repChartsWithStaticData css hc maxcs (zipWith Chart anns xs)

writeChartExample :: FilePath -> Ex -> IO ()
writeChartExample fp (Ex css' hc' _ anns' xys') =
  writeHudOptionsChart fp css' hc' [] (zipWith Chart anns' xys')

-- | minimal example
memptyExample :: Ex
memptyExample = Ex defaultSvgOptions mempty 1 [] []

-- | unit example
unitExample :: Ex
unitExample = Ex defaultSvgOptions mempty 1 [RectA defaultRectStyle] [[one]]

-- | hud example
hudExample :: Ex
hudExample = Ex defaultSvgOptions defaultHudOptions 1 [] []

-- | rect example
rectExample :: Ex
rectExample =
  Ex
    defaultSvgOptions
    (defaultHudOptions & set #hudAxes [defaultAxisOptions])
    2
    (RectA <$> ropts)
    (fmap RectXY <$> rss)

rss :: [[Rect Double]]
rss =
  [ gridR (\x -> exp (- (x ** 2) / 2)) (Range (-5) 5) 50,
    gridR (\x -> 0.5 * exp (- (x ** 2) / 8)) (Range (-5) 5) 50
  ]

ropts :: [RectStyle]
ropts =
  [ blob (fromRGB (palette !! 0) 0.4),
    blob (fromRGB (palette !! 5) 0.4)
  ]

-- | line example
lineExample :: Ex
lineExample =
  Ex
    defaultSvgOptions
    ( exampleLineHudOptions
        "Line Chart"
        (Just "An example from chart-svg")
        (Just (legopts, zip (LineA <$> lopts) ["hockey", "line", "vertical"]))
    )
    3
    (LineA <$> lopts)
    (fmap PointXY <$> ls)

ls :: [[Point Double]]
ls =
  fmap (uncurry Point)
    <$> [ [(0.0, 1.0), (1.0, 1.0), (2.0, 5.0)],
          [(0.0, 0.0), (2.8, 3.0)],
          [(0.5, 4.0), (0.5, 0)]
        ]

lopts :: [LineStyle]
lopts =
  [ defaultLineStyle & #color .~ (palette1 !! 0) & #width .~ 0.015,
    defaultLineStyle & #color .~ (palette1 !! 1) & #width .~ 0.03,
    defaultLineStyle & #color .~ (palette1 !! 5) & #width .~ 0.01
  ]

legopts :: LegendOptions
legopts =
  defaultLegendOptions
    & #lsize .~ 0.2
    & #ltext . #size .~ 0.25
    & #innerPad .~ 0.05
    & #lscale .~ 0.25
    & #lplace .~ PlaceAbsolute (Point 0.5 (-0.3))

exampleLineHudOptions :: Text -> Maybe Text -> Maybe (LegendOptions, [(Annotation, Text)]) -> HudOptions
exampleLineHudOptions t1 t2 legends' =
  defaultHudOptions
    & #hudTitles
      .~ ( [ defaultTitle t1
               & #style . #size .~ 0.08
           ]
             <> maybe
               []
               ( \x ->
                   [ defaultTitle x
                       & #style . #size .~ 0.05
                       & #place .~ PlaceBottom
                       & #anchor .~ AnchorEnd
                   ]
               )
               t2
         )
    & #hudLegend .~ legends'

-- | text example
textExample :: Ex
textExample =
  Ex
    defaultSvgOptions
    defaultHudOptions
    26
    (TextA (defaultTextStyle & (#size .~ (0.05 :: Double))) . (: []) . fst <$> ts)
    ((: []) . PointXY . snd <$> ts)
  where
    ts :: [(Text, Point Double)]
    ts =
      zip
        (fmap Text.singleton ['a' .. 'y'])
        [Point (sin (x * 0.1)) x | x <- [0 .. 25]]

-- | glyph example
glyphExample :: Ex
glyphExample = makeExample mempty glyphs

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
      (VLineGlyph 0.005, 0.01),
      (HLineGlyph 0.005, 0.01),
      (TriangleGlyph (Point 0.0 0.0) (Point 1 1) (Point 1 0), 0.01),
      (PathGlyph "M0.05,-0.03660254037844387 A0.1 0.1 0.0 0 1 0.0,0.05 0.1 0.1 0.0 0 1 -0.05,-0.03660254037844387 0.1 0.1 0.0 0 1 0.05,-0.03660254037844387 Z", 0.01)
    ]
    [P x 0 | x <- [0 .. (8 :: Double)]]

-- | bar example
barDataExample :: BarData
barDataExample =
  BarData
    [[1, 2, 3, 5, 8, 0, -2, 11, 2, 1], [1 .. 10]]
    (Just (("row " <>) . pack . show <$> [1 .. 11]))
    (Just (("column " <>) . pack . show <$> [1 .. 2]))

barExample :: Ex
barExample = makeExample hc cs
  where
    (hc, cs) = barChart defaultBarOptions barDataExample

-- | pixel example
pixelEx :: ([Chart Double], [Hud Double])
pixelEx = pixelfl f1 (defaultPixelOptions & #poGrain .~ Point 100 100 & #poRange .~ Rect 1 2 1 2) (defaultPixelLegendOptions "pixel test")

f1 :: (TrigField a) => Point a -> a
f1 (Point x y) = sin (cos (tan x)) * sin (cos (tan y))

-- * stuff

boundTextBug :: [Chart Double]
boundTextBug =
  [ t1,
    t2,
    Chart BlankA [R 0 0.1 (-0.5) 0.5],
    Chart (RectA defaultRectStyle) [RectXY (fixRect $ styleBox t1)],
    Chart (RectA defaultRectStyle) [RectXY (fixRect $ styleBox t2)]
  ]
  where
    t1 =
      Chart
        ( TextA
            (defaultTextStyle & #anchor .~ AnchorStart & #hsize .~ 0.45 & #size .~ 0.08)
            ["a pretty long piece of text"]
        )
        [zero]
    t2 =
      Chart
        ( TextA
            (defaultTextStyle & #anchor .~ AnchorStart & #hsize .~ 0.45 & #size .~ 0.08)
            ["another pretty long piece of text"]
        )
        [P 1 1]

-- | compound chart
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
    palette1
    [EllipseGlyph 1.5, SquareGlyph, CircleGlyph]

glines :: [Chart Double]
glines = cs <> gs
  where
    cs = zipWith (\d s -> Chart (LineA s) (PointXY <$> d)) ls lopts
    gs = zipWith (\d s -> Chart (GlyphA s) (PointXY <$> d)) ls gopts3

lgdata :: [(Text, Point Double)]
lgdata =
  (\p@(Point x y) -> (pack (show x <> "," <> show y), fromIntegral <$> p))
    <$> (Point <$> [0 .. 5] <*> [0 .. 5] :: [Point Int])

lglyph :: [Chart Double]
lglyph = txt <> gly
  where
    txt =
      ( \(t, p) ->
          Chart
            ( TextA
                ( defaultTextStyle
                    & #translate ?~ Point 0 0.04
                    & #color %~ setOpac 0.2
                )
                [t]
            )
            (PointXY <$> [p])
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
            (PointXY <$> [d])
      )
        <$> (snd <$> lgdata)

-- | label example
labelExample :: Ex
labelExample =
  Ex
    defaultSvgOptions
    defaultHudOptions
    1
    (annotation <$> label)
    (xys <$> label)

placedLabel :: (Real a) => Point a -> a -> Text -> Chart a
placedLabel p d t =
  Chart (TextA (defaultTextStyle & #rotation ?~ realToFrac d) [t]) [PointXY p]

label :: [Chart Double]
label =
  [placedLabel (Point (1.0 :: Double) 1.0) (45.0 :: Double) "text at (1,1) rotated by 45 degrees"]

-- | legend test
legendTest :: HudOptions
legendTest =
  defaultHudOptions
    & #hudLegend
    ?~
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

-- | main example
mainExample :: Ex
mainExample =
  makeExample
    defaultHudOptions
    [Chart (GlyphA defaultGlyphStyle) (PointXY <$> gridP sin (Range 0 (2 * pi)) 30)]

writeAllExamples :: IO ()
writeAllExamples = do
  -- haddocks
  let ls' =
        fmap (PointXY . uncurry Point)
          <$> [ [(0.0, 1.0), (1.0, 1.0), (2.0, 5.0 :: Double)],
                [(0.0, 0.0), (3.0, 3.0)],
                [(0.5, 4.0), (0.5, 0)]
              ]
  let anns =
        LineA
          <$> [ defaultLineStyle & #color .~ (palette1 !! 0) & #width .~ 0.015,
                defaultLineStyle & #color .~ (palette1 !! 1) & #width .~ 0.03,
                defaultLineStyle & #color .~ (palette1 !! 5) & #width .~ 0.01
              ]
  let lines' = zipWith Chart anns ls'
  writeCharts "other/lines.svg" lines'
  writeHudOptionsChart "other/linehud.svg" defaultSvgOptions defaultHudOptions [] lines'
  writeCharts "other/unit.svg" [Chart (RectA defaultRectStyle) [one]]
  let t = zipWith (\x y -> Chart (TextA (defaultTextStyle & (#size .~ (0.05 :: Double))) [x]) [PointXY y]) (fmap Text.singleton ['a' .. 'y']) [Point (sin (x * 0.1)) x | x <- [0 .. 25]]
  writeCharts "other/text.svg" t
  writeCharts "other/glyph.svg" glyphs
  writeHudOptionsChart "other/pixel.svg" defaultSvgOptions defaultHudOptions (snd pixelEx) (fst pixelEx)
  writeChartsWith "other/svgoptions.svg" (defaultSvgOptions & #svgAspect .~ ManualAspect 0.7) lines'
  writeHudOptionsChart "other/hud.svg" defaultSvgOptions defaultHudOptions [] []
  -- basics
  writeCharts "other/mempty.svg" []
  writeChartExample "other/bar.svg" barExample
  writeCharts "other/boundText.svg" boundTextBug
  writeHudOptionsChart "other/legend.svg" defaultSvgOptions legendTest [] []
  putStrLn (" 👍" :: Text)

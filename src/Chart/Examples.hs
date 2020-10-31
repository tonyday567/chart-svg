{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Examples of chart construction.
module Chart.Examples
  ( unitExample,
    svgOptionsExample,
    hudOptionsExample,
    rectExample,
    textExample,
    glyphsExample,
    lineExample,
    surfaceExample,
    barDataExample,
    barExample,
    waveExample,
    lglyphExample,
    glinesExample,
    compoundExample,
    boundTextBugExample,
    labelExample,
    legendExample,
    sinCosTan,
    writeAllExamples,
  )
where

import Chart
import Control.Lens
import Data.List ((!!))
import qualified Data.Text as Text
import NumHask.Prelude hiding (lines)

-- | unit example
--
-- ![unit example](other/unit.svg)
unitExample :: ChartSvg
unitExample = mempty & #chartList .~ [Chart (RectA defaultRectStyle) [one]]

-- | 'HudOptions' example
--
-- ![hudoptions example](other/hudoptions.svg)
hudOptionsExample :: ChartSvg
hudOptionsExample =
  mempty
    & #hudOptions .~ defaultHudOptions
    & #chartList .~ [Chart BlankA [one]]

-- | 'SvgOptions' example.
--
-- ![svgoptions example](other/svgoptions.svg)
svgOptionsExample :: ChartSvg
svgOptionsExample =
  mempty
    & #svgOptions %~ #svgAspect .~ ManualAspect 0.7
    & #chartList .~ zipWith (\s d -> Chart (LineA s) (fmap PointXY d)) lopts ls

-- | rect example
--
-- ![rect example](other/rect.svg)
rectExample :: ChartSvg
rectExample =
  mempty
    & #hudOptions .~ (defaultHudOptions & #hudAxes .~ [defaultAxisOptions])
    & #chartList .~ zipWith Chart (RectA <$> ropts) (fmap RectXY <$> rss)

rss :: [[Rect Double]]
rss =
  [ gridR (\x -> exp (- (x ** 2) / 2)) (Range -5 5) 50,
    gridR (\x -> 0.5 * exp (- (x ** 2) / 8)) (Range -5 5) 50
  ]

ropts :: [RectStyle]
ropts =
  [ blob (fromRGB (palette !! 0) 0.4),
    blob (fromRGB (palette !! 5) 0.4)
  ]

-- | line example
--
-- ![line example](other/line.svg)
lineExample :: ChartSvg
lineExample =
  mempty
    & #hudOptions
    .~ exampleLineHudOptions
           "Line Chart"
           (Just "An example from chart-svg")
           (Just (legopts, zip (LineA <$> lopts) ["hockey", "line", "vertical"]))
    & #chartList
    .~ zipWith (\s d -> Chart (LineA s) (fmap PointXY d)) lopts ls

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
    & #lplace .~ PlaceAbsolute (Point 0.5 -0.3)

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
--
-- ![text example](other/text.svg)
textExample :: ChartSvg
textExample =
  mempty & #chartList
    .~ zipWith
      Chart
      (TextA (defaultTextStyle & (#size .~ (0.05 :: Double))) . (: []) . fst <$> ts)
      ((: []) . PointXY . snd <$> ts)
  where
    ts :: [(Text, Point Double)]
    ts =
      zip
        (fmap Text.singleton ['a' .. 'y'])
        [Point (sin (x * 0.1)) x | x <- [0 .. 25]]

-- | glyphs example
--
-- ![glyphs example](other/glyphs.svg)
glyphsExample :: ChartSvg
glyphsExample =
  mempty & #chartList
    .~ zipWith
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
        (PathGlyph "M0.05,-0.03660254037844387 A0.1 0.1 0.0 0 1 0.0,0.05 0.1 0.1 0.0 0 1 -0.05,-0.03660254037844387 0.1 0.1 0.0 0 1 0.05,-0.03660254037844387 Z" ((0.01*) <$> one), 0.01)
      ]
      [P x 0 | x <- [0 .. (8 :: Double)]]

-- | Example data for Bar chart
barDataExample :: BarData
barDataExample =
  BarData
    [[1, 2, 3, 5, 8, 0, -2, 11, 2, 1], [1 .. 10]]
    (Just (("row " <>) . pack . show <$> [1 .. 11]))
    (Just (("column " <>) . pack . show <$> [1 .. 2]))

-- | Bar chart example.
--
-- ![bar example](other/bar.svg)
barExample :: ChartSvg
barExample = mempty & #hudOptions .~ hc & #chartList .~ cs
  where
    (hc, cs) = barChart defaultBarOptions barDataExample

-- | surface example
--
-- ![surface example](other/surface.svg)
surfaceExample :: ChartSvg
surfaceExample =
  mempty &
  #hudList .~ hs &
  #chartList .~ cs &
  #svgOptions .~ (defaultSvgOptions & #useCssCrisp .~ UseCssCrisp)
  where
    (cs, hs) =
      surfacefl sinCosTan
      (defaultSurfaceOptions &
       #soGrain .~ Point 100 100 &
       #soRange .~ Rect 1 2 1 2 &
       #soStyle . #surfaceColors .~ (take 6 palette1))
      (defaultSurfaceLegendOptions "surface" &
       #sloStyle . #surfaceColors .~ (take 6 palette1))

-- | Function for surface example.
sinCosTan :: (TrigField a) => Point a -> a
sinCosTan (Point x y) = sin (cos (tan x)) * sin (cos (tan y))

-- | An example of how bounding box calculations for text is broken.
--
-- ![bound text bug example](other/boundtextbug.svg)
boundTextBugExample :: ChartSvg
boundTextBugExample =
  mempty & #chartList
    .~ [ t1,
         t2,
         Chart BlankA [R 0 0.1 -0.5 0.5],
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

-- | Glyph + Lines
--
-- ![glines example](other/glines.svg)
glinesExample :: ChartSvg
glinesExample = mempty & #chartList .~ (cs <> gs)
  where
    cs = zipWith (\d s -> Chart (LineA s) (PointXY <$> d)) ls lopts
    gs = zipWith (\d s -> Chart (GlyphA s) (PointXY <$> d)) ls gopts3

lgdata :: [(Text, Point Double)]
lgdata =
  (\p@(Point x y) -> (pack (show x <> "," <> show y), fromIntegral <$> p))
    <$> (Point <$> [0 .. 5] <*> [0 .. 5] :: [Point Int])

-- | Labelled Glyphs
--
-- ![lglyph example](other/lglyph.svg)
lglyphExample :: ChartSvg
lglyphExample = mempty & #chartList .~ (txt <> gly)
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

-- | mappend of lglyph and glines examples
--
-- ![compound example](other/compound.svg)
compoundExample :: ChartSvg
compoundExample = lglyphExample <> glinesExample

-- | label example.
--
-- ![label example](other/label.svg)
labelExample :: ChartSvg
labelExample =
  mempty & #chartList
    .~ [Chart (TextA (defaultTextStyle & #rotation ?~ 45.0) ["text at (1,1) rotated by 45 degrees"]) [PointXY (Point 1.0 1.0)]]

-- | legend test
--
-- ![legend example](other/legend.svg)
legendExample :: ChartSvg
legendExample =
  mempty & #hudOptions
    .~ ( defaultHudOptions
           & #hudLegend
           ?~ ( defaultLegendOptions
                  & #lscale .~ 0.3
                  & #lplace .~ PlaceAbsolute (Point 0.0 0.0)
                  & #lsize .~ 0.12
                  & #ltext . #size .~ 0.16,
                l1
              )
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

-- | wave example
--
-- ![wave example](other/wave.svg)
waveExample :: ChartSvg
waveExample = mempty & #chartList .~ [Chart (GlyphA defaultGlyphStyle) (PointXY <$> gridP sin (Range 0 (2 * pi)) 30)]

-- | Run this to refresh haddock example SVGs.
writeAllExamples :: IO ()
writeAllExamples = do
  -- Example in cabal file
  let xs = [[(0.0, 1.0), (1.0, 1.0), (2.0, 5.0)], [(0.0, 0.0), (3.2, 3.0)], [(0.5, 4.0), (0.5, 0)]] :: [[(Double, Double)]]
  let ls = fmap (PointXY . uncurry Point) <$> xs
  let anns = zipWith (\w c -> LineA (LineStyle w c)) [0.015, 0.03, 0.01] palette1
  let lineChart = zipWith Chart anns ls
  writeChartSvgDefault "other/lines.svg" lineChart
  writeChartSvgHud "other/lineshud.svg" lineChart

  -- charts in Chart.Types
  writeChartSvg "other/unit.svg" unitExample
  writeChartSvg "other/rect.svg" rectExample
  writeChartSvg "other/text.svg" textExample
  writeChartSvg "other/glyphs.svg" glyphsExample
  writeChartSvg "other/line.svg" lineExample
  writeChartSvg "other/surface.svg" surfaceExample
  writeChartSvg "other/svgoptions.svg" svgOptionsExample
  writeChartSvg "other/hudoptions.svg" hudOptionsExample
  writeChartSvg "other/legend.svg" legendExample
  -- charts in Chart.Bar
  writeChartSvg "other/bar.svg" barExample
  -- extra Charts in Chart.Example
  writeChartSvg "other/wave.svg" waveExample
  writeChartSvg "other/lglyph.svg" lglyphExample
  writeChartSvg "other/glines.svg" glinesExample
  writeChartSvg "other/compound.svg" compoundExample
  writeChartSvg "other/boundTextBug.svg" boundTextBugExample
  writeChartSvg "other/label.svg" labelExample
  putStrLn ("ok" :: Text)

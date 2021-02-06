{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NegativeLiterals #-}
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
    barDataExample,
    barExample,
    waveExample,
    lglyphExample,
    glinesExample,
    compoundExample,
    textLocalExample,
    labelExample,
    legendExample,
    surfaceExample,
    rosenbrock,
    arcExample,
    arcFlagsExample,
    ellipseExample,
    quadExample,
    cubicExample,
    pathExample,
    vennExample,
    arrowExample,
    writeAllExamples,
  )
where

import Chart
import Control.Lens
import qualified Data.List as List
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
    & #hudOptions .~ colourHudOptions dark defaultHudOptions
    & #chartList .~ [Chart BlankA [one]]

-- | 'SvgOptions' example.
--
-- ![svgoptions example](other/svgoptions.svg)
svgOptionsExample :: ChartSvg
svgOptionsExample =
  mempty
    & #svgOptions %~ #chartAspect .~ FixedAspect 0.7
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
  [ blob (palette1 List.!! 1),
    blob (palette1 List.!! 2)
  ]

-- | line example
--
-- Example in cabal file
--
-- This 'lineExample' provides a bit more detail for testing huds.
--
-- Simplified example:
--
-- >>> :set -XOverloadedLabels
-- >>> import Chart
-- >>> let xs = fmap (fmap (uncurry Point)) [[(0.0, 1.0), (1.0, 1.0), (2.0, 5.0)], [(0.0, 0.0), (3.2, 3.0)], [(0.5, 4.0), (0.5, 0)]] :: [[Point Double]]
-- >>> xs
-- [[Point 0.0 1.0,Point 1.0 1.0,Point 2.0 5.0],[Point 0.0 0.0,Point 3.2 3.0],[Point 0.5 4.0,Point 0.5 0.0]]
--
-- >>> let anns = zipWith (\w c -> LineA (defaultLineStyle & #width .~ w & #color .~ c)) [0.015, 0.03, 0.01] palette1
-- >>> anns
-- [LineA (LineStyle {width = 1.5e-2, color = Colour 0.69 0.35 0.16 1.00, linecap = Nothing, linejoin = Nothing, dasharray = Nothing, dashoffset = Nothing}),LineA (LineStyle {width = 3.0e-2, color = Colour 0.65 0.81 0.89 1.00, linecap = Nothing, linejoin = Nothing, dasharray = Nothing, dashoffset = Nothing}),LineA (LineStyle {width = 1.0e-2, color = Colour 0.12 0.47 0.71 1.00, linecap = Nothing, linejoin = Nothing, dasharray = Nothing, dashoffset = Nothing})]
--
-- >>> let lineExample = mempty & (#chartList .~ zipWith Chart anns (fmap (fmap PointXY) xs)) & #hudOptions .~ defaultHudOptions & #svgOptions .~ defaultSvgOptions :: ChartSvg
-- >>> :t lineExample
-- lineExample :: ChartSvg
--
-- > writeChartSvg "other/line.svg" lineExample
--
-- ![line example](other/line.svg)
lineExample :: ChartSvg
lineExample =
  mempty
    & #svgOptions . #chartAspect .~ CanvasAspect 1.5
    & #hudOptions
    .~ exampleLineHudOptions
           "Line Chart"
           (Just "An example from chart-svg")
           (Just (defaultLegendOptions, zip (LineA <$> lopts) ["hockey", "line", "vertical"]))
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
  [ defaultLineStyle & #color .~ (palette1 List.!! 0) & #width .~ 0.015,
    defaultLineStyle & #color .~ (palette1 List.!! 1) & #width .~ 0.03,
    defaultLineStyle & #color .~ (palette1 List.!! 2) & #width .~ 0.01
  ]

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
    & #hudAxes %~ fmap (#axisTick . #tstyle .~ TickRound (FormatFixed (Just 1)) 8 TickExtend)

-- | text example
--
-- ![text example](other/text.svg)
textExample :: ChartSvg
textExample =
  mempty & #chartList
    .~ zipWith
      Chart 
      (TextA (defaultTextStyle & (#color .~ dark) & (#size .~ (0.05 :: Double))) . (: []) . fst <$> ts)
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
  mempty &
  #svgOptions . #svgHeight .~ 50 &
  #chartList
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
        (PathGlyph "M0.05,-0.03660254037844387 A0.1 0.1 0.0 0 1 0.0,0.05 0.1 0.1 0.0 0 1 -0.05,-0.03660254037844387 0.1 0.1 0.0 0 1 0.05,-0.03660254037844387 Z", 0.01)
      ]
      [P x 0 | x <- [0 .. (8 :: Double)]]

-- | Example data for Bar chart
barDataExample :: BarData
barDataExample =
  BarData
    [[1, 2, 3, 5, 8, 0, -2, 11, 2, 1], [1 .. 10]]
    (Just (("row " <>) . pack . show <$> ([1 .. 11]::[Int])))
    (Just (("column " <>) . pack . show <$> ([1 .. 2]::[Int])))

-- | Bar chart example.
--
-- ![bar example](other/bar.svg)
barExample :: ChartSvg
barExample = mempty & #hudOptions .~ hc & #chartList .~ cs
  where
    (hc, cs) = barChart defaultBarOptions barDataExample

-- | A reminder that Text scale is at representation level, and so doesn't scale compared with other chart elements, such as a rectangle.
--
-- ![text local example](other/textlocal.svg) 
textLocalExample :: ChartSvg
textLocalExample =
  mempty & #chartList
    .~ [ t1,
         t2,
         Chart (RectA rs) [RectXY (padBox $ styleBox t1)],
         Chart (RectA rs) [RectXY (padBox $ styleBox t2)]
       ]
  where
    rs = defaultRectStyle & #color %~ setOpac 0.1
    t1 =
      Chart
        ( TextA
            (defaultTextStyle & #anchor .~ AnchorStart & #hsize .~ 0.5 & #size .~ 0.08)
            ["a pretty long piece of text"]
        )
        [zero]
    t2 =
      Chart
        ( TextA
            (defaultTextStyle & #anchor .~ AnchorStart & #hsize .~ 0.5 & #size .~ 0.08)
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
                    & #color %~ setOpac 0.2
                )
                [t]
            )
            (PointXY <$> [p + Point 0 0.2])
      )
        <$> lgdata
    gly =
      ( \d ->
          Chart
            ( GlyphA
                ( defaultGlyphStyle
                    & #size .~ 0.01
                    & #borderSize .~ 0
                    & #color .~ palette1 List.!! 2
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
    .~ [Chart (TextA (defaultTextStyle & #rotation ?~ -pi/4) ["text at (1,1) rotated by -(pi/4) radians"]) [PointXY (Point 1.0 1.0)]]

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

-- | venn diagram
--
-- ![venn diagram](other/venn.svg)
vennExample :: ChartSvg
vennExample =
  mempty &
  #chartList .~ zipWith (\c x -> Chart (PathA (defaultPathStyle & #color .~ setOpac 0.2 c) (fst <$> x)) (PointXY . snd <$> x)) palette1 (toPathXYs . parsePath <$> vennSegs) &
  #svgOptions .~ (defaultSvgOptions & #chartAspect .~ FixedAspect 1) &
  #hudOptions .~ defaultHudOptions

{-
These were originally based on:

    [ ("origin", Point 0 0), -- origin
      ("circle1", Point 0.5 (-0.5 + cos (pi / 6))), -- center of circle 1
      ("circle2", Point 0 -0.5), -- center of circle 2
      ("circle3", Point -0.5 (-0.5 + cos (pi / 6))), -- center of circle 3
      ("corner1", Point 0 (-0.5 + 2 * cos (pi / 6))), -- corner 1
      ("corner2", Point 1 -0.5), -- corner 2
      ("corner3", Point -1 -0.5) -- corner 3
    ]
-}
vennSegs :: [Text]
vennSegs =
      [ "M0.0,-1.2320508075688774 A0.5 0.5 0.0 1 1 1.0,0.5 1.0 1.0 0.0 0 0 0.5,-0.3660254037844387 1.0 1.0 0.0 0 0 0.0,-1.2320508075688774 Z",
        "M-1.0,0.5 A0.5 0.5 0.0 1 0 1.0,0.5 1.0 1.0 0.0 0 1 0.0,0.5 1.0 1.0 0.0 0 1 -1.0,0.5 Z",
        "M-1.0,0.5 A0.5 0.5 0.0 1 1 0.0,-1.2320508075688774 1.0 1.0 0.0 0 0 -0.5,-0.3660254037844387 1.0 1.0 0.0 0 0 -1.0,0.5 Z",
        "M0.5,-0.3660254037844387 A1.0 1.0 0.0 0 1 1.0,0.5 1.0 1.0 0.0 0 1 0.0,0.5 1.0 1.0 0.0 0 0 0.5,-0.3660254037844387 Z",
        "M0.0,0.5 A1.0 1.0 0.0 0 1 -1.0,0.5 1.0 1.0 0.0 0 1 -0.5,-0.3660254037844387 1.0 1.0 0.0 0 0 0.0,0.5 Z",
        "M0.0,-1.2320508075688774 A1.0 1.0 0.0 0 1 0.5,-0.3660254037844387 1.0 1.0 0.0 0 0 -0.5,-0.3660254037844387 1.0 1.0 0.0 0 1 0.0,-1.2320508075688774 Z",
        "M0.5,-0.3660254037844387 A1.0 1.0 0.0 0 1 0.0,0.5 1.0 1.0 0.0 0 1 -0.5,-0.3660254037844387 1.0 1.0 0.0 0 1 0.5,-0.3660254037844387 Z"
      ]

-- | Compound path example.
--
-- ![path test](other/path.svg)
pathExample :: ChartSvg
pathExample =
  mempty &
   #chartList .~ [path', c0] &
   #hudOptions .~ defaultHudOptions &
   #svgOptions %~
   ((#outerPad ?~ 0.1) .
    (#chartAspect .~ ChartAspect))
  where
    ps =
      [
        (StartI, Point 0 0),
        (LineI, Point 1 0),
        (CubicI (Point 0.2 0) (Point 0.25 1), Point 1 1),
        (QuadI (Point -1 2), Point 0 1),
        (ArcI (ArcInfo (Point 1 1) (-pi/6) False False), Point 0 0)
      ]
    path' = Chart (PathA (defaultPathStyle & #color .~ setOpac 0.1 (palette1 List.!! 2) & #borderColor .~ Colour 0.2 0.8 0.4 0.3) (fst <$> ps)) (PointXY . snd <$> ps)
    c0 = Chart (GlyphA defaultGlyphStyle) (PointXY . snd <$> ps)

-- | ellipse example
--
-- (ArcPosition (Point 1 0) (Point 0 1) (ArcInfo (Point 1.5 1) 0 True True))
--
-- ![ellipse example](other/ellipse.svg)
--
ellipseExample :: ChartSvg
ellipseExample =
  mempty &
   #chartList .~ [ell, ellFull, c0, bbox, xradii, yradii] &
   #hudOptions .~ defaultHudOptions &
   #svgOptions %~ ((#outerPad .~ Nothing) . (#chartAspect .~ UnadjustedAspect))
  where
    p@(ArcPosition p1 p2 _) = ArcPosition (Point 1 0) (Point 0 1) (ArcInfo (Point 1.5 1) (pi/3) True True)
    (ArcCentroid c r phi' ang0 angd) = arcCentroid p
    ellFull = Chart (LineA $ defaultLineStyle & #width .~ 0.002 & #color .~ (palette1 List.!! 1)) (PointXY . ellipse c r phi' . (\x -> 2 * pi * x / 100.0) <$> [0..100])
    ell = Chart (LineA $ defaultLineStyle & #width .~ 0.002 & #color .~ (palette1 List.!! 1)) (PointXY . ellipse c r phi' . (\x -> ang0 + angd * x / 100.0) <$> [0..100])
    c0 = Chart (GlyphA defaultGlyphStyle) (PointXY <$> [c,p1,p2])
    bbox = Chart (RectA $ defaultRectStyle & #borderSize .~ 0.002 & #color .~ Colour 0.4 0.4 0.8 0.1 & #borderColor .~ Colour 0.5 0.5 0.5 1) [RectXY (arcBox p)]
    xradii = Chart (LineA $ defaultLineStyle & #color .~ Colour 0.9 0.2 0.02 1 & #width .~ 0.005 & #dasharray .~ Just [0.03, 0.01] & #linecap .~ Just LineCapRound) (PointXY <$> [ellipse c r phi' 0, ellipse c r phi' pi])
    yradii = Chart (LineA $ defaultLineStyle & #color .~ Colour 0.9 0.9 0.02 1 & #width .~ 0.005 & #dasharray .~ Just [0.03, 0.01] & #linecap .~ Just LineCapRound) (PointXY <$> [ellipse c r phi' (pi/2), ellipse c r phi' (3/2*pi)])

-- | arc example
--
-- ![arc example](other/arc.svg)
--
-- There is a bug for rotated ellipses. See 'problematic2' for scaling issue when phi is non-zero.
--
arcExample :: ChartSvg
arcExample =
  mempty &
   #chartList .~ [arc, ell, c0, bbox] &
   #hudOptions .~ defaultHudOptions &
   #svgOptions %~ ((#outerPad .~ Nothing) . (#chartAspect .~ FixedAspect 1))
  where
    p1 = ArcPosition (Point 1.0 0.0) (Point 0.0 1.0) (ArcInfo (Point 1.0 0.5) 0 False True)
    ps = singletonArc p1
    (ArcCentroid c r phi' ang0 angd) = arcCentroid p1
    arc = Chart (PathA (defaultPathStyle & #color .~ setOpac 0.1 (palette1 List.!! 2) & #borderColor .~ transparent) (fst <$> ps)) (PointXY . snd <$> ps)
    ell = Chart (LineA $ defaultLineStyle & #width .~ 0.002 & #color .~ (palette1 List.!! 1)) (PointXY . ellipse c r phi' . (\x -> ang0 + angd * x / 100.0) <$> [0..100])
    c0 = Chart (GlyphA defaultGlyphStyle) [PointXY c]
    bbox = Chart (RectA $ defaultRectStyle & #borderSize .~ 0.002 & #color .~ Colour 0.4 0.4 0.8 0.1 & #borderColor .~ Colour 0.5 0.5 0.5 1) [RectXY (arcBox p1)]

-- | Reproduction of the flag explanation chart in <https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths>
--
-- ![arc flags example](other/arcflags.svg)
arcFlagsExample :: ChartSvg
arcFlagsExample =
  mempty &
   #chartList .~
     vert 0.02
     [hori 0.02
       [ [Chart BlankA [R -0.4 0.4 -1 5],
          Chart (TextA (defaultTextStyle & #size .~ 0.6 & #rotation .~ Just (pi/2)) ["Sweep"]) [P 0.1 2]],
         vert 0.02
         [[Chart BlankA [R -0.25 0.25 -1 2],
           Chart (TextA (defaultTextStyle & #size .~ 0.4 & #rotation .~ Just (pi/2)) ["True"]) [P 0.1 0.5]],
          [Chart BlankA [R -0.25 0.25 -1 2],
           Chart (TextA (defaultTextStyle & #size .~ 0.4 & #rotation .~ Just (pi/2)) ["False"]) [P 0.1 0.5]]
       ],
         vert 0.02
         [checkFlags False True (setOpac 0.3 dark) & view #chartList,
          checkFlags False False (setOpac 0.3 dark) & view #chartList,
          [Chart BlankA [R -1 2 -0.25 0.25],
           Chart (TextA (defaultTextStyle & #size .~ 0.4) ["False"]) [P 0.5 -0.1]]
         ],
         vert 0.02
         [checkFlags True True (setOpac 0.3 dark) & view #chartList,
          checkFlags True False (setOpac 0.3 dark) & view #chartList,
           [Chart BlankA [R -1 2 -0.25 0.25],
            Chart (TextA (defaultTextStyle & #size .~ 0.4) ["True"]) [P 0.5 -0.1]]
         ]
       ],
      [ Chart BlankA [R 0 9 -2.75 -3.25],
        Chart (TextA (defaultTextStyle & #size .~ 0.6) ["Large"]) [P 5.5 -3.0]]
      ] &
   #hudOptions .~ mempty &
   #svgOptions %~ ((#outerPad .~ Nothing) . (#chartAspect .~ UnadjustedAspect))

checkFlags :: Bool -> Bool -> Colour -> ChartSvg
checkFlags large sweep co =
  mempty &
  #hudOptions .~ defaultHudOptions &
  #svgOptions . #chartAspect .~ UnadjustedAspect &
  #chartList .~ [c1, c2, ell, arc1]
  where
    c = Point 1.0 1.0
    p1 = ArcPosition (Point 0.0 1.0) (Point 1.0 0.0) (ArcInfo (Point 1.0 1.0) 0 large sweep)
    ps1 = singletonPie' c p1
    (ArcCentroid c' r phi' ang0 angd) = arcCentroid p1
    arc1 = Chart (PathA (defaultPathStyle & #color .~ co & #borderColor .~ setOpac 0.5 dark) (fst <$> ps1)) (PointXY . snd <$> ps1)
    c1 = Chart (LineA $ defaultLineStyle & #width .~ 0.02 & #color .~ setOpac 0.2 dark) (PointXY . ellipse (Point 1.0 1.0) (Point 1.0 1.0) 0 . (\x -> 0 + 2 * pi * x / 100.0) <$> [0..100])
    c2 = Chart (LineA $ defaultLineStyle & #width .~ 0.02 & #color .~ setOpac 0.2 dark) (PointXY . ellipse (Point 0.0 0.0) (Point 1.0 1.0) 0 . (\x -> 0 + 2 * pi * x / 100.0) <$> [0..100])
    ell = Chart (LineA $ defaultLineStyle & #width .~ 0.05 & #color .~ setOpac 0.5 co) (PointXY . ellipse c' r phi' . (\x -> ang0 + angd * x / 100.0) <$> [0..100])

-- | quad example
--
-- ![quad example](other/quad.svg)
quadExample :: ChartSvg
quadExample =
  mempty &
   #chartList .~ [path', curve, c0, bbox] &
   #hudOptions .~ defaultHudOptions &
   #svgOptions %~ ((#outerPad ?~ 0.05) . (#chartAspect .~ ChartAspect))
  where
    p@(QuadPosition start end control) = QuadPosition (Point 0 0) (Point 1 1) (Point 2 -1)
    ps = singletonQuad p
    path' = Chart (PathA (defaultPathStyle & #color .~ setOpac 0.1 (palette1 List.!! 2) & #borderColor .~ transparent) (fst <$> ps)) (PointXY . snd <$> ps)
    curve = Chart (LineA $ defaultLineStyle & #width .~ 0.002 & #color .~ (palette1 List.!! 1)) (PointXY . quadBezier p . (/100.0) <$> [0..100])
    c0 = Chart (GlyphA defaultGlyphStyle) (PointXY <$> [start, end, control])
    bbox = Chart (RectA $ defaultRectStyle & #borderSize .~ 0.002 & #color .~ Colour 0.4 0.4 0.8 0.1 & #borderColor .~ Colour 0.5 0.5 0.5 1) [RectXY (quadBox p)]

-- | cubic example
--
-- ![cubic example](other/cubic.svg)
cubicExample :: ChartSvg
cubicExample =
  mempty &
   #chartList .~ [path', curve, c0, bbox] &
   #hudOptions .~ mempty &
   #svgOptions %~ ((#outerPad ?~ 0.02) . (#chartAspect .~ ChartAspect))
  where
    p@(CubicPosition start end control1 control2) = CubicPosition (Point 0 0) (Point 1 1) (Point 1 0) (Point 0 1)
    ps = singletonCubic p
    path' = Chart (PathA (defaultPathStyle & #color .~ setOpac 0.1 (palette1 List.!! 2) & #borderColor .~ transparent) (fst <$> ps)) (PointXY . snd <$> ps)
    curve = Chart (LineA $ defaultLineStyle & #width .~ 0.002 & #color .~ (palette1 List.!! 1)) (PointXY . cubicBezier p . (/100.0) <$> [0..100])
    c0 = Chart (GlyphA defaultGlyphStyle) (PointXY <$> [start, end, control1, control2, cubicBezier p 0.8])
    bbox = Chart (RectA $ defaultRectStyle & #borderSize .~ 0.002 & #color .~ Colour 0.4 0.4 0.8 0.1 & #borderColor .~ Colour 0.5 0.5 0.5 1) [RectXY (cubicBox p)]

-- | The common way to create a surface chart is usually a grid over a function.
--
-- ![surface example](other/surface.svg)
--
surfaceExample :: ChartSvg
surfaceExample =
  mempty &
  #hudList .~ hs &
  #chartList .~ cs &
  #svgOptions .~ (defaultSvgOptions & #cssOptions .~ UseCssCrisp)
  where
    t = "rosenbrock"
    grain = Point 20 20
    r = one
    f = fst . bimap (-1.0 *) (-1.0 .*) . rosenbrock 1 10
    (cs, hs) =
      surfacefl f
      (defaultSurfaceOptions &
       #soGrain .~ grain &
       #soRange .~ r &
       #soStyle . #surfaceColors .~ take 6 palette1)
      (defaultSurfaceLegendOptions t &
       #sloStyle . #surfaceColors .~ take 6 palette1)

-- | arrow example
--
-- Which happens to be the gradient of the surface example.
--
-- ![arrow example](other/arrow.svg)
arrowExample :: ChartSvg
arrowExample =
  mempty &
  #hudOptions .~ (defaultHudOptions & #hudAxes %~ fmap (#axisTick . #ltick .~ Nothing)) &
  #chartList .~ ((\p -> chart (tail . f $ p) (angle . f $ p) p) <$> ps) &
  #svgOptions .~ defaultSvgOptions
  where
    f = snd . bimap (-1.0 *) (-1.0 .*) . rosenbrock 1 10
    ps = grid MidPos (one :: Rect Double) (Point 10 10 :: Point Int) :: [Point Double]
    arrow = PathGlyph "M -1 0 L 1 0 M 1 0 L 0.4 0.3 M 1 0 L 0.4 -0.3"
    gs s r' =
      defaultGlyphStyle &
      #borderSize .~ 0.05 &
      #size .~ s &
      #borderColor .~ dark &
      #rotation .~ Just r' &
      #shape .~ arrow
    chart s r' p = Chart (GlyphA (gs s r')) [PointXY p]

    tail :: Point Double -> Double
    tail = max 0.05 . min 0.02 . (*0.01) . (/avmag) . norm

    avmag = sum (norm . f <$> ps) / fromIntegral (length ps)

-- | function for testing
--
-- > f(x,y) = (a-x)^2 + b * (y - x^2)^2
-- >        = a^2 - 2ax + x^2 + b * y^2 - b * 2 * y * x^2 + b * x ^ 4
-- > f'x = -2a + 2 * x - b * 4 * y * x + 4 * b * x ^ 3
-- > f'y = 2 * b * y - 2 * b * x^2
-- > f a b (Point x y) = (a^2 - 2ax + x^2 + b * y^2 - b * 2 * y * x^2 + b * x^4, Point (-2a + 2 * x - b * 4 * y * x + 4 * b * x ^ 3), 2 * b * y - 2 * b * x^2)
rosenbrock :: Double -> Double -> Point Double -> (Double, Point Double)
rosenbrock a b (Point x y) = (a^2 - 2*a*x + x^2 + b * y^2 - b * 2 * y * x^2 + b * x^4, Point (-2*a + 2 * x - b * 4 * y * x + 4 * b * x^3) (2 * b * y - 2 * b * x^2))

-- | Run this to refresh haddock example SVGs.
writeAllExamples :: IO ()
writeAllExamples = do
  -- charts in Chart docs
  writeChartSvg "other/unit.svg" unitExample
  writeChartSvg "other/rect.svg" rectExample
  writeChartSvg "other/text.svg" textExample
  writeChartSvg "other/glyphs.svg" glyphsExample
  writeChartSvg "other/line.svg" lineExample
  writeChartSvg "other/svgoptions.svg" svgOptionsExample
  writeChartSvg "other/hudoptions.svg" hudOptionsExample
  writeChartSvg "other/legend.svg" legendExample
  -- charts in Chart.Bar docs
  writeChartSvg "other/bar.svg" barExample
  -- charts in Chart.Surface docs
  writeChartSvg "other/surface.svg" surfaceExample
  -- extra Charts
  writeChartSvg "other/wave.svg" waveExample
  writeChartSvg "other/lglyph.svg" lglyphExample
  writeChartSvg "other/glines.svg" glinesExample
  writeChartSvg "other/compound.svg" compoundExample
  writeChartSvg "other/textlocal.svg" textLocalExample
  writeChartSvg "other/label.svg" labelExample
  writeChartSvg "other/venn.svg" vennExample
  writeChartSvg "other/path.svg" pathExample
  writeChartSvg "other/arc.svg" arcExample
  writeChartSvg "other/arcflags.svg" arcFlagsExample
  writeChartSvg "other/ellipse.svg" ellipseExample
  writeChartSvg "other/quad.svg" quadExample
  writeChartSvg "other/cubic.svg" cubicExample
  writeChartSvg "other/arrow.svg" arrowExample

  putStrLn ("ok" :: Text)

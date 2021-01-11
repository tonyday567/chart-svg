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
    boundTextBugExample,
    labelExample,
    legendExample,
    surfaceExample,
    arrowgExample,
    surfacegExample,
    rosenbrock,
    arcExample,
    arcFlagsExample,
    ellipseExample,
    quadExample,
    cubicExample,
    pathExample,
    vennExample,
    problematic1,
    problematic2,
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

-- | An example of how bounding box calculations for text is broken.
--
-- ![bound text bug example](other/boundtextbug.svg)
boundTextBugExample :: ChartSvg
boundTextBugExample =
  mempty & #chartList
    .~ [ t1,
         t2,
         Chart BlankA [R 0 0.1 -0.5 0.5],
         Chart (RectA defaultRectStyle) [RectXY (padBox $ styleBox t1)],
         Chart (RectA defaultRectStyle) [RectXY (padBox $ styleBox t2)]
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
    path' = Chart (PathA (defaultPathStyle & #color .~ setOpac 0.1 (palette1!!2) & #borderColor .~ Colour 0.2 0.8 0.4 0.3) (fst <$> ps)) (PointXY . snd <$> ps)
    c0 = Chart (GlyphA defaultGlyphStyle) (PointXY . snd <$> ps)

-- | ellipse example
--
-- (ArcPosition (Point 1 0) (Point 0 1) (ArcInfo (Point 1.5 1) (pi/3) True True))
--
--
-- ![ellipse example](other/ellipse.svg)
--
ellipseExample :: ArcPosition Double -> ChartSvg
ellipseExample p@(ArcPosition p1 p2 _) =
  mempty &
   #chartList .~ [ell, ellFull, c0, bbox, xradii, yradii] &
   #hudOptions .~ defaultHudOptions &
   #svgOptions %~ ((#outerPad .~ Nothing) . (#chartAspect .~ UnadjustedAspect))
  where
    (ArcCentroid c r phi' ang0 angd) = arcCentroid p
    ellFull = Chart (LineA $ defaultLineStyle & #width .~ 0.002 & #color .~ (palette1!!1)) (PointXY . ellipse c r phi' . (\x -> 2 * pi * x / 100.0) <$> [0..100])
    ell = Chart (LineA $ defaultLineStyle & #width .~ 0.002 & #color .~ (palette1!!1)) (PointXY . ellipse c r phi' . (\x -> ang0 + angd * x / 100.0) <$> [0..100])
    c0 = Chart (GlyphA defaultGlyphStyle) (PointXY <$> [c,p1,p2])
    bbox = Chart (RectA $ defaultRectStyle & #borderSize .~ 0.002 & #color .~ Colour 0.4 0.4 0.8 0.1 & #borderColor .~ Colour 0.5 0.5 0.5 1) [RectXY (arcBox p)]
    xradii = Chart (LineA $ defaultLineStyle & #color .~ Colour 0.9 0.2 0.02 1 & #width .~ 0.005 & #dasharray .~ Just [0.03, 0.01] & #linecap .~ Just LineCapRound) (PointXY <$> [ellipse c r phi' 0, ellipse c r phi' pi])
    yradii = Chart (LineA $ defaultLineStyle & #color .~ Colour 0.9 0.9 0.02 1 & #width .~ 0.005 & #dasharray .~ Just [0.03, 0.01] & #linecap .~ Just LineCapRound) (PointXY <$> [ellipse c r phi' (pi/2), ellipse c r phi' (3/2*pi)])

-- | arc example
--
-- > arcExample (ArcPosition (Point 1.0 0.0) (Point 0.0 1.0) (ArcInfo (Point 1.0 0.5) 0 False True))
--
-- ![arc example](other/arc.svg)
--
-- See also 'problematic2' for scaling issue when phi is non-zero.
--
arcExample :: ArcPosition Double -> ChartSvg
arcExample p1 =
  mempty &
   #chartList .~ [arc, ell, c0, bbox] &
   #hudOptions .~ defaultHudOptions &
   #svgOptions %~ ((#outerPad .~ Nothing) . (#chartAspect .~ FixedAspect 1))
  where
    ps = singletonArc p1
    (ArcCentroid c r phi' ang0 angd) = arcCentroid p1
    arc = Chart (PathA (defaultPathStyle & #color .~ setOpac 0.1 (palette1!!2) & #borderColor .~ transparent) (fst <$> ps)) (PointXY . snd <$> ps)
    ell = Chart (LineA $ defaultLineStyle & #width .~ 0.002 & #color .~ (palette1!!1)) (PointXY . ellipse c r phi' . (\x -> ang0 + angd * x / 100.0) <$> [0..100])
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
         [checkFlags False True (Colour 1 0 1 0.3) & view #chartList,
          checkFlags False False (Colour 0 1 0 0.3) & view #chartList,
          [Chart BlankA [R -1 2 -0.25 0.25],
           Chart (TextA (defaultTextStyle & #size .~ 0.4) ["False"]) [P 0.5 -0.1]]
         ],
         vert 0.02
         [checkFlags True True (Colour 0 0 1 0.3) & view #chartList,
          checkFlags True False (Colour 1 0 0 0.3) & view #chartList,
           [Chart BlankA [R -1 2 -0.25 0.25],
            Chart (TextA (defaultTextStyle & #size .~ 0.4) ["True"]) [P 0.5 -0.1]]
         ]
       ],
      [Chart BlankA [R 0 9 0 0.75],
       Chart (TextA (defaultTextStyle & #size .~ 0.6) ["Large"]) [P 5.5 0.2]]
     ] &
   #hudOptions .~ defaultHudOptions &
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
    arc1 = Chart (PathA (defaultPathStyle & #color .~ co & #borderColor .~ Colour 0 0 0 0.5) (fst <$> ps1)) (PointXY . snd <$> ps1)
    c1 = Chart (LineA $ defaultLineStyle & #width .~ 0.02 & #color .~ Colour 0 0 0 0.2) (PointXY . ellipse (Point 1.0 1.0) (Point 1.0 1.0) 0 . (\x -> 0 + 2 * pi * x / 100.0) <$> [0..100])
    c2 = Chart (LineA $ defaultLineStyle & #width .~ 0.02 & #color .~ Colour 0 0 0 0.2) (PointXY . ellipse (Point 0.0 0.0) (Point 1.0 1.0) 0 . (\x -> 0 + 2 * pi * x / 100.0) <$> [0..100])
    ell = Chart (LineA $ defaultLineStyle & #width .~ 0.05 & #color .~ setOpac 0.5 co) (PointXY . ellipse c' r phi' . (\x -> ang0 + angd * x / 100.0) <$> [0..100])

-- | quad example
--
-- > quadExample (QuadPosition (Point 0 0) (Point 1 1) (Point 2 -1))
--
-- ![quad example](other/quad.svg)
quadExample :: QuadPosition Double -> ChartSvg
quadExample p@(QuadPosition start end control) =
  mempty &
   #chartList .~ [path', curve, c0, bbox] &
   #hudOptions .~ defaultHudOptions &
   #svgOptions %~ ((#outerPad ?~ 0.05) . (#chartAspect .~ ChartAspect))
  where
    ps = singletonQuad p
    path' = Chart (PathA (defaultPathStyle & #color .~ setOpac 0.1 (palette1!!2) & #borderColor .~ transparent) (fst <$> ps)) (PointXY . snd <$> ps)
    curve = Chart (LineA $ defaultLineStyle & #width .~ 0.002 & #color .~ (palette1!!1)) (PointXY . quadBezier p . (/100.0) <$> [0..100])
    c0 = Chart (GlyphA defaultGlyphStyle) (PointXY <$> [start, end, control])
    bbox = Chart (RectA $ defaultRectStyle & #borderSize .~ 0.002 & #color .~ Colour 0.4 0.4 0.8 0.1 & #borderColor .~ Colour 0.5 0.5 0.5 1) [RectXY (quadBox p)]

-- | cubic example
--
-- > cubicExample (CubicPosition (Point 0 0) (Point 1 1) (Point 1 -1) (Point 0 2))
--
-- ![cubic example](other/cubic.svg)
cubicExample :: CubicPosition Double -> ChartSvg
cubicExample p@(CubicPosition start end control1 control2) =
  mempty &
   #chartList .~ [path', curve, c0, bbox] &
   #hudOptions .~
   ( defaultHudOptions &
     #hudTitles .~
     [ defaultTitle pathText &
       #place .~ PlaceBottom &
       #style . #size .~ 0.06
     ] &
     #hudAxes %~ fmap (#axisTick . #tstyle .~
                       TickRound (FormatComma (Just 2)) 8 NoTickExtend)
   ) &
   #svgOptions %~ ((#outerPad ?~ 0.02) . (#chartAspect .~ ChartAspect))
  where
    ps = singletonCubic p
    path' = Chart (PathA (defaultPathStyle & #color .~ setOpac 0.1 (palette1!!2) & #borderColor .~ transparent) (fst <$> ps)) (PointXY . snd <$> ps)
    curve = Chart (LineA $ defaultLineStyle & #width .~ 0.002 & #color .~ (palette1!!1)) (PointXY . cubicBezier p . (/100.0) <$> [0..100])
    c0 = Chart (GlyphA defaultGlyphStyle) (PointXY <$> [start, end, control1, control2, cubicBezier p 0.8])
    bbox = Chart (RectA $ defaultRectStyle & #borderSize .~ 0.002 & #color .~ Colour 0.4 0.4 0.8 0.1 & #borderColor .~ Colour 0.5 0.5 0.5 1) [RectXY (cubicBox p)]
    pathText = toPathAbsolutes ps

-- | Create a chart across a surface using a function.
--
-- > writeChartSvg "other/surface.svg" $ surfaceExample "rosenbrock" (Point 100 100) one (fst . first (-1.0 *) . second (-1.0 .*) . rosenbrock 1 10)
--
-- ![surface example](other/surface.svg)
--
surfaceExample ::
  -- | Legend title
  Text ->
  -- | Surface grid
  Point Int ->
  -- | Surface range
  Rect Double ->
  -- | Surface function
  (Point Double -> Double) ->
  ChartSvg
surfaceExample t grain r f =
  mempty &
  #hudList .~ hs &
  #chartList .~ cs &
  #svgOptions .~ (defaultSvgOptions & #cssOptions .~ UseCssCrisp)
  where
    (cs, hs) =
      surfacefl f
      (defaultSurfaceOptions &
       #soGrain .~ grain &
       #soRange .~ r &
       #soStyle . #surfaceColors .~ take 6 palette1)
      (defaultSurfaceLegendOptions t &
       #sloStyle . #surfaceColors .~ take 6 palette1)

-- | Create an arrow chart across a surface using a function.
--
-- Typically used to represent a gradient.
--
-- > writeChartSvg "other/arrowg.svg" $ arrowgExample (Point 20 20) one (fst . first (-1.0 *) . second (-1.0 .*) . rosenbrock 1 10)
--
-- ![arrowg example](other/arrowg.svg)
--
--
arrowgExample ::
  -- | Surface grid
  Point Int ->
  -- | Surface range
  Rect Double ->
  -- | Surface gradient function
  (Point Double -> Point Double) ->
  ChartSvg
arrowgExample grain r f =
  mempty &
  #hudOptions .~ (defaultHudOptions & #hudAxes %~ fmap (#axisTick . #ltick .~ Nothing)) &
  #chartList .~ ((\p -> chart (tail . f $ p) (angle . f $ p) p) <$> ps) &
  #svgOptions .~ (defaultSvgOptions & #cssOptions .~ UseCssCrisp)
  where
    ps = grid MidPos r grain
    arrow = PathGlyph "M -1 0 L 1 0 M 1 0 L 0.4 0.3 M 1 0 L 0.4 -0.3"
    gs s r' =
      defaultGlyphStyle &
      #borderSize .~ 0.05 &
      #size .~ s &
      #borderColor .~ black &
      #rotation .~ Just r' &
      #shape .~ arrow
    chart s r' p = Chart (GlyphA (gs s r')) [PointXY p]

    tail :: Point Double -> Double
    tail = max 0.005 . min 0.02 . (*0.01) . (/avmag) . norm

    avmag = sum (norm . f <$> ps) / fromIntegral (length ps)

-- | A surface chart with gradient arrows.
--
-- > writeChartSvg "other/surfaceg.svg" $ surfacegExample "rosenbrock" (Point 100 100) (Point 20 20) one (first (-1.0 *) . second (-1.0 .*) . rosenbrock 1 10)
--
-- ![surfaceg example](other/surfaceg.svg)
--
surfacegExample ::
  -- | Title
  Text ->
  -- | Surface grid
  Point Int ->
  -- | Gradient grid
  Point Int ->
  -- | Surface range
  Rect Double ->
  -- | Surface + gradient function
  (Point Double -> (Double, Point Double)) ->
  ChartSvg
surfacegExample t grainS grainG r f  =
  surfaceExample t grainS r (fst . f) <>
  arrowgExample grainG r (snd . f)

-- | function for testing
--
-- > f(x,y) = (a-x)^2 + b * (y - x^2)^2
-- >        = a^2 - 2ax + x^2 + b * y^2 - b * 2 * y * x^2 + b * x ^ 4
-- > f'x = -2a + 2 * x - b * 4 * y * x + 4 * b * x ^ 3
-- > f'y = 2 * b * y - 2 * b * x^2
-- > f a b (Point x y) = (a^2 - 2ax + x^2 + b * y^2 - b * 2 * y * x^2 + b * x^4, Point (-2a + 2 * x - b * 4 * y * x + 4 * b * x ^ 3), 2 * b * y - 2 * b * x^2)
rosenbrock :: Double -> Double -> Point Double -> (Double, Point Double)
rosenbrock a b (Point x y) = (a^2 - 2*a*x + x^2 + b * y^2 - b * 2 * y * x^2 + b * x^4, Point (-2*a + 2 * x - b * 4 * y * x + 4 * b * x^3) (2 * b * y - 2 * b * x^2))

-- | This was a problem chart that helped me get the aspect scaling of curves right.
--
-- The answer was that radii of an Arc needs to be transformed by scaling changes but not translation ones (radii are relative to existing points which are already being translated).
--
-- The problem can be isolated to RunHud ...
--
-- >>> let cs = [toPathChart defaultPathStyle $ singletonArc $ ArcPosition (Point 0 1) (Point 1 0) (ArcInfo (Point 1 1) 0 False False)]
-- >>> runHud (aspect 3) [canvas $ blob (Colour 0.2 0.1 0.7 0.1)] cs
-- [Chart {annotation = RectA (RectStyle {borderSize = 0.0, borderColor = RGBA 0.00 0.00 0.00 0.00, color = RGBA 0.20 0.10 0.70 0.10}), xys = [R -1.5 1.5000000000000002 -0.5 0.5]},Chart {annotation = PathA (PathStyle {borderSize = 1.0e-2, borderColor = RGBA 0.12 0.47 0.71 0.80, color = RGBA 0.12 0.47 0.71 0.30}) [StartI,ArcI (ArcInfo {radii = Point 3.0 1.0, phi = 0.0, large = False, clockwise = False})], xys = [P -1.5 0.5,P 1.5 -0.5]}]
--
-- ![problematic1](other/problematic1.svg)
--
-- Incorrect scaling of an Arc was occuring on x-axis gaps, but not with gapless x-axis elements, titles or any y axis variations.
--
-- > problematic1 (FixedAspect 1) (ArcPosition (Point 1 0) (Point 0 1) (ArcInfo (Point 1.0 0.5) (0) False True)) & #hudOptions .~ (mempty & #hudAxes .~ [defaultAxisOptions & #place .~ PlaceTop & #adjust .~ Nothing & #axisBar .~ Nothing & #axisTick . #ltick .~ Nothing & #axisTick . #tstyle .~ TickRound (FormatComma (Just 2)) 2 NoTickExtend & #axisTick . #ttick .~ Nothing & #axisTick . #gtick .~ Just (defaultGlyphTick, 0.2), defaultAxisOptions & #place .~ PlaceRight & #adjust .~ Nothing & #axisBar .~ Nothing & #axisTick . #ltick .~ Nothing & #axisTick . #tstyle .~ TickRound (FormatComma (Just 2)) 2 NoTickExtend & #axisTick . #ttick .~ Nothing & #axisTick . #gtick .~ Just (defaultGlyphTick, 0.2)])
--
-- Again isolating to runHud ...
--
-- > let cs = [toPathChart defaultPathStyle $ singletonArc $ (ArcPosition (Point 1 0) (Point 0 1) (ArcInfo (Point 1.0 0.5) (0) False True))]
-- > let dbox = padBox $ dataBoxes cs
-- > dbox
-- Rect -0.6180339784260676 1.0 -5.901699179399067e-2 1.0
--
-- > let ho = (mempty & #hudAxes .~ [defaultAxisOptions & #place .~ PlaceBottom & #adjust .~ Nothing & #axisBar .~ Nothing & #axisTick . #ltick .~ Nothing & #axisTick . #tstyle .~ TickRound (FormatComma (Just 2)) 2 NoTickExtend & #axisTick . #ttick .~ Nothing & #axisTick . #gtick .~ Just (defaultGlyphTick, 0.2), defaultAxisOptions & #place .~ PlaceRight & #adjust .~ Nothing & #axisBar .~ Nothing & #axisTick . #ltick .~ Nothing & #axisTick . #tstyle .~ TickRound (FormatComma (Just 2)) 2 NoTickExtend & #axisTick . #ttick .~ Nothing & #axisTick . #gtick .~ Just (defaultGlyphTick, 0.2)])
-- > let (hs',cs') = makeHud dbox ho
-- > let cs1 = runHud dbox hs' cs
-- > cs1
--
-- > [Chart {annotation = GlyphA (GlyphStyle {size = 3.0e-2, color = RGBA 0.50 0.50 0.50 1.00, borderColor = RGBA 0.50 0.50 0.50 1.00, borderSize = 5.0e-3, shape = VLineGlyph 5.0e-3, rotation = Just 1.5707963267948966, translate = Nothing}), xys = [P 1.2049999999999998 0.0,P 1.2049999999999998 0.5,P 1.2049999999999998 1.0]},Chart {annotation = GlyphA (GlyphStyle {size = 3.0e-2, color = RGBA 0.50 0.50 0.50 1.00, borderColor = RGBA 0.50 0.50 0.50 1.00, borderSize = 5.0e-3, shape = VLineGlyph 5.0e-3, rotation = Nothing, translate = Nothing}), xys = [P 0.0 -0.2640169917939907,P 1.0 -0.2640169917939907]},Chart {annotation = PathA (PathStyle {borderSize = 1.0e-2, borderColor = RGBA 0.12 0.47 0.71 0.80, color = RGBA 0.12 0.47 0.71 0.30}) [StartI,ArcI (ArcInfo {radii = Point 1.0 0.5, phi = 0.0, large = False, clockwise = True})], xys = [P 1.0 0.0,P 0.0 1.0]}]
--
-- The output from runHud looks ok, so the problem was isolated to projectXYsWith ...
--
-- phi was then causing aspect scaling problems. Unadjusted was good:
--
-- > writeChartSvg "other/t1.svg" $ problematic1 (UnadjustedAspect) (ArcPosition (Point 1 0) (Point 0 1) (ArcInfo (Point 1.0 0.5) (-pi/4) False True)) & #hudOptions .~ defaultHudOptions
--
-- And then I realised that the x and y radii of the ellipse was __firstly__ rotated in the XY-space and only then should be subject to scaling...
problematic1 :: ChartAspect -> ArcPosition Double -> ChartSvg
problematic1 ca p =
  mempty &
  #chartList .~ [arc, ell, pts, bbox] &
  #hudOptions .~ defaultHudOptions &
  #svgOptions .~ (defaultSvgOptions & #chartAspect .~ ca)
  where
    -- p = ArcPosition (Point 0 1) (Point 1 0) (ArcInfo (Point 1 1) 0 False False)
    (ArcCentroid c r phi' ang0 angd) = arcCentroid p
    ps = singletonArc p
    arc = toPathChart defaultPathStyle ps
    ell = Chart (LineA $ defaultLineStyle & #width .~ 0.002 & #color .~ (palette1!!1)) (PointXY . ellipse c r phi' . (\x -> ang0 + angd * x / 10.0) <$> [0..10])
    pts = Chart (GlyphA defaultGlyphStyle) (fmap PointXY [c, p ^. #posStart, p ^. #posEnd])
    bbox = Chart (RectA $ defaultRectStyle & #borderSize .~ 0.002 & #color .~ Colour 0.4 0.4 0.8 0.1 & #borderColor .~ Colour 0.5 0.5 0.5 1) [RectXY (arcBox p)]

-- | FIXME: A guesstimate for arc scaling
--
-- In a chart-svg projection (See 'projectOnP' say), points on the ellipse scale, but radii and angles do not, and not sure about the centroid.
--
-- This causes distortion for chart-svg re-scaling.
--
-- This is a chart of a guess for values for (aspect 2)
--
-- > let p = ArcPosition (Point 0 0) (Point 1 0) (ArcInfo (Point 1 2) (pi/6) True True)
-- > let guess = ArcCentroid (Point -0.2864867185179476 1.6092991486979669) (Point 1.3266801807145205 3.0142082605509395) 1.0962340928888052 -2.8 -5.5
--
-- ![problematic2](other/problematic2.svg)
problematic2 :: ArcPosition Double -> ArcCentroid Double -> ChartSvg
problematic2 p@(ArcPosition p1 p2 _) (ArcCentroid gc gr gphi gang0 gangd)=
  mempty &
   #chartList .~ [ellGuess] <> projectXYsWith asp one [c0, xradii, yradii, dir0, dir1, ell, ellFull, rtxt, gs] &
   #hudOptions .~ defaultHudOptions &
   #svgOptions %~ ((#outerPad .~ Nothing) . (#chartAspect .~ UnadjustedAspect))
  where
    asp = aspect 2
    (ArcCentroid c r a ang0 angd) = arcCentroid p
    ang1 = ang0+angd
    ax = c+_x r .* ray a
    ay = c+_y r .* ray (a + pi/2)
    ellFull = Chart (LineA $ defaultLineStyle & #width .~ 0.002 & #color .~ (palette1!!1)) (PointXY . ellipse c r a . (\x -> 2 * pi * x / 100.0) <$> [0..100])
    ell = Chart (LineA $ defaultLineStyle & #width .~ 0.04 & #color .~ setOpac 0.3 (palette1!!2)) (PointXY . ellipse c r a . (\x -> ang0 + (ang1 - ang0) * x / 100.0) <$> [0..100])
    ellGuess = Chart (LineA $ defaultLineStyle & #width .~ 0.03 & #color .~ setOpac 0.4 (palette1!!4)) (PointXY . ellipse gc gr gphi . (\x -> gang0 + gangd * x / 100.0) <$> [0..100])
    c0 = Chart (GlyphA defaultGlyphStyle) (PointXY <$> [c,p1,p2,ax,ay])
    gs = Chart (GlyphA $ defaultGlyphStyle & #shape .~ CircleGlyph & #size .~ 0.05) (PointXY <$> [ellipse c r a 0.472, ellipse c r a (0.472 + pi/2)])
    xradii = Chart (LineA $ defaultLineStyle & #color .~ Colour 0.9 0.2 0.02 1 & #width .~ 0.005 & #dasharray .~ Just [0.03, 0.01] & #linecap .~ Just LineCapRound) (PointXY <$> [c, ax])
    yradii = Chart (LineA $ defaultLineStyle & #color .~ Colour 0.9 0.8 0.02 1 & #width .~ 0.005 & #dasharray .~ Just [0.03, 0.01] & #linecap .~ Just LineCapRound) (PointXY <$> [c, ay])
    dir0 = Chart (LineA $ defaultLineStyle & #color .~ Colour 0.2 0.9 0.8 1 & #width .~ 0.005 & #dasharray .~ Just [0.03, 0.01] & #linecap .~ Just LineCapRound) (PointXY <$> [c, p1])
    dir1 = Chart (LineA $ defaultLineStyle & #color .~ Colour 0.2 0.2 0.9 1 & #width .~ 0.005 & #dasharray .~ Just [0.03, 0.01] & #linecap .~ Just LineCapRound) (PointXY <$> [c, p2])
    rtxt = Chart (TextA defaultTextStyle ((\(Point x y) -> fixed (Just 3) x <> "," <> fixed (Just 3) y) <$> [r])) [P 0 2.5, P 0 2]

-- | Run this to refresh haddock example SVGs.
writeAllExamples :: IO ()
writeAllExamples = do
  -- Example in cabal file
  let xs = [[(0.0, 1.0), (1.0, 1.0), (2.0, 5.0)], [(0.0, 0.0), (3.2, 3.0)], [(0.5, 4.0), (0.5, 0)]] :: [[(Double, Double)]]
  let ls = fmap (PointXY . uncurry Point) <$> xs
  let anns = zipWith (\w c -> LineA (defaultLineStyle & #width .~ w & #color .~ c)) [0.015, 0.03, 0.01] palette1
  let lineChart = zipWith Chart anns ls
  writeChartSvgDefault "other/lines.svg" lineChart
  writeChartSvgHud "other/lineshud.svg" lineChart

  -- charts in Chart.Types
  writeChartSvg "other/unit.svg" unitExample
  writeChartSvg "other/rect.svg" rectExample
  writeChartSvg "other/text.svg" textExample
  writeChartSvg "other/glyphs.svg" glyphsExample
  writeChartSvg "other/line.svg" lineExample
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
  writeChartSvg "other/venn.svg" vennExample
  writeChartSvg "other/path.svg" pathExample
  writeChartSvg "other/arc.svg" $
    arcExample (ArcPosition (Point 1.0 0.0) (Point 0.0 1.0)
             (ArcInfo (Point 1.0 0.5) (-pi/3) False True))
  writeChartSvg "other/arcflags.svg" arcFlagsExample
  writeChartSvg "other/ellipse.svg" $ ellipseExample
    (ArcPosition (Point 1 0) (Point 0 1) (ArcInfo (Point 1.5 1) (pi/3) True True))
  writeChartSvg "other/quad.svg" $
    quadExample (QuadPosition (Point 0 0) (Point 1 1) (Point 2 -1))
  writeChartSvg "other/cubic.svg" $
    cubicExample (CubicPosition (Point 0 0) (Point 1 1) (Point 1 0) (Point 0 1))
  writeChartSvg "other/surface.svg" $
    surfaceExample "rosenbrock" (Point 100 100) one
    (fst . bimap (-1.0 *) (-1.0 .*) . rosenbrock 1 10)
  writeChartSvg "other/arrowg.svg" $
    arrowgExample (Point 20 20) one
    (snd . bimap (-1.0 *) (-1.0 .*) . rosenbrock 1 10)
  writeChartSvg "other/surfaceg.svg" $
    surfacegExample "rosenbrock" (Point 100 100) (Point 20 20) one
    (bimap (-1.0 *) (-1.0 .*) . rosenbrock 1 10)

  -- problematic charts
  writeChartSvg "other/problematic1.svg" $
    problematic1 (FixedAspect 2) (ArcPosition (Point 0 1) (Point 1 0) (ArcInfo (Point 1 1) 0 False False))
  writeChartSvg "other/problematic2.svg" $
    problematic2 (ArcPosition (Point 0 0) (Point 1 0) (ArcInfo (Point 1 2) (pi/6) True True)) (ArcCentroid (Point -0.2864867185179476 1.6092991486979669) (Point 1.3266801807145205 3.0142082605509395) 1.0962340928888052 -2.8 -5.5)

  putStrLn ("ok" :: Text)

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Examples of chart construction.
module Chart.Examples
  ( unitExample,
    svgOptionsExample,
    hudOptionsExample,
    hudOptionsDarkExample,
    rectExample,
    textExample,
    glyphsExample,
    lineExample,
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
    dateExample,
    writeAllExamples,
    writeAllExamplesDark,
  )
where

import Chart
import Control.Lens
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Time
import Data.Bool
import Data.Foldable

-- $setup
-- >>> import Chart
-- >>> import Control.Lens
--

-- | unit example
--
-- ![unit example](other/unit.svg)
unitExample :: ChartSvg
unitExample = mempty & #chartTree .~ [RectChart defaultRectStyle [one]]

-- | 'HudOptions' example
--
-- ![hudoptions example](other/hudoptions.svg)
hudOptionsExample :: ChartSvg
hudOptionsExample =
  mempty
    & #hudOptions .~ colourHudOptions dark defaultHudOptions
    & #chartTree .~ [BlankChart [one]]
    & #svgOptions . #cssOptions . #preferColorScheme .~ PreferLight

-- | 'HudOptions' PreferDark example
--
-- ![hudoptions dark example](other/hudoptionsdark.svg)
hudOptionsDarkExample :: ChartSvg
hudOptionsDarkExample =
  mempty
    & #hudOptions .~ colourHudOptions light defaultHudOptions
    & #chartTree .~ [BlankChart [one]]
    & #svgOptions . #cssOptions . #preferColorScheme .~ PreferDark
    & #svgOptions . #background .~ Just dark

-- | 'SvgOptions' example.
--
-- ![svgoptions example](other/svgoptions.svg)
svgOptionsExample :: ChartSvg
svgOptionsExample =
  mempty
    & #svgOptions %~ #chartAspect .~ FixedAspect 0.7
    & #chartTree .~ zipWith (\s l -> LineChart s [l]) lopts ls

-- | rect example
--
-- ![rect example](other/rect.svg)
rectExample :: ChartSvg
rectExample =
  mempty
    & #hudOptions .~ (defaultHudOptions &
                      #hudAxes .~ [defaultAxisOptions & #axisTick . #ltick .~ Nothing] &
                      #hudCanvas .~ Nothing)
    & #chartTree .~ zipWith RectChart ropts rss

rss :: [NonEmpty (Rect Double)]
rss = fmap NonEmpty.fromList
  [ gridR (\x -> exp (-(x ** 2) / 2)) (Range (-5) 5) 50,
    gridR (\x -> 0.5 * exp (-(x ** 2) / 8)) (Range (-5) 5) 50
  ]

ropts :: [RectStyle]
ropts =
  [ blob (setOpac 0.3 (palette1 3)),
    blob (setOpac 0.3 (palette1 5))
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
-- >>> let anns = zipWith (\w c -> LineA (defaultLineStyle & #width .~ w & #color .~ c)) [0.015, 0.03, 0.01] palette1_
-- >>> anns
-- [LineA (LineStyle {width = 1.5e-2, color = Colour 0.69 0.35 0.16 1.00, linecap = Nothing, linejoin = Nothing, dasharray = Nothing, dashoffset = Nothing}),LineA (LineStyle {width = 3.0e-2, color = Colour 0.65 0.81 0.89 1.00, linecap = Nothing, linejoin = Nothing, dasharray = Nothing, dashoffset = Nothing}),LineA (LineStyle {width = 1.0e-2, color = Colour 0.12 0.47 0.71 1.00, linecap = Nothing, linejoin = Nothing, dasharray = Nothing, dashoffset = Nothing})]
--
-- >>> let lineExample = mempty & (#chartTree .~ zipWith Chart anns (fmap (fmap PointXY) xs)) & #hudOptions .~ defaultHudOptions & #svgOptions .~ defaultSvgOptions :: ChartSvg
-- >>> :t lineExample
-- lineExample :: ChartSvg
--
-- > writeChartSvg "other/line.svg" lineExample
--
-- ![line example](other/line.svg)
lineExample :: ChartSvg
lineExample =
  mempty
    & #svgOptions . #chartAspect
    .~ CanvasAspect 1.5
    & #hudOptions
    .~ exampleLineHudOptions
      "Line Chart"
      (Just "An example from chart-svg")
      (Just (defaultLegendOptions, zip (LineA <$> lopts) ["hockey", "line", "vertical"]))
    & #chartTree
    .~ zipWith (\s l -> LineChart s [l]) lopts ls

ls :: [NonEmpty (Point Double)]
ls =
  fmap (uncurry Point)
    <$> [ [(0.0, 1.0), (1.0, 1.0), (2.0, 5.0)],
          [(0.0, 0.0), (2.8, 3.0)],
          [(0.5, 4.0), (0.5, 0)]
        ]

lopts :: [LineStyle]
lopts =
  [ defaultLineStyle & #color .~ palette1 0 & #width .~ 0.015,
    defaultLineStyle & #color .~ palette1 1 & #width .~ 0.03,
    defaultLineStyle & #color .~ palette1 2 & #width .~ 0.01
  ]

exampleLineHudOptions :: Text -> Maybe Text -> Maybe (LegendOptions, [(Styles, Text)]) -> HudOptions
exampleLineHudOptions t1 t2 legends' =
  defaultHudOptions
    & #hudTitles
      .~ ( [ defaultTitle t1
               & #style . #size .~ 0.08
           ]
             <> foldMap
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
textExample :: Colour -> ChartSvg
textExample textColour =
  mempty &
    #chartTree .~
      [TextChart
       (defaultTextStyle & (#color .~ textColour) & (#size .~ (0.05 :: Double)))
       ts] &
    #hudOptions .~ colourHudOptions (bool dark light (textColour==light)) defaultHudOptions &
    #svgOptions . #cssOptions . #preferColorScheme .~ bool PreferLight PreferDark (textColour==light) &
    #svgOptions . #background .~ Just (bool light dark (textColour==light))
  where
    ts :: NonEmpty (Text, Point Double)
    ts =
      NonEmpty.zip
        (fmap Text.singleton ['a' .. 'y'])
        ((\x -> Point (sin (x * 0.1)) x) <$> [0 .. 25])

-- | glyphs example
--
-- ![glyphs example](other/glyphs.svg)
glyphsExample :: ChartSvg
glyphsExample =
  mempty
    & #svgOptions . #svgHeight
    .~ 50
      & #chartTree
    .~ zipWith
      ( \(sh, bs) p ->
          GlyphChart
                ( defaultGlyphStyle
                    & #size .~ (0.1 :: Double)
                    & #borderSize .~ bs
                    & #shape .~ sh
                )
            [p]
      )
      [ (CircleGlyph, 0.01 :: Double),
        (SquareGlyph, 0.01),
        (RectSharpGlyph 0.75, 0.01),
        (RectRoundedGlyph 0.75 0.01 0.01, 0.01),
        (EllipseGlyph 0.75, 0.01),
        (VLineGlyph, 0.01),
        (HLineGlyph, 0.01),
        (TriangleGlyph (Point 0.0 0.0) (Point 1 1) (Point 1 0), 0.01),
        (PathGlyph "M 0.5,-0.3660 A 1.0 1.0 -0.0 0 1 0,0.5 A 1.0 1.0 -0.0 0 1 -0.5,-0.3660 A 1.0 1.0 -0.0 0 1 0.5,-0.3660 L 0.5,-0.3660 Z" ScaleBorder, 0.01)
      ]
      [Point x 0 | x <- [0 .. (8 :: Double)]]

-- | Example data for Bar chart
barDataExample :: BarData
barDataExample =
  BarData
    [[1, 2, 3, 5, 8, 0, -2, 11, 2, 1], [1 .. 10]]
    (Just (("row " <>) . pack . show <$> [1 .. 11::Int]))
    (Just (("column " <>) . pack . show <$> [1 .. 2::Int]))

-- | Bar chart example.
--
-- ![bar example](other/bar.svg)
barExample :: ChartSvg
barExample = barChart defaultBarOptions barDataExample

-- | Text is not scaled as a data element of the chart. It could be, but projecting a piece of text from a 1x10 size to a 10x1 size is problematic with respect to human perception.
-- This example shows the results of this design, with the rectangles having been computed as the bounded rectangle of the text, prior to scaling of the chart. The rectangles scale correctly, but the text does not change size in the transformation.
--
-- ![text local example](other/textlocal.svg)
textLocalExample :: ChartSvg
textLocalExample =
  mempty & #chartTree
    .~ [ t1,
         t2,
         RectChart rs [padSingletons $ sbox t1],
         RectChart rs [padSingletons $ sbox t2]
       ]
  where
    rs = defaultRectStyle & #color %~ setOpac 0.1
    t1 =
      TextChart
        (defaultTextStyle & #anchor .~ AnchorStart & #hsize .~ 0.5 & #size .~ 0.08)
        [("a pretty long piece of text", zero)]
    t2 =
      TextChart
        (defaultTextStyle & #anchor .~ AnchorStart & #hsize .~ 0.5 & #size .~ 0.08)
        [("another pretty long piece of text", Point 1 1)]

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
    (toList palette1_)
    [EllipseGlyph 1.5, SquareGlyph, CircleGlyph]

-- | Glyph + Lines
--
-- ![glines example](other/glines.svg)
glinesExample :: ChartSvg
glinesExample = mempty & #chartTree .~ (cs <> gs)
  where
    cs = zipWith (\s l -> LineChart s [l]) lopts ls
    gs = zipWith GlyphChart gopts3 ls

lgdata :: [(Text, Point Double)]
lgdata =
  (\p@(Point x y) -> (pack (show x <> "," <> show y), fromIntegral <$> p))
    <$> (Point <$> [0 .. 5] <*> [0 .. 5] :: [Point Int])

-- | Labelled Glyphs
--
-- ![lglyph example](other/lglyph.svg)
lglyphExample :: ChartSvg
lglyphExample = mempty & #chartTree .~ (txt <> gly)
  where
    txt =
      ( \(t, Point x y) ->
          TextChart
                ( defaultTextStyle
                    & #color %~ setOpac 0.2
                )
            [(t,Point x (y + 0.2))])
        <$> lgdata
    gly =
      ( \d ->
          GlyphChart
                ( defaultGlyphStyle
                    & #size .~ 0.01
                    & #borderSize .~ 0
                    & #color .~ palette1 2
                )
            [d]
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
  mempty & #chartTree
    .~ [TextChart (defaultTextStyle & #rotation ?~ -pi / 4) [("text at (1,1) rotated by -(pi/4) radians", Point 1.0 1.0)]]

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
        (TextA (defaultTextStyle & #anchor .~ AnchorStart), "text"),
        (LineA defaultLineStyle, "line"),
        (GlyphA defaultGlyphStyle, "abcdefghijklmnopqrst")
      ]

-- | wave example
--
-- ![wave example](other/wave.svg)
waveExample :: ChartSvg
waveExample = mempty & #chartTree .~ [GlyphChart defaultGlyphStyle $ NonEmpty.fromList $ gridP sin (Range 0 (2 * pi)) 30]

-- | venn diagram
--
-- ![venn diagram](other/venn.svg)
vennExample :: ChartSvg
vennExample =
  mempty
    & #chartTree .~ zipWith (\c x -> PathChart (defaultPathStyle & #color .~ setOpac 0.2 c) x) (toList palette1_) (fmap pathInfoToSvgCoords . toPathXYs . either error id . parsePath <$> vennSegs)
    & #svgOptions .~ (defaultSvgOptions & #chartAspect .~ FixedAspect 1)
    & #hudOptions .~ defaultHudOptions

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
  mempty
    & #chartTree .~ [path', c0]
    & #hudOptions .~ defaultHudOptions
    & #svgOptions
    %~ ( (#outerPad ?~ 0.1)
           . (#chartAspect .~ ChartAspect)
       )
  where
    ps =
      [ (StartI, Point 0 0),
        (LineI, Point 1 0),
        (CubicI (Point 0.2 0) (Point 0.25 1), Point 1 1),
        (QuadI (Point (-1) 2), Point 0 1),
        (ArcI (ArcInfo (Point 1 1) (-pi / 6) False False), Point 0 0)
      ]
    path' = PathChart (defaultPathStyle & #color .~ setOpac 0.1 (palette1 2) & #borderColor .~ Colour 0.2 0.8 0.4 0.3) ps
    c0 = GlyphChart defaultGlyphStyle (snd <$> ps)

-- | ellipse example
--
-- (ArcPosition (Point 1 0) (Point 0 1) (ArcInfo (Point 1.5 1) 0 True True))
--
-- ![ellipse example](other/ellipse.svg)
ellipseExample :: ChartSvg
ellipseExample =
  mempty
    & #chartTree .~ [ell, ellFull, c0, bbox, xradii, yradii]
    & #hudOptions .~ defaultHudOptions
    & #svgOptions %~ ((#outerPad .~ Nothing) . (#chartAspect .~ UnadjustedAspect))
  where
    p@(ArcPosition p1 p2 _) = ArcPosition (Point 1 0) (Point 0 1) (ArcInfo (Point 1.5 1) (pi / 3) True True)
    (ArcCentroid c r phi' ang0' angd) = arcCentroid p
    ellFull = LineChart (defaultLineStyle & #width .~ 0.002 & #color .~ palette1 1) [ellipse c r phi' . (\x -> 2 * pi * x / 100.0) <$> [0 .. 100]]
    ell = LineChart (defaultLineStyle & #width .~ 0.002 & #color .~ palette1 1) [ellipse c r phi' . (\x -> ang0' + angd * x / 100.0) <$> [0 .. 100]]
    c0 = GlyphChart defaultGlyphStyle [c, p1, p2]
    bbox = RectChart (defaultRectStyle & #borderSize .~ 0.002 & #color .~ Colour 0.4 0.4 0.8 0.1 & #borderColor .~ Colour 0.5 0.5 0.5 1) [arcBox p]
    xradii = LineChart (defaultLineStyle & #color .~ Colour 0.9 0.2 0.02 1 & #width .~ 0.005 & #dasharray .~ Just [0.03, 0.01] & #linecap .~ Just LineCapRound) [[ellipse c r phi' 0, ellipse c r phi' pi]]
    yradii = LineChart (defaultLineStyle & #color .~ Colour 0.9 0.9 0.02 1 & #width .~ 0.005 & #dasharray .~ Just [0.03, 0.01] & #linecap .~ Just LineCapRound) [[ellipse c r phi' (pi / 2), ellipse c r phi' (3 / 2 * pi)]]

-- | arc example
--
-- ![arc example](other/arc.svg)
arcExample :: ChartSvg
arcExample =
  mempty
    & #chartTree .~ [arc, ell, c0, bbox]
    & #hudOptions .~ defaultHudOptions
    & #svgOptions %~ ((#outerPad .~ Nothing) . (#chartAspect .~ FixedAspect 1))
  where
    p1 = ArcPosition (Point 1.0 0.0) (Point 0.0 1.0) (ArcInfo (Point 1.0 0.5) 0 False True)
    ps = singletonArc p1
    (ArcCentroid c r phi' ang0' angd) = arcCentroid p1
    arc = PathChart (defaultPathStyle & #color .~ setOpac 0.1 (palette1 2) & #borderColor .~ transparent) ps
    ell = LineChart (defaultLineStyle & #width .~ 0.002 & #color .~ palette1 1) [ellipse c r phi' . (\x -> ang0' + angd * x / 100.0) <$> [0 .. 100]]
    c0 = GlyphChart defaultGlyphStyle [c]
    bbox = RectChart (defaultRectStyle & #borderSize .~ 0.002 & #color .~ Colour 0.4 0.4 0.8 0.1 & #borderColor .~ Colour 0.5 0.5 0.5 1) [arcBox p1]

-- | Reproduction of the flag explanation chart in <https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths>
--
-- ![arc flags example](other/arcflags.svg)
arcFlagsExample :: ChartSvg
arcFlagsExample =
  mempty
    & #chartTree
    .~ vert
      0.02
      [ hori
          0.02
          [ [ BlankChart [Rect (-0.4) 0.4 (-1) 5],
              TextChart (defaultTextStyle & #size .~ 0.6 & #rotation .~ Just (pi / 2)) [("Sweep", Point 0.1 2)]
            ],
            vert
              0.02
              [ [ BlankChart [Rect (-0.25) 0.25 (-1) 2],
                  TextChart (defaultTextStyle & #size .~ 0.4 & #rotation .~ Just (pi / 2)) [("True", Point 0.1 0.5)]
                ],
                [ BlankChart [Rect (-0.25) 0.25 (-1) 2],
                  TextChart (defaultTextStyle & #size .~ 0.4 & #rotation .~ Just (pi / 2)) [("False", Point 0.1 0.5)]
                ]
              ],
            vert
              0.02
              [ checkFlags False True (setOpac 0.3 dark) & view #chartTree,
                checkFlags False False (setOpac 0.3 dark) & view #chartTree,
                [ BlankChart [Rect (-1) 2 (-0.25) 0.25],
                  TextChart (defaultTextStyle & #size .~ 0.4) [("False", Point 0.5 (-0.1))]
                ]
              ],
            vert
              0.02
              [ checkFlags True True (setOpac 0.3 dark) & view #chartTree,
                checkFlags True False (setOpac 0.3 dark) & view #chartTree,
                [ BlankChart [Rect (-1) 2 (-0.25) 0.25],
                  TextChart (defaultTextStyle & #size .~ 0.4) [("True", Point 0.5 (-0.1))]
                ]
              ]
          ],
        [ BlankChart [Rect 0 9 (-2.75) (-3.25)],
          TextChart (defaultTextStyle & #size .~ 0.6) [("Large", Point 5.5 (-3.0))]
        ]
      ]
    & #hudOptions
    .~ mempty
    & #svgOptions %~ ((#outerPad .~ Nothing) . (#chartAspect .~ UnadjustedAspect))

checkFlags :: Bool -> Bool -> Colour -> ChartSvg
checkFlags large' sweep co =
  mempty
    & #hudOptions .~ defaultHudOptions
    & #svgOptions . #chartAspect .~ UnadjustedAspect
    & #chartTree .~ [c1, c2, ell, arc1]
  where
    c = Point 1.0 1.0
    p1 = ArcPosition (Point 0.0 1.0) (Point 1.0 0.0) (ArcInfo (Point 1.0 1.0) 0 large' sweep)
    ps1 = singletonPie c p1
    (ArcCentroid c' r phi' ang0' angd) = arcCentroid p1
    arc1 = PathChart (defaultPathStyle & #color .~ co & #borderColor .~ setOpac 0.5 dark) ps1
    c1 = LineChart (defaultLineStyle & #width .~ 0.02 & #color .~ setOpac 0.2 dark) [ellipse (Point 1.0 1.0) (Point 1.0 1.0) 0 . (\x -> 0 + 2 * pi * x / 100.0) <$> [0 .. 100]]
    c2 = LineChart (defaultLineStyle & #width .~ 0.02 & #color .~ setOpac 0.2 dark) [ellipse (Point 0.0 0.0) (Point 1.0 1.0) 0 . (\x -> 0 + 2 * pi * x / 100.0) <$> [0 .. 100]]
    ell = LineChart (defaultLineStyle & #width .~ 0.05 & #color .~ setOpac 0.5 co) [ellipse c' r phi' . (\x -> ang0' + angd * x / 100.0) <$> [0 .. 100]]

-- | quad example
--
-- ![quad example](other/quad.svg)
quadExample :: ChartSvg
quadExample =
  mempty
    & #chartTree .~ [path', curve, c0, bbox]
    & #hudOptions .~ defaultHudOptions
    & #svgOptions %~ ((#outerPad ?~ 0.05) . (#chartAspect .~ ChartAspect))
  where
    p@(QuadPosition start end control) = QuadPosition (Point 0 0) (Point 1 1) (Point 2 (-1))
    ps = singletonQuad p
    path' = PathChart (defaultPathStyle & #color .~ setOpac 0.1 (palette1 2) & #borderColor .~ transparent) ps
    curve = LineChart (defaultLineStyle & #width .~ 0.002 & #color .~ palette1 1) [quadBezier p . (/ 100.0) <$> [0 .. 100]]
    c0 = GlyphChart defaultGlyphStyle [start, end, control]
    bbox = RectChart (defaultRectStyle & #borderSize .~ 0.002 & #color .~ Colour 0.4 0.4 0.8 0.1 & #borderColor .~ Colour 0.5 0.5 0.5 1) [quadBox p]

-- | cubic example
--
-- ![cubic example](other/cubic.svg)
cubicExample :: ChartSvg
cubicExample =
  mempty
    & #chartTree .~ [path', curve, c0, bbox]
    & #hudOptions .~ mempty
    & #svgOptions %~ ((#outerPad ?~ 0.02) . (#chartAspect .~ ChartAspect))
  where
    p@(CubicPosition start end control1 control2) = CubicPosition (Point 0 0) (Point 1 1) (Point 1 0) (Point 0 1)
    ps = singletonCubic p
    path' = PathChart (defaultPathStyle & #color .~ setOpac 0.1 (palette1 2) & #borderColor .~ transparent) ps
    curve = LineChart (defaultLineStyle & #width .~ 0.002 & #color .~ palette1 1) [cubicBezier p . (/ 100.0) <$> [0 .. 100]]
    c0 = GlyphChart defaultGlyphStyle [start, end, control1, control2, cubicBezier p 0.8]
    bbox = RectChart (defaultRectStyle & #borderSize .~ 0.002 & #color .~ Colour 0.4 0.4 0.8 0.1 & #borderColor .~ Colour 0.5 0.5 0.5 1) [cubicBox p]

-- | The common way to create a surface chart is usually a grid over a function.
--
-- ![surface example](other/surface.svg)
surfaceExample :: Colour -> ChartSvg
surfaceExample c =
  mempty
    & #hudList .~ hs
    & #chartTree .~ cs
    & #svgOptions .~ (defaultSvgOptions & #cssOptions . #shapeRendering .~ UseCssCrisp)
  where
    t = "rosenbrock"
    grain = Point 20 20
    r = one
    f = fst . bimap ((-1.0) *) (fmap ((-1.0) *)) . rosenbrock 1 10
    (cs, hs) =
      surfacefl
        f
        ( defaultSurfaceOptions
            & #soGrain .~ grain
            & #soRange .~ r
            & #soStyle . #surfaceColors .~ (palette1 <$> [0 .. 5])
        )
        ( defaultSurfaceLegendOptions c t
            & #sloStyle . #surfaceColors .~ (palette1 <$> [0 .. 5])
            & #sloLegendOptions . #ltext . #color .~ c
            & #sloAxisOptions .~ surfaceAxisOptions c
            & #sloLegendOptions . #legendFrame %~ fmap (#borderColor .~ c)
        )

-- | arrow example
--
-- Which happens to be the gradient of the surface example.
--
-- ![arrow example](other/arrow.svg)
arrowExample :: Colour -> ChartSvg
arrowExample arrowColour =
  mempty
    & #hudOptions .~ (defaultHudOptions & #hudAxes %~ fmap (#axisTick . #ltick .~ Nothing))
    & #chartTree .~ ((\p -> chart (tail' . f $ p) (angle . f $ p) p) <$> ps)
    & #svgOptions .~ defaultSvgOptions
  where
    f = snd . bimap ((-1.0) *) (fmap ((-1.0) *)) . rosenbrock 1 10
    ps = grid MidPos (one :: Rect Double) (Point 10 10 :: Point Int) :: [Point Double]
    arrow = PathGlyph "M -1 0 L 1 0 M 1 0 L 0.4 0.3 M 1 0 L 0.4 -0.3" NoScaleBorder
    gs s r' =
      defaultGlyphStyle
        & #borderSize .~ 0.05
        & #size .~ s
        & #borderColor .~ arrowColour
        & #rotation .~ Just r'
        & #shape .~ arrow
    chart s r' p = GlyphChart (gs s r') [p]

    tail' :: Point Double -> Double
    tail' = max 0.05 . min 0.02 . (* 0.01) . (/ avmag) . norm

    avmag = sum (norm . f <$> ps) / fromIntegral (length ps)

-- | function for testing
--
-- > f(x,y) = (a-x)^2 + b * (y - x^2)^2
-- >        = a^2 - 2ax + x^2 + b * y^2 - b * 2 * y * x^2 + b * x ^ 4
-- > f'x = -2a + 2 * x - b * 4 * y * x + 4 * b * x ^ 3
-- > f'y = 2 * b * y - 2 * b * x^2
-- > f a b (Point x y) = (a^2 - 2ax + x^2 + b * y^2 - b * 2 * y * x^2 + b * x^4, Point (-2a + 2 * x - b * 4 * y * x + 4 * b * x ^ 3), 2 * b * y - 2 * b * x^2)
rosenbrock :: Double -> Double -> Point Double -> (Double, Point Double)
rosenbrock a b (Point x y) = (a ** 2 - 2 * a * x + x ** 2 + b * y ** 2 - b * 2 * y * x ** 2 + b * x ** 4, Point (-2 * a + 2 * x - b * 4 * y * x + 4 * b * x ** 3) (2 * b * y - 2 * b * x ** 2))

-- | date example
--
-- A hud that has date as the x-axis, and times as the y-axis. See 'placedTimeLabelContinuous'.
--
-- ![date example](other/date.svg)
dateExample :: ChartSvg
dateExample = mempty &
  #chartTree .~ [BlankChart [Rect 0 1 0 1]] &
  #hudOptions . #hudAxes .~
  [ defaultAxisOptions & #place .~ PlaceLeft & #axisTick . #tstyle .~ TickPlaced tsTime,
    defaultAxisOptions & #axisTick . #tstyle .~ TickPlaced tsDate
  ]
  where
    tsTime = placedTimeLabelContinuous PosIncludeBoundaries Nothing 12 (Range (UTCTime (fromGregorian 2021 12 6) (toDiffTime 0)) (UTCTime (fromGregorian 2021 12 7) (toDiffTime 0)))
    tsDate = placedTimeLabelContinuous PosIncludeBoundaries (Just (pack "%d %b")) 2 (Range (UTCTime (fromGregorian 2021 12 6) (toDiffTime 0)) (UTCTime (fromGregorian 2022 3 13) (toDiffTime 0)))

pathChartSvg :: Colour -> [(FilePath, ChartSvg)]
pathChartSvg c =
  [
    ("other/unit.svg",unitExample),
    ("other/rect.svg",rectExample),
    ("other/text.svg",textExample c),
    ("other/glyphs.svg",glyphsExample),
    ("other/line.svg",lineExample),
    ("other/svgoptions.svg",svgOptionsExample),
    ("other/hudoptions.svg",hudOptionsExample),
    ("other/hudoptionsdark.svg",hudOptionsDarkExample),
    ("other/legend.svg",legendExample),
      -- charts in Chart.Bar docs
    ("other/bar.svg",barExample),
      -- charts in Chart.Surface docs
    ("other/surface.svg",surfaceExample c),
      -- extra Charts
    ("other/wave.svg",waveExample),
    ("other/lglyph.svg",lglyphExample),
    ("other/glines.svg",glinesExample),
    ("other/compound.svg",compoundExample),
    ("other/textlocal.svg",textLocalExample),
    ("other/label.svg",labelExample),
    ("other/venn.svg",vennExample),
    ("other/path.svg",pathExample),
    ("other/arc.svg",arcExample),
    ("other/arcflags.svg",arcFlagsExample),
    ("other/ellipse.svg",ellipseExample),
    ("other/quad.svg",quadExample),
    ("other/cubic.svg",cubicExample),
    ("other/arrow.svg",arrowExample c),
    ("other/date.svg",dateExample)
  ]

-- | Run this to refresh haddock example SVGs.
writeAllExamples :: IO ()
writeAllExamples = do
  sequence_ $ uncurry writeChartSvg <$> pathChartSvg dark
  putStrLn "ok"

-- | Version of charts with a dark-friendly hud
writeAllExamplesDark :: IO ()
writeAllExamplesDark = do
  sequence_ $
    uncurry writeChartSvg .
    bimap
    ((<> "d.svg") . reverse . drop 4 . reverse)
    (\x ->
       x &
       #hudOptions %~ colourHudOptions light &
       #svgOptions . #cssOptions . #preferColorScheme .~ PreferDark
    ) <$>
    pathChartSvg light
  putStrLn "dark version, ok"

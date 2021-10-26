{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}

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

    -- * sub-chart patterns
    -- $subcharts
    subChartExample,
    xify,
    yify,
    addLineX,
    addLineY,
    lineLegend,
    titlesHud,
    blendMidLineStyles,
    blendExample,

    writeAllExamples,
    writeAllExamplesDark,
  )
where

import Chart
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Data.List.NonEmpty (NonEmpty, fromList)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Time
import Data.Bool
import Prelude hiding (abs)
import Data.Bifunctor
import Data.Function
import Optics.Core

-- $setup
-- >>> import Chart
-- >>> import Optics.Core
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
    & #hudOptions .~ colourHudOptions (rgb dark) defaultHudOptions
    & #chartTree .~ [BlankChart [one]]
    & #svgOptions % #cssOptions % #preferColorScheme .~ PreferLight

-- | 'HudOptions' PreferDark example
--
-- ![hudoptions dark example](other/hudoptionsdark.svg)
hudOptionsDarkExample :: ChartSvg
hudOptionsDarkExample =
  mempty
    & #hudOptions .~ colourHudOptions (rgb light) defaultHudOptions
    & #chartTree .~ [BlankChart [one]]
    & #svgOptions % #cssOptions % #preferColorScheme .~ PreferDark
    & #svgOptions % #chartFrame .~ Just (RectStyle 0 dark dark)

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
  mempty &
  #hudOptions .~ (mempty & set #axes
    [ (1, (defaultAxisOptions & #axisTick % #ltick .~ Nothing))]) &
  #chartTree .~ zipWith RectChart ropts rss

rss :: [NonEmpty (Rect Double)]
rss = fmap fromList
  [ gridR (\x -> exp (-(x ** 2) / 2)) (Range (-5) 5) 50,
    gridR (\x -> 0.5 * exp (-(x ** 2) / 8)) (Range (-5) 5) 50
  ]

ropts :: [RectStyle]
ropts =
  [ blob (set opac' 0.3 (palette1 3)),
    blob (set opac' 0.3 (palette1 5))
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
-- >>> let anns = zipWith (\w c -> LineA (defaultLineStyle & #size .~ w % #color .~ palette1 c)) [0.015, 0.03, 0.01] [0..2]
-- >>> anns
-- [LineA (LineStyle {size = 1.5e-2, color = Colour 0.69 0.35 0.16 1.00, linecap = Nothing, linejoin = Nothing, dasharray = Nothing, dashoffset = Nothing}),LineA (LineStyle {size = 3.0e-2, color = Colour 0.65 0.81 0.89 1.00, linecap = Nothing, linejoin = Nothing, dasharray = Nothing, dashoffset = Nothing}),LineA (LineStyle {size = 1.0e-2, color = Colour 0.12 0.47 0.71 1.00, linecap = Nothing, linejoin = Nothing, dasharray = Nothing, dashoffset = Nothing})]
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
  mempty &
  #hudOptions .~
  ( mempty &
    #axes .~
    [(2, defaultAxisOptions),
     (2, defaultAxisOptions & #place .~ PlaceLeft)
    ] &
    #titles .~
    [ (6, defaultTitle "Line Chart"),
      (5, defaultTitle "Made with love and chart-svg" &
         #style % #size .~ 0.05 & #place .~ PlaceBottom & #anchor .~ AnchorEnd)
    ] &
    #legends .~
    [ (7, defaultLegendOptions & #content .~ (zip ["hockey", "line", "vertical"] cs))
    ]
  )
  & #chartTree .~ cs
  where
    cs = zipWith (\s l -> LineChart s [l]) lopts ls

ls :: [NonEmpty (Point Double)]
ls =
  fmap (uncurry Point)
    <$> [ [(0.0, 1.0), (1.0, 1.0), (2.0, 5.0)],
          [(0.0, 0.0), (2.8, 3.0)],
          [(0.5, 4.0), (0.5, 0)]
        ]

lopts :: [LineStyle]
lopts =
  [ defaultLineStyle & #color .~ palette1 0 & #size .~ 0.015,
    defaultLineStyle & #color .~ palette1 1 & #size .~ 0.03,
    defaultLineStyle & #color .~ palette1 2 & #size .~ 0.01
  ]

-- | text example
--
-- ![text example](other/text.svg)
textExample :: Colour -> ChartSvg
textExample fg =
  mempty &
    #chartTree .~
      [TextChart
       (defaultTextStyle & (#color .~ fg) & (#size .~ 0.05) & (#nudge1 .~ 0))
       ts] &
    #hudOptions .~ colourHudOptions (rgb (bool dark light (fg==light))) defaultHudOptions &
    #svgOptions % #cssOptions % #preferColorScheme .~ bool PreferLight PreferDark (fg==light)
  where
    ts :: NonEmpty (Text, Point Double)
    ts =
      NonEmpty.zip
        (fmap Text.singleton ['a' .. 'z'])
        ((\x -> Point (sin (x * 0.1)) x) <$> [0 .. 25])

-- | glyphs example
--
-- ![glyphs example](other/glyphs.svg)
glyphsExample :: ChartSvg
glyphsExample =
  mempty
    & #svgOptions % #svgHeight
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
    (("row " <>) . pack . show <$> [1 .. 11::Int])
    (("column " <>) . pack . show <$> [1 .. 2::Int])

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
    rs = defaultRectStyle & #color %~ set opac' 0.1
    t1 =
      TextChart
        (defaultTextStyle & #anchor .~ AnchorStart & #hsize .~ 0.5 & #size .~ 0.08)
        [("a pretty long piece of text", zero)]
    t2 =
      TextChart
        (defaultTextStyle & #anchor .~ AnchorStart & #hsize .~ 0.5 & #size .~ 0.08)
        [("another pretty long piece of text", Point 1 1)]

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
    .~ ( defaultHudOptions & #legends .~
           [(10, defaultLegendOptions
                  & #lscale .~ 0.3
                  & #lplace .~ PlaceAbsolute (Point 0.0 0.0)
                  & #lsize .~ 0.16
                  & #vgap .~ (-0.1)
                  & #hgap .~ 0.06
                  & #ltext % #size .~ 0.16
                  & #content .~ l1)]
       )
  where
    l1 =
      [ ("glyph", GlyphChart defaultGlyphStyle [zero]),
        ("rect", RectChart defaultRectStyle [one]),
        ("text", TextChart (defaultTextStyle & #anchor .~ AnchorStart) [("text", zero)]),
        ("line", LineChart defaultLineStyle [[zero]]),
        ("abcdefghijklmnopqrst", GlyphChart defaultGlyphStyle [one])
      ]

-- | wave example
--
-- ![wave example](other/wave.svg)
waveExample :: ChartSvg
waveExample = mempty & #chartTree .~ [GlyphChart defaultGlyphStyle $ fromList $ gridP sin (Range 0 (2 * pi)) 30]

-- | venn diagram
--
-- ![venn diagram](other/venn.svg)
vennExample :: ChartSvg
vennExample =
  mempty
    & #chartTree .~ zipWith (\c x -> PathChart (defaultPathStyle & #color .~ set opac' 0.2 (palette1 c)) x) [0..] (svgToPathData <$> vennSegs)
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
      [ StartP (Point 0 0),
        LineP (Point 1 0),
        CubicP (Point 0.2 0) (Point 0.25 1) (Point 1 1),
        QuadP (Point (-1) 2) (Point 0 1),
        ArcP (ArcInfo (Point 1 1) (-pi / 6) False False) (Point 0 0)
      ]
    path' = PathChart (defaultPathStyle & #color .~ set opac' 0.1 (palette1 2) & #borderColor .~ Colour 0.2 0.8 0.4 0.3) ps
    c0 = GlyphChart defaultGlyphStyle (pointPath <$> ps)

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
    ellFull = LineChart (defaultLineStyle & #size .~ 0.002 & #color .~ palette1 1) [ellipse c r phi' . (\x -> 2 * pi * x / 100.0) <$> [0 .. 100]]
    ell = LineChart (defaultLineStyle & #size .~ 0.002 & #color .~ palette1 1) [ellipse c r phi' . (\x -> ang0' + angd * x / 100.0) <$> [0 .. 100]]
    c0 = GlyphChart defaultGlyphStyle [c, p1, p2]
    bbox = RectChart (defaultRectStyle & #borderSize .~ 0.002 & #color .~ Colour 0.4 0.4 0.8 0.1 & #borderColor .~ Colour 0.5 0.5 0.5 1) [arcBox p]
    xradii = LineChart (defaultLineStyle & #color .~ Colour 0.9 0.2 0.02 1 & #size .~ 0.005 & #dasharray .~ Just [0.03, 0.01] & #linecap .~ Just LineCapRound) [[ellipse c r phi' 0, ellipse c r phi' pi]]
    yradii = LineChart (defaultLineStyle & #color .~ Colour 0.9 0.9 0.02 1 & #size .~ 0.005 & #dasharray .~ Just [0.03, 0.01] & #linecap .~ Just LineCapRound) [[ellipse c r phi' (pi / 2), ellipse c r phi' (3 / 2 * pi)]]

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
    arc = PathChart (defaultPathStyle & #color .~ set opac' 0.1 (palette1 2) & #borderColor .~ transparent) ps
    ell = LineChart (defaultLineStyle & #size .~ 0.002 & #color .~ palette1 1) [ellipse c r phi' . (\x -> ang0' + angd * x / 100.0) <$> [0 .. 100]]
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
              [ checkFlags False True (set opac' 0.3 dark) & view #chartTree,
                checkFlags False False (set opac' 0.3 dark) & view #chartTree,
                [ BlankChart [Rect (-1) 2 (-0.25) 0.25],
                  TextChart (defaultTextStyle & #size .~ 0.4) [("False", Point 0.5 (-0.1))]
                ]
              ],
            vert
              0.02
              [ checkFlags True True (set opac' 0.3 dark) & view #chartTree,
                checkFlags True False (set opac' 0.3 dark) & view #chartTree,
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
    & #svgOptions % #chartAspect .~ UnadjustedAspect
    & #chartTree .~ [c1, c2, ell, arc1]
  where
    c = Point 1.0 1.0
    p1 = ArcPosition (Point 0.0 1.0) (Point 1.0 0.0) (ArcInfo (Point 1.0 1.0) 0 large' sweep)
    ps1 = singletonPie c p1
    (ArcCentroid c' r phi' ang0' angd) = arcCentroid p1
    arc1 = PathChart (defaultPathStyle & #color .~ co & #borderColor .~ set opac' 0.5 dark) ps1
    c1 = LineChart (defaultLineStyle & #size .~ 0.02 & #color .~ set opac' 0.2 dark) [ellipse (Point 1.0 1.0) (Point 1.0 1.0) 0 . (\x -> 0 + 2 * pi * x / 100.0) <$> [0 .. 100]]
    c2 = LineChart (defaultLineStyle & #size .~ 0.02 & #color .~ set opac' 0.2 dark) [ellipse (Point 0.0 0.0) (Point 1.0 1.0) 0 . (\x -> 0 + 2 * pi * x / 100.0) <$> [0 .. 100]]
    ell = LineChart (defaultLineStyle & #size .~ 0.05 & #color .~ set opac' 0.5 co) [ellipse c' r phi' . (\x -> ang0' + angd * x / 100.0) <$> [0 .. 100]]

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
    path' = PathChart (defaultPathStyle & #color .~ set opac' 0.1 (palette1 2) & #borderColor .~ transparent) ps
    curve = LineChart (defaultLineStyle & #size .~ 0.002 & #color .~ palette1 1) [quadBezier p . (/ 100.0) <$> [0 .. 100]]
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
    path' = PathChart (defaultPathStyle & #color .~ set opac' 0.1 (palette1 2) & #borderColor .~ transparent) ps
    curve = LineChart (defaultLineStyle & #size .~ 0.002 & #color .~ palette1 1) [cubicBezier p . (/ 100.0) <$> [0 .. 100]]
    c0 = GlyphChart defaultGlyphStyle [start, end, control1, control2, cubicBezier p 0.8]
    bbox = RectChart (defaultRectStyle & #borderSize .~ 0.002 & #color .~ Colour 0.4 0.4 0.8 0.1 & #borderColor .~ Colour 0.5 0.5 0.5 1) [cubicBox p]

-- | The common way to create a surface chart is usually a grid over a function.
--
-- ![surface example](other/surface.svg)
surfaceExample :: Colour -> ChartSvg
surfaceExample c =
  mempty
    & #extraHuds .~ h
    & #chartTree .~ cs
    & #svgOptions .~ (defaultSvgOptions & #cssOptions % #shapeRendering .~ UseCssCrisp)
  where
    t = "rosenbrock"
    grain = Point 20 20
    r = one
    f = fst . bimap ((-1.0) *) (fmap ((-1.0) *)) . rosenbrock 1 10
    (cs, h) =
      surfacefl
        f
        ( defaultSurfaceOptions
            & #soGrain .~ grain
            & #soRange .~ r
            & #soStyle % #surfaceColors .~ (palette1 <$> [0 .. 5])
        )
        ( defaultSurfaceLegendOptions c t
            & #sloStyle % #surfaceColors .~ (palette1 <$> [0 .. 5])
            & #sloLegendOptions % #ltext % #color .~ c
            & #sloAxisOptions .~ surfaceAxisOptions c
            & #sloLegendOptions % #legendFrame %~ fmap (#borderColor .~ c)
        )

-- | arrow example
--
-- Which happens to be the gradient of the surface example.
--
-- ![arrow example](other/arrow.svg)
arrowExample :: Colour -> ChartSvg
arrowExample arrowColour =
  mempty
    & #hudOptions .~ (defaultHudOptions & #axes %~ fmap (second (#axisTick % #ltick .~ Nothing)))
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
  #hudOptions .~ (mempty & #axes .~
  [ (1, defaultAxisOptions & #place .~ PlaceLeft & #axisTick % #tstyle .~ TickPlaced tsTime),
    (1, defaultAxisOptions & #axisTick % #tstyle .~ TickPlaced tsDate)
  ])
  where
    tsTime = placedTimeLabelContinuous PosIncludeBoundaries Nothing 12 (Range (UTCTime (fromGregorian 2021 12 6) (toDiffTime 0)) (UTCTime (fromGregorian 2021 12 7) (toDiffTime 0)))
    tsDate = placedTimeLabelContinuous PosIncludeBoundaries (Just (pack "%d %b")) 2 (Range (UTCTime (fromGregorian 2021 12 6) (toDiffTime 0)) (UTCTime (fromGregorian 2022 3 13) (toDiffTime 0)))

-- | subchart example
--
-- chart-svg is (hopefully) ergonomic when putting together compound, complex charts by combining lots of subchart components.
--
-- The code in subChartExample is an (&) (left-to-right composition), non-operator lensy example:
--
-- >   mempty &
-- Using mempty :: ChartSvg as a baseline to begin composing charts, which can be visualised at any time with writeChartSvg "example.svg" $ mempty & ...
--
-- >   over #chartTree
-- >     (LineChart (defaultLineStyle & set #color (palette1 8)) [xify [1,2,7,3,13,14]]:) &
--
-- Convert a one-dimensional list of numbers to points using 'xify', and make it an orange line. A classical line with a data point for every x.
--
-- >   over #chartTree
-- >     (LineChart (defaultLineStyle & set #color (palette1 5)) [yify [1,2,7,3,13,14]]:) &
--
-- Convert the same numbers to points using 'yify', and make it a green line. Not so classic line of a chart with a data point for every y.
--
-- >   over #chartTree
-- >     (addLineX 3
-- >     (defaultLineStyle &
-- >      set #color (set opac' 0.3 (palette1 5)) &
-- >      set #dasharray (Just [0.04, 0.01]))) &
--
-- Add a vertical, long-dash line at y=3.
--
-- >   over #chartTree
-- >     (addLineY 5
-- >     (defaultLineStyle &
-- >      set #color (set opac' 0.3 (palette1 8)) &
-- >      set #dasharray (Just [0.01]))) &
--
-- Add a horizontal, short-dash line at x=5.
--
-- >   over #hudOptions
-- >     (<> titlesHud "subchart example" "x axis title" "y axis title") &
--
-- Add some titles.
--
-- >   set (#hudOptions % #hudLegend)
-- >     (Just (lineLegend 0.01
-- >      ["xify", "yify", "addLineX", "addLineY"]
-- >      [ (palette1 8, Nothing),
-- >        (palette1 5, Nothing),
-- >        (set opac' 0.3 $ palette1 8, Just [0.01]),
-- >        (set opac' 0.3 $ palette1 5, Just [0.04, 0.01])])) &
--
-- Add a legend, reusing the chosen line styles.
--
-- >   over (#hudOptions % #hudLegend)
-- >     (fmap (first
-- >       ( set #lscale 0.4 .
-- >         set #vgap 0.3 .
-- >         set #lplace (PlaceAbsolute (Point 0.1 0.1)) .
-- >         over #legendFrame
-- >           (fmap
-- >            (set #color (Colour 1 1 1 1) .
-- >             set #borderColor (set opac' 0.1 dark)))))) &
--
-- Fix up the legend a bit.
--
-- >   over (#hudOptions % #hudAxes)
-- >     (fmap
-- >       (set (#axisTick % #tstyle) (TickRound (FormatComma (Just 2)) 8 NoTickExtend))) &
--
-- Stop the axes from extending past the data ranges.
--
-- >   set (#svgOptions % #chartAspect) (CanvasAspect 1.5)
--
-- Rescale the chart so that the canvas element is a pleasant ratio.
--
-- ![subchart example](other/subchart.svg)
subChartExample :: ChartSvg
subChartExample =
  mempty &
  over #chartTree
    (LineChart (defaultLineStyle & set #color (palette1 8)) [xify [1,2,7,3,13,14]]:) &
  over #chartTree
    (LineChart (defaultLineStyle & set #color (palette1 5)) [yify [1,2,7,3,13,14]]:) &
  over #chartTree
    (addLineX 3
    (defaultLineStyle &
     set #color (set opac' 0.3 (palette1 5)) &
     set #dasharray (Just [0.04, 0.01]))) &
  over #chartTree
    (addLineY 5
    (defaultLineStyle &
     set #color (set opac' 0.3 (palette1 8)) &
     set #dasharray (Just [0.01]))) &
  set #hudOptions
    (titlesHud "subchart example" "x axis title" "y axis title") &
  set (#hudOptions % #axes)
      [ (1, defaultAxisOptions & set (#axisTick % #tstyle) (TickRound (FormatComma (Just 2)) 8 NoTickExtend)),
        (1, defaultAxisOptions & set #place PlaceLeft & set (#axisTick % #tstyle) (TickRound (FormatComma (Just 2)) 8 NoTickExtend))] &
  set (#hudOptions % #legends)
    [(20, (lineLegend 0.01
     ["xify", "yify", "addLineX", "addLineY"]
     [ (palette1 8, Nothing),
       (palette1 5, Nothing),
       (set opac' 0.3 $ palette1 8, Just [0.01]),
       (set opac' 0.3 $ palette1 5, Just [0.04, 0.01])]))] &
  set (#svgOptions % #chartAspect) (CanvasAspect 1.5)

-- | convert from [a] to [Point a], by adding the index as the x axis
--
xify :: NonEmpty Double -> NonEmpty (Point Double)
xify ys =
  NonEmpty.zipWith Point [0 ..] ys

-- | convert from [a] to [Point a], by adding the index as the y axis
yify :: NonEmpty Double -> NonEmpty (Point Double)
yify xs =
  NonEmpty.zipWith Point xs [0 ..]

-- | add a horizontal line at y
addLineX :: Double -> LineStyle -> [Chart Double] -> [Chart Double]
addLineX y ls' cs = cs <> [l]
  where
    l = LineChart ls' [[Point lx y, Point ux y]]
    (Rect lx ux _ _) = styleBoxes cs

-- | add a verticle line at x
addLineY :: Double -> LineStyle -> [Chart Double] -> [Chart Double]
addLineY x ls' cs = cs <> [zeroLine]
  where
    zeroLine = LineChart ls' [[Point x ly, Point x uy]]
    (Rect _ _ ly uy) = styleBoxes cs

-- | Legend template for a line chart.
lineLegend :: Double -> [Text] -> [(Colour, Maybe [Double])] -> LegendOptions
lineLegend w rs cs =
  defaultLegendOptions
      & #vgap .~ 0.4
      & #ltext % #size .~ 0.1
      & #lplace .~ PlaceRight
      & #legendFrame .~ Just (RectStyle 0.02 (palette1 9) (set opac' 0.05 $ palette1 4))
      & #content .~
      zipWith (\a r -> (r,LineChart a [[zero]])) ((\c -> defaultLineStyle & #color .~ fst c & #size .~ w & #dasharray .~ snd c) <$> cs) rs

-- | common pattern of chart title, x-axis title and y-axis title
titlesHud :: Text -> Text -> Text -> HudOptions
titlesHud t x y =
  mempty & #titles .~
  (fmap (10,)
    [ defaultTitle t & #style % #size .~ 0.08,
      defaultTitle x & #place .~ PlaceBottom & #style % #size .~ 0.06,
      defaultTitle y & #place .~ PlaceLeft & #style % #size .~ 0.06
    ])

-- | /blendMidLineStyle n w/ produces n lines of size w interpolated between two colors.
blendMidLineStyles :: Int -> Double -> (Colour, Colour) -> [LineStyle]
blendMidLineStyles l w (c1, c2) = lo
  where
    m = (fromIntegral l - 1) / 2 :: Double
    cs = (\x -> 1 - abs (fromIntegral x - m) / m) <$> [0 .. (l - 1)]
    bs = (\x -> blend x c1 c2) <$> cs
    lo = (\c -> defaultLineStyle & #size .~ w & #color .~ c) <$> bs

-- | blend example
--
-- Interpolation of colours and points.
--
-- ![blend example](other/blend.svg)
blendExample :: ChartSvg
blendExample = mempty & #hudOptions .~ defaultHudOptions & #chartTree .~ blendExampleChart 8 5 100 0.005 (Range 0 1) (Range 0 1) (Range 0 1) (Range 0 0)

blendExampleChart :: Int -> Int -> Int -> Double -> Range Double -> Range Double -> Range Double -> Range Double -> [Chart Double]
blendExampleChart cl c2 n s gx' gy' hx' hy' = l
  where
    gx = grid OuterPos gx' n
    gy = grid OuterPos gy' n
    h0 = reverse $ grid OuterPos hx' n
    h1 = reverse $ grid OuterPos hy' n
    g = zipWith Point gx gy
    h = zipWith Point h0 h1
    gh = zipWith (\p p' -> fromList [fromList [p,p']]) g h
    c = blendMidLineStyles n s (palette1 cl, palette1 c2)
    l = zipWith LineChart c gh

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
    ("other/date.svg",dateExample),
    ("other/subchart.svg",subChartExample),
    ("other/blend.svg",blendExample)
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
       #hudOptions %~ colourHudOptions (rgb light) &
       #svgOptions % #cssOptions % #preferColorScheme .~ PreferDark
    ) <$>
    pathChartSvg light
  putStrLn "dark version, ok"

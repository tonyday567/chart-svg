{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Examples of chart construction.
module Chart.ExamplesNew
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
    writeAllExamples,
    writeAllExamplesDark,
  )
where

import Chart.Chart
import Chart.Svg
import Chart.Style
import Chart.Hud
import NumHask.Space
import Data.Colour
import Chart.Bar
import Control.Lens
import Data.Text (Text, pack)
import qualified Data.Text as Text
import GHC.OverloadedLabels
import NumHask.Prelude hiding (lines)
import Data.FormatN
import Data.Path
import Chart.Surface
import Data.List.NonEmpty (NonEmpty)

-- $setup
-- >>> import Chart
-- >>> import Control.Lens
--

-- | unit example
--
-- ![unit example](other/unit.svg)
unitExample :: ChartSvg
unitExample = mempty & #chartTree .~ [RectChart defaultRectStyle (fromList [one])]

-- | 'HudOptions' example
--
-- ![hudoptions example](other/hudoptions.svg)
hudOptionsExample :: ChartSvg
hudOptionsExample =
  mempty
    & #hudOptions .~ colourHudOptions dark defaultHudOptions
    & #chartTree .~ [BlankChart (fromList [one])]
    & #svgOptions . #cssOptions . #preferColorScheme .~ PreferLight

-- | 'HudOptions' PreferDark example
--
-- ![hudoptions dark example](other/hudoptionsdark.svg)
hudOptionsDarkExample :: ChartSvg
hudOptionsDarkExample =
  mempty
    & #hudOptions .~ colourHudOptions light defaultHudOptions
    & #chartTree .~ [BlankChart (fromList [one])]
    & #svgOptions . #cssOptions . #preferColorScheme .~ PreferDark
    & #svgOptions . #background .~ Just dark

-- | 'SvgOptions' example.
--
-- ![svgoptions example](other/svgoptions.svg)
svgOptionsExample :: ChartSvg
svgOptionsExample =
  mempty
    & #svgOptions %~ #chartAspect .~ FixedAspect 0.7
    & #chartTree .~ zipWith LineChart lopts ls

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
rss = fmap fromList
  [ gridR (\x -> exp (-(x ** 2) / 2)) (Range -5 5) 50,
    gridR (\x -> 0.5 * exp (-(x ** 2) / 8)) (Range -5 5) 50
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
    .~ zipWith LineChart lopts ls

ls :: [NonEmpty (Point Double)]
ls =
  fromList . fmap (uncurry Point)
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
  mempty & #chartTree
    .~ [TextChart
       (defaultTextStyle & (#color .~ textColour) & (#size .~ (0.05 :: Double)))
       (fromList ts)]
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
            (fromList [p])
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
      [Point x 0 | x <- [0 .. (8 :: Double)]]

-- | Example data for Bar chart
barDataExample :: BarData
barDataExample =
  BarData
    [[1, 2, 3, 5, 8, 0, -2, 11, 2, 1], [1 .. 10]]
    (Just (("row " <>) . pack . show <$> ([1 .. 11] :: [Int])))
    (Just (("column " <>) . pack . show <$> ([1 .. 2] :: [Int])))

-- | Bar chart example.
--
-- ![bar example](other/bar.svg)
barExample :: ChartSvg
barExample = barChart defaultBarOptions barDataExample

-- | A reminder that Text scale is at representation level, and so doesn't scale compared with other chart elements, such as a rectangle.
--
-- ![text local example](other/textlocal.svg)
textLocalExample :: ChartSvg
textLocalExample =
  mempty & #chartTree
    .~ [ t1,
         t2,
         RectChart rs (fromList [padBox $ sbox_ t1]),
         RectChart rs (fromList [padBox $ sbox_ t2])
       ]
  where
    rs = defaultRectStyle & #color %~ setOpac 0.1
    t1 =
      TextChart
        (defaultTextStyle & #anchor .~ AnchorStart & #hsize .~ 0.5 & #size .~ 0.08)
        (fromList [("a pretty long piece of text", zero)])
    t2 =
      TextChart
        (defaultTextStyle & #anchor .~ AnchorStart & #hsize .~ 0.5 & #size .~ 0.08)
        (fromList [("another pretty long piece of text", Point 1 1)])

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
    palette1_
    [EllipseGlyph 1.5, SquareGlyph, CircleGlyph]

-- | Glyph + Lines
--
-- ![glines example](other/glines.svg)
glinesExample :: ChartSvg
glinesExample = mempty & #chartTree .~ (cs <> gs)
  where
    cs = zipWith LineChart lopts ls
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
      ( \(t, p) ->
          TextChart
                ( defaultTextStyle
                    & #color %~ setOpac 0.2
                )
            (fromList [(t,p + Point 0 0.2)]))
        <$> lgdata
    gly =
      ( \d ->
          GlyphChart
                ( defaultGlyphStyle
                    & #size .~ 0.01
                    & #borderSize .~ 0
                    & #color .~ palette1 2
                )
            (fromList [d])
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
    .~ [TextChart (defaultTextStyle & #rotation ?~ -pi / 4) (fromList [("text at (1,1) rotated by -(pi/4) radians", Point 1.0 1.0)])]

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
waveExample = mempty & #chartTree .~ [GlyphChart defaultGlyphStyle (fromList $ gridP sin (Range 0 (2 * pi)) 30)]

-- | venn diagram
--
-- ![venn diagram](other/venn.svg)
vennExample :: ChartSvg
vennExample =
  mempty
    & #chartTree .~ zipWith (\c x -> PathChart (defaultPathStyle & #color .~ setOpac 0.2 c) (fromList x)) palette1_ (toPathXYs . parsePath <$> vennSegs)
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
    ps = fromList
      [ (StartI, Point 0 0),
        (LineI, Point 1 0),
        (CubicI (Point 0.2 0) (Point 0.25 1), Point 1 1),
        (QuadI (Point -1 2), Point 0 1),
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
    (ArcCentroid c r phi' ang0 angd) = arcCentroid p
    ellFull = LineChart (defaultLineStyle & #width .~ 0.002 & #color .~ palette1 1) (fromList $ ellipse c r phi' . (\x -> 2 * pi * x / 100.0) <$> [0 .. 100])
    ell = LineChart (defaultLineStyle & #width .~ 0.002 & #color .~ palette1 1) (fromList $ ellipse c r phi' . (\x -> ang0 + angd * x / 100.0) <$> [0 .. 100])
    c0 = GlyphChart defaultGlyphStyle (fromList [c, p1, p2])
    bbox = RectChart (defaultRectStyle & #borderSize .~ 0.002 & #color .~ Colour 0.4 0.4 0.8 0.1 & #borderColor .~ Colour 0.5 0.5 0.5 1) (fromList [arcBox p])
    xradii = LineChart (defaultLineStyle & #color .~ Colour 0.9 0.2 0.02 1 & #width .~ 0.005 & #dasharray .~ Just [0.03, 0.01] & #linecap .~ Just LineCapRound) (fromList [ellipse c r phi' 0, ellipse c r phi' pi])
    yradii = LineChart (defaultLineStyle & #color .~ Colour 0.9 0.9 0.02 1 & #width .~ 0.005 & #dasharray .~ Just [0.03, 0.01] & #linecap .~ Just LineCapRound) (fromList [ellipse c r phi' (pi / 2), ellipse c r phi' (3 / 2 * pi)])

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
    (ArcCentroid c r phi' ang0 angd) = arcCentroid p1
    arc = PathChart (defaultPathStyle & #color .~ setOpac 0.1 (palette1 2) & #borderColor .~ transparent) (fromList ps)
    ell = LineChart (defaultLineStyle & #width .~ 0.002 & #color .~ palette1 1) (fromList $ ellipse c r phi' . (\x -> ang0 + angd * x / 100.0) <$> [0 .. 100])
    c0 = GlyphChart defaultGlyphStyle (fromList [c])
    bbox = RectChart (defaultRectStyle & #borderSize .~ 0.002 & #color .~ Colour 0.4 0.4 0.8 0.1 & #borderColor .~ Colour 0.5 0.5 0.5 1) (fromList [arcBox p1])

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
          [ [ BlankChart (fromList [Rect -0.4 0.4 -1 5]),
              TextChart (defaultTextStyle & #size .~ 0.6 & #rotation .~ Just (pi / 2)) (fromList [("Sweep", Point 0.1 2)])
            ],
            vert
              0.02
              [ [ BlankChart (fromList [Rect -0.25 0.25 -1 2]),
                  TextChart (defaultTextStyle & #size .~ 0.4 & #rotation .~ Just (pi / 2)) (fromList [("True", Point 0.1 0.5)])
                ],
                [ BlankChart (fromList [Rect -0.25 0.25 -1 2]),
                  TextChart (defaultTextStyle & #size .~ 0.4 & #rotation .~ Just (pi / 2)) (fromList [("False", Point 0.1 0.5)])
                ]
              ],
            vert
              0.02
              [ checkFlags False True (setOpac 0.3 dark) & view #chartTree,
                checkFlags False False (setOpac 0.3 dark) & view #chartTree,
                [ BlankChart (fromList [Rect -1 2 -0.25 0.25]),
                  TextChart (defaultTextStyle & #size .~ 0.4) (fromList [("False", Point 0.5 -0.1)])
                ]
              ],
            vert
              0.02
              [ checkFlags True True (setOpac 0.3 dark) & view #chartTree,
                checkFlags True False (setOpac 0.3 dark) & view #chartTree,
                [ BlankChart (fromList [Rect -1 2 -0.25 0.25]),
                  TextChart (defaultTextStyle & #size .~ 0.4) (fromList [("True", Point 0.5 -0.1)])
                ]
              ]
          ],
        [ BlankChart (fromList [Rect 0 9 -2.75 -3.25]),
          TextChart (defaultTextStyle & #size .~ 0.6) (fromList [("Large", Point 5.5 -3.0)])
        ]
      ]
    & #hudOptions
    .~ mempty
    & #svgOptions %~ ((#outerPad .~ Nothing) . (#chartAspect .~ UnadjustedAspect))

checkFlags :: Bool -> Bool -> Colour -> ChartSvg
checkFlags large sweep co =
  mempty
    & #hudOptions .~ defaultHudOptions
    & #svgOptions . #chartAspect .~ UnadjustedAspect
    & #chartTree .~ [c1, c2, ell, arc1]
  where
    c = Point 1.0 1.0
    p1 = ArcPosition (Point 0.0 1.0) (Point 1.0 0.0) (ArcInfo (Point 1.0 1.0) 0 large sweep)
    ps1 = singletonPie' c p1
    (ArcCentroid c' r phi' ang0 angd) = arcCentroid p1
    arc1 = PathChart (defaultPathStyle & #color .~ co & #borderColor .~ setOpac 0.5 dark) (fromList ps1)
    c1 = LineChart (defaultLineStyle & #width .~ 0.02 & #color .~ setOpac 0.2 dark) (fromList $ ellipse (Point 1.0 1.0) (Point 1.0 1.0) 0 . (\x -> 0 + 2 * pi * x / 100.0) <$> [0 .. 100])
    c2 = LineChart (defaultLineStyle & #width .~ 0.02 & #color .~ setOpac 0.2 dark) (fromList $ ellipse (Point 0.0 0.0) (Point 1.0 1.0) 0 . (\x -> 0 + 2 * pi * x / 100.0) <$> [0 .. 100])
    ell = LineChart (defaultLineStyle & #width .~ 0.05 & #color .~ setOpac 0.5 co) (fromList $ ellipse c' r phi' . (\x -> ang0 + angd * x / 100.0) <$> [0 .. 100])

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
    p@(QuadPosition start end control) = QuadPosition (Point 0 0) (Point 1 1) (Point 2 -1)
    ps = singletonQuad p
    path' = PathChart (defaultPathStyle & #color .~ setOpac 0.1 (palette1 2) & #borderColor .~ transparent) (fromList ps)
    curve = LineChart (defaultLineStyle & #width .~ 0.002 & #color .~ palette1 1) (fromList $ quadBezier p . (/ 100.0) <$> [0 .. 100])
    c0 = GlyphChart defaultGlyphStyle (fromList [start, end, control])
    bbox = RectChart (defaultRectStyle & #borderSize .~ 0.002 & #color .~ Colour 0.4 0.4 0.8 0.1 & #borderColor .~ Colour 0.5 0.5 0.5 1) (fromList [quadBox p])

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
    path' = PathChart (defaultPathStyle & #color .~ setOpac 0.1 (palette1 2) & #borderColor .~ transparent) (fromList ps)
    curve = LineChart (defaultLineStyle & #width .~ 0.002 & #color .~ palette1 1) (fromList $ cubicBezier p . (/ 100.0) <$> [0 .. 100])
    c0 = GlyphChart defaultGlyphStyle (fromList [start, end, control1, control2, cubicBezier p 0.8])
    bbox = RectChart (defaultRectStyle & #borderSize .~ 0.002 & #color .~ Colour 0.4 0.4 0.8 0.1 & #borderColor .~ Colour 0.5 0.5 0.5 1) (fromList [cubicBox p])

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
    f = fst . bimap (-1.0 *) (-1.0 .*) . rosenbrock 1 10
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
    & #chartTree .~ ((\p -> chart (tail . f $ p) (angle . f $ p) p) <$> ps)
    & #svgOptions .~ defaultSvgOptions
  where
    f = snd . bimap (-1.0 *) (-1.0 .*) . rosenbrock 1 10
    ps = grid MidPos (one :: Rect Double) (Point 10 10 :: Point Int) :: [Point Double]
    arrow = PathGlyph "M -1 0 L 1 0 M 1 0 L 0.4 0.3 M 1 0 L 0.4 -0.3"
    gs s r' =
      defaultGlyphStyle
        & #borderSize .~ 0.05
        & #size .~ s
        & #borderColor .~ arrowColour
        & #rotation .~ Just r'
        & #shape .~ arrow
    chart s r' p = GlyphChart (gs s r') (fromList [p])

    tail :: Point Double -> Double
    tail = max 0.05 . min 0.02 . (* 0.01) . (/ avmag) . norm

    avmag = sum (norm . f <$> ps) / fromIntegral (length ps)

-- | function for testing
--
-- > f(x,y) = (a-x)^2 + b * (y - x^2)^2
-- >        = a^2 - 2ax + x^2 + b * y^2 - b * 2 * y * x^2 + b * x ^ 4
-- > f'x = -2a + 2 * x - b * 4 * y * x + 4 * b * x ^ 3
-- > f'y = 2 * b * y - 2 * b * x^2
-- > f a b (Point x y) = (a^2 - 2ax + x^2 + b * y^2 - b * 2 * y * x^2 + b * x^4, Point (-2a + 2 * x - b * 4 * y * x + 4 * b * x ^ 3), 2 * b * y - 2 * b * x^2)
rosenbrock :: Double -> Double -> Point Double -> (Double, Point Double)
rosenbrock a b (Point x y) = (a ^ 2 - 2 * a * x + x ^ 2 + b * y ^ 2 - b * 2 * y * x ^ 2 + b * x ^ 4, Point (-2 * a + 2 * x - b * 4 * y * x + 4 * b * x ^ 3) (2 * b * y - 2 * b * x ^ 2))


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
    ("other/arrow.svg",arrowExample c)
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

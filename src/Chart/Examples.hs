{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Examples of chart construction.
module Chart.Examples
  ( -- * Unit & Hud
    unitExample,
    hudOptionsExample,

    -- * Iconic primitives.
    lineExample,
    rectExample,
    textExample,
    glyphsExample,
    pathExample,

    -- * Compounds
    barExample,
    barDataExample,
    sbarExample,
    waveExample,
    surfaceExample,
    rosenbrock,
    arcFlagsExample,
    ellipseExample,
    quadExample,
    cubicExample,
    vennExample,
    arrowExample,
    dateExample,

    -- * Colour
    gradientExample,
    wheelExample,

    -- * Debugging
    debugExample,

    -- * Compound Charts
    compoundExample,
    stackExample,

    -- * Priority
    priorityv1Example,
    priorityv2Example,

    -- * Writing to file
    pathChartOptions,
    writeAllExamples,
    writeAllExamplesDark,
  )
where

import Chart
import Data.Bifunctor
import Data.Bool
import Data.ByteString (ByteString)
import Data.Function
import Data.Maybe
import Data.String.Interpolate
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time
import Optics.Core
import Prelude hiding (abs)

-- | unit example
--
-- ![unit example](other/unit.svg)
unitExample :: ChartOptions
unitExample = mempty & #chartTree .~ named "unit" [Chart defaultRectStyle (RectData [one])] & #hudOptions .~ defaultHudOptions

-- | A 'BlankChart', 'defaultHudOptions' example.
--
-- ![hudoptions example](other/hudoptions.svg)
hudOptionsExample :: ChartOptions
hudOptionsExample =
  mempty
    & #hudOptions .~ defaultHudOptions
    & #chartTree .~ blank one

-- | rect example
--
-- ![rect example](other/rect.svg)
rectExample :: ChartOptions
rectExample =
  mempty
    & #hudOptions
      .~ ( mempty
             & set
               #axes
               [Priority 5 (defaultXAxisOptions & #ticks % #lineTick .~ Nothing)]
         )
    & #chartTree .~ named "rect" (zipWith (\s x -> Chart s (RectData x)) ropts rss)

rss :: [[Rect Double]]
rss =
  [ gridR (\x -> exp (-x ** 2 / 2)) (Range (-5) 5) 50,
    gridR (\x -> 0.5 * exp (-x ** 2 / 8)) (Range (-5) 5) 50
  ]

ropts :: [Style]
ropts =
  [ blob (paletteO 1 0.4),
    blob (paletteO 2 0.4)
  ]

-- | line example
--
-- ![line example](other/line.svg)
lineExample :: ChartOptions
lineExample =
  mempty & set #hudOptions ho & #chartTree .~ named "line" cs
  where
    ho =
      defaultHudOptions
        & set
          #titles
          [ Priority 6 (defaultTitle "Line Chart" & set (#style % #size) 0.08),
            Priority 13 $
              defaultTitle "Made with 🧡 and chart-svg"
                & set (#style % #size) 0.04
                & set #place PlaceBottom
                & set #anchor AnchorEnd
          ]
        & set
          #legends
          [ Priority 12 $
              defaultLegendOptions
                & set #scaleP ScalePX
                & set #place (PlaceAbsolute (Point 0.35 (-0.35)))
                & set #legendCharts (zipWith (\t c -> (t, [c])) ["palette #0", "palette #1", "palette #2"] cs)
          ]
    cs =
      zipWith
        ( \c l ->
            LineChart
              ( defaultLineStyle
                  & set #color (palette c)
                  & set #size 0.015
              )
              [l]
        )
        [0 ..]
        ls
    ls =
      [ [Point 0.0 1.0, Point 1.0 1.0, Point 2.0 5.0],
        [Point 0.0 0.0, Point 2.8 3.0],
        [Point 0.5 4.0, Point 0.5 0]
      ]

priorityv1Example :: ChartOptions
priorityv1Example =
  lineExample &
  set (#hudOptions % #frames)
    [Priority 1 (FrameOptions (Just defaultRectStyle) CanvasStyleSection 0),
     Priority 100 (FrameOptions (Just (defaultRectStyle & #color .~ (palette 4 & opac' .~ 0.05) & #borderColor .~ palette 4)) HudStyleSection 0.1)] &
   set (#hudOptions % #legends % each % #priority) 50 &
   set (#hudOptions % #legends % each % #item % #place) PlaceRight

priorityv2Example :: ChartOptions
priorityv2Example =
  priorityv1Example &
  set (#hudOptions % #titles % each % #priority) 51

-- | text example
--
-- ![text example](other/text.svg)
textExample :: ChartOptions
textExample =
  mempty
    & #chartTree
      .~ named
        "text"
        [ TextChart
            (defaultTextStyle & #color .~ dark & #size .~ 0.1)
            ts
        ]
    & #hudOptions .~ defaultHudOptions
    & #markupOptions % #cssOptions % #preferColorScheme .~ PreferHud
    & #markupOptions % #cssOptions % #cssExtra .~ fillSwitch (dark, light) "dark" "text"
  where
    ts :: [(Text, Point Double)]
    ts =
      zip
        (fmap Text.singleton ['a' .. 'z'])
        ((\x -> Point (sin (x * 0.1)) x) <$> [0 .. 25])

-- | glyphs example
--
-- ![glyphs example](other/glyphs.svg)
glyphsExample :: ChartOptions
glyphsExample =
  mempty
    & set (#markupOptions % #markupHeight) (Just 50)
    & set (#markupOptions % #chartAspect) (FixedAspect 12)
    & set
      #chartTree
      ( named "glyphs" $
          zipWith
            ( \(sh, bs) p ->
                GlyphChart
                  ( defaultGlyphStyle
                      & #shape .~ sh
                      & #size .~ (0.8 :: Double)
                      & #borderSize .~ bs
                  )
                  [p]
            )
            [ (CircleGlyph, 0.02 :: Double),
              (SquareGlyph, 0.02),
              (RectSharpGlyph 0.75, 0.02),
              (RectRoundedGlyph 0.75 0.01 0.01, 0.02),
              (EllipseGlyph 0.75, 0.02),
              (VLineGlyph, 0.02),
              (HLineGlyph, 0.02),
              (TriangleGlyph (Point 0.0 (0.5 * sqrt 2)) (Point (-cos (pi / 3)) (-sin (pi / 3) / 2)) (Point (cos (pi / 3)) (-sin (pi / 3) / 2)), 0.02),
              (PathGlyph "M 0.5,-0.3660 A 1.0 1.0 -0.0 0 1 0,0.5 A 1.0 1.0 -0.0 0 1 -0.5,-0.3660 A 1.0 1.0 -0.0 0 1 0.5,-0.3660 L 0.5,-0.3660 Z", 0.02)
            ]
            (fmap (\x -> Point x 0) [0 ..])
      )

-- | Example data for Bar chart
barDataExample :: BarData
barDataExample =
  BarData
    [[1, 2, 3, 5, 8, 0, -2, 11, 2, 1], [1 .. 10]]
    (("row " <>) . Text.pack . show <$> [1 .. 11 :: Int])
    (("column " <>) . Text.pack . show <$> [1 .. 2 :: Int])

-- | Bar chart example.
--
-- ![bar example](other/bar.svg)
barExample :: ChartOptions
barExample = barChart defaultBarOptions barDataExample &
  set (#hudOptions % #frames) [Priority 101 (defaultFrameOptions & set #buffer 0.02)]

-- | Stacked bar chart example.
--
-- ![sbar example](other/sbar.svg)
sbarExample :: ChartOptions
sbarExample = barChart (defaultBarOptions & set #barOrientation Vert & set #barStacked Stacked & #displayValues .~ False & #barRectStyles %~ fmap (#borderSize .~ 0)) barDataExample

-- | wave example
--
-- ![wave example](other/wave.svg)
waveExample :: ChartOptions
waveExample = mempty & #chartTree .~ named "wave" [GlyphChart (defaultGlyphStyle & set #shape SquareGlyph) (gridP sin (Range 0 (2 * pi)) 30)] & #hudOptions .~ defaultHudOptions

-- | venn diagram
--
-- ![venn diagram](other/venn.svg)
vennExample :: ChartOptions
vennExample =
  mempty
    & #chartTree .~ named "venn" (zipWith (\c x -> PathChart (defaultPathStyle & #borderSize .~ 0.005 & #color .~ paletteO c 0.2 & over #borderColor (set opac' 1)) x) [0 ..] (svgToPathData <$> vennSegs))
    & #hudOptions .~ defaultHudOptions
    & #markupOptions % #chartAspect .~ FixedAspect 1

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
vennSegs :: [ByteString]
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
pathExample :: ChartOptions
pathExample =
  mempty
    & #chartTree .~ named "path" [path', c0] <> named "pathtext" [t0]
    & #hudOptions .~ defaultHudOptions
    & set (#hudOptions % #axes % each % #item % #ticks % #glyphTick %? #anchorTo) CanvasStyleSection
    & set (#hudOptions % #axes % each % #item % #bar %? #anchorTo) CanvasStyleSection
    & #markupOptions % #chartAspect .~ ChartAspect
    & #markupOptions % #cssOptions % #preferColorScheme .~ PreferHud
    & #markupOptions % #cssOptions % #cssExtra .~ fillSwitch (dark, light) "dark" "pathtext"
  where
    ps =
      [ StartP (Point 0 0),
        LineP (Point 1 0),
        CubicP (Point 0.2 0) (Point 0.25 1) (Point 1 1),
        QuadP (Point (-1) 2) (Point 0 1),
        ArcP (ArcInfo (Point 1 1) (-pi / 6) False False) (Point 0 0)
      ]
    ts =
      [ "StartP (Point 0 0)",
        "LineP (Point 1 0)",
        "CubicP (Point 0.2 0) (Point 0.25 1) (Point 1 1)",
        "QuadP (Point (-1) 2) (Point 0 1)",
        "ArcP (ArcInfo (Point 1 1) (-pi / 6) False False) (Point 0 0)"
      ]
    path' = PathChart (defaultPathStyle & #color .~ paletteO 0 0.1 & #borderColor .~ paletteO 1 1) ps
    c0 = GlyphChart (defaultGlyphStyle & set #shape SquareGlyph) (pointPath <$> ps)
    midp = Point 0 0 : zipWith (\(Point x y) (Point x' y') -> Point ((x + x') / 2) ((y + y') / 2)) (drop 1 (pointPath <$> ps)) (pointPath <$> ps)
    offp = [Point (-0.35) 0.05, Point 0 0.05, Point (-0.2) 0, Point (-0.1) 0.1, Point 0 (-0.1)]
    t0 = TextChart (defaultTextStyle & set #size 0.025) (zip ts (zipWith addp offp midp))

-- | ellipse example
--
-- Under scaling, angles are not invariant, and this effects the shape of ellipses and thus SVG arc paths. Compare the effect of aspect changes to the axes of this ellipse:
--
-- ![ellipse example](other/ellipse.svg)
--
-- Below is the same ellipse with FixedAspect 2. Points scale exactly, but the original points that represent the end points of the axes are no longer on the new axes of the ellipse.
--
-- ![ellipse2 example](other/ellipse2.svg)
ellipseExample :: ChartAspect -> ChartOptions
ellipseExample a =
  mempty
    & #chartTree .~ named "ellipse" [ell, ellFull, c0, c1, bbox, xradii, yradii]
    & #hudOptions .~ defaultHudOptions
    & #markupOptions % #chartAspect .~ a
    & #hudOptions % #legends .~ [Priority 10 (defaultLegendOptions & #legendCharts .~ lrows & #textStyle % #size .~ 0.2 & #size .~ 0.1 & #vgap .~ 0.3)]
    & #hudOptions % #titles .~ [Priority 11 (defaultTitle "ArcPosition (Point 1 0) (Point 0 1) (ArcInfo (Point 1.5 1) (pi / 3) True True)" & #style % #size .~ 0.032)]
    & #hudOptions % #axes % ix 1 % #item % #ticks % #textTick %? #buffer .~ 0.04
    & #hudOptions % #axes % ix 1 % #item % #ticks % #glyphTick %? #buffer .~ 0.01
  where
    p@(ArcPosition p1 p2 _) = ArcPosition (Point 1 0) (Point 0 1) (ArcInfo (Point 1.5 1) (pi / 3) True True)
    (ArcCentroid c r phi' ang0' angd) = arcCentroid p
    ellFull = LineChart fullels [ellipse c r phi' . (\x -> 2 * pi * x / 100.0) <$> [0 .. 100]]
    ell = LineChart els [ellipse c r phi' . (\x -> ang0' + angd * x / 100.0) <$> [0 .. 100]]
    g0 = defaultGlyphStyle & set #shape CircleGlyph
    c0 = GlyphChart g0 [c]
    g1 = defaultGlyphStyle & #color .~ paletteO 4 0.2 & set #shape CircleGlyph
    c1 = GlyphChart g1 [p1, p2]
    bbox = RectChart bbs [arcBox p]
    bbs = defaultRectStyle & #borderSize .~ 0.002 & #color .~ paletteO 7 0.005 & #borderColor .~ grey 0.5 1
    xradii = LineChart xals [[ellipse c r phi' 0, ellipse c r phi' pi]]
    yradii = LineChart yals [[ellipse c r phi' (pi / 2), ellipse c r phi' (3 / 2 * pi)]]
    xals = defaultLineStyle & #color .~ palette 6 & #size .~ 0.005 & #dasharray .~ Just [0.03, 0.01] & #linecap .~ Just LineCapRound
    yals = defaultLineStyle & #color .~ palette 5 & #size .~ 0.005 & #dasharray .~ Just [0.03, 0.01] & #linecap .~ Just LineCapRound
    fullels = defaultLineStyle & #size .~ 0.002 & #color .~ palette 1
    els = defaultLineStyle & #size .~ 0.005 & #color .~ palette 2
    lrows =
      second (: [])
        <$> [ ("Major Axis", LineChart xals [[zero]]),
              ("Minor Axis", LineChart yals [[zero]]),
              ("Full Ellipse", LineChart fullels [[zero]]),
              ("Arc", LineChart els [[zero]]),
              ("Centroid", GlyphChart (g0 & #size .~ 0.01 & set #shape SquareGlyph) [zero]),
              ("Endpoints", GlyphChart (g1 & #size .~ 0.01 & set #shape SquareGlyph) [zero]),
              ("Bounding Box", RectChart (bbs & #borderSize .~ 0.01) [fmap (2 *) one])
            ]

-- | Reproduction of the flag explanation chart in <https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths>
--
-- ![arc flags example](other/arcflags.svg)
arcFlagsExample :: ChartOptions
arcFlagsExample =
  mempty
    & set
      #chartTree
      ( vert
          0.02
          [ hori 0.02 [colSweep, colSweep2, colLargeFalse, colLargeTrue],
            rowLarge
          ]
      )
    & #markupOptions % #chartAspect .~ UnscaledAspect
    & #markupOptions % #cssOptions % #preferColorScheme .~ PreferHud
    & #markupOptions % #cssOptions % #cssExtra
      .~ [i|
{
  .chart g {
    stroke: #{showRGBA dark};
  }
  .chart g text {
    fill: #{showRGBA dark};
  }
}
@media (prefers-color-scheme:dark) {
  .chart g {
    stroke: #{showRGBA light};
  }
  .chart g text {
    fill: #{showRGBA light};
  }
}
|]
  where
    rowLarge =
      unnamed
        [ blankChart1 (Rect 0 9 (-2.75) (-3.25)),
          TextChart (defaultTextStyle & #size .~ 0.6) [("Large", Point 5.5 (-3.0))]
        ]
    colLargeFalse =
      vert
        0.02
        [ unnamed (checkFlags False True (set opac' 0.3 dark)),
          unnamed (checkFlags False False (set opac' 0.3 dark)),
          unnamed
            [ blankChart1 (Rect (-1) 2 (-0.25) 0.25),
              TextChart (defaultTextStyle & #size .~ 0.4) [("False", Point 0.5 (-0.1))]
            ]
        ]
    colLargeTrue =
      vert
        0.02
        [ unnamed (checkFlags True True (set opac' 0.3 dark)),
          unnamed (checkFlags True False (set opac' 0.3 dark)),
          unnamed
            [ blankChart1 (Rect (-1) 2 (-0.25) 0.25),
              TextChart (defaultTextStyle & #size .~ 0.4) [("True", Point 0.5 (-0.1))]
            ]
        ]
    colSweep =
      unnamed
        [ blankChart1 (Rect (-0.4) 0.4 (-1) 5),
          TextChart
            (defaultTextStyle & #size .~ 0.6 & #rotation .~ Just (pi / 2))
            [("Sweep", Point 0.1 2)]
        ]
    colSweep2 =
      vert
        0.02
        [ unnamed
            [ blankChart1 (Rect (-0.25) 0.25 (-1) 2),
              TextChart
                (defaultTextStyle & #size .~ 0.4 & #rotation .~ Just (pi / 2))
                [("True", Point 0.1 0.5)]
            ],
          unnamed
            [ blankChart1 (Rect (-0.25) 0.25 (-1) 2),
              TextChart
                (defaultTextStyle & #size .~ 0.4 & #rotation .~ Just (pi / 2))
                [("False", Point 0.1 0.5)]
            ]
        ]

checkFlags :: Bool -> Bool -> Colour -> [Chart]
checkFlags large' sweep co = [c1, c2, ell, arc1]
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
quadExample :: ChartOptions
quadExample =
  mempty
    & #chartTree .~ named "quad" [path', curve, c0, c1, bbox]
    & #hudOptions .~ defaultHudOptions
    & #markupOptions % #chartAspect .~ FixedAspect 1.5
    & #hudOptions % #legends .~ [Priority 10 (defaultLegendOptions & #legendCharts .~ lrows & #textStyle % #size .~ 0.2 & #size .~ 0.2 & #vgap .~ 0.3)]
    & #hudOptions % #titles .~ [Priority 11 (defaultTitle "QuadPosition (Point 0 0) (Point 1 1) (Point 2 (-1))" & #style % #size .~ 0.03)]
    & #hudOptions % #axes % ix 1 % #item % #ticks % #textTick %? #buffer .~ 0.04
    & #hudOptions % #axes % ix 1 % #item % #ticks % #glyphTick %? #buffer .~ 0.01
  where
    p@(QuadPosition start end control) = QuadPosition (Point 0 0) (Point 1 1) (Point 2 (-1))
    ps = singletonQuad p
    path' = PathChart pathStyle ps
    curve = LineChart curveStyle [quadBezier p . (/ 100.0) <$> [0 .. 100]]
    curveStyle = defaultLineStyle & #size .~ 0.002 & #color .~ palette 1
    c0 = GlyphChart (defaultGlyphStyle & set #shape SquareGlyph) [start, end]
    c1 = GlyphChart (controlStyle & set #shape CircleGlyph) [control]
    bbox = RectChart bbs [quadBox p]
    bbs = defaultRectStyle & #borderSize .~ 0.002 & #color .~ paletteO 0 0.05 & #borderColor .~ grey 0.4 1
    pathStyle = defaultPathStyle & #color .~ paletteO 2 0.2 & #borderColor .~ transparent
    controlStyle = defaultGlyphStyle & #shape .~ CircleGlyph
    lrows =
      second (: [])
        <$> [ ("Path Fill", PathChart pathStyle [StartP zero]),
              ("Path Chord", LineChart curveStyle [[zero]]),
              ("Path Endpoints", GlyphChart (defaultGlyphStyle & set #shape SquareGlyph) [zero]),
              ("Path Control Point", GlyphChart (controlStyle & set #shape CircleGlyph) [zero]),
              ("Bounding Box", RectChart (bbs & #borderSize .~ 0.01) [one])
            ]

-- | cubic example
--
-- ![cubic example](other/cubic.svg)
cubicExample :: ChartOptions
cubicExample =
  mempty
    & #chartTree .~ named "cubic" [path', curve, c0, c1, bbox]
    & #hudOptions .~ mempty
    & #markupOptions % #chartAspect .~ FixedAspect 1.5
    & #hudOptions % #legends .~ [Priority 10 (defaultLegendOptions & #legendCharts .~ lrows & #textStyle % #size .~ 0.2 & #size .~ 0.2 & #vgap .~ 0.3)]
    & #hudOptions % #titles .~ [Priority 11 (defaultTitle "CubicPosition (Point 0 0) (Point 1 1) (Point 1 0) (Point 0 1)" & #style % #size .~ 0.03)]
  where
    p@(CubicPosition start end control1 control2) = CubicPosition (Point 0 0) (Point 1 1) (Point 1 0) (Point 0 1)
    ps = singletonCubic p
    path' = PathChart pathStyle ps
    curve = LineChart curveStyle [cubicBezier p . (/ 100.0) <$> [0 .. 100]]
    c0 = GlyphChart (defaultGlyphStyle & set #shape SquareGlyph) [start, end]
    c1 = GlyphChart (controlStyle & set #shape CircleGlyph) [control1, control2]
    bbox = RectChart bbs [cubicBox p]
    bbs = defaultRectStyle & #borderSize .~ 0.002 & #color .~ paletteO 0 0.05 & #borderColor .~ grey 0.4 1
    pathStyle = defaultPathStyle & #color .~ paletteO 3 0.2 & #borderColor .~ transparent
    controlStyle = defaultGlyphStyle
    curveStyle = defaultLineStyle & #size .~ 0.002 & #color .~ palette 7
    lrows =
      second (: [])
        <$> [ ("Path Fill", PathChart pathStyle [StartP zero]),
              ("Path Chord", LineChart curveStyle [[zero]]),
              ("Path Endpoints", GlyphChart (defaultGlyphStyle & set #shape SquareGlyph) [zero]),
              ("Path Control Point", GlyphChart (controlStyle & set #shape CircleGlyph) [zero]),
              ("Bounding Box", RectChart (bbs & #borderSize .~ 0.01) [one])
            ]

-- | The common way to create a surface chart (or contour chart or heat map) is usually a grid over a function, a process reified in 'surfacef'.
--
-- This is also an example of 'mix' and 'mixes'. In this example, colors with the same lightness have been chosen in the gradient and the result should appear a fairly uniform lightness across the surface.
--
-- ![surface example](other/surface.svg)
surfaceExample :: ChartOptions
surfaceExample = mempty & set #chartTree cs' & set #markupOptions (defaultMarkupOptions & set (#cssOptions % #shapeRendering) UseCssCrisp)
  where
    grain = Point 20 20
    r = one
    f = fst . bimap ((-1.0) *) (fmap ((-1.0) *)) . rosenbrock 1 10
    evenColors = trimColour . over lightness' (const 0.55) . palette <$> [0 .. 5]
    so = defaultSurfaceOptions & #soGrain .~ grain & #soRange .~ r & #soStyle % #surfaceColors .~ evenColors
    (cs, rangef) = surfacef f so
    slo = defaultSurfaceLegendOptions & set (#sloSurfaceStyle % #surfaceColors) evenColors & set #sloDataRange rangef
    cs' = addSurfaceLegend slo (unnamed cs)

-- | arrow example
--
-- Which happens to be the gradient of the surface example.
--
-- ![arrow example](other/arrow.svg)
arrowExample :: ChartOptions
arrowExample =
  mempty
    & #hudOptions .~ (defaultHudOptions & set (#axes % each % #item % #ticks % #lineTick) Nothing)
    & #chartTree .~ named "arrow" ((\p -> gchart (tail' . f $ p) (angle . f $ p) p) <$> ps)
    & #markupOptions % #cssOptions % #preferColorScheme .~ PreferHud
    & #markupOptions % #cssOptions % #cssExtra
      .~ [i|
{
  .arrow g {
    fill: #{showRGBA dark};
    stroke: #{showRGBA dark};
  }
}
@media (prefers-color-scheme:dark) {
  .arrow g {
    fill: #{showRGBA light};
    stroke: #{showRGBA light};
  }
}
|]
  where
    f = snd . bimap ((-1.0) *) (fmap ((-1.0) *)) . rosenbrock 1 10
    ps = grid MidPos (one :: Rect Double) (Point 10 10 :: Point Int) :: [Point Double]
    arrow = PathGlyph "M -1 0 L 1 0 M 1 0 L 0.4 0.3 M 1 0 L 0.4 -0.3"
    gs s r' =
      defaultGlyphStyle
        & #borderSize .~ 0.05
        & #size .~ s
        & #borderColor .~ dark
        & #rotation .~ Just r'
        & #shape .~ arrow
    gchart s r' p = GlyphChart (gs s r') [p]

    tail' :: Point Double -> Double
    tail' = max 0.05 . min 0.02 . (* 0.01) . (/ avmag) . magnitude

    avmag = sum (magnitude . f <$> ps) / fromIntegral (length ps)

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
-- A hud that has date as the x-axis, and time as the y-axis. See 'placedTimeLabelContinuous'.
--
-- ![date example](other/date.svg)
dateExample :: ChartOptions
dateExample =
  mempty
    & #chartTree .~ blank (Rect 0 1 0 1)
    & #markupOptions % #chartAspect .~ FixedAspect 1.5
    & over (#hudOptions % #frames) (<> [Priority 100 (defaultFrameOptions & set #buffer 0.05)])
    & set (#hudOptions % #axes) [Priority 5 (defaultYAxisOptions & #ticks % #tick .~ TickPlaced tsTime), Priority 6 (defaultXAxisOptions & #ticks % #tick .~ TickPlaced tsDate)]
  where
    tsTime = placedTimeLabelContinuous PosIncludeBoundaries Nothing 12 (Range (UTCTime (fromGregorian 2021 12 6) (toDiffTime 0)) (UTCTime (fromGregorian 2021 12 7) (toDiffTime 0)))
    tsDate = placedTimeLabelContinuous PosIncludeBoundaries (Just (Text.pack "%d %b")) 2 (Range (UTCTime (fromGregorian 2021 12 6) (toDiffTime 0)) (UTCTime (fromGregorian 2022 3 13) (toDiffTime 0)))

-- | gradient example
--
-- Mixing Colours using the <https://bottosson.github.io/posts/oklab/ oklch> color model.
--
-- ![gradient example](other/gradient.svg)
gradientExample :: ChartOptions
gradientExample = gradient (Just (orig / 360)) 100 6 100 c0 c1
  where
    ok = LCHA 0.5 0.12 127 1
    c0 = ok & lch' % hLCH' .~ 0.001
    c1 = ok & lch' % hLCH' .~ 360
    orig = view (lch' % hLCH') ok

gradientChart_ :: Int -> LCHA -> LCHA -> [Chart]
gradientChart_ grain c0 c1 =
  (\(r, c) -> RectChart (defaultRectStyle & #color .~ c & #borderSize .~ 0) [r])
    . (\x -> (Rect x (x + d) 0 1, view lcha2colour' (mixLCHA x c0 c1)))
    <$> grid LowerPos (Range 0 1) grain
  where
    d = 1 / fromIntegral grain

gradient :: Maybe Double -> Double -> Double -> Int -> LCHA -> LCHA -> ChartOptions
gradient marker h fa grain ok0 ok1 =
  mempty
    & #markupOptions % #markupHeight
      .~ Just h
    & #markupOptions % #cssOptions % #shapeRendering
      .~ UseCssCrisp
    & #markupOptions % #chartAspect .~ FixedAspect fa
    & #hudOptions
      .~ ( mempty
             & #frames .~ [Priority 1 (FrameOptions (Just (border 0.004 white)) CanvasStyleSection 0.1)]
         )
    & #chartTree
      .~ named "gradient" (gradientChart_ grain ok0 ok1) <> strip
  where
    strip = case marker of
      Nothing -> mempty
      Just marker' ->
        named
          "border"
          [borderStrip 0.02 light (Rect (marker' - 0.02) (marker' + 0.02) (-0.1) 1.1)]

borderStrip :: Double -> Colour -> Rect Double -> Chart
borderStrip w c r = RectChart (defaultRectStyle & #color .~ transparent & #borderSize .~ w & #borderColor .~ c) [r]

-- | Color wheel displaying palette choices
--
-- ![wheel example](other/wheel.svg)
wheelExample :: ChartOptions
wheelExample = dotMap 0.01 50 0.5 0.5 (palette <$> [0 .. 7])

-- | The dotMap
--
-- > dotMap 0.01 20 0.8 0.3
dotMap :: Double -> Int -> Double -> Double -> [Colour] -> ChartOptions
dotMap s grain l maxchroma cs =
  mempty
    & #hudOptions
      .~ defaultHudOptions
    & #chartTree
      .~ named "dots" (dot_ <$> cs)
        <> named
          "wheel"
          ( ( \(p, c) ->
                GlyphChart
                  ( defaultGlyphStyle
                      & #size .~ s
                      & #color .~ c
                      & #borderSize .~ 0
                      & #shape .~ CircleGlyph
                  )
                  [p]
            )
              <$> filter (validColour . snd) (wheelPoints grain l maxchroma)
          )

dot_ :: Colour -> Chart
dot_ x = (\(p, c) -> GlyphChart (defaultGlyphStyle & #size .~ 0.08 & #color .~ c & #borderColor .~ Colour 0.5 0.5 0.5 1 & #shape .~ CircleGlyph) [p]) (colour2Point x, x)
  where
    colour2Point c = review lcha2colour' c & (\(LCHA _ ch h _) -> uncurry Point (review xy2ch' (ch, h)))

wheelPoints :: Int -> Double -> Double -> [(Point Double, Colour)]
wheelPoints grain l maxchroma =
  (\(Point c h) -> (uncurry Point $ view (re xy2ch') (c, h), view lcha2colour' (LCHA l c h 1)))
    <$> grid LowerPos (Rect 0 maxchroma 0 360) (Point grain grain)

-- | Adding reference points and bounding boxes to visualize chart alignment for use in debugging charts.
--
-- ![debug example](other/debug.svg)
debugExample :: ChartOptions -> ChartOptions
debugExample cs =
  mempty
    & set #markupOptions (view #markupOptions cs)
    & set (#markupOptions % #chartAspect) asp
    & set #chartTree (e1 <> e2 <> e3)
  where
    asp = view (#markupOptions % #chartAspect) cs
    e1 = view #chartTree (forgetHud cs)
    e2 = glyphize (defaultGlyphStyle & #size .~ 0.01 & #shape .~ CircleGlyph) e1
    e3 = rectangularize (defaultRectStyle & #borderColor .~ dark & #borderSize .~ 0.001 & #color % opac' .~ 0.05) e1

-- | A merge of two rect charts with different data ranges.
--
-- ![compound example](other/compound.svg)
compoundExample :: ChartOptions
compoundExample = compoundMerge [c1,c2]
  where
    ho1 = (mempty :: HudOptions) & set #titles [Priority 3 (defaultTitle "chart1")] & set #axes [Priority 2 defaultXAxisOptions, Priority 2 defaultYAxisOptions] & colourHudOptions (const (palette 0))
    c1 = (mempty :: ChartOptions) & set #hudOptions ho1 & set #chartTree (named "c1" [Chart defaultRectStyle (RectData [fmap (2*) one])])
    ho2 = (mempty :: HudOptions) & set #titles [Priority 3.1 (defaultTitle "chart2")] & set #axes [Priority 2 (defaultXAxisOptions & set #place PlaceTop), Priority 2 (defaultYAxisOptions & set #place PlaceRight)] & colourHudOptions (const (palette 3))
    c2 = (mempty :: ChartOptions) & set #hudOptions ho2 & set #chartTree (named "c2" [Chart (blob (set opac' 0.3 $ palette 3)) (RectData [fmap (*0.8) one]), BlankChart defaultStyle [one]])

-- | Usage of stack.
--
-- ![stack example](other/stack.svg)
stackExample :: ChartOptions
stackExample = mempty & set #chartTree (stack 5 0.1 (replicate 25 (view #chartTree $ forgetHud lineExample)))

-- | All the examples and the associated filepaths
pathChartOptions :: [(FilePath, ChartOptions)]
pathChartOptions =
  [ ("other/unit.svg", unitExample),
    ("other/rect.svg", rectExample),
    ("other/text.svg", textExample),
    ("other/glyphs.svg", glyphsExample),
    ("other/line.svg", lineExample),
    ("other/hudoptions.svg", hudOptionsExample),
    ("other/bar.svg", barExample),
    ("other/sbar.svg", sbarExample),
    ("other/surface.svg", surfaceExample),
    ("other/wave.svg", waveExample),
    ("other/venn.svg", vennExample),
    ("other/path.svg", pathExample),
    ("other/arcflags.svg", arcFlagsExample),
    ("other/ellipse.svg", ellipseExample (FixedAspect 1.5)),
    ("other/ellipse2.svg", ellipseExample (FixedAspect 2)),
    ("other/quad.svg", quadExample),
    ("other/cubic.svg", cubicExample),
    ("other/arrow.svg", arrowExample),
    ("other/date.svg", dateExample),
    ("other/gradient.svg", gradientExample),
    ("other/wheel.svg", wheelExample),
    ("other/debug.svg", debugExample lineExample),
    ("other/priorityv1.svg", priorityv1Example),
    ("other/priorityv2.svg", priorityv2Example),
    ("other/compound.svg", compoundExample),
    ("other/stack.svg", stackExample)
  ]

-- | Run this to refresh example SVG's.
writeAllExamples :: IO ()
writeAllExamples = do
  mapM_ (uncurry writeChartOptions) pathChartOptions
  putStrLn "ok"

-- | Version of charts with a dark-friendly hud
writeAllExamplesDark :: IO ()
writeAllExamplesDark = do
  mapM_
    ( uncurry writeChartOptions
        . bimap
          ((<> "d.svg") . reverse . drop 4 . reverse)
          ( \x ->
              x
                & #hudOptions %~ colourHudOptions (rgb light)
                & #markupOptions % #cssOptions % #preferColorScheme .~ PreferDark
          )
    )
    pathChartOptions
  putStrLn "dark version, ok"

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Examples of chart construction.
module Chart.Examples
  ( unitExample,
    lineExample,
    hudOptionsExample,
    rectExample,
    textExample,
    glyphsExample,
    barExample,
    waveExample,
    surfaceExample,
    rosenbrock,
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
    mixMidLineStyles,
    mixExample,

    -- * debugging
    debugExample,

    writeAllExamples,
    writeAllExamplesDark,
  )
where

import Chart
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Data.Time
import Data.Bool
import Prelude hiding (abs)
import Data.Bifunctor
import Data.Function
import Optics.Core
import NeatInterpolation

-- | unit example
--
-- ![unit example](other/unit.svg)
unitExample :: ChartSvg
unitExample = mempty & #charts .~ named "unit" [RectChart defaultRectStyle [one]] & #hudOptions .~ defaultHudOptions

-- | 'HudOptions' example
--
-- ![hudoptions example](other/hudoptions.svg)
hudOptionsExample :: ChartSvg
hudOptionsExample =
  mempty
    & #hudOptions .~ colourHudOptions (rgb dark) defaultHudOptions
    & #charts .~ blank one

-- | rect example
--
-- ![rect example](other/rect.svg)
rectExample :: ChartSvg
rectExample =
  mempty &
  #hudOptions .~ (mempty & set #axes
    [ (1, defaultAxisOptions & #ticks % #ltick .~ Nothing)]) &
  #charts .~ named "rect" (zipWith RectChart ropts rss)

rss :: [[Rect Double]]
rss =
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
-- ![line example](other/line.svg)
lineExample :: ChartSvg
lineExample =
  mempty &
  #hudOptions .~
  ( mempty &
    #axes .~
    [(2, defaultAxisOptions & #ticks % #style .~ TickRound (FormatN (FSFixed 1) Nothing True) 8 TickExtend),
     (2, defaultAxisOptions & #place .~ PlaceLeft & #ticks % #style .~ TickRound (FormatN (FSFixed 1) Nothing True) 8 TickExtend)
    ] &
    #titles .~
    [ (6, defaultTitle "Line Chart" & #style % #size .~ 0.08 ),
      (5, defaultTitle "Made with love and chart-svg" &
         #style % #size .~ 0.05 & #place .~ PlaceBottom & #anchor .~ AnchorEnd)
    ] &
    #legends .~
    [ (7, defaultLegendOptions & #content .~ zip ["hockey", "line", "vertical"] cs)
    ]
  )
  & #charts .~ named "line" cs
  where
    cs = zipWith (\s l -> LineChart s [l]) lopts ls

ls :: [[Point Double]]
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
textExample :: ChartSvg
textExample =
  mempty &
    #charts .~ named "text"
      [TextChart
       (defaultTextStyle & (#color .~ dark) & (#size .~ 0.05) & (#vshift .~ 0))
       ts] &
    #hudOptions .~ defaultHudOptions &
    #svgOptions % #cssOptions % #preferColorScheme .~ PreferHud &
    #svgOptions % #cssOptions % #cssExtra .~ textSwitch (light, dark)
  where
    ts :: [(Text, Point Double)]
    ts =
      zip
        (fmap Text.singleton ['a' .. 'z'])
        ((\x -> Point (sin (x * 0.1)) x) <$> [0 .. 25])

    textSwitch :: (Colour, Colour) -> Text
    textSwitch (cl, cd) =
      [trimming|
{
  .text g {
    fill: $hexDark;
  }
}
@media (prefers-color-scheme:dark) {
  .text g {
    fill: $hexLight;
  }
}
|]
        where
          hexLight = hex cl
          hexDark = hex cd

-- | glyphs example
--
-- ![glyphs example](other/glyphs.svg)
glyphsExample :: ChartSvg
glyphsExample =
  mempty &
  set (#svgOptions % #svgHeight) 60 &
  set #charts
    (named "glyphs" $ zipWith
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
        (TriangleGlyph (Point 0.0 (0.5 * sqrt 2)) (Point (-cos (pi/3)) (-sin (pi/3) / 2)) (Point (cos (pi/3)) (-sin (pi/3) / 2)), 0.01),
        (PathGlyph "M 0.5,-0.3660 A 1.0 1.0 -0.0 0 1 0,0.5 A 1.0 1.0 -0.0 0 1 -0.5,-0.3660 A 1.0 1.0 -0.0 0 1 0.5,-0.3660 L 0.5,-0.3660 Z" ScaleBorder, 0.01)
      ]
      [Point x 0 | x <- [0 .. (8 :: Double)]])

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

-- | wave example
--
-- ![wave example](other/wave.svg)
waveExample :: ChartSvg
waveExample = mempty & #charts .~ named "wave" [GlyphChart defaultGlyphStyle $ gridP sin (Range 0 (2 * pi)) 30] & #hudOptions .~ defaultHudOptions

-- | venn diagram
--
-- ![venn diagram](other/venn.svg)
vennExample :: ChartSvg
vennExample =
  mempty
    & #charts .~ named "venn" (zipWith (\c x -> PathChart (defaultPathStyle & #color .~ set opac' 0.2 (palette1 c)) x) [0..] (svgToPathData <$> vennSegs))
    & #hudOptions .~ defaultHudOptions
    & #hudOptions % #chartAspect .~ FixedAspect 1

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
    & #charts .~ named "path" [path', c0, t0]
    & #hudOptions .~ defaultHudOptions
    & #hudOptions % #chartAspect .~ ChartAspect
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
    path' = PathChart (defaultPathStyle & #color .~ set opac' 0.1 (palette1 2) & #borderColor .~ Colour 0.2 0.8 0.4 0.3) ps
    c0 = GlyphChart defaultGlyphStyle (pointPath <$> ps)
    midp = Point 0 0:zipWith (\(Point x y) (Point x' y') -> Point ((x+x')/2) ((y+y')/2)) (drop 1 (pointPath <$> ps)) (pointPath <$> ps)
    offp = [Point 0 0.05, Point 0 0, Point (-0.2) 0, Point (-0.1) 0.1, Point 0 (-0.1)]
    t0 = TextChart (defaultTextStyle & set #size 0.05) (zip ts (zipWith addp offp midp))

-- | ellipse example
--
-- Under scaling, angles are not invariant, and this effects the shape of ellipses and thus SVG arc paths. Compare the effect of aspect changes to the axes of this ellipse:
--
-- ![ellipse example](other/ellipse.svg)
--
-- Below is the same ellipse with FixedAspect 2. Points scale exactly, but the original points that represent the end points of the axes are no longer on the new axes of the ellipse.
--
-- ![ellipse2 example](other/ellipse2.svg)
ellipseExample :: ChartAspect -> ChartSvg
ellipseExample a =
  mempty
    & #charts .~ named "ellipse" [ell, ellFull, c0, c1, bbox, xradii, yradii]
    & #hudOptions .~ defaultHudOptions
    & #hudOptions % #chartAspect .~ a
    & #hudOptions % #legends .~ [(10,defaultLegendOptions & #content .~ lrows & #textStyle % #size .~ 0.2 & #size .~ 0.1)]
    & #hudOptions % #titles .~ [(11,defaultTitle "ArcPosition (Point 1 0) (Point 0 1) (ArcInfo (Point 1.5 1) (pi / 3) True True)" & #style % #size .~ 0.08)]
  where
    p@(ArcPosition p1 p2 _) = ArcPosition (Point 1 0) (Point 0 1) (ArcInfo (Point 1.5 1) (pi / 3) True True)
    (ArcCentroid c r phi' ang0' angd) = arcCentroid p
    ellFull = LineChart fullels [ellipse c r phi' . (\x -> 2 * pi * x / 100.0) <$> [0 .. 100]]
    ell = LineChart els [ellipse c r phi' . (\x -> ang0' + angd * x / 100.0) <$> [0 .. 100]]
    g0 = defaultGlyphStyle & #shape .~ CircleGlyph
    c0 = GlyphChart g0 [c]
    g1 = defaultGlyphStyle & #color .~ palette1 6
    c1= GlyphChart g1 [p1,p2]
    bbox = RectChart bbs [arcBox p]
    bbs = defaultRectStyle & #borderSize .~ 0.002 & #color .~ Colour 0.4 0.4 0.8 0.1 & #borderColor .~ Colour 0.5 0.5 0.5 1
    xradii = LineChart xals [[ellipse c r phi' 0, ellipse c r phi' pi]]
    yradii = LineChart yals [[ellipse c r phi' (pi / 2), ellipse c r phi' (3 / 2 * pi)]]
    xals = defaultLineStyle & #color .~ Colour 0.9 0.2 0.02 1 & #size .~ 0.005 & #dasharray .~ Just [0.03, 0.01] & #linecap .~ Just LineCapRound
    yals = defaultLineStyle & #color .~ Colour 0.9 0.9 0.02 1 & #size .~ 0.005 & #dasharray .~ Just [0.03, 0.01] & #linecap .~ Just LineCapRound
    fullels = defaultLineStyle & #size .~ 0.002 & #color .~ palette1 1
    els = defaultLineStyle & #size .~ 0.005 & #color .~ palette1 2
    lrows = [ ("Major Axis", LineChart xals [[zero]]),
              ("Minor Axis", LineChart yals [[zero]]),
              ("Full Ellipse", LineChart fullels [[zero]]),
              ("Arc", LineChart els [[zero]]),
              ("Centroid", GlyphChart (g0 & #size .~ 0.01) [zero]),
              ("Endpoints", GlyphChart (g1 & #size .~ 0.01) [zero]),
              ("Bounding Box", RectChart (bbs & #borderSize .~ 0.01) [fmap (2*) one])
            ]

-- | Reproduction of the flag explanation chart in <https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths>
--
-- ![arc flags example](other/arcflags.svg)
arcFlagsExample :: ChartSvg
arcFlagsExample =
  mempty & set #charts
    (vert 0.02
      [ hori 0.02 [ colSweep, colSweep2, colLargeFalse, colLargeTrue ],
        rowLarge
      ]
    )
    & #hudOptions % #chartAspect .~ ChartAspect
    & #svgOptions % #cssOptions % #preferColorScheme .~ PreferHud
    & #svgOptions % #cssOptions % #cssExtra .~
      [trimming|
{
  .chart g {
    stroke: $hexDark;
  }
  .chart g text {
    fill: $hexDark;
  }
}
@media (prefers-color-scheme:dark) {
  .chart g {
    stroke: $hexLight;
  }
  .chart g text {
    fill: $hexLight;
  }
}
|]
  where
    hexDark = hex dark
    hexLight = hex light
    rowLarge = unnamed
      [ BlankChart [Rect 0 9 (-2.75) (-3.25)],
          TextChart (defaultTextStyle & #size .~ 0.6) [("Large", Point 5.5 (-3.0))]
      ]
    colLargeFalse = vert 0.02
      [ unnamed (checkFlags False True (set opac' 0.3 dark)),
        unnamed (checkFlags False False (set opac' 0.3 dark)),
        unnamed [ BlankChart [Rect (-1) 2 (-0.25) 0.25],
                  TextChart (defaultTextStyle & #size .~ 0.4) [("False", Point 0.5 (-0.1))]]
      ]
    colLargeTrue = vert 0.02
      [ unnamed (checkFlags True True (set opac' 0.3 dark)),
        unnamed (checkFlags True False (set opac' 0.3 dark)),
        unnamed [ BlankChart [Rect (-1) 2 (-0.25) 0.25],
                  TextChart (defaultTextStyle & #size .~ 0.4) [("True", Point 0.5 (-0.1))]
                ]
      ]
    colSweep = unnamed
      [ BlankChart [Rect (-0.4) 0.4 (-1) 5],
        TextChart (defaultTextStyle & #size .~ 0.6 & #rotation .~ Just (pi / 2))
        [("Sweep", Point 0.1 2)]
      ]
    colSweep2 = vert 0.02
      [ unnamed [ BlankChart [Rect (-0.25) 0.25 (-1) 2],
                  TextChart (defaultTextStyle & #size .~ 0.4 & #rotation .~ Just (pi / 2))
                  [("True", Point 0.1 0.5)]
                ],
        unnamed [ BlankChart [Rect (-0.25) 0.25 (-1) 2],
                  TextChart (defaultTextStyle & #size .~ 0.4 & #rotation .~ Just (pi / 2))
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
quadExample :: ChartSvg
quadExample =
  mempty
    & #charts .~ named "quad" [path', curve, c0, c1, bbox]
    & #hudOptions .~ defaultHudOptions
    & #hudOptions % #chartAspect .~ FixedAspect 1.5
    & #hudOptions % #legends .~ [(10,defaultLegendOptions & #content .~ lrows & #textStyle % #size .~ 0.2 & #size .~ 0.2)]
    & #hudOptions % #titles .~ [(11,defaultTitle "QuadPosition (Point 0 0) (Point 1 1) (Point 2 (-1))" & #style % #size .~ 0.08)]
  where
    p@(QuadPosition start end control) = QuadPosition (Point 0 0) (Point 1 1) (Point 2 (-1))
    ps = singletonQuad p
    path' = PathChart pathStyle ps
    curve = LineChart curveStyle [quadBezier p . (/ 100.0) <$> [0 .. 100]]
    curveStyle = defaultLineStyle & #size .~ 0.002 & #color .~ palette1 1
    c0 = GlyphChart defaultGlyphStyle [start, end]
    c1 = GlyphChart controlStyle [control]
    bbox = RectChart bbs [quadBox p]
    bbs = defaultRectStyle & #borderSize .~ 0.002 & #color .~ Colour 0.4 0.4 0.8 0.1 & #borderColor .~ Colour 0.5 0.5 0.5 1
    pathStyle = defaultPathStyle & #color .~ set opac' 0.1 (palette1 2) & #borderColor .~ transparent
    controlStyle = defaultGlyphStyle & #shape .~ CircleGlyph
    lrows = [ ("Path Fill", PathChart pathStyle [StartP zero]),
              ("Path Chord", LineChart curveStyle [[zero]]),
              ("Path Endpoints", GlyphChart defaultGlyphStyle [zero]),
              ("Path Control Point", GlyphChart controlStyle [zero]),
              ("Bounding Box", RectChart (bbs & #borderSize .~ 0.01) [one])
            ]

-- | cubic example
--
-- ![cubic example](other/cubic.svg)
cubicExample :: ChartSvg
cubicExample =
  mempty
    & #charts .~ named "cubic" [path', curve, c0, c1, bbox]
    & #hudOptions .~ mempty
    & #hudOptions % #chartAspect .~ FixedAspect 1.5
    & #hudOptions % #legends .~ [(10,defaultLegendOptions & #content .~ lrows & #textStyle % #size .~ 0.2 & #size .~ 0.2)]
    & #hudOptions % #titles .~ [(11,defaultTitle "CubicPosition (Point 0 0) (Point 1 1) (Point 1 0) (Point 0 1)" & #style % #size .~ 0.08)]
  where
    p@(CubicPosition start end control1 control2) = CubicPosition (Point 0 0) (Point 1 1) (Point 1 0) (Point 0 1)
    ps = singletonCubic p
    path' = PathChart pathStyle ps
    curve = LineChart curveStyle [cubicBezier p . (/ 100.0) <$> [0 .. 100]]
    c0 = GlyphChart defaultGlyphStyle [start, end]
    c1 = GlyphChart controlStyle [control1, control2]
    bbox = RectChart bbs [cubicBox p]
    bbs = defaultRectStyle & #borderSize .~ 0.002 & #color .~ Colour 0.4 0.4 0.8 0.1 & #borderColor .~ Colour 0.5 0.5 0.5 1
    pathStyle = defaultPathStyle & #color .~ set opac' 0.1 (palette1 2) & #borderColor .~ transparent
    controlStyle = defaultGlyphStyle & #shape .~ CircleGlyph
    curveStyle = defaultLineStyle & #size .~ 0.002 & #color .~ palette1 1
    lrows = [ ("Path Fill", PathChart pathStyle [StartP zero]),
              ("Path Chord", LineChart curveStyle [[zero]]),
              ("Path Endpoints", GlyphChart defaultGlyphStyle [zero]),
              ("Path Control Point", GlyphChart controlStyle [zero]),
              ("Bounding Box", RectChart (bbs & #borderSize .~ 0.01) [one])
            ]

-- | The common way to create a surface chart is usually a grid over a function.
--
-- ![surface example](other/surface.svg)
surfaceExample :: ChartSvg
surfaceExample =
  mempty
    & #extraHuds .~ h
    & #charts .~ named "surface" cs
    & #svgOptions .~ (defaultSvgOptions & #cssOptions % #shapeRendering .~ UseCssCrisp)
  where
    t = "rosenbrock function"
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
        ( defaultSurfaceLegendOptions dark t
            & #sloStyle % #surfaceColors .~ (palette1 <$> [0 .. 5])
            & #sloLegendOptions % #textStyle % #color .~ dark
            & #sloAxisOptions .~ surfaceAxisOptions dark
            & #sloLegendOptions % #frame %~ fmap (#borderColor .~ dark)
        )

-- | arrow example
--
-- Which happens to be the gradient of the surface example.
--
-- ![arrow example](other/arrow.svg)
arrowExample :: ChartSvg
arrowExample =
  mempty
    & #hudOptions .~ (defaultHudOptions & #axes %~ fmap (second (#ticks % #ltick .~ Nothing)))
    & #charts .~ named "arrow" ((\p -> gchart (tail' . f $ p) (angle . f $ p) p) <$> ps)
    & #svgOptions % #cssOptions % #preferColorScheme .~ PreferHud
    & #svgOptions % #cssOptions % #cssExtra .~
      [trimming|
{
  .arrow g {
    fill: $hexDark;
    stroke: $hexDark;
  }
}
@media (prefers-color-scheme:dark) {
  .arrow g {
    fill: $hexLight;
    stroke: $hexLight;
  }
}
|]
  where
    hexLight = hex light
    hexDark = hex dark
    f = snd . bimap ((-1.0) *) (fmap ((-1.0) *)) . rosenbrock 1 10
    ps = grid MidPos (one :: Rect Double) (Point 10 10 :: Point Int) :: [Point Double]
    arrow = PathGlyph "M -1 0 L 1 0 M 1 0 L 0.4 0.3 M 1 0 L 0.4 -0.3" NoScaleBorder
    gs s r' =
      defaultGlyphStyle
        & #borderSize .~ 0.05
        & #size .~ s
        & #borderColor .~ dark
        & #rotation .~ Just r'
        & #shape .~ arrow
    gchart s r' p = GlyphChart (gs s r') [p]

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
  #charts .~ blank (Rect 0 1 0 1) &
  #hudOptions .~ (mempty & #chartAspect .~ FixedAspect 1.5 & #axes .~
  [ (1, defaultAxisOptions & #place .~ PlaceLeft & #ticks % #style .~ TickPlaced tsTime),
    (1, defaultAxisOptions & #ticks % #style .~ TickPlaced tsDate)
  ])
  where
    tsTime = placedTimeLabelContinuous PosIncludeBoundaries Nothing 12 (Range (UTCTime (fromGregorian 2021 12 6) (toDiffTime 0)) (UTCTime (fromGregorian 2021 12 7) (toDiffTime 0)))
    tsDate = placedTimeLabelContinuous PosIncludeBoundaries (Just (pack "%d %b")) 2 (Range (UTCTime (fromGregorian 2021 12 6) (toDiffTime 0)) (UTCTime (fromGregorian 2022 3 13) (toDiffTime 0)))

-- | subchart example
--
-- chart-svg is (hopefully) ergonomic when putting together compound, complex charts by combining lots of subchart components.
--
-- The code in subChartExample is an (&) (left-to-right composition), non-operator lensy example.
--
-- ![subchart example](other/subchart.svg)
subChartExample :: ChartSvg
subChartExample =
  mempty &
  over #charts
    (<> named "xline"
     [LineChart (defaultLineStyle & set #color (palette1 8)) [xify [1,2,7,3,13,14]]]) &
  over #charts
    (<> named "yline"
     [LineChart (defaultLineStyle & set #color (palette1 5)) [yify [1,2,7,3,13,14]]]) &
  over #charts
    (\x -> x <> named "addlinex"
      (addLineX 3 (foldOf charts' x))) &
  over #charts
    (\x -> x <> named "addliney"
      (addLineY 5 (foldOf charts' x))) &
  set #hudOptions
    (titlesHud "subchart example" "x axis title" "y axis title") &
  set (#hudOptions % #axes)
      [ (1, defaultAxisOptions & set (#ticks % #style) (TickRound (FormatN (FSFixed 1) Nothing True) 8 TickExtend)),
        (1, defaultAxisOptions & set #place PlaceLeft & set (#ticks % #style) (TickRound (FormatN (FSFixed 1) Nothing True) 8 TickExtend))] &
  set (#hudOptions % #legends)
    [(8000, lineLegend 0.01
     ["xify", "yify", "addLineX", "addLineY"]
     [ (palette1 8, Nothing),
       (palette1 5, Nothing),
       (set opac' 0.3 $ palette1 8, Just [0.01]),
       (set opac' 0.3 $ palette1 5, Just [0.04, 0.01])])] &
  set (#hudOptions % #chartAspect) (FixedAspect 1.5)

-- | convert from [a] to [Point a], by adding the index as the x axis
--
xify :: [Double] -> [Point Double]
xify ys =
  zipWith Point [0 ..] ys

-- | convert from [a] to [Point a], by adding the index as the y axis
yify :: [Double] -> [Point Double]
yify xs =
  zipWith Point xs [0 ..]

-- | add a horizontal line at y
addLineX :: Double -> [Chart] -> [Chart]
addLineX y cs = case boxes cs of
  Nothing -> []
  Just (Rect lx ux  _ _) ->
    [LineChart ls' [[Point lx y, Point ux y]]]
    where
      ls' = defaultLineStyle &
        set #color (set opac' 0.3 (palette1 8)) &
        set #dasharray (Just [0.01])

-- | add a verticle line at x
addLineY :: Double -> [Chart] -> [Chart]
addLineY x cs = case boxes cs of
  Nothing -> []
  Just (Rect _ _ ly uy) ->
    [LineChart ls' [[Point x ly, Point x uy]]]
    where
      ls' = defaultLineStyle &
        set #color (set opac' 0.3 (palette1 5)) &
        set #dasharray (Just [0.04, 0.01])

-- | Legend template for a line chart.
lineLegend :: Double -> [Text] -> [(Colour, Maybe [Double])] -> LegendOptions
lineLegend w rs cs =
  defaultLegendOptions
      & #frame .~ Just (RectStyle 0.02 (palette1 9) (set opac' 0.05 $ palette1 4))
      & #content .~
      zipWith (\a r -> (r,LineChart a [[zero]])) ((\c -> defaultLineStyle & #color .~ fst c & #size .~ w & #dasharray .~ snd c) <$> cs) rs

-- | common pattern of chart title, x-axis title and y-axis title
titlesHud :: Text -> Text -> Text -> HudOptions
titlesHud t x y =
  mempty & #titles .~
  fmap (10,)
    [ defaultTitle t & #style % #size .~ 0.1,
      defaultTitle x & #place .~ PlaceBottom & #style % #size .~ 0.08,
      defaultTitle y & #place .~ PlaceLeft & #style % #size .~ 0.08
    ]

-- | /mixMidLineStyle n w/ produces n lines of size w interpolated between two colors.
mixMidLineStyles :: Int -> Double -> (Colour, Colour) -> [LineStyle]
mixMidLineStyles l w (c1, c2) = lo
  where
    m = (fromIntegral l - 1) / 2 :: Double
    cs = (\x -> 1 - abs (fromIntegral x - m) / m) <$> [0 .. (l - 1)]
    bs = (\x -> mix x c1 c2) <$> cs
    lo = (\c -> defaultLineStyle & #size .~ w & #color .~ c) <$> bs

-- | mix example
--
-- Interpolation of colours and points.
--
-- ![mix example](other/mix.svg)
mixExample :: ChartSvg
mixExample = mempty & #hudOptions .~ defaultHudOptions & #charts .~ named "mix" (mixExampleChart 8 5 100 0.005 (Range 0 1) (Range 0 1) (Range 0 1) (Range 0 0))

mixExampleChart :: Int -> Int -> Int -> Double -> Range Double -> Range Double -> Range Double -> Range Double -> [Chart]
mixExampleChart cl c2 n s gx' gy' hx' hy' = l
  where
    gx = grid OuterPos gx' n
    gy = grid OuterPos gy' n
    h0 = reverse $ grid OuterPos hx' n
    h1 = reverse $ grid OuterPos hy' n
    g = zipWith Point gx gy
    h = zipWith Point h0 h1
    gh = zipWith (\p p' -> [[p,p']]) g h
    c = mixMidLineStyles (n+1) s (palette1 cl, palette1 c2)
    l = zipWith LineChart c gh

-- | Adding reference points and bounding boxes to visualize chart alignment and debug.
--
-- -- ![debug example](other/debug.svg)
debugExample :: ChartSvg -> ChartSvg
debugExample cs =
  mempty &
  set #charts (e1 <> e2 <> e3)

  where
    e1 = toCharts cs
    e2 = glyphize (defaultGlyphStyle & #size .~ 0.01 & #shape .~ CircleGlyph) e1
    e3 = rectangularize (defaultRectStyle & #borderColor .~ dark & #borderSize .~ 0.001 & #color % opac' .~ 0.2) e1

pathChartSvg :: [(FilePath, ChartSvg)]
pathChartSvg =
  [
    ("other/unit.svg",unitExample),
    ("other/rect.svg",rectExample),
    ("other/text.svg",textExample),
    ("other/glyphs.svg",glyphsExample),
    ("other/line.svg",lineExample),
    ("other/hudoptions.svg",hudOptionsExample),
    ("other/bar.svg",barExample),
    ("other/surface.svg",surfaceExample),
    ("other/wave.svg",waveExample),
    ("other/venn.svg",vennExample),
    ("other/path.svg",pathExample),
    ("other/arcflags.svg",arcFlagsExample),
    ("other/ellipse.svg",ellipseExample (FixedAspect 1.7)),
    ("other/ellipse2.svg",ellipseExample (FixedAspect 2)),
    ("other/quad.svg",quadExample),
    ("other/cubic.svg",cubicExample),
    ("other/arrow.svg",arrowExample),
    ("other/date.svg",dateExample),
    ("other/subchart.svg",subChartExample),
    ("other/mix.svg",mixExample),
    ("other/debug.svg",debugExample lineExample)
  ]

-- | Run this to refresh haddock example SVGs.
writeAllExamples :: IO ()
writeAllExamples = do
  sequence_ $ uncurry writeChartSvg <$> pathChartSvg
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
    pathChartSvg
  putStrLn "dark version, ok"

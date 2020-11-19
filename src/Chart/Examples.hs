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
    barDataExample,
    barExample,
    waveExample,
    lglyphExample,
    glinesExample,
    compoundExample,
    boundTextBugExample,
    labelExample,
    legendExample,
    pathExample,
    vennExample,
    surfaceExample,
    arrowgExample,
    surfacegExample,
    rosenbrock,
    testArc,
    testQuad,
    testCubic,
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
        (PathGlyph "M0.05,-0.03660254037844387 A0.1 0.1 0.0 0 1 0.0,0.05 0.1 0.1 0.0 0 1 -0.05,-0.03660254037844387 0.1 0.1 0.0 0 1 0.05,-0.03660254037844387 Z", 0.01)
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

-- | venn diagram
--
-- ![venn diagram](other/venn.svg)
vennExample :: ChartSvg
vennExample =
  mempty &
  #chartList .~ zipWith (\c x -> Chart (PathA $ defaultPathStyle & #color .~ setOpac 0.2 c & #pathInfo .~ (fst <$> x)) (PointXY . snd <$> x)) palette1 (toInfos . parsePath <$> vennSegs) &
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

-- | PathA test
--
-- ![path test](other/path.svg)
pathExample :: ChartSvg
pathExample =
  (mempty &
   #chartList .~ [path', c0] &
   #hudOptions .~ defaultHudOptions &
   #svgOptions %~
   ((#outerPad .~ Just 0.1) .
    (#chartAspect .~ ChartAspect))
  )
  where
    ps =
      [ 
        (StartI, Point 0 0),
        (LineI, Point 1 0),
        (CubicI (polar (Point 0.2 0)) (polar (Point 0.25 1)), Point 1 1),
        (QuadI (polar (Point -1 2)), Point 0 1),
        (ArcI (ArcInfo (Point 1 1) (-pi/6) False False), Point 0 0)
      ]
    path' = Chart (PathA $ defaultPathStyle & #color .~ setOpac 0.1 (palette1!!2) & #borderColor .~ Colour 0.2 0.8 0.4 0.3 & #pathInfo .~ (fst <$> ps)) (PointXY . snd <$> ps)
    c0 = Chart (GlyphA defaultGlyphStyle) (PointXY . snd <$> ps)

-- | arc test
--
-- > testArc (ArcPosition (Point 1.0 0.0) (Point 0.0 1.0) (ArcInfo (Point 1.0 0.5) (-pi/3) False True))
--
-- ![arc Test](other/arc.svg)
--
testArc :: ArcPosition Double -> ChartSvg
testArc p1 =
  (mempty &
   #chartList .~ [arc, ell, c0, as, bbox] &
   #hudOptions .~ defaultHudOptions &
   #svgOptions %~ ((#outerPad .~ Just 0.4) . (#chartAspect .~ ChartAspect))
  )
  where
    ps = [(StartI, (p1 ^. #posStart)), (ArcI (p1 ^. #posInfo), (p1 ^. #posEnd))]
    arc = Chart (PathA $ defaultPathStyle & #color .~ setOpac 0.1 (palette1!!2) & #borderColor .~ transparent & #pathInfo .~ (fst <$> ps)) (PointXY . snd <$> ps)
    (ArcCentroid c r phi' ang0 angd) = arcCentroid p1
    ell = Chart (LineA $ defaultLineStyle & #width .~ 0.002 & #color .~ (palette1!!1)) (PointXY <$> ellipse c r (phi') . (\x -> ang0 + angd * x / 100.0) <$> [0..100])
    c0 = Chart (GlyphA defaultGlyphStyle) [PointXY c]
    as = Chart (TextA (defaultTextStyle & #anchor .~ AnchorStart & #size .~ 0.04) stats)
      (PointXY . Point 0 . (*0.1) <$>
       take (length stats) [0..])
    stats =
      [ (let (Point cx cy) = c in "c: (" <> fixed (Just 3) cx <> "," <> fixed (Just 3) cy <> ")"),
        (let (Point rx ry) = r in "r: (" <> fixed (Just 3) rx <> "," <> fixed (Just 3) ry <> ")"),
        "phi: " <> fixed (Just 3) phi',
        "ang0: " <> fixed (Just 3) ang0,
        "angd: " <> fixed (Just 3) angd
      ]
    bbox = Chart (RectA $ defaultRectStyle & #borderSize .~ 0.002 & #color .~ Colour 0.4 0.4 0.8 0.1 & #borderColor .~ Colour 0.5 0.5 0.5 1) [RectXY (arcBox p1)]

-- |
--
-- >>> testQuad (QuadPosition (Point 0 0) (Point 1 1) (Point 1 0))
--
-- ![quad test](other/quad.svg)
-- 
testQuad :: QuadPosition Double -> ChartSvg
testQuad p@(QuadPosition start end control) =
  (mempty &
   #chartList .~ [path', curve, c0, bbox] &
   #hudOptions .~ defaultHudOptions &
   #svgOptions %~ ((#outerPad .~ Just 0.4) . (#chartAspect .~ ChartAspect))
  )
  where
    (QuadPolar _ _ control') = quadPolar p
    ps = [(StartI, start), (QuadI control', end), (LineI, start)]
    path' = Chart (PathA $ defaultPathStyle & #color .~ setOpac 0.1 (palette1!!2) & #borderColor .~ transparent & #pathInfo .~ (fst <$> ps)) (PointXY . snd <$> ps)
    curve = Chart (LineA $ defaultLineStyle & #width .~ 0.002 & #color .~ (palette1!!1)) (PointXY <$> quadBezier p . (/100.0) <$> [0..100])
    c0 = Chart (GlyphA defaultGlyphStyle) (PointXY <$> [start, end, control])
    bbox = Chart (RectA $ defaultRectStyle & #borderSize .~ 0.002 & #color .~ Colour 0.4 0.4 0.8 0.1 & #borderColor .~ Colour 0.5 0.5 0.5 1) [RectXY (quadBox p)]

-- | cubic test
--
-- >>> testCubic (CubicPosition (Point 0 0) (Point 1 1) (Point 1 0) (Point 0 1))
--
-- ![cubic test](other/cubic.svg)
--
testCubic :: CubicPosition Double -> ChartSvg
testCubic p@(CubicPosition start end control1 control2) =
  (mempty &
   #chartList .~ [path', curve, c0, bbox, pt, bl] &
   #hudOptions .~ defaultHudOptions &
   #svgOptions %~ ((#outerPad .~ Just 0.4) . (#chartAspect .~ ChartAspect))
  )
  where
    (CubicPolar _ _ control1' control2') = cubicPolar p
    ps = [(StartI, start), (CubicI control1' control2', end), (LineI, start)]
    path' = Chart (PathA $ defaultPathStyle & #color .~ setOpac 0.1 (palette1!!2) & #borderColor .~ transparent & #pathInfo .~ (fst <$> ps)) (PointXY . snd <$> ps)
    curve = Chart (LineA $ defaultLineStyle & #width .~ 0.002 & #color .~ (palette1!!1)) (PointXY <$> cubicBezier p . (/100.0) <$> [0..100])
    c0 = Chart (GlyphA defaultGlyphStyle) (PointXY <$> [start, end, control1, control2, cubicBezier p 0.655])
    bbox = Chart (RectA $ defaultRectStyle & #borderSize .~ 0.002 & #color .~ Colour 0.4 0.4 0.8 0.1 & #borderColor .~ Colour 0.5 0.5 0.5 1) [RectXY (cubicBox p)]
    pt = Chart (TextA (defaultTextStyle & #size .~ 0.05) [pathText])
      [PointXY (Point 1 1.6)]
    pathText = toPathAbsolutes ps
    bl = Chart BlankA [RectXY (Rect 0 2 0 1.5)]

-- | Create a chart across a surface using a function.
--
-- Typically used to represent a gradient.
--
-- >>> writeChartSvg "other/surface.svg" $ surfaceExample "rosenbrock" (Point 100 100) one (fst . first (-1.0 *) . second (-1.0 .*) . rosenbrock 1 10)
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
       #soStyle . #surfaceColors .~ (take 6 palette1))
      (defaultSurfaceLegendOptions t &
       #sloStyle . #surfaceColors .~ (take 6 palette1))

-- | Create an arrow chart across a surface using a function.
--
-- Typically used to represent a gradient.
--
-- >>> writeChartSvg "other/arrowg.svg" $ arrowgExample (Point 20 20) one (fst . first (-1.0 *) . second (-1.0 .*) . rosenbrock 1 10)
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
  #hudOptions .~ (defaultHudOptions & #hudAxes %~ fmap (#atick . #ltick .~ Nothing)) &
  #chartList .~ ((\p -> chart (tmag . f $ p) (dir . f $ p) p) <$> ps) &
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

    dir :: Point Double -> Double
    dir (Point x y) = -1 * atan2 y x * (180/pi)

    mag :: Point Double -> Double
    mag (Point x y) = sqrt (x*x + y*y)

    tmag :: Point Double -> Double
    tmag = max 0.005 . min 0.02 . (*0.01) . (/avmag) . mag

    avmag = (sum $ mag . f <$> ps) / (fromIntegral $ length ps)

-- | A surface chart with gradient arrows.
--
-- >>> writeChartSvg "other/surfaceg.svg" $ surfacegExample "rosenbrock" (Point 100 100) (Point 20 20) one (first (-1.0 *) . second (-1.0 .*) . rosenbrock 1 10)
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

-- | Run this to refresh haddock example SVGs.
writeAllExamples :: IO ()
writeAllExamples = do
  -- Example in cabal file
  let xs = [[(0.0, 1.0), (1.0, 1.0), (2.0, 5.0)], [(0.0, 0.0), (3.2, 3.0)], [(0.5, 4.0), (0.5, 0)]] :: [[(Double, Double)]]
  let ls = fmap (PointXY . uncurry Point) <$> xs
  let anns = zipWith (\w c -> LineA (LineStyle w c Nothing Nothing)) [0.015, 0.03, 0.01] palette1
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
    testArc (ArcPosition (Point 1.0 0.0) (Point 0.0 1.0)
             (ArcInfo (Point 1.0 0.5) (-pi/3) False True))
  writeChartSvg "other/quad.svg" $
    testQuad (QuadPosition (Point 0 0) (Point 1 1) (Point 1 0))    
  writeChartSvg "other/cubic.svg" $
    testCubic (CubicPosition (Point 0 0) (Point 1 1) (Point 1 0) (Point 0 1))
  writeChartSvg "other/surface.svg" $
    surfaceExample "rosenbrock" (Point 100 100) one
    (fst . first (-1.0 *) . second (-1.0 .*) . rosenbrock 1 10)
  writeChartSvg "other/arrowg.svg" $
    arrowgExample (Point 20 20) one
    (snd . first (-1.0 *) . second (-1.0 .*) . rosenbrock 1 10)
  writeChartSvg "other/surfaceg.svg" $
    surfacegExample "rosenbrock" (Point 100 100) (Point 20 20) one
    (first (-1.0 *) . second (-1.0 .*) . rosenbrock 1 10)
  putStrLn ("ok" :: Text)

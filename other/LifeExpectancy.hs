-- | US Life Expectancy 1900-1940, extracted from a Stata-generated SVG
-- via agent-mediated destructive round-trip: SVG → chart-svg combinators.
--
-- The original 554-line SVG (uslifeexp_stcolor.svg) was reduced to:
--   1 data series (41 points), 2 axes, 1 title, 2 axis labels.
-- 500 grid lines, 82 redundant circle markers, background rects dropped.

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Chart
import Optics.Core

-- | (year, lifeExpectancy) — extracted from Stata SVG path data
lifeExpectancyData :: [(Double, Double)]
lifeExpectancyData =
    [ (1900.00, 47.30), (1901.00, 49.10), (1902.00, 51.50), (1903.00, 50.50)
    , (1904.00, 47.60), (1905.00, 48.70), (1906.00, 48.70), (1907.00, 47.60)
    , (1908.00, 51.10), (1909.00, 52.10), (1910.00, 50.00), (1911.00, 52.60)
    , (1912.00, 53.50), (1913.00, 52.50), (1914.00, 54.20), (1915.00, 54.50)
    , (1916.00, 51.70), (1917.00, 50.90), (1918.00, 39.10), (1919.00, 54.70)
    , (1920.00, 54.10), (1921.00, 60.80), (1922.00, 59.60), (1923.00, 57.20)
    , (1924.00, 59.70), (1925.00, 59.00), (1926.00, 56.70), (1927.00, 60.40)
    , (1928.00, 56.80), (1929.00, 57.10), (1930.00, 59.70), (1931.00, 61.10)
    , (1932.00, 62.10), (1933.00, 63.30), (1934.00, 61.10), (1935.00, 61.70)
    , (1936.00, 58.50), (1937.00, 60.00), (1938.00, 63.50), (1939.00, 63.70)
    , (1940.00, 62.90)
    ]

-- | Chart as chart-svg combinators — composable, themeable, animatable
lifeExpectancyChart :: ChartOptions
lifeExpectancyChart =
  mempty
    & set #chartTree
        ( named "us-life-expectancy"
            [ line, markers, fluMarker, fluLabel ]
        )
    & set #hudOptions hud
    & set (#markupOptions % #chartAspect) (FixedAspect 1.8)
    & set (#markupOptions % #cssOptions % #preferColorScheme) PreferHud
  where
    points = uncurry Point <$> lifeExpectancyData

    -- blue line with soft opacity
    line =
      LineChart
        ( defaultLineStyle
            & set #color (Colour 0.102 0.522 1.0 0.85)
            & set #size 0.008
        )
        [points]

    -- dot markers at each data point
    markers =
      GlyphChart
        ( defaultGlyphStyle
            & set #size 0.015
            & set #color (Colour 0.102 0.522 1.0 0.6)
            & set #glyphShape CircleGlyph
            & set #borderSize 0
        )
        points

    -- Spanish flu drop (1918)
    fluMarker =
      GlyphChart
        ( defaultGlyphStyle
            & set #size 0.04
            & set #color (Colour 0.9 0.2 0.2 0.6)
            & set #glyphShape CircleGlyph
            & set #borderSize 0.005
            & set #borderColor (Colour 0.9 0.2 0.2 1.0)
        )
        [Point 1918 39.1]

    fluLabel =
      TextChart
        ( defaultTextStyle
            & set #size 0.03
            & set #color (Colour 0.9 0.25 0.25 0.9)
        )
        [("1918 flu pandemic", Point 1919.5 40.5)]

    hud =
      defaultHudOptions
        & set #titles
            [ Priority 5
                ( defaultTitleOptions "US Life Expectancy 1900–1940"
                    & set (#style % #size) 0.06
                )
            ]
        & set #axes
            [ Priority 5
                ( defaultXAxisOptions
                    & set (#ticks % #tick)
                        (TickPlaced [(1900, "1900"), (1910, "1910"), (1920, "1920"), (1930, "1930"), (1940, "1940")])
                    & set (#ticks % #lineTick) Nothing
                )
            , Priority 5
                ( defaultYAxisOptions
                    & set (#ticks % #tick)
                        (TickPlaced [(40, "40"), (45, "45"), (50, "50"), (55, "55"), (60, "60"), (65, "65")])
                    & set (#ticks % #lineTick) Nothing
                    & set (#ticks % #textTick %? #style % #size) 0.04
                )
            , Priority 60
                (defaultYAxisOptions
                    & set #place PlaceRight
                    & set (#ticks % #tick) (TickPlaced [(39.1, "1918")])
                    & set (#ticks % #lineTick) Nothing
                    & set (#ticks % #textTick %? #style % #size) 0.025
                    & set (#ticks % #textTick %? #style % #color) (Colour 0.9 0.25 0.25 0.7)
                )
            ]
        & set #legends []
        & set #frames [Priority 101 (defaultFrameOptions & set #buffer 0.05)]

-- | Write to file
main :: IO ()
main = do
  let fp = "life-expectancy.svg"
  writeChartOptions fp lifeExpectancyChart
  putStrLn $ "Wrote " <> fp

-- After writing, view with: cabal run
-- Or in ghci:
--   import Chart
--   writeChartOptions "le.svg" lifeExpectancyChart

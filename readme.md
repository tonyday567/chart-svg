
[![img](https://img.shields.io/hackage/v/chart-svg.svg)](https://hackage.haskell.org/package/chart-svg) [![img](https://github.com/tonyday567/chart-svg/workflows/haskell-ci/badge.svg)](https://github.com/tonyday567/chart-svg/actions)

![img](other/banner.svg)

A charting library targetting SVG.

# Usage

    :r
    :set prompt "> "
    :set -XOverloadedLabels
    :set -XOverloadedStrings
    import Chart
    import Optics.Core
    lines = [[Point 0.0 1.0, Point 1.0 1.0, Point 2.0 5.0],[Point 0.0 0.0, Point 2.8 3.0],[Point 0.5 4.0, Point 0.5 0]]
    styles = (\c -> defaultLineStyle & #color .~ palette c & #size .~ 0.015) <$> [0..2]
    cs = zipWith (\s x -> LineChart s [x]) styles lines
    lineExample = mempty & #chartTree .~ named "line" cs & #hudOptions .~ defaultHudOptions :: ChartOptions
    writeChartOptions "other/usage.svg" lineExample

![img](other/usage.svg)

See the haddock documentation for a detailed overview.

# Examples

To redraw all the examples in Chart.Examples

    import Chart.Examples
    writeAllExamples

    ok


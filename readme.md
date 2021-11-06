chart-svg
=========
[![Hackage](https://img.shields.io/hackage/v/chart-svg.svg)](https://hackage.haskell.org/package/chart-svg)
[![Build Status](https://github.com/tonyday567/chart-svg/workflows/haskell-ci/badge.svg)](https://github.com/tonyday567/chart-svg/actions?query=workflow%3Ahaskell-ci) [![Hackage Deps](https://img.shields.io/hackage-deps/v/chart-svg.svg)](http://packdeps.haskellers.com/reverse/chart-svg)

A chart library targeting SVG.

Usage
===

``` haskell
import Chart

main :: IO ()
main = do
  let xs = [[(0.0, 1.0), (1.0, 1.0), (2.0, 5.0)], [(0.0, 0.0), (3.2, 3.0)], [(0.5, 4.0), (0.5, 0)]] :: [[(Double, Double)]]
  let ls = fmap (uncurry P) <$> xs
  let anns = zipWith (\w c -> LineA (LineStyle w (palette1 c) Nothing Nothing Nothing Nothing)) [0.015, 0.03, 0.01] [0..2]
  let lineChart = zipWith Chart anns ls
  writeChartSvgHud "lineshud.svg" lineChart
```

![chart-svg example](other/line.svg)

Examples
===

See the code in Chart.Examples for practical usage.

Chart Types
===

rect

![](other/rect.svg)

line

![](other/line.svg)

text

![](other/text.svg)

glyph

![](other/glyphs.svg)

bar

![](other/bar.svg)

surface

![](other/surface.svg)

arrow

![](other/arrow.svg)

hud
---

default hud

![](other/hudoptions.svg)





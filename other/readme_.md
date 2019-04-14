chart-svg
===

[![Build Status](https://travis-ci.org/tonyday567/chart-svg.svg)](https://travis-ci.org/tonyday567/chart-svg) [![Hackage](https://img.shields.io/hackage/v/chart-svg.svg)](https://hackage.haskell.org/package/chart-svg) [![lts](https://www.stackage.org/package/chart-svg/badge/lts)](http://stackage.org/lts/package/chart-svg) [![nightly](https://www.stackage.org/package/chart-svg/badge/nightly)](http://stackage.org/nightly/package/chart-svg) 

Try `stack build --exec "$(stack path --local-install-root)/bin/style-page" --file-watch` for an interactive style builder.

test svgs
===

mempty
---

`render (Point (200.0 :: Double) 200.0) mempty` produces the following svg text:

```include
other/mempty.md
```

one
---

`chartSvg (one :: ViewBox Double) [Chart (RectA defaultRectStyle) mempty [one]]`

![](other/one.svg)

ChartSvg transforms
---

rotate

![](other/rotateOne.svg)

translate

![](other/translateOne.svg)

rectangle
---

![](other/rectChart.svg)

![](other/rectCharts.svg)

![](other/pixel.svg)

text
---

![](other/textChart.svg)

text chart

![](other/textsChart.svg)

boundText

![](other/boundText.svg)

rotated label

![](other/label.svg)

glyphs
---

circle

![](other/circle.svg)

glyphs

![](other/glyphs.svg)

smiley

![](other/smiley.svg)

glyph

![](other/glyphsChart.svg)


line charts
---

line

![](other/lines.svg)

gline

![](other/glines.svg)

labelled glyph
---

lglyph

![](other/lglyph.svg)

putting it all together

![](other/compound.svg)

recipe
---

```
stack build --test --exec "$(stack path --local-install-root)/bin/chart-svg" --exec "$(stack path --local-bin)/pandoc -f markdown -i other/readme_.md -t html -o index.html --filter pandoc-include --mathjax" --file-watch --ghc-options -freverse-errors
```

```
stack build --test --exec "$(stack path --local-install-root)/bin/chart-hud" --exec "$(stack path --local-bin)/pandoc -f markdown+lhs -i app/hud.lhs -t html -o hud.html --filter pandoc-include --mathjax" --file-watch --ghc-options -freverse-errors

stack build --exec "$(stack path --local-install-root)/bin/style-page" --file-watch
```


reference
---

- [MDN svg](https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial)
- [SVG2](https://www.w3.org/TR/SVG2/text.html#TextAnchoringProperties)
- [plot-light](https://hackage.haskell.org/package/plot-light-0.4.3/docs/src/Graphics.Rendering.Plot.Light.Internal.html#text)
- [svg-tree](http://hackage.haskell.org/package/svg-tree-0.6.2.2/docs/Graphics-Svg-Types.html#v:documentLocation)
- [JuicyPixels](http://hackage.haskell.org/package/JuicyPixels-3.2.9.5/docs/Codec-Picture-Types.html#t:PixelRGBA8)

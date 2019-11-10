chart-svg
=========

[![Build
Status](https://travis-ci.org/tonyday567/chart-svg.svg)](https://travis-ci.org/tonyday567/chart-svg)
[![Hackage](https://img.shields.io/hackage/v/chart-svg.svg)](https://hackage.haskell.org/package/chart-svg)
[![lts](https://www.stackage.org/package/chart-svg/badge/lts)](http://stackage.org/lts/package/chart-svg)
[![nightly](https://www.stackage.org/package/chart-svg/badge/nightly)](http://stackage.org/nightly/package/chart-svg)

test svgs
=========

rects
-----

mempty

```
<svg viewBox="-0.5 -0.5 1.0 1.0" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" width="200.0" height="200.0" />
```

unit rectangle

![](other/one.svg)

```
<?xml version='1.0' ?>
<svg viewBox="-0.603 -0.603 1.206 1.206" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" width="200.0" height="200.0">
  <g stroke-width="5.0e-3" stroke="#666666" fill="#5DA5DA" fill-opacity="0.5" stroke-opacity="0.5">
    <rect width="1.0" height="1.0" x="-0.5" y="-0.5" />
  </g>
</svg>
```

```
(pad 1.1 $ chartSvg (one :: ViewBox Double) [Chart (RectA defaultRectStyle) mempty [one]])
```

rotate

![](other/rotateOne.svg)

translate

![](other/translateOne.svg)

rectChart

![](other/rectChart.svg)

rectCharts

![](other/rectCharts.svg)

pixelChart

![](other/pixel.svg)

text
----

text

![](other/textChart.svg)

textsChart

![](other/textsChart.svg)

boundText

![](other/boundText.svg)

label + bounding box

![](other/label.svg)

glyphs
------

circle

![](other/circle.svg)

glyphs

![](other/glyphs.svg)

smiley

![](other/smiley.svg)

glyphsChart

![](other/glyphsChart.svg)

line charts
-----------

line

![](other/lines.svg)

gline

![](other/glines.svg)

labelled glyph
--------------

lglyph

![](other/lglyph.svg)

putting it all together

![](other/compound.svg)

recipe
------
```
stack build --exec "$(stack path --local-install-root)/bin/examples" --file-watch
```

reference
---------

-   [libraries](https://www.stackage.org/)
-   [MDN svg](https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial)
-   [SVG2](https://www.w3.org/TR/SVG2/)
-   [lucid](http://hackage.haskell.org/package/lucid)
-   [svg-tree](http://hackage.haskell.org/package/svg-tree-0.6.2.2/docs/Graphics-Svg-Types.html#v:documentLocation)
-   [JuicyPixels](http://hackage.haskell.org/package/JuicyPixels-3.2.9.5/docs/Codec-Picture-Types.html#t:PixelRGBA8)


hud development
===

Notes, testing and code for chart-svg develoment.

<br>

\begin{code}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}

import Chart.Svg
import NumHask.Prelude hiding (Text)
import Lens.Micro
import Codec.Picture.Types
import Data.Generics.Product (field)
import Chart.Hud
import Chart.Core
import Chart.Spot

\end{code}

A Chart consists of a few conceptual layers:

data to be represented
---

Data for chart-svg charts is typically a double-list of either points or rectangles.  The data below are three simple lines.

\begin{code}

ls :: [[Point Double]]
ls =
  map (uncurry Point) <$>
  [ [(0.0, 1.0), (1.0, 1.0), (2.0, 5.0)]
  , [(0.0, 0.0), (3.0, 3.0)]
  , [(0.5, 4.0), (0.5, 0)]
  ]

\end{code}

manifest representations of the data
---

Physical, on-the-page representations of data are separate to the data itself.

chart-svg categorises representations into different style categories.  The code below specifies a line style for representing chart data, with differing colors and line widths.

\begin{code}

gopts :: [GlyphStyle]
gopts =
  [ field @"borderSize" .~ 0 $
    field @"size" .~ 0.1 $
    defaultGlyphStyle
  , field @"borderSize" .~ 0 $
    field @"size" .~ 0.1 $
    field @"color" .~ PixelRGBA8 100 30 30 100 $
    field @"shape" .~ RectRoundedGlyph 1.5 0.01 (0.01 :: Double) $
    defaultGlyphStyle
  , field @"borderSize" .~ 0 $
    field @"size" .~ 0.1 $
    field @"color" .~ PixelRGBA8 100 130 80 100 $
    field @"shape" .~ EllipseGlyph 1.5 $
    defaultGlyphStyle
  ]

\end{code}

and this is enough to create a chart:

\begin{code}

glyphs :: [Chart Double]
glyphs = zipWith (\d s -> Chart (GlyphA s) mempty (SpotPoint <$> d)) ls gopts

\end{code}

![](other/glyphs.svg)


This layering is not exactly canonical. [ggplot2](http://r4ds.had.co.nz/visualize.html) uses the term [aesthetics](https://ggplot2.tidyverse.org/reference/aes.html) for the process of deciding which features of the data will be brought out in the visualisation, so that style is seen as a function of the data.

By clearly specifying what is a position on the page (the Spot) from what may be some other style decision (the color or shape of a geometric object), however, other chart features, such as grid-lines and axes, can reuse the same functionality used to build the data representation.

3. the hud
===

In addition to raw data, and to the manifestation of that data on a physical plane, a chart usually includes decoration that assists in interpreting the chart data - axes, grid lines, titles, legends and friends that visual explain what is being seen in the chart.

chart-svg calls this decoration the heads-up-display - the hud.

The remainder of this *.lhs records hud development.

decorating the canvas
---

huds are different to pure chart data only in the sense that they require both information on the raw data (eg data ranges to calculate tick values) and information on the representation (eg how wide the chart is to properly locate the tick text on the page)

The xy-plane on which the data is represented - the canvas - is a good place to tease this out.

chart-svg and, specifically, `chartSvg` recognises that the physical representation of a data point may fall outside the data range, so that the viewbox becomes wider than the raw data suggests. 

Here's an attempt to create a background canvas for our example chart corners:

\begin{code}

corners :: Double -> [Chart Double]
corners s =
  [Chart
  (GlyphA $
   field @"borderSize" .~ 0 $
    field @"size" .~ s $
    defaultGlyphStyle)
  mempty
  [SP (-0.5) (-0.5), SP (-0.5) 0.5, SP 0.5 (-0.5), SP 0.5 0.5]]

canvas1 :: ChartSvg Double
canvas1 = chartSvg one (corners 0.1 <> [Chart (RectA (blob grey 0.2)) mempty [one]])

\end{code}
<br>
![](other/canvas1.svg)
<br>

The canvas has to take into account both the data area and the physical representation of the data; in this case the circle size of the data points.

\begin{code}

canvas2 :: (ToRatio a, FromRatio a, Subtractive a, Field a, BoundedLattice a) => ViewBox a -> [Chart a] -> ChartSvg a
canvas2 (ViewBox asp) cs =
  chartSvg_ (ViewBox asp') (cs' <> [canvas'])
  where
    cs' = projectSpots asp cs
    asp' = styleBoxes cs'
    canvas' = Chart (RectA (blob grey 0.2)) mempty
      [SpotArea asp']

\end{code}

<br>
![](other/canvas2.svg)
<br>

So the introduction of a hud to a chart requires a different api to chartSvg.  Together with a list of Charts and a ViewBox, we require a function that takes in a ViewBox - the viewbox of the main chart list - and gives back a list of Charts which is the pre-rendered hud.

<br>

\begin{code}

canvas3 :: Hud a
canvas3 = canvas (blob grey 0.2) mempty

\end{code}

`hudSvg one canvas3 (corners 0.25)`

<br>
![](other/canvas3.svg)
<br>


basic axes
---

\begin{code}

bbars :: [Bar Double]
bbars =
  [ Bar PlaceBottom defaultRectStyle 0.05 0.02
  , Bar PlaceLeft defaultRectStyle 0.05 0.02
  ]

hud1 :: ViewBox Double -> [Chart Double] -> ChartSvg Double
hud1 vb cs = 
  hudSvg vb [c,b] cs
  where
    c = canvas (blob grey 0.2) mempty
    b = bars bbars mempty

\end{code}

`hud1 (aspect 1.5) glyphs`

![](other/hud1.svg)

\begin{code}

main :: IO ()
main = do
  scratchWith
    ( clearScratchStyle &
    field @"fileName" .~ "other/glyphs.svg" &
    field @"ratioAspect" .~ 1.5) glyphs
  write "other/canvas1.svg" (Point 200 200) canvas1
  write "other/canvas2.svg" (Point 200 200) (canvas2 one (corners 0.2))
  write "other/canvas3.svg" (Point 200 200) (hudSvg one [canvas3] (corners 0.25))
  write "other/hud1.svg" (Point 400 400) $ hud1 (aspect 1.5) glyphs

\end{code}

<br>

λ> workflow

<br>

```
stack build --test --exec "$(stack path --local-install-root)/bin/chart-hud" --exec "$(stack path --local-bin)/pandoc -f markdown+lhs -i other/header.md -i app/hud.lhs -i other/footer.md -t html -o hud.html --filter pandoc-include --mathjax" --file-watch --ghc-options -freverse-errors
```

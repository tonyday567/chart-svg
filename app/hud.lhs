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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}

import Chart.Svg
import NumHask.Prelude
import Control.Lens
import Codec.Picture.Types

import Data.Generics.Labels ()
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

manifest of the data
---

Physical, on-the-page representations of data are separate to the data itself.

chart-svg categorises representations into different style categories.  The code below specifies a line style for representing chart data, with differing colors and line widths.

\begin{code}

gopts :: [GlyphStyle]
gopts =
  [ #borderSize .~ 0 $
    #size .~ 0.1 $
    defaultGlyphStyle
  , #borderSize .~ 0 $
    #size .~ 0.1 $
    #color .~ PixelRGB8 100 30 30 $
    #shape .~ RectRoundedGlyph 1.5 0.01 (0.01 :: Double) $
    defaultGlyphStyle
  , #borderSize .~ 0 $
    #size .~ 0.1 $
    #color .~ PixelRGB8 100 130 80 $
    #shape .~ EllipseGlyph 1.5 $
    defaultGlyphStyle
  ]

\end{code}

and this is enough to create a chart:

\begin{code}

glyphs :: [Chart Double]
glyphs = zipWith (\d s -> Chart (GlyphA s) mempty (SpotPoint <$> d)) ls gopts

\end{code}

![](other/glyphs_.svg)

This layering is not exactly canonical. [ggplot2](http://r4ds.had.co.nz/visualize.html) uses the term [aesthetics](https://ggplot2.tidyverse.org/reference/aes.html) for the process of deciding which features of the data will be brought out in the visualisation, so that style is seen as a function of the data.

By clearly specifying what is a position on the page (the Spot) from what may be some other style decision (the color or shape of a geometric object), however, other chart features, such as grid-lines and axes, can reuse the same functionality used to build the data representation.

the hud
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
   #borderSize .~ 0 $
    #size .~ s $
    defaultGlyphStyle)
  mempty
  [SP (-0.5) (-0.5), SP (-0.5) 0.5, SP 0.5 (-0.5), SP 0.5 0.5]]

can1 :: ChartSvg Double
can1 = chartSvg one (corners 0.1 <> [Chart (RectA (blob grey 0.2)) mempty [one]])

\end{code}
<br>
![](other/canvas1.svg)
<br>

The canvas has to take into account the data area and the physical representation of the data; in this case the circle size of the data points.

\begin{code}

can2 :: (Chartable a) => ViewBox a -> [Chart a] -> ChartSvg a
can2 (ViewBox asp) cs =
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


basic axis bars and titles
---

Building the Hud includes a layering process: the next Hud element being added (which is a [Chart a]) takes into account the ViewBox of the chart and all the previous hud elements.

The `foldl layer mempty` fold of the Huds effectively forgets what is the chart and what are the hud elements, and text anchoring cannot reference the original chart rep.

\begin{code}

b1 :: [(Place Double, Bar Double)]
b1 =
  [ (PlaceTop, Bar defaultRectStyle 0.05 0.02)
  , (PlaceRight, Bar defaultRectStyle 0.05 0.02)
  , (PlaceBottom, Bar defaultRectStyle 0.05 0.02)
  , (PlaceLeft, Bar defaultRectStyle 0.05 0.02)
  ]

t1 :: [Title Double]
t1 = (\a p -> (#place .~ p :: Title Double -> Title Double) $
        #anchor .~ a $
        defaultTitle (show a <> ":" <> show p)) <$>
      [AnchorStart, AnchorMiddle, AnchorEnd] <*>
      [PlaceBottom, PlaceTop, PlaceLeft, PlaceRight]

hud1 :: ViewBox Double -> [Chart Double] -> ChartSvg Double
hud1 vb cs =
  hudSvg vb [[c], [b], [t]] cs
  where
    c = canvas (blob grey 0.2) mempty
    b = mconcat $ (\(p, x) -> bar p x mempty) <$> b1
    t = foldl layer mempty $ (\x -> title x mempty) <$>
      ((#place .~ PlaceAbsolute (Point 0 0) :: Title Double -> Title Double)
      (defaultTitle "PlaceAbsolute") : t1)

\end{code}

`hud1 (aspect 1.5) glyphs`

![](other/hud1.svg)


`fold` ignores the other Hud elements

\begin{code}
hud2 :: ViewBox Double -> [Chart Double] -> ChartSvg Double
hud2 vb cs =
  hudSvg vb [[c], [b], [t]] cs
  where
    c = canvas (blob grey 0.2) mempty
    b = mconcat $ (\(p,x) -> bar p x mempty) <$> b1
    t = fold $ (\x -> title x mempty) <$>
      ((#place .~ PlaceAbsolute (Point 0 0) :: Title Double -> Title Double)
      (defaultTitle "PlaceAbsolute") : t1)
\end{code}

![](other/hud2.svg)

tick marks and labels
---

Tick marks is an example of computing and placing Hud elements according to the original range of the data.

\begin{code}

hud3a :: ViewBox Double -> [Chart Double] -> ChartSvg Double
hud3a vb cs =
  hudSvg vb [[c], [bBot, tBot, bLeft, tLeft]] cs
  where
    c = canvas (blob grey 0.2) mempty
    bBot = bar PlaceBottom (Bar defaultRectStyle 0.005 0.01) mempty
    bLeft = bar PlaceLeft (Bar defaultRectStyle 0.005 0.01) mempty
    tBot = tick PlaceBottom defaultTick mempty
    tLeft = tick PlaceLeft defaultTick mempty

\end{code}

![](other/hud3a.svg)

\begin{code}

hud3 :: ViewBox Double -> [Chart Double] -> ChartSvg Double
hud3 vb cs =
  hudSvg vb
  [[c], [bBot, tBot, bLeft, tLeft, bTop, tTop, bRight, tRight], t'] cs
  where
    c = canvas (blob grey 0.2) mempty
    bBot = bar  PlaceBottom (Bar defaultRectStyle 0.005 0.01) mempty
    bLeft = bar PlaceLeft (Bar defaultRectStyle 0.005 0.01) mempty
    bTop = bar PlaceTop (Bar defaultRectStyle 0.005 0.01) mempty
    bRight = bar PlaceRight (Bar defaultRectStyle 0.005 0.01) mempty
    tBot = tick PlaceBottom defaultTick mempty
    tLeft = tick PlaceLeft defaultTick mempty
    tTop = tick PlaceTop defaultTick mempty
    tRight = tick PlaceRight defaultTick mempty
    t' = (\x -> title ((#place .~ x  :: Title Double -> Title Double) $ defaultTitle "tick marks") mempty) <$>
      ([PlaceRight, PlaceLeft, PlaceTop, PlaceBottom] :: [Place Double])

\end{code}

![](other/hud3.svg)

<br>

tick label rotation
---

Getting ticks to display sensibly can be labor intensive.  Starting with some longer labels to illustrate the process:

\begin{code}

hud4 :: ViewBox Double -> [Chart Double] -> ChartSvg Double
hud4 vb cs =
  hudSvg vb [[c], [bBot, tBot, bLeft, tLeft, bTop, tTop, bRight, tRight], t'] cs
  where
    labels = ["tick labels", "often need to be", "manipulated", "by text anchoring", ", by rotation", "and by adjustments to font size"] :: [Text]
    ts = (#tstyle .~ TickLabels labels :: Tick Double -> Tick Double) defaultTick
    c = canvas (blob grey 0.2) mempty
    bBot = bar PlaceBottom (Bar defaultRectStyle 0.005 0.01) mempty
    bLeft = bar PlaceLeft (Bar defaultRectStyle 0.005 0.01) mempty
    bTop = bar PlaceTop (Bar defaultRectStyle 0.005 0.01) mempty
    bRight = bar PlaceRight (Bar defaultRectStyle 0.005 0.01) mempty
    tBot :: Hud Double
    tBot = adjustedTickHud (AxisConfig Nothing (Just defaultAdjustments) ts PlaceBottom)
    tLeft = tick PlaceLeft ts mempty
    tTop :: Hud Double
    tTop = adjustedTickHud (AxisConfig Nothing (Just defaultAdjustments) ts PlaceTop)
    tRight = tick PlaceRight ts mempty
    t' = (\x -> title ((#place .~ x  :: Title Double -> Title Double) $ defaultTitle "automated tick style") mempty) <$>
      ([PlaceRight, PlaceLeft, PlaceTop, PlaceBottom] :: [Place Double])
\end{code}

![](other/hud4.svg)

\begin{code}

main :: IO ()
main = do
  scratchWith
    ( clearScratchStyle &
    #fileName .~ "other/glyphs_.svg" &
    #ratioAspect .~ 1.5) glyphs
  write "other/canvas1.svg" (Point 200 200) can1
  write "other/canvas2.svg" (Point 200 200) (can2 one (corners 0.2))
  write "other/canvas3.svg" (Point 200 200) (hudSvg one [[canvas3]] (corners 0.25))
  write "other/hud1.svg" (Point 400 400) $ hud1 (aspect 1.5) glyphs
  write "other/hud2.svg" (Point 400 400) $ hud2 (aspect 1.5) glyphs
  write "other/hud3.svg" (Point 400 400) $ hud3 (aspect 1.5) glyphs
  write "other/hud3a.svg" (Point 400 400) $ hud3a (aspect 1.5) glyphs
  write "other/hud4.svg" (Point 400 400) $ hud4 (aspect 1.5) glyphs

\end{code}

<br>

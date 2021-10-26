{-# OPTIONS_GHC -Wall #-}

-- | A haskell Charting library targetting SVGs
module Chart
  ( -- * Usage

    --
    -- $usage

    -- * Overview

    --
    -- $overview

    -- * Re-exports
    module Chart.Primitive,
    module Chart.Data,
    module Chart.Hud,
    module Chart.Style,
    module Chart.Svg,
    module Chart.Bar,
    module Chart.Surface,
    module Data.Colour,
    module Data.FormatN,
    module Data.Path,
    module Data.Path.Parser,
    module NumHask.Space,
  )
where


import Chart.Bar
import Chart.Primitive
import Chart.Style
import Chart.Hud
import Chart.Svg
import Chart.Surface
import Data.Colour
import Data.FormatN
import Data.Path
import NumHask.Space hiding (singleton)
import Chart.Data
import Data.Path.Parser

-- $setup
--
-- >>> :set -XOverloadedLabels
-- >>> :set -XOverloadedLists
-- >>> import Chart
-- >>> import Optics.Core

-- $overview
--
-- Charting consists of three highly-coupled conceptual layers:
--
-- 1. the data to be represented.
-- 2. how the data will be represented on a screen, and.
-- 3. the creation of visual aids that help interpret the data; such as axes, gridlines and titles.
--
-- == What is a 'Chart'?
--
-- A 'Chart' in this library consists of a specification of the first two items in the above list; data and data annotation.
--
-- - 'XY': a list of values, specified as either 2D points or rectangles.
--
-- - 'Annotation': a description of how the data should be represented on the screen.
--
-- What exactly is annotation and what is data is highly variant within charting practice. This construction treats position on the XY plane differently from other quantitative manifests such as color and size. The chief advantage of priveliging XY position is that scaling and integrating data with other chart elements becomes much easier. The disadvantage is that, to use quantitative tools such as size, data needs to be consciously separated into that which is position-orientated, and that which is defined as 'Annotation'.
--
--
-- Here's some data; three lists of points that will form a line:
--
-- >>> let xs = fmap (fmap (uncurry Point)) [[(0.0, 1.0), (1.0, 1.0), (2.0, 5.0)], [(0.0, 0.0), (3.2, 3.0)], [(0.5, 4.0), (0.5, 0)]] :: [[Point Double]]
--
-- and an Annotation to describe representation of this data; three line styles with different colors and widths:
--
-- >>> let anns = zipWith (\w (palette1 c) -> LineA (LineStyle w c Nothing Nothing Nothing Nothing)) [0.015, 0.03, 0.01] [0..2]
--
-- and this is enough to create a Chart.
--
-- >>> let lineExample = mempty & (#chartList .~ zipWith Chart anns (fmap (fmap PointXY) xs)) & #hudOptions .~ defaultHudOptions & #svgOptions .~ defaultSvgOptions :: ChartSvg
-- >>> :t lineExample
-- lineExample :: ChartSvg
--
-- > writeChartSvg "other/line.svg" lineExample
--
-- ![lines example](other/line.svg)

-- $hud
--
-- Axes, titles, tick marks, grid lines and legends are chart elements that exist to provide references for the viewer that helps explain the data that is being represented by the chart. They can be clearly distinguished from data representations such as a mark where a value is.
--
-- Apart from this, hud elements are pretty much the same as data elements. They simulateously have two reference frames: a data domain (tick values reference the data value range) and a page domain (a title needs to be placed on the left say, and has a size of 0.1 versus the page size). They are also typically composed of the same sorts of primitives as data elements, such as rectangles and lines and text and colors.
--
-- Given this similarity, an efficient process for chart creation is roughly:
--
-- - collect the chart data and data annotations into a [Chart]
--
-- - measure the range of the data values
--
-- - begin a process of folding a [Hud ()] in with the charts, supplying the the data values to the hud elements if needed, and keeping track of the overall page size of the chart.
--
-- This process is encapsulated in 'runHud'.
--
-- An important quality of 'runHud' (and conversion of charts to svg in general)is that this is the point at which the 'XY's of the chart are converted from the data domain to the page domain. Once the hud and the chart has been integrated there is no going back and the original data is forgotten. This is an opinionated aspect of chart-svg. A counter-example is d3 which stores the raw data in the svg element it represents.

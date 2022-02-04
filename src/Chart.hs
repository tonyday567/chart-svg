{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK prune #-}

-- | A haskell Charting library targetting SVGs
module Chart
  ( -- * Usage

    --
    -- $usage

    -- * Overview

    --
    -- $overview

    -- * Hud

    --
    -- $hud

    -- * Optics Usage

    --
    -- $optics

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
  )
where

import Chart.Bar
import Chart.Data
import Chart.Hud
import Chart.Primitive
import Chart.Style
import Chart.Surface
import Chart.Svg
import Data.Colour
import Data.FormatN
import Data.Path
import Data.Path.Parser

-- $setup
--
-- >>> :set -XOverloadedLabels
-- >>> :set -XOverloadedStrings
-- >>> import Chart
-- >>> import Optics.Core

-- $usage
--
-- >>> :set -XOverloadedLabels
-- >>> :set -XOverloadedStrings
-- >>> import Chart
-- >>> import Optics.Core
-- >>> let lines = [[Point 0.0 1.0, Point 1.0 1.0, Point 2.0 5.0],[Point 0.0 0.0, Point 2.8 3.0],[Point 0.5 4.0, Point 0.5 0]]
-- >>> let styles = (\c -> defaultLineStyle & #color .~ palette1 c & #size .~ 0.015) <$> [0..2]
-- >>> let cs = zipWith (\s x -> LineChart s [x]) styles lines
-- >>> let lineExample = mempty & #charts .~ named "line" cs & #hudOptions .~ defaultHudOptions :: ChartSvg
-- >>> writeChartSvg "other/usage.svg" lineExample

-- $overview
--
-- Charting consists of three highly-coupled conceptual layers:
--
-- 1. the data to be represented.
-- 2. how the data is to be represented on a screen, and.
-- 3. the creation of visual aids that help interpret the data; such as axes, gridlines and titles.
--
-- == What is a 'Chart'?
--
-- A 'Chart' in this library consists of a specification of the first two items in the above list; data and representation.
--
-- Here's some data; three lists of points that form lines to be charted:
--
-- >>> let lines = [[Point 0.0 1.0, Point 1.0 1.0, Point 2.0 5.0],[Point 0.0 0.0, Point 2.8 3.0],[Point 0.5 4.0, Point 0.5 0]]
--
-- and some line styles with different colors in order to distinguish the data:
--
-- >>> let styles = (\c -> defaultLineStyle & #color .~ palette1 c & #size .~ 0.015) <$> [0..2]
-- >>> styles
-- [LineStyle {size = 1.5e-2, color = Colour 0.02 0.73 0.80 1.00, linecap = Nothing, linejoin = Nothing, dasharray = Nothing, dashoffset = Nothing},LineStyle {size = 1.5e-2, color = Colour 0.02 0.29 0.48 1.00, linecap = Nothing, linejoin = Nothing, dasharray = Nothing, dashoffset = Nothing},LineStyle {size = 1.5e-2, color = Colour 0.66 0.07 0.55 1.00, linecap = Nothing, linejoin = Nothing, dasharray = Nothing, dashoffset = Nothing}]
--
-- This is enough to create the charts.
--
-- >>> let cs = zipWith (\s x -> LineChart s [x]) styles lines
-- >>> let lineExample = mempty & #charts .~ named "line" cs & #hudOptions .~ defaultHudOptions :: ChartSvg
-- >>> :t lineExample
-- lineExample :: ChartSvg
--
-- > writeChartSvg "other/usage.svg" lineExample
--
-- ![usage example](other/usage.svg)

-- $hud
--
-- Axes, titles, tick marks, grid lines and legends are chart elements that exist to provide references for the viewer that helps explain the data that is being represented by the chart. They can usually be distinguished from data representations such as a mark where a value is.
--
-- Apart from this representational aspect, hud elements are pretty much the same as data elements. They simulateously have two reference frames: a data domain (tick values reference the data value range) and a page domain (a title needs to be placed on the left say, and has a size of 0.1 versus the page size). They are also typically composed of the same sorts of primitives as data elements, such as rectangles and lines and text and colors.
--
-- Given this similarity, an efficient process for chart creation is roughly:
--
-- - collect the chart data and data annotations into a [Chart]
--
-- - measure the range of the data values
--
-- - begin a process of folding hud elements in, supplying the the data values to the hud elements as needed, and keep track of the overall page size of the chart.
--
-- This process is encapsulated in 'Chart.Hud'.
--
-- An important quality of 'runHud' (and conversion of charts to svg in general) is that this is the point at which chart data is converted from the data domain to the page domain.
--
-- The most common Hud concepts, such as axes and titles, have been collected into the 'HudOptions' type.

-- $optics
--
-- Usage suggests the use of optics-core and OverloadedLabels, but this is not required. 'Chart', 'HudOptions' and associated chart configuration types are big and sometimes deep syntax trees, and simple optics getting, setting and modding makes manipulation more pleasant. Lens works as well, and the library is perfectly capable of being used with records.
--
-- Lenses are supplied, for the optics-core library, but can be easily modified for lens. The chart-svg convention is that lenses are either OverloadedLabels, or suffixed with '.

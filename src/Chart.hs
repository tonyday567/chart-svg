{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK prune #-}

-- | A haskell Charting library targetting SVG.
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
-- Charting consists of three tightly-coupled domains:
--
-- 1. /data domain/: the data to be represented.
-- 2. /screen syntax/: the syntactics of the data. How and where data is to be represented on a screen (or page), and.
-- 3. /the hud/: visual aids that help interpret the screened data; such as axes, gridlines and titles.
--
-- == What is a 'Chart'?
--
-- A 'Chart' in this library consists of a specification of the first two items in the above list; data and its syntax.
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
-- Axes, titles, tick marks, grid lines and legends are chart elements that exist to provide references to help explain the data being represented. The collective noun used by the library for these elements is /hud/. Hud elements can usually be distinguished from data syntax, but need information from the chart domain (data domain and style domain) to function. A tick mark and tick value on an axis need to know the range of the data to be placed properly on the screen. A chart border needs to know the syntactic range of the entire data representation inclusive of representational artifacts that might extend beyond the data domain (A circle glyph representing a point has dimension).
--
-- Apart from this functional usage, however, hud elements are pretty much the same as data elements. They are typically composed of the same stuff; rectangles and lines and text and colors.
--
-- Given this similarity, the library process for chart creation is roughly:
--
-- - collect the chart data and syntax (or style) into a collection of charts (a list or a tree). See 'Chart' and 'ChartTree'
--
-- - measure the range of the data values
--
-- - fold hud elements into a chart, /creating new 'ChartTree' from the hud/, keeping track of chart dimensions across three domains:
--
--   - The domain of the underlying data
--
--   - The domain of the canvas where the data is being represented
--
--   - The domain of the chart, inclusive of Hud or decorative elements.
--
-- This process is reified in 'runHudWith'. The most common Hud concepts, such as axes and titles, have been collected into the 'HudOptions' type.
--
-- An important quality of 'runHud' (and conversion of charts to svg in general) is that this is the point at which chart data is converted from the data domain to the page domain.
--

-- $optics
--
-- Usage suggests the use of optics-core and OverloadedLabels, but this is not required. 'Chart', 'HudOptions' and associated chart configuration types are big and sometimes deep syntax trees, and simple optics getting, setting and modding makes manipulation more pleasant. Lens works as well, and the library is perfectly capable of being used with records.
--
-- Lenses are supplied, for the optics-core library, but can be easily modified for lens. The chart-svg convention is that lenses are either OverloadedLabels, or suffixed with a single quote /'/.

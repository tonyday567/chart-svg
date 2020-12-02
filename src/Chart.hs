{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

-- | A haskell Charting library targetting SVGs
module Chart
  ( -- * Usage
    --
    -- $setup

    -- * Overview
    --
    -- $overview

    -- * Chart
    Chart (..),
    moveChart,
    projectXYs,
    projectXYsWith,
    projectArcInfo,
    
    -- * Annotation
    Annotation (..),
    annotationText,
    scaleAnn,
    padRect,

    -- * Styles
    RectStyle (..),
    defaultRectStyle,
    blob,
    clear,
    border,
    TextStyle (..),
    defaultTextStyle,
    GlyphStyle (..),
    defaultGlyphStyle,
    GlyphShape (..),
    glyphText,
    LineStyle (..),
    defaultLineStyle,
    LineCap (..),
    fromLineCap,
    toLineCap,
    fromDashArray,
    Anchor (..),
    fromAnchor,
    toAnchor,

    -- | Path combinators
    PathType (..),
    ArcDetails (..),
    Marker (..),
    MarkerPos (..),
    PathStyle (..),
    toPathChart,
    PathClosure (..),
    defaultPathStyle,

    -- * Hud types
    --
    -- $hud
    ChartDims (..),
    HudT (..),
    Hud,
    HudOptions (..),
    defaultHudOptions,
    defaultCanvas,
    runHudWith,
    runHud,
    makeHud,
    ChartAspect (..),
    toChartAspect,
    fromChartAspect,
    initialCanvas,
    chartAspectHud,
    canvas,
    title,
    tick,

    -- * Hud primitives
    AxisOptions (..),
    defaultAxisOptions,
    flipAxis,
    Place (..),
    placeText,
    AxisBar (..),
    defaultAxisBar,
    Title (..),
    defaultTitle,
    Tick (..),
    defaultGlyphTick,
    defaultTextTick,
    defaultLineTick,
    defaultTick,
    TickStyle (..),
    defaultTickStyle,
    tickStyleText,
    TickExtend (..),
    adjustTick,
    makeTickDates,
    makeTickDatesContinuous,
    Adjustments (..),
    defaultAdjustments,
    LegendOptions (..),
    defaultLegendOptions,
    legendHud,
    Orientation (..),
    fromOrientation,
    toOrientation,

    -- * SVG primitives
    CssOptions (..),
    SvgOptions (..),
    defaultSvgOptions,
    defaultSvgFrame,

    -- * Chart manipulation
    padChart,
    frameChart,
    hori,
    vert,
    stack,

    -- * Bounding box calculation
    padBox,
    dataBox,
    dataBoxes,
    dataBoxesS,
    styleBox,
    styleBoxes,
    styleBoxesS,
    styleBoxText,
    styleBoxGlyph,

    -- * Re-exports
    module Chart.Render,
    module Chart.Bar,
    module Chart.Surface,
    module Data.Colour,
    module Data.FormatN,
    module Data.Path,
    module NumHask.Space,
  )
where

import Chart.Bar
import Chart.Render
import Chart.Surface
import Chart.Types
import Data.Colour
import Data.FormatN
import Data.Path
import NumHask.Space

-- $setup
--
-- >>> import Chart
--
-- chart-svg works well with "NumHask.Prelude" and "Control.Lens" but neither are necessary.
--
-- >>> :set -XOverloadedLabels
-- >>> :set -XNoImplicitPrelude
-- >>> import Control.Lens
-- >>> import NumHask.Prelude

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
-- >>> :t Chart
-- Chart :: Annotation -> [XY a] -> Chart a
--
-- What exactly is annotation and what is data is highly variant within charting practice. This construction treats position on the XY plane differently from other quantitative manifests such as color and size. The chief advantage of priveliging XY position is that scaling and integrating data with other chart elements becomes much easier. The disadvantage is that, to use quantitative tools such as size, data needs to be consciously separated into that which is position-orientated, and that which is defined as 'Annotation'.
--
--
-- Here's some data; three lists of points that will form a line:
--
-- >>> let xs = [[(0.0, 1.0), (1.0, 1.0), (2.0, 5.0)], [(0.0, 0.0), (3.2, 3.0)], [(0.5, 4.0), (0.5, 0)]] :: [[(Double, Double)]]
-- >>> let ls = fmap (uncurry P) <$> xs
--
-- >>> :t ls
-- ls :: [[XY Double]]
--
-- and an Annotation to describe representation of this data; three line styles with different colors and widths:
--
-- >>> let anns = zipWith (\w c -> LineA (LineStyle w c Nothing Nothing)) [0.015, 0.03, 0.01] palette1
--
-- and this is enough to create a Chart.
--
-- >>> let lineChart = zipWith Chart anns ls
-- >>> :t lineChart
-- lineChart :: [Chart Double]
--
-- Most charts will, in reality, be a list of charts such as this, and much of the library API is designed for this.
--
-- > writeChartSvgDefault "other/lines.svg" lineChart
--
-- ![lines example](other/lines.svg)
--
-- chart-svg takes inspiration from gaming heads-up display aesthetics. Chart decorations such as titles and axes are tools to interpret the landscape of data being viewed. They should be readily transparent, have sane defaults but be fully configurable.
--
-- The library considers a hud to be a recipe for the creation of a 'Chart' list, but with the physical, on-the-page representation of the data in mind.
--
-- Here is the line chart presented with default hud options.
--
-- > writeChartSvgHud "other/lineshud.svg" lineChart
--
-- ![hud example](other/lineshud.svg)
--
-- 'Hud' creation is a process of integrating the data domain and the physical representation. In the chart above, for example, the axis placement takes into account the physical attributes of the thick blue line which extends slightly beyond the abstract data range. The data area (the canvas) has also been extended so that a tick value (3.5 on the x-axis) can be included.
--
-- Beyond this, there is nothing special about hud elements such as tick marks and titles, axes. Once they are created (with 'runHudWith') they themselves become charts.
--

-- $hud
--
-- Axes, titles, tick marks, grid lines and legends are chart elements that exist to provide references for the viewer that helps explain the data that is being represented by the chart. They can be clearly distinguished from data representations such as a mark where a value is.
--
-- Apart from this, hud elements are pretty much the same as data elements. They simulateously have two reference frames: a data domain (tick values reference the data value range) and a page domain (a title needs to be placed on the left say, and has a size of 0.1 versus the page size). They are also typically composed of the same sorts of primitives as data elements, such as rectangles and lines and text and colors.
--
-- Given this similarity, an efficient process for chart creation is roughly:
--
-- - collect the chart data and data annotations into a [Chart Double]
--
-- - measure the range of the data values
--
-- - begin a process of folding a [Hud Double] in with the charts, supplying the the data values to the hud elements if needed, and keeping track of the overall page size of the chart.
--
-- This process is encapsulated in 'runHud'.
--
-- An important quality of 'runHud' (and conversion of charts to svg in general)is that this is the point at which the 'XY's of the chart are converted from the data domain to the page domain. Once the hud and the chart has been integrated there is no going back and the original data is forgotten. This is an opinionated aspect of chart-svg. A counter-example is d3 which stores the raw data in the svg element it represents.


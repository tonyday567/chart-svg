{-# OPTIONS_GHC -Wall #-}

-- | A haskell Charting library targetting SVG
module Chart
  ( -- $overview
    Chart (..),
    Annotation (..),
    annotationText,
    padRect,
    RectStyle (..),
    defaultRectStyle,
    blob,
    clear,
    border,
    TextStyle (..),
    defaultTextStyle,
    Anchor (..),
    fromAnchor,
    toAnchor,
    GlyphStyle (..),
    defaultGlyphStyle,
    GlyphShape (..),
    glyphText,
    LineStyle (..),
    defaultLineStyle,
    PixelStyle (..),
    defaultPixelStyle,
    Direction (..),
    fromDirection,
    toDirection,
    SvgAspect (..),
    toSvgAspect,
    fromSvgAspect,
    EscapeText (..),
    CssOptions (..),
    ScaleCharts (..),
    SvgOptions (..),
    defaultSvgOptions,
    defaultSvgFrame,
    ChartDims (..),
    HudT (..),
    Hud,
    HudOptions (..),
    defaultHudOptions,
    defaultCanvas,
    AxisOptions (..),
    defaultAxisOptions,
    Place (..),
    placeText,
    Bar (..),
    defaultBar,
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
    Adjustments (..),
    defaultAdjustments,
    LegendOptions (..),
    defaultLegendOptions,

    -- $core
    dataBox,
    toAspect,
    scaleAnn,
    moveChart,

    -- $hud
    runHudWith,
    runHud,
    makeHud,
    freezeTicks,
    flipAxis,
    canvas,
    title,
    tick,
    adjustTick,
    makeTickDates,
    makeTickDatesContinuous,
    legendHud,
    legendEntry,
    legendChart,
    legendFromChart,

    -- $svg
    svg,
    svgt,
    chartDef,
    chartDefs,
    styleBox,
    styleBoxes,
    noStyleBoxes,
    styleBoxText,
    styleBoxGlyph,
    padChart,
    frameChart,
    hori,
    vert,
    stack,
    addChartBox,
    addChartBoxes,

    -- * re-exports
    module Chart.Page,
    module Chart.Render,
    module Chart.Bar,
    module Chart.Pixel,
    module Data.Colour,
    module Data.FormatN,
    module NumHask.Space,
  )
where

import Chart.Bar
import Data.FormatN
import Chart.Page
import Chart.Pixel
import Chart.Render
import Chart.Types
import Data.Colour
import NumHask.Space

-- $setup
--
-- >>> :set -XOverloadedLabels
-- >>> :set -XNoImplicitPrelude
-- >>> -- import NumHask.Prelude
-- >>> import Control.Lens

-- $overview
--
-- A chart consists of related conceptual layers:
--
-- 1. the data to be represented
-- 2. the manifestation of the data on the screen
-- 3. visual aids to help interpret the data, such as axes, gridlines and titles.
--
-- >>> :t Chart
-- Chart :: Annotation -> [XY a] -> Chart a
--
-- A 'Chart' in this library specifically consists of
--
-- - a list of values, either 2D points or rectangles (unified as a 'XY')
--
-- - an 'Annotation', which describes the way that the data should be represented on a 2-dimensional plane.
--
-- What exactly is annotation and what is data is highly variant within charting practice. This construction treats position on a plane differently from other quantitative manifestations such as color and size. The chief advantage of priveliging XY position is that scaling and integrating data with other chart elements becomes much easier. The disadvantage is that, to use quantitative tools such as size, data needs to be consciously separated into that which is position orientated, and that which is defined as 'Annotation'.
--
--
-- Here's some data:
--
-- >>> :{
--  let ls = fmap (uncurry P) <$>
--           [ [(0.0, 1.0), (1.0, 1.0), (2.0, 5.0 :: Double)],
--             [(0.0, 0.0), (3.0, 3.0)],
--             [(0.5, 4.0), (0.5, 0)] ]
--  :}
--
-- >>> :t ls
-- ls :: [[XY Double]]
--
-- and an Annotation to describe representation of this data.
--
-- >>> :{
--  let anns = LineA <$>
--             [ defaultLineStyle & #color .~ (palette1 !! 0) & #width .~ 0.015,
--               defaultLineStyle & #color .~ (palette1 !! 1) & #width .~ 0.03,
--               defaultLineStyle & #color .~ (palette1 !! 5) & #width .~ 0.01
--             ]
--  :}
--
-- and this is enough to put together a Chart.
--
-- >>> let lines = zipWith Chart anns ls
-- >>> :t lines
-- lines :: [Chart Double]
--
-- Most charts will, in reality, be a list of charts, and much of the library API is designed for this.
--
-- > writeCharts "other/lines.svg" lines
--
-- ![lines example](other/lines.svg)
--
-- Physical, on-the-page representations of data is separate to need to be considered separately to the domain of the data itself but their concerns bleed into each other. An axis on a chart needs to know the range of the data to render a tick value.
--
-- > writeHudOptionsChart "other/linehud.svg" defaultSvgOptions defaultHudOptions [] lines
--
-- ![hud example](other/linehud.svg)
--
-- chart-svg takes inspiration from gaming heads-up display aesthetics. Chart decorations such as titles and axes are tools to interpret the landscape of data being viewed, should be readily transparent, have sane defaults but be fully configurable.
--
-- The library considers a hud to be a recipe for the creation of a 'Chart' list, in the domain of the viewbox of an svg file or similar conception.
--
-- Being charts all the way down creates a direct mapping between a chart and its representation. The best way to understand this would be to run the example app, which serves a websocket receiving chart options and updating a chart.

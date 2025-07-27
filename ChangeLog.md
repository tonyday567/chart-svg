0.8.2
===

- added Data instances

0.8.1
===

- bumped upper bounds for markup-parse and containers.

0.8
===

- reversed hori, vert; added Align to hori, vert, stack api
- projectWithAspect added
- computeRangeTick added
- exposed legenedChart, freezeAxes & freezeTicks
- renamed anchor ==> anchoring
- added anchoring, achorTo, numStacks & alignCharts to LegendOptions
- added asChartTree, vertCO, horiCO, stackCO
- added beside, besiseChart
- added Align
- renamed Anchor ==> TextAnchor

0.7
===

- switch to harpie

0.6.1
===

- numhask upper bound bumped to 0.12
- switch to doctest-parallel

0.6
===

0.6 is a major refactor.

* Chart

  - refactored Chart from a sum type to a named Style, ChartData pair.
  - in Chart (..) + chartStyle - style
  - introduced ChartData
  - integrated the various *Style types to a single type.
  - introduced partial data lenses: *Data'
  - added chart patterns: *Chart
  - + blankChart1

* ScaleP

  - introduced the ScaleP type
  - added scaleP to Style
  - added the scaleP function
  - - maybeProjectWith + projectChartDataWith
  - - scaleStyle - overText + scaleChartData
  - + scaleRatio

* Chart.Style
  - integrated styles
  - - colourChart + colourStyle
  - + Style (..)
  - + defaultStyle
  - + scaleStyle
  - + default*Style
  - + gpalette -gpalette1
  
* boxes
  - added safeBox', safeStyleBox'

* Chart.Compound

* Factored State out of Hud
  - ChartHud lenses removed: canvasBox', canvasStyleBox', hudBox', hudStyleBox'
  - Introduced HudChartSection type and hudChartBox' which replaces ChartHud lenses.
  - removed runHud
  - removed hud effect functionality: closes, fromEffect, applyChartAspect, getHudBox
  - new hud manipulation tools: appendHud, makeHuds, projectChartTreeWith, addHud, finalCanvas

* Chart.Hud API
  - - axis + axisHud - title + titleHud - legend + legendHud
  - removed placeText
  - added flipPlace
  - + TitleOptions - Title
  - - TickStyle + Tick
  - + axisHud + titleHud
  - - legend - legendFrame
  - new tick lenses: + formatN', numTicks', tickExtend'
  - In LegendOptions: + scaleChartBy - overallScale + legendSize - size
  - In AxisOptions: + adjustments - adjust + axisBar - bar
  
* refactored Priority to a named pair

* Chart.Markup
  - added forgetHud
  - - CssPreferColorScheme + PreferColorScheme
  - - CssShapeRendering + ShapeRendering
  - + defaultCssFontFamilies

* Integrated chart styles to a single Style type

* changed defaults
  - + defaultXAxisOptions + defaultYAxisOptions - defaultAxisOptions
  - - defaultGlyphTick + defaultGlyphTickStyleX + defaultGlyphTickStyleY
  - - defaultTicks + defaultXTicks + defaultYTicks 
  - + defaultTick - defaultTickStyle

* Chart.Bar
  - added barTextCharts
  - added textShiftVert to BarOptions

* Data.Colour
  - + palette - palette1
  - + paletteO - palette1a

* Chart.Surface
  - - surfaceLegendChart
  - - surfaceAxisOptions
  - + surfaceLegendAxisOptions
  - + gridReferenceChart
  - + addSurfaceLegend

* Chart.Data
  - - singletonGuard + isSingleton


0.5.1.1
===
* test suite removed

0.5.1.0
===
* Bumped markup-parse lower bound to >=0.1
* Renamed LegendOptions.content to LegendOptions.legendCharts
* markup* functions now return Markup
* renderStyle added as a MarkupOptions component

0.5.0
===
* Library split into markup-parse and chart-svg.
* test removed

0.4.1
===
* Changes due to numhask-0.11 upgrade
* remove broken surface legend

0.4
===

- Markup type introduced, representing an abstract markup DSL that could be described as simplified but non-compliant XML
  - Chart.Svg replaced by Chart.Markup & Chart.Markup.Parser
  - ChartSvg replaced by ChartOptions
  - functionality includes both printing and parsing.
  - the rendering pipeline is now ChartOptions => Markup => ByteString
- lucid removed as a dependency.
- tree-diff introduced in the test routines.
- flatparse replaces attoparsec
- string-interpolate replaces neat-interpolation

0.3
===

- Chart type rewritten
  - Chart data is no longer a separate element
  - charts are monomorphic (underlying data is Double)
- Aligned with prefer-color-scheme usage
- oklab usage as per emerging CSS standards
- chart-reanimate is a separate library
- formatn is a seprate library
- introduced a ChartTree type as a tree of named charts to facilitate downstream usage of classes.

0.2.2
===

* Changed api for palette

0.2.1
===

* Changed api for reanimate hooks.
* Rationalised default options.

0.2.0
=====

* Reanimate support. See app/reanimate-example.hs
* Data.Path added: support for Path style charts.
* Chart.Examples expanded
* Improvements to documentation.
* web-rep support removed.

0.1.2
=====

* basic charts

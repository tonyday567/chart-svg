0.5.0
===
* Library split into markup-parse and chart-svg.

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

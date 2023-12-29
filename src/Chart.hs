{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_HADDOCK prune #-}

-- | A haskell Charting library targetting SVG.
module Chart
  ( -- * Usage

    --
    -- $usage

    -- * Design
    -- $design

    -- * What is a chart?

    --
    -- $overview

    -- * What is style and what is data?

    --
    -- $fancy

    -- * What is a hud?

    --
    -- $hud

    -- * The primitives

    --
    -- $primitives

    -- * Optics Usage

    --
    -- $optics

    -- * Ecosystem

    --
    -- $ecosystem

    -- * Re-exports
    module Chart.Primitive,
    module Chart.Data,
    module Chart.Hud,
    module Chart.Style,
    module Chart.Markup,
    module Chart.Compound,
    module Chart.Bar,
    module Chart.Surface,
    module Data.Colour,
    module Data.FormatN,
    module Data.Path,
    module Data.Path.Parser,
  )
where

import Chart.Bar
import Chart.Compound
import Chart.Data
import Chart.Hud
import Chart.Markup
import Chart.Primitive
import Chart.Style
import Chart.Surface
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
-- >>> let styles = (\c -> defaultLineStyle & set #color (palette c) & set #size 0.015) <$> [0..2]
-- >>> let cs = zipWith (\s x -> LineChart s [x]) styles lines
-- >>> let lineExample = mempty & set #chartTree (named "line" cs) & set #hudOptions defaultHudOptions :: ChartOptions
-- >>> writeChartOptions "other/usage.svg" lineExample

-- $design
--
-- The library could be cleaved into two parts:
--
-- - An abstract syntax tree (AST) that describes a chart,
--
-- - Functionality that makes a chart given an AST.
--
-- The majority of /work/ from a user perspective is in describing the chart. The library design reflects this and preference has been given to:
--
-- - making the representation concrete,finite and non-recursive. 'ChartOptions' is the broadest type representing a chart and this has a Show instance.
--
-- - providing ease of specification of what can be long trails of lenses leading to a chart component.
--
-- With these design preferences in mind, the complete tree of non-trivial types (in base or IsString instances, say) is contained in the chart below:
-- ![AST](other/AST.svg)
--
-- The underlying SVG is clickable and contains links to the relevant haddocks:  [ChartOptions AST](https://hackage.haskell.org/package/chart-svg/docs/other/ast.svg)
--
-- The labels on the chart arrows represent lenses between components that are container types (eg lists or maybes). Otherwise, arrows represent either generic lenses (such as #chartTree) or lenses supplied by the library (suffixed with a single quote, such as lineData'). As an example, to move all lines contained in line charts left by 1 (in data terms), you could say:
--
-- > lineExample & over (#chartTree % charts' % each % #chartData % lineData' % _Just % each % each % _x) (+1)

-- $overview
--
-- Charting consists of three tightly-coupled domains:
--
-- 1. /the data domain/: the data to be represented.
-- 2. /the screen syntax/: the syntactics of the data. How and where data is to be represented on a screen (or page), and.
-- 3. /the hud/: visual aids that help interpret the screened data; such as axes, gridlines and titles.
--
-- A 'Chart' in this library consists of a specification of the first two items in the above list; data and its syntax.
--
-- Here's some data; three lists of points that form lines to be charted:
--
-- >>> let lines = [[Point 0.0 1.0, Point 1.0 1.0, Point 2.0 5.0],[Point 0.0 0.0, Point 2.8 3.0],[Point 0.5 4.0, Point 0.5 0]]
--
-- and some line styles with different colors in order to distinguish the data:
--
-- >>> let styles = (\c -> defaultLineStyle & #color .~ palette c & #size .~ 0.015) <$> [0..2]
-- >>> styles
-- [Style {size = 1.5e-2, borderSize = 1.0e-2, color = Colour 0.02 0.73 0.80 1.00, borderColor = Colour 0.02 0.29 0.48 1.00, scaleP = NoScaleP, anchor = AnchorMiddle, rotation = Nothing, translate = Nothing, escapeText = EscapeText, frame = Nothing, lineCap = Nothing, lineJoin = Nothing, dasharray = Nothing, dashoffset = Nothing, hsize = 0.6, vsize = 1.1, vshift = -0.25, glyphShape = SquareGlyph},Style {size = 1.5e-2, borderSize = 1.0e-2, color = Colour 0.02 0.29 0.48 1.00, borderColor = Colour 0.02 0.29 0.48 1.00, scaleP = NoScaleP, anchor = AnchorMiddle, rotation = Nothing, translate = Nothing, escapeText = EscapeText, frame = Nothing, lineCap = Nothing, lineJoin = Nothing, dasharray = Nothing, dashoffset = Nothing, hsize = 0.6, vsize = 1.1, vshift = -0.25, glyphShape = SquareGlyph},Style {size = 1.5e-2, borderSize = 1.0e-2, color = Colour 0.66 0.07 0.55 1.00, borderColor = Colour 0.02 0.29 0.48 1.00, scaleP = NoScaleP, anchor = AnchorMiddle, rotation = Nothing, translate = Nothing, escapeText = EscapeText, frame = Nothing, lineCap = Nothing, lineJoin = Nothing, dasharray = Nothing, dashoffset = Nothing, hsize = 0.6, vsize = 1.1, vshift = -0.25, glyphShape = SquareGlyph}]
--
-- This is enough to create the charts.
--
-- >>> let cs = zipWith (\s x -> LineChart s [x]) styles lines
-- >>> let lineExample = mempty & #chartTree .~ named "line" cs & #hudOptions .~ defaultHudOptions :: ChartOptions
-- >>> :t lineExample
-- lineExample :: ChartOptions
--
-- > writeChartOptions "other/usage.svg" lineExample
--
-- ![usage example](other/usage.svg)

-- $fancy
--
-- The division between what is data and what is style is an arbitrary one, in this library and elsewhere.  Hofstadter mused that "content is just fancy form" and this very much applies to charting. Color and glyph shape are just two examples of style used to represent data (See 'Chart.Examples.surfaceExample' and 'Chart.Examples.arrowExample').
--
-- If pushed, the canvas of a chart represents the interactions of two domains: a data domain and a physical space (a screen or a html element or a piece of paper). What is 'ChartData' is practically the data domain represented by the xy-plane that is the canvas. Thus it is a point, a rectangle, a path or some text. Everything else; color, shape, finish and polish, is designated 'Style'.

-- $hud
--
-- A 'Hud' is a type whose lower-case interpretation is as a collective noun for axes, titles, tick marks, grid lines and legends for a 'Chart'. Formally, a 'Hud':
--
-- - is a 'Priority' helping to order the drawing of hud elements, and
--
-- - a function with the signature: 'HudChart' -> 'ChartTree'. Given a tree of charts and huds, provide a 'ChartTree' that represents the hud component.
--
-- A hud does not explicitly represent the data but, instead, exists to provide references to help explain the data being represented. Hud elements can usually be distinguished from data syntax, but need information from the chart domain (data domain and style domain) to function. A tick mark and tick value on an axis need to know the range of the data to be placed properly on the screen. A chart border needs to know the syntactic range of the entire data representation inclusive of representational artifacts that might extend beyond the data domain. A glyph representing a one-dimensional point exists in 2 dimensions, or we wouldn't be able to see it.
--
-- Apart from this functional usage, however, hud elements are pretty much the same as data elements. They are typically composed of the same stuff; rectangles and lines and text and colors.
--
-- Given this similarity, the library process for chart creation is roughly:
--
-- - collect the chart data and syntax (or style) into a collection of charts (a list or a tree). See 'Chart' and 'ChartTree'
--
-- - measure the range of the data values
--
-- - fold hud elements into a chart, creating new 'ChartTree's from the hud, keeping track of chart dimensions:
--
--   1. across the domain of the underlying data
--
--   2. across the domain of the physical chart, inclusive of Hud or decorative elements.
--
-- This protocol is reified in 'runHudWith'. The most common Hud concepts, such as axes and titles, have been collected into the 'HudOptions' type.
--
-- An important quality of 'runHudWith' (and conversion of charts to svg in general) is that this is the point at which chart data is converted from the data domain to the page domain, and is destructive. Typically, at that point of the pipeline, information about the data disappears, so that we no longer can tell what is chart and what is hud.

-- $primitives
--
-- A trick of the library is that huds become processed into charts, and complex charts are just compositions of simpler charts. There are exactly six primitive chart types represented by these patterns:
--
-- - 'RectChart': (list of) rectangles
-- - 'LineChart': (list of list of) points
-- - 'TextChart': (list of) text, point tuples
-- - 'GlyphChart': (list of) points (and 'glyphShape' from 'chartStyle')
-- - 'PathChart': (list of) 'PathData'
-- - 'BlankChart': (list of) rectangles with no stylistic manifestation.

-- $optics
--
-- Usage suggests the use of optics-core and OverloadedLabels, but this is not required. 'Chart', 'HudOptions' and associated chart configuration types are big and sometimes deep syntax trees, and simple optics; getting, setting and modding, makes manipulation more pleasant. Lens works as well, and the library is perfectly capable of being used with records.
--
-- Lenses are supplied, for the optics-core library, but can be easily modified for lens. The chart-svg convention is that lenses are either OverloadedLabels, and thus prefixed with a #, or suffixed with a single quote /'/.

-- $ecosystem
--
-- Charting is a broad concern and several upstream libraries provide aspects of charting that are used within and in conjunction with this library:
--
-- - [numhask-space](https://hackage.haskell.org/package/numhask-space): provides abstract mathematics for the 'Space' domain, including representations of points ('Point') and rectangles ('Rect').
--
-- - [markup-parse](https://hackage.haskell.org/package/markup-parse): provides conversion to and from SVG, HTML and any other XML-like text-based representations.
--
-- - [formatn](https://hackage.haskell.org/package/formatn): provides significant figure and rounding compoutations for Doubles.
--
-- Downstream tools for this library include:
--
-- - [prettychart](https://hackage.haskell.org/package/prettychart): live creation of charts.
--
-- - [dotparse](https://hackage.haskell.org/package/dotparse): creation of charts using graphviz and the dot language.

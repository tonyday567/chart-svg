---
name: chart-svg
description: Composable SVG charting in Haskell. Path: ~/haskell/chart-svg/. optics-core + NumHask + GHC2024.
---

# chart-svg

## Quick start

```haskell
:set -XOverloadedLabels -XOverloadedStrings
import Chart
import Optics.Core

let r = RectChart defaultRectStyle [one]
writeChartOptions "demo.svg" $ mempty & #hudOptions .~ defaultHudOptions & #chartTree .~ unnamed [r]
```

## Six primitives

```haskell
RectChart  Style [Rect Double]           -- filled rectangles
LineChart  Style [[Point Double]]        -- connected line segments
GlyphChart Style [Point Double]          -- glyphs at points
TextChart  Style [(Text, Point Double)]  -- text labels
PathChart  Style [PathData Double]       -- SVG paths (bezier, arc)
BlankChart Style [Rect Double]           -- invisible spacer
```

Style defaults: `defaultRectStyle`, `defaultLineStyle`, `defaultGlyphStyle`, `defaultTextStyle`, `defaultPathStyle`.

## ChartTree composition

```haskell
vert      :: Align -> Double -> [ChartTree] -> ChartTree    -- vertical stack
hori      :: Align -> Double -> [ChartTree] -> ChartTree    -- horizontal stack
stack     :: Int -> Align -> Align -> Double -> [ChartTree] -> ChartTree
beside    :: Align -> Align -> Place -> ChartTree -> ChartTree -> ChartTree

named   :: Text -> [Chart] -> ChartTree      -- labeled leaf
unnamed :: [Chart] -> ChartTree              -- unlabeled leaf
```

## ChartOptions — the full renderable unit

```haskell
co = mempty
  & #hudOptions .~ defaultHudOptions   -- axes, ticks, titles, frame, legends
  & #chartTree  .~ unnamed [charts]    -- the data charts
  & #markupOptions .~ defaultMarkupOptions  -- layout, aspect, CSS

writeChartOptions :: FilePath -> ChartOptions -> IO ()
```

Zero-hud raw SVG: `#hudOptions .~ mempty`.

## Style customization

Use optics to reach into records:

```haskell
defaultLineStyle & set #color (Colour 0.2 0.4 0.6 1.0) & set #size 0.005
```

`Style` fields: `color`, `borderColor`, `size`, `borderSize`, `lineCap`, `glyphShape`.

## Colour

```haskell
Colour r g b a                  -- rgba 0..1
palette :: Double -> Colour     -- viridis via linspace
```

## Patterns

### Domain anchor

Grid lines (`lineTick`) are clipped to the data bounding box (issue [#55](https://github.com/tonyday567/chart-svg/issues/55)). Stretch the domain with an invisible `BlankChart`:

```haskell
domain = BlankChart (defaultStyle & set #color transparent & set #borderSize 0)
                    [Rect xmin xmax ymin ymax]
```

Append to chartTree: `& #chartTree %~ (<> unnamed [domain])`.

### Ticks

Three tick strategies:

```haskell
TickPlaced [(Double, Text)]           -- exact positions + labels
TickRound fmt n TickExtend            -- auto, round numbers, grid lines span full
TickRound fmt n TickLabels            -- auto, round numbers, labels only
```

Common: `TickPlaced` for curated labels, `TickRound (FormatN FSCommaPrec (Just 1) 4 True True) 5 TickExtend` for auto.

### Frame

```haskell
defaultFrameOptions
  & set (#frame %? #borderColor) gridGray   -- border stroke
  & set (#frame %? #color) transparent      -- fill
  & set (#frame %? #borderSize) 5.0e-3      -- stroke width
  & set #buffer 0                            -- no padding
```

### Axis spine control

```haskell
& set #axisBar Nothing               -- hide spine entirely
& set (#ticks % #glyphTick) Nothing  -- no tick marks on spine
& set (#ticks % #lineTick) (Just gridLine)  -- grid lines instead
```

### Hud priority system

Axes, titles, legends, frames are `Priority n x` lists. Lower priority = drawn first (behind). Higher priority items override.

```haskell
defaultHudOptions
  & set #axes   [Priority 5 yAxis, Priority 5 xAxis]
  & set #titles [Priority 5 title]
  & set #frames [Priority 1 frame]
```

## .chart files

`ChartOptions` has `Show`/`Read`. Save and reload:

```haskell
writeFile "my.chart" (show myChartOptions)
co <- read <$> readFile "my.chart"
writeChartOptions "my.svg" co
```

CLI: `prettychart-render input.chart [-o out.svg]`

## SVG ingestion

Foreign SVGs (Stata, R, Matplotlib, Inkscape) → chart-svg combinators:

1. Read the SVG, identify data visually
2. Extract data points into chart-svg types (`Point`, `Rect`, `PathData`)
3. Rebuild with chart-svg primitives and hud
4. Save `.chart` for reuse

Example: `intake/LifeExpectancy.hs` — US life expectancy 1900-1940 from Stata SVG.

## Examples

One `.md` + one or few artifact files per example in `examples/`:

```
examples/seaborn-sine.md      -- doc + inline code
examples/seaborn-sine.svg     -- rendered output
examples/boxplot.md           -- doc + inline code
examples/boxplot.svg          -- rendered output
examples/stackedbar.svg       -- rendered output
```

No `.hs` files in `examples/` — they confuse readers (is it doctested? does it need a cabal entry?). Everything goes in `.md`.

## Doctesting

```bash
cabal-docspec    # evaluates all doctests in full package context
```

Pitfalls:
- Module `where`-block imports not available in docspec. Use `$setup` with `import Chart`.
- Blank `--` line required between last `>>>` and any `-- |` heading.
- Never convert `>>>` to `>` to mask failures. Fix expected output or setup.
- `NoImplicitPrelude` active: if you need Prelude, import it explicitly.

## Build & run

```bash
cabal build          # build library
cabal test           # run test suite
cabal-docspec        # run docspec

# run an example (extract code from .md to a temp file)
cabal repl <<< ':l examples/boxplot.md'
# or copy the code block to a .hs file and run via cabal repl
```

## Cabal

Only executables needed for CI/testing go in `chart-svg.cabal`. Examples in `examples/*.md` are documentation, not build targets.

## Project conventions

- No new modules/files for build fixes — edit existing files.
- No generated output in project root — keep in `examples/` or `intake/`.
- optics-core (not lens): `set`, `view`, `over`, `%`, `.~`.
- `OverloadedLabels` required for `#fieldName`.
- `OverloadedStrings` required for `Text` labels.
- Rect convention: `Rect xmin xmax ymin ymax`.

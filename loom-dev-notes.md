# chart-svg development notes

## Read instances — status (2026-05-28)

Added `Read` to the full chart-svg type hierarchy to enable `.chart` file
serialization (show/read round-trip of `ChartOptions`).

### Packages touched

| Package | Types | Notes |
|---|---|---|
| numhask-space | `Point`, `Rect`, `Range` | `Range` added to deriving clause. `Point` had existing custom `Read` handling paren-wrapped negatives. `Rect` added standalone deriving. |
| numhask | `Polar` | Added `Read` to deriving; needed `Read` import in NumHask.Algebra.Metric |
| formatn | `FormatN`, `FStyle` | Both added to deriving clause |
| circuits-parser | `RenderStyle` | Added to deriving clause |
| chart-svg | 50+ types across 8 files | sed: `deriving (Eq, Show, Generic, Data)` → `deriving (Eq, Show, Read, Generic, Data)`. Plus standalone for `Colour`. |

### What works

- `mempty :: ChartOptions` round-trips (show → read → identical value)
- `CssOptions` round-trips — fields are already `Text`, not `ByteString`
- `Style` round-trips
- `Chart` + `ChartData` round-trip
- `prettychart-render` binary renders simple `.chart` files

### What doesn't round-trip yet

- `HudOptions` with `lineExample` — `axes` returns `False` (value mismatch,
  likely floating-point), `titles` returns `False`, `legends` parses fail
- `LegendOptions` fails `readMaybe` — suspect `legendCharts :: [(Text, [Chart])]`
  field interaction with record syntax parser
- `[Priority LegendOptions]` also fails

### Next steps

1. **Fix `LegendOptions` read** — write a standalone `Read` instance or debug
   the derived one. The `legendCharts` field containing `[(Text, [Chart])]`
   may need special handling in record syntax parsing.
2. **Audit `Show` instances** — some Hud types may have custom `Show` that
   doesn't match derived `Read` format (as `Point` did with paren-wrapping).
   Check `HudOptions`, `AxisOptions`, `TitleOptions`, `Tick`, `LegendOptions`.
3. **Floating-point tolerance** — `axes` and `titles` show `False` on `==`
   comparison despite parsing successfully. This is benign for `.chart` files
   (semantically identical) but needs `Eq`-relaxed comparison in tests.
4. **Maybe change `legendCharts` type** — `[(Text, [Chart])]` is a moment of
   madness; could become `[(Text, ChartTree)]` or a named wrapper. Would
   simplify the Read parser significantly.
5. **Patch releases** — once all types round-trip:
   - numhask-space (Point Read fix already there, Rect+Range new)
   - numhask (Polar Read)
   - formatn (FormatN, FStyle Read)
   - circuits-parser (RenderStyle Read)
   - chart-svg (50+ Read instances + LifeExpectancy example + intake/ pattern)

### New files in chart-svg

- `intake/LifeExpectancy.hs` — destructive SVG→chart-svg round-trip demo
  (US life expectancy 1900-1940, extracted from Stata SVG)
- `intake/TestFields.hs` — round-trip test harness (temporary, for debugging)
- `chart-svg.cabal` — added `life-expectancy` and `test-read` executables

### New in prettychart

- `app/prettychart-render.hs` — render `.chart` files to SVG
  Usage: `prettychart-render file.chart [-o out.svg | -p PORT]`
- `prettychart.cabal` — added `prettychart-render` executable

### Build setup

All five packages wired as local sources in `cabal.project`:
```
chart-svg/cabal.project:
  packages: chart-svg, ../numhask, ../numhask-space, ../formatn, ../circuits-parser

prettychart/cabal.project:
  packages: prettychart, ../chart-svg, ../numhask, ../numhask-space, ../formatn, ../circuits-parser
```

This ensures `Read` instances propagate. Remove local wiring after patch releases.

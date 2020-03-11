{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module Chart.Render
  ( scaleCharts,
    frameChart,
    padChart,
    getAspect,
    getViewbox,
    getSize,
    renderToSvg,
    renderChartsWith,
    renderCharts,
    renderCharts_,
    writeChartsWith,
    writeCharts,
    writeCharts_,
    renderHudChart,
    renderHudOptionsChart,
    writeHudOptionsChart,
  )
where

import Chart.Core
import Chart.Hud (makeHud, runHud)
import Chart.Svg
import Chart.Types
import Control.Lens hiding (transform)
import Data.Generics.Labels ()
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as Lazy
import NumHask.Space hiding (Element)
import Protolude hiding (writeFile)
import Lucid.Svg hiding (z)
import Control.Category (id)
import qualified Lucid.Base as Lucid

-- | scale chart data, projecting to the supplied Rect, and expanding the resultant Rect for chart style if necessary.
--
-- Note that this modifies the underlying chart data.
-- FIXME: do a divide to make an exact fit
scaleCharts ::
  Rect Double ->
  [Chart Double] ->
  (Rect Double, [Chart Double])
scaleCharts cs r = (defRect $ styleBoxes cs', cs')
  where
    cs' = projectSpots cs r

getAspect :: SvgAspect -> [Chart Double] -> Double
getAspect (ManualAspect a) _ = a
getAspect ChartAspect cs = toAspect $ defRect $ styleBoxes cs

getSize :: SvgOptions -> [Chart Double] -> Point Double
getSize o cs = case view #svgAspect o of
  ManualAspect a -> (view #svgHeight o *) <$> Point a 1
  ChartAspect -> (\(Rect x z y w) -> Point (view #svgHeight o * (z - x)) (view #svgHeight o * (w - y))) $ defRect $ styleBoxes cs

getViewbox :: SvgOptions -> [Chart Double] -> Rect Double
getViewbox o cs = case view #svgAspect o of
  ManualAspect a -> Rect (a * (-0.5)) (a * 0.5) (-0.5) 0.5
  ChartAspect -> defRect $ styleBoxes cs

-- * rendering

-- | @svg@ element + svg 2 attributes
svg2_:: Term [Attribute] (s -> t) => s -> t
svg2_ m = svg_ [ Lucid.makeAttribute "xmlns" "http://www.w3.org/2000/svg"
               , Lucid.makeAttribute "xmlns:xlink" "http://www.w3.org/1999/xlink"
               ]
          m

renderToSvg :: CssOptions -> Point Double -> Rect Double -> [Chart Double] -> Svg ()
renderToSvg csso (Point w' h') (Rect x z y w) cs =
  with (svg2_ (bool id (cssCrisp<>) (csso == UseCssCrisp) $ chartDefs cs <> mconcat (svg <$> cs))) [width_ (show w'), height_ (show h'), viewBox_ (show x <> " " <> show (-w) <> " " <> show (z - x) <> " " <> show (w - y))]

cssCrisp :: Svg ()
cssCrisp = style_ [type_ "text/css"] "{ shape-rendering: 'crispEdges'; }"

-- | render Charts with the supplied css options, size and viewbox.
renderCharts_ :: CssOptions -> Point Double -> Rect Double -> [Chart Double] -> Text.Text
renderCharts_ csso p r cs =
  Lazy.toStrict $ prettyText (renderToSvg csso p r cs)

-- | render Charts with the supplied options.
renderChartsWith :: SvgOptions -> [Chart Double] -> Text.Text
renderChartsWith so cs =
  Lazy.toStrict $ prettyText (renderToSvg (so ^. #useCssCrisp) (getSize so cs'') r' cs'')
  where
    r' = r & maybe id padRect (so ^. #outerPad)
    cs'' =
      cs'
        & maybe id (\x -> frameChart x (fromMaybe 1 (so ^. #innerPad))) (so ^. #chartFrame)
    (r, cs') =
      bool
        (getViewbox so cs, cs)
        (scaleCharts (getViewbox so cs) cs)
        (ScaleCharts == so ^. #scaleCharts')

-- | render charts with the default options.
renderCharts :: [Chart Double] -> Text.Text
renderCharts = renderChartsWith defaultSvgOptions

writeChartsWith :: FilePath -> SvgOptions -> [Chart Double] -> IO ()
writeChartsWith fp so cs = Text.writeFile fp (renderChartsWith so cs)

writeCharts :: FilePath -> [Chart Double] -> IO ()
writeCharts fp cs = Text.writeFile fp (renderCharts cs)

-- | write Charts to a file with the supplied css options, size and viewbox.
writeCharts_ :: FilePath -> CssOptions -> Point Double -> Rect Double -> [Chart Double] -> IO ()
writeCharts_ fp csso p r cs =
  Text.writeFile fp (renderCharts_ csso p r cs)

-- * rendering huds and charts
-- | Render some huds and charts.
renderHudChart :: SvgOptions -> [Hud Double] -> [Chart Double] -> Text
renderHudChart so hs cs = renderChartsWith so (runHud (getViewbox so cs) hs cs)

-- | Render a chart using the supplied svg and hud config.
renderHudOptionsChart :: SvgOptions -> HudOptions -> [Hud Double] -> [Chart Double] -> Text
renderHudOptionsChart so hc hs cs = renderHudChart so (hs <> hs') (cs <> cs')
  where
    (hs', cs') = makeHud (defRect $ styleBoxes cs) hc

writeHudOptionsChart :: FilePath -> SvgOptions -> HudOptions -> [Hud Double] -> [Chart Double] -> IO ()
writeHudOptionsChart fp so hc hs cs =
  Text.writeFile fp (renderHudOptionsChart so hc hs cs)

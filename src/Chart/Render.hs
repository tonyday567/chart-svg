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
    renderChartsWith,
    renderCharts,
    writeChartsWith,
    writeCharts,
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

renderToSvg :: Point Double -> Rect Double -> [Chart Double] -> Svg ()
renderToSvg (Point w' h') (Rect x z y w) cs =
  doctype_ <>
  with (svg11_ (cssCrisp <> chartDefs cs <> mconcat (svg <$> cs))) [version_ "1.1", width_ (show w') , height_ (show h'), viewBox_ (show x <> " " <> show (-w) <> " " <> show (z - x) <> " " <> show (w - y))]

cssCrisp :: Svg ()
cssCrisp = style_ [type_ "text/css"] "{ shape-rendering: 'crispEdges'; }"

-- | render Charts with the supplied options.
renderChartsWith :: SvgOptions -> [Chart Double] -> Text.Text
renderChartsWith so cs =
  Lazy.toStrict $ prettyText (renderToSvg (getSize so cs'') r' cs'')
  where
    r' = r & maybe id padRect (so ^. #outerPad)
    cs'' =
      cs'
        & maybe id (\x -> frameChart x (fromMaybe 1 (so ^. #innerPad))) (so ^. #chartFrame)
    (r, cs') =
      bool
        (scaleCharts (getViewbox so cs) cs)
        (getViewbox so cs, cs)
        (ScaleCharts == so ^. #scaleCharts')

-- | render charts with the default options.
renderCharts :: [Chart Double] -> Text.Text
renderCharts = renderChartsWith defaultSvgOptions

writeChartsWith :: FilePath -> SvgOptions -> [Chart Double] -> IO ()
writeChartsWith fp so cs = Text.writeFile fp (renderChartsWith so cs)

writeCharts :: FilePath -> [Chart Double] -> IO ()
writeCharts fp cs = Text.writeFile fp (renderCharts cs)

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

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import Chart
import Chart.Examples hiding (ts)
import Control.Lens
import Control.Monad (void)
import Control.Monad.Trans.State.Lazy
import Data.Biapplicative
import Data.Bool
import Data.Maybe
import Data.Text (Text)
import Network.Wai.Middleware.Static ((>->), addBase, noDots, staticPolicy)
import Web.Page
import Web.Scotty
import Prelude

repMain :: (Monad m) => ChartSvgStyle -> Annotation -> HudConfig -> SharedRep m (Text, Text)
repMain css ann hc =
  repChartsWithSharedData
    css
    hc
    10
    [Chart ann []]
    (fmap (: []) <$> const (repData "sin"))

repNoData :: (Monad m) => ChartSvgStyle -> Annotation -> HudConfig -> SharedRep m (Text, Text)
repNoData css ann hc =
  repChartsWithStaticData
    css
    hc
    10
    [Chart ann [SR (-0.5) 0.5 (-0.5) 0.5]]

repBarChart :: (Monad m) => ChartSvgStyle -> BarData -> BarOptions -> SharedRep m (Text, Text)
repBarChart css bd bo = bimap hmap mmap rcss <<*>> rbd <<*>> rbo <<*>> debugFlags
  where
    rcss = repChartSvgStyle css
    rbo = repBarOptions 5 defaultRectStyle defaultTextStyle bo
    rbd = repBarData bd
    barchartsvg css' bd' bo' =
      renderChartSvgWith
        css'
        (\x -> barChart x bo' bd')
    mmap css' bd' bo' debug =
      ( barchartsvg css' bd' bo',
        debugHtml debug css' (bo' ^. #barHudConfig) (bars bo' bd')
      )
    hmap css' bd' bo' debug =
      accordion_
        "accbc"
        Nothing
        [ ("Svg", css'),
          ("Bar Data", bd'),
          ("Bar Options", bo'),
          ("Debug", debug)
        ]

repPixelChart :: (Monad m) => (ChartSvgStyle, PixelOptions, HudConfig, PixelLegendOptions, Point Double -> Double) ->
  SharedRep m (Text, Text)
repPixelChart (css, po, hc, plo, f) = bimap hmap mmap rcss <<*>> rpo <<*>> rhc <<*>> rplo <<*>> debugFlags
  where
    rcss = repChartSvgStyle css
    rpo = repPixelOptions po
    rhc = repHudConfigDefault hc
    rplo = repPixelLegendOptions plo
    mmap rcss' rpo' rhc' rplo' debug = let (cs,hs) = pixelfl f rpo' rplo' in
      ( renderHudConfigChart rcss' rhc' hs cs,
        debugHtml debug rcss' rhc' [])
    hmap rcss' rpo' rhc' rplo' debug =
      accordion_
        "accpc"
        Nothing
        [ ("Svg", rcss'),
          ("Hud", rhc'),
          ("Pixel Options", rpo'),
          ("Pixel Legend Options", rplo'),
          ("Debug", debug)
        ]

repEx :: (Monad m) => Ex -> SharedRep m (Text, Text)
repEx (Ex css hc maxcs anns xs) = repChartsWithStaticData css hc maxcs (zipWith Chart anns xs)

midChart ::
  SharedRep IO (Text, Text) ->
  Application ->
  Application
midChart sr = midShared sr initChartRender updateChart

initChartRender ::
  Engine ->
  Rep (Text, Text) ->
  StateT (HashMap Text Text) IO ()
initChartRender e r =
  void $
    oneRep
      r
      ( \(Rep h fa) m -> do
          append e "input" (toText h)
          replace e "output" (either id fst . snd $ fa m)
          replace e "debug" (either id snd . snd $ fa m)
      )

updateChart :: Engine -> Either Text (HashMap Text Text, Either Text (Text, Text)) -> IO ()
updateChart e (Left err) = append e "debug" ("map error: " <> err)
updateChart e (Right (_, Left err)) = append e "debug" ("parse error: " <> err)
updateChart e (Right (_, Right (c, d))) = do
  replace e "output" c
  replace e "debug" d

-- main example

main :: IO ()
main =
  scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "other")
    middleware
      ( midChart
          ( repChoice
              0
              [ ( "examples",
                  repChoice
                    0
                    [ ("hockey", repEx hockey),
                      ("unit", repEx oneExample),
                      ("rect", repEx normExample),
                      ("text", repEx textExample),
                      ("glyphs", repEx (makeExample defaultHudConfig glyphs)),
                      ( "mathjax",
                        repEx
                          ( makeExample
                              sinHudConfig
                              glyphsChart &
                              #excss . #escapeText .~ False
                          )
                      ),
                      ("boundText", repEx (makeExample defaultHudConfig boundText)),
                      ("label", repEx (makeExample defaultHudConfig label)),
                      ("glines", repEx (makeExample defaultHudConfig glines)),
                      ("lglyph", repEx (makeExample defaultHudConfig lglyph)),
                      ("glines <> lglyph", repEx (makeExample defaultHudConfig (lglyph <> glines))),
                      ("bar", repBarChart defaultChartSvgStyle barDataExample (defaultBarOptions (fromMaybe [] (barDataExample ^. #barColumnLabels))))
                    ]
                ),
                ( "main",
                  repMain
                    defaultChartSvgStyle
                    (GlyphA defaultGlyphStyle)
                    (defaultHudConfig & #hudTitles .~ [defaultTitle "chart-svg"])
                ),
                ( "pixel",
                  repPixelChart
                  (defaultChartSvgStyle, defaultPixelOptions & #poGrain .~ Point 100 100 & #poRange .~ Rect 1 2 1 2, defaultHudConfig, defaultPixelLegendOptions "pixel test", f1)
                ),
                ( "legend test",
                  repNoData
                    defaultChartSvgStyle
                    BlankA
                    legendTest
                )
              ]
          )
      )
    servePageWith
      ""
      (defaultPageConfig "default")
      (chartStyler True)

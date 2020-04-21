{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

import Chart
import Chart.Examples
import Control.Category (id)
import Control.Lens
import Control.Monad.Trans.State.Lazy
import Network.Wai.Middleware.Static ((>->), addBase, noDots, staticPolicy)
import Protolude hiding ((<<*>>), Rep, replace)
import Web.Page
import Web.Scotty

repNoData :: (Monad m) => SvgOptions -> Annotation -> HudOptions -> SharedRep m (Text, Text)
repNoData css ann hc =
  repChartsWithStaticData css hc 10 [Chart ann [SR (-0.5) 0.5 (-0.5) 0.5]]


repEx :: (Monad m) => Ex -> SharedRep m (Text, Text)
repEx (Ex css hc maxcs anns xs) =
  repChartsWithStaticData css hc maxcs (zipWith Chart anns xs)

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
                    [ ("mempty", repEx memptyExample),
                      ("unit", repEx unitExample),
                      ("hud", repEx hudExample),
                      ("rect", repEx rectExample),
                      ("line", repEx lineExample),
                      ("text", repEx textExample),
                      ("glyph", repEx glyphExample),
                      ("bar", repBarChart defaultSvgOptions barDataExample defaultBarOptions),
                      ("pixel", repPixelChart (defaultSvgOptions, defaultPixelOptions & #poGrain .~ Point 100 100 & #poRange .~ Rect 1 2 1 2, defaultHudOptions, defaultPixelLegendOptions "pixel test", f1))
                    ]
                ),
                ( "stuff",
                  repChoice
                    0
                    [ ("bound text bug", repEx (makeExample defaultHudOptions boundTextBug)),
                      ("compound chart", repEx (makeExample defaultHudOptions (lglyph <> glines))),
                      ("label", repEx (makeExample defaultHudOptions label)),
                      ("legend test", repNoData defaultSvgOptions BlankA legendTest)
                    ]
                ),
                ("main", repEx mainExample)
              ]
          )
      )
    servePageWith "" (defaultPageConfig "default") (chartStyler True)

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

import Chart
import Chart.Examples
import Control.Lens
import Lucid
import NumHask.Prelude hiding (replace)
import Web.Rep

chartServer :: SharedRep IO (Text, Text) -> IO ()
chartServer srep = sharedServer srep defaultSocketConfig (chartStyler True) defaultInputCode chartOutputCode

chartOutputCode :: Either Text (Text, Text) -> IO [Code]
chartOutputCode ea =
  pure $ case ea of
    Left err -> [Append "debug" ("hashmap error: " <> err)]
    Right (chart', debug') ->
      [ Replace "output" chart',
        Replace "debug" debug'
      ]

chartStyler :: Bool -> Page
chartStyler doDebug =
  bootstrapPage
    <> socketPage
    & #htmlHeader .~ title_ "chart styler"
    & #htmlBody
      .~ divClass_
        "container"
        ( divClass_
            "row d-flex justify-content-between"
            ( sec "col4" "input"
                <> sec "col8" "output"
            )
            <> bool mempty (divClass_ "row" (with div_ [id_ "debug"] mempty)) doDebug
        )
  where
    sec d n = divClass_ d (with div_ [id_ n] mempty)

-- main example
main :: IO ()
main =
  chartServer
    ( repChoice
        5
        [ ("mempty", repChartSvg 1 mempty),
          ("unit", repChartSvg 1 unitExample),
          ("hud", repChartSvg 1 hudOptionsExample),
          ("rect", repChartSvg 1 rectExample),
          ("text", repChartSvg 26 textExample),
          ("glyphs", repChartSvg 10 glyphsExample),
          ("line", repChartSvg 3 lineExample),
          ( "surface",
            repSurfaceChart
              ( defaultSvgOptions,
                defaultSurfaceOptions
                  & #soGrain .~ Point 20 20
                  & #soRange .~ Rect 1 2 1 2,
                defaultHudOptions,
                defaultSurfaceLegendOptions "surface test",
                sinCosTan
              )
          ),
          ("bar", repBarChart defaultSvgOptions barDataExample defaultBarOptions),
          ("wave", repChartSvg 3 waveExample),
          ("bound text bug", repChartSvg 10 boundTextBugExample),
          ("compound chart", repChartSvg 10 (lglyphExample <> glinesExample)),
          ("label", repChartSvg 10 labelExample),
          ("legend", repChartSvg 10 legendExample)
        ]
    )

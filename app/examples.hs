{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

import Chart
import Chart.Examples
import Control.Lens
import NumHask.Prelude hiding (replace)
import Web.Page
import Lucid

chartServer :: SharedRep IO (Text, Text) -> IO ()
chartServer srep = sharedServer srep defaultSocketConfig (chartStyler True) defaultInputCode chartOutputCode

chartOutputCode :: Either Text (Text,Text) -> [Code]
chartOutputCode ea = do
    case ea of
        Left err -> [Append "debug" ("hashmap error: " <> err)]
        Right (chart',debug') ->
          [ Replace "output" chart',
            Replace "debug" debug'
          ]

chartStyler :: Bool -> Page
chartStyler doDebug =
  mathjaxSvgPage "hasmathjax"
    <> bootstrapPage
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

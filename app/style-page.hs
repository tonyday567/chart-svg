{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Chart.Core
import Chart.Hud
import Chart.Spot
import Chart.Svg
import Data.HashMap.Strict
import Lucid
import Network.JavaScript
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import NumHask.Data.Range
import Protolude hiding (replace, empty, Rep)
import Web.Page
import Web.Page.Bridge
import Web.Page.Bridge.Rep
import Chart.Page
import Web.Scotty
import qualified Control.Exception as E
import qualified Data.Text.Lazy as Lazy

chartTest :: GlyphStyle -> HudConfig Double -> ChartSvg Double
chartTest gs cfg =
  hud cfg
  (aspect 1.5)
  [ Chart (GlyphA gs) mempty
    (SpotPoint <$> dataXY sin (Range 0 (2*pi)) 30)
  ]

results :: (a -> Text) -> Engine -> a -> IO ()
results r e x = replace e "results" (r x)

logResults :: (a -> Text) -> Engine -> Either Text a -> IO ()
logResults _ e (Left err) = append e "log" err
logResults r e (Right x) = results r e x

midChartTest :: (Show a) => SharedRep IO a -> (a -> Text) -> Application -> Application
midChartTest sc rend = start $ \ ev e -> do
  (Rep h fa, (_, hm)) <- flip runStateT (0,empty) (unrep sc)
  append e "inputs" (Lazy.toStrict $ renderText h)
  either (const $ append e "log" "fa hm stuffed up") (results rend e) (fa hm)
  final <- consumeSharedBridge (logResults rend) hm (either Left fa) ev e
    `E.finally` putStrLn ("finalled" :: Text)
  putStrLn $ ("final value was: " :: Text) <> show final

renderChart :: GlyphStyle -> HudConfig Double -> Text
renderChart gs cfg =
  xmlToText . renderXml (Point (600.0 :: Double) 400) $ chartTest gs cfg

main :: IO ()
main =
  scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "other")
    middleware 
      ( midChartTest (repChart defaultHudConfig)
        (\(gs,cfg) -> renderChart gs cfg))
    servePageWith "/chart/style" defaultPageConfig chartStylePage

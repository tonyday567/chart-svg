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
import Control.Lens
import Data.HashMap.Strict
import Lucid
import Network.JavaScript
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import NumHask.Data.Range
import Protolude hiding (replace, Rep)
import Web.Page
import Web.Page.Bridge
import Web.Page.Rep
import Chart.Page
import Web.Scotty hiding (get, put)
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

initShared
  :: (MonadIO m)
  => MonadState (HashMap Text Text) m
  => Html ()
  -> (HashMap Text Text -> (HashMap Text Text, Either Text a))
  -> (a -> Text)
  -> Engine
  -> m ()
initShared h fa rend e = do
  liftIO $ append e "inputs" (Lazy.toStrict $ renderText h)
  hm0 <- get
  let (hm1, r) = fa hm0
  put hm1
  liftIO $ replace e "results" $ either (const "") rend r

midEvalShared ::
  SharedRep IO a ->
  (Engine -> Either Text a -> IO ()) ->
  (a -> Text) ->
  Application -> Application
midEvalShared s action rend = start $ \ ev e ->
  evalSharedRepOnEvent
  s
  (\h fa -> zoom _2 $ initShared h fa rend e)
  (do
      f <- get
      liftIO $ putStrLn $ ("final value was: " :: Text) <> show f)
  (action e)
  (bridge ev e)

renderChart :: GlyphStyle -> HudConfig Double -> Text
renderChart gs cfg =
  xmlToText . renderXml (Point (600.0 :: Double) 400) $ chartTest gs cfg

main :: IO ()
main =
  scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "other")
    middleware 
      ( midEvalShared (repChart defaultHudConfig)
        (logResults (\(gs,cfg) -> renderChart gs cfg))
        (\(gs,cfg) -> renderChart gs cfg)
      )
    servePageWith "/chart/style" defaultPageConfig chartStylePage

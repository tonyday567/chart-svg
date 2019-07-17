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

import Chart.Core
import Chart.Hud
import Chart.Page
import Chart.Spot
import Chart.Svg
import Control.Category (id)
import Control.Lens
import Data.HashMap.Strict
import Network.JavaScript
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import NumHask.Data.Range
import Protolude hiding (replace, Rep, (<<*>>))
import Data.Biapplicative
import Web.Page
import Web.Scotty
-- import qualified Data.Text as Text
import Lucid.Base
import Lucid hiding (b_)

testPage :: Text -> Text -> [(Text, Html ())] -> Page
testPage title' mid sections =
  bootstrapPage <>
  bridgePage &
  #htmlHeader .~ title_ "iroTestPage" &
  #htmlBody .~ b_ "container" (mconcat
    [ b_ "row" (h1_ (toHtml title'))
    , b_ "row" (h2_ ("middleware: " <> toHtml mid))
    , b_ "row" $ mconcat $ (\(t,h) -> b_ "col" (h2_ (toHtml t) <> with div_ [id_ t] h)) <$> sections
    ])

repMain :: (Monad m) => ChartSvgStyle -> Chart Double -> HudConfig Double -> SharedRep m Text
repMain cscfg ccfg hcfg =
  bimap hmap mmap cs <<*>> ann <<*>> d <<*>> h
  where
    h = repHudConfig hcfg
    cs = repChartSvgStyle cscfg
    ann = repAnnotation (ccfg ^. #annotation)
    d = repData
    mmap cs' ann' d' h' =
      renderChartWith cs' h'
        [Chart ann' mempty d']
    hmap cs' ann' d' h' =
      accordion_ "acca" (Just "Chart Configuration")
      [ ("Chart Svg Stylings", cs')
      , ("Chart", accordion_ "accb" Nothing [("Annotation", ann')
                                            , ("data", d')])
      , ("Chart Hud", h')
      ]

chartSvgTest :: GlyphStyle -> HudConfig Double -> ChartSvg Double
chartSvgTest gs cfg =
  hud cfg
  (aspect 1.5)
  [ Chart (GlyphA gs) mempty
    (SpotPoint <$> dataXY sin (Range 0 (2*pi)) 30)
  ]

chartTest :: Chart Double
chartTest = Chart (GlyphA defaultGlyphStyle) mempty
    (SpotPoint <$> dataXY sin (Range 0 (2*pi)) 30)

toChart' :: GlyphStyle -> Chart Double
toChart' gs =
  Chart (GlyphA gs) mempty
  (SpotPoint <$> dataXY sin (Range 0 (2*pi)) 30)

toChartSvg' :: Chart Double -> HudConfig Double -> ChartSvg Double
toChartSvg' c h = hud h (aspect 1.5) [c]

updateChart :: Engine -> Either Text (HashMap Text Text, Either Text Text) -> IO ()
updateChart e (Left err) = append e "log" ("map error: " <> err)
updateChart e (Right (_,Left err)) = append e "log" ("parse error: " <> err)
updateChart e (Right (_, Right c)) = replace e "output" c

midShared ::
  SharedRep IO Text ->
  Application -> Application
midShared sr = start $ \ ev e ->
  void $ runOnEvent
  sr
  (zoom _2 . initChartRender e)
  (updateChart e)
  (bridge ev e)

initChartRender
  :: Engine
  -> Rep Text
  -> StateT (HashMap Text Text) IO ()
initChartRender e r =
  void $ oneRep r
  (\(Rep h fa) m -> do
      append e "input" (toText h)
      replace e "output" (either id id . snd $ fa m))

renderChart :: Double -> Double -> ChartSvg Double -> Text
renderChart x y =
  xmlToText . renderXml (Point x y)

main :: IO ()
main =
  scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "other")
    middleware
      (midShared (repMain defaultChartSvgStyle chartTest defaultHudConfig))
    servePageWith "/chart/style" defaultPageConfig
      (testPage "chart style" "repMain"
       [ ("input", mempty)
       , ("output", mempty)
       ])

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
import Chart.Hud hiding (title_)
import Chart.Page
import Chart.Spot
import Chart.Svg
import Control.Category (id)
import Control.Lens
import Data.HashMap.Strict
-- import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import Network.JavaScript
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
-- import NumHask.Data.Range
import Protolude hiding (replace, Rep, (<<*>>))
import Data.Biapplicative
import Web.Page
import Web.Scotty
import Lucid.Base
import Lucid hiding (b_)
import NumHask.Data.Range

chartStyler :: Bool -> Page
chartStyler doDebug =
  bootstrapPage <>
  bridgePage &
  #htmlHeader .~ title_ "chart styler" &
  #htmlBody .~
    b_ "container"
    ( b_ "row d-flex justify-content-between"
      ( sec "col4" "input" <>
        sec "col8" "output") <>
      bool mempty (b_ "row" (with div_ [id_ "debug"] mempty)) doDebug)
  where
    sec d n = b_ d (with div_ [id_ n] mempty)

-- writeFile "main.svg" $ renderChartWith defaultChartSvgStyle defaultHudConfig [Chart (GlyphA defaultGlyphStyle) mempty (SpotPoint <$> dataXY sin (Range 0 (2*pi)) 30)]
-- let c2 = [Chart (GlyphA defaultGlyphStyle) mempty (SpotPoint <$> dataXY sin (Range 0 (2*pi)) 30)]
repMain :: (Monad m) => ChartSvgStyle -> Annotation -> HudConfig -> SharedRep m (Text, Text)
repMain cscfg a hcfg =
  bimap hmap mmap cs <<*>> ann <<*>> d <<*>> h <<*>> debug
  where
    h = repHudConfig 2 3 defaultAxisConfig (defaultTitle "example title") defaultLegendOptions hcfg
    cs = repChartSvgStyle cscfg
    ann = repAnnotation a
    d = repData "sin"
    debug = bimap (<>) (,)
      (checkbox (Just "show style values" ) True) <<*>>
      checkbox (Just "show chart svg text" ) False
    mmap cs' ann' d' h' debug' =
      ( renderChartWith cs' h'
        [Chart ann' mempty d']
      , bool mempty
        (mconcat $ (\x -> "<p>" <> x <> "</p>") <$>
         [ "<h2>raw values</h2>"
         , show cs', show ann', show h']) (fst debug')
      <> bool mempty
        (mconcat $ (\x -> "<p>" <> x <> "</p>") <$>
         [ "<h2>raw chart svg</h2>"
         , Lazy.toStrict $ renderText $ toHtml (renderChartWith cs' h' [Chart ann' mempty d'])
         ]) (snd debug')
      )
    hmap cs' ann' d' h' debug' =
      accordion_ "acca" Nothing
      [ ("Svg", cs')
      , ("Annotation", ann')
      , ("Data", d')
      , ("Hud", h')
      , ("Debug", debug')
      ]


-- let cs = (\tps -> [Chart (TextA defaultTextStyle (fst <$> tps)) mempty (snd <$> tps)]) [("text1", SP 0 0),("text2", SP 1 1)] :: [Chart Double]
-- writeFile "c2.svg" $ renderChartWith defaultChartSvgStyle defaultHudConfig cs
repTextBB :: (Monad m) => ChartSvgStyle -> SharedRep m (Text, Text)
repTextBB cscfg =
  bimap hmap mmap cs <<*>> txtstyle <<*>> bb <<*>> tps <<*>> debug
  where
    cs = repChartSvgStyle cscfg
    txtstyle = repTextStyle defaultTextStyle
    bb = repRectStyle (border 0.01 grey 0.01)
    tps = second (fmap (second SpotPoint)) $
      listifyMaybe' (Just "text examples") "te" (checkbox Nothing)
      repTextPoint 5 ("another example", Point 0 0 ) [("test1", Point 0 0), ("test2", Point 1 1)]
    repTextPoint (t, p) = bimap (<>) (,) (textbox Nothing t) <<*>>
      repPoint (Point (Range 0 1) (Range 0 1)) (Point 0.01 0.01) p
    debug = bimap (<>) (,)
      (checkbox (Just "show style values" ) True) <<*>>
      checkbox (Just "show chart svg text" ) False
    txtchart tstyle' tps' =
      [Chart (TextA tstyle' (fst <$> tps')) mempty (snd <$> tps')]
    chartsvg cs' tstyle' tps' bb' =
      renderChartWith cs' defaultHudConfig
      (txtchart tstyle' tps' <>
       boxes bb' (txtchart tstyle' tps'))
    mmap cs' tstyle' bb' tps' debug' =
      ( chartsvg cs' tstyle' tps' bb'
      , bool mempty
        (mconcat $ (\x -> "<p>" <> x <> "</p>") <$>
         [ "<h2>raw values</h2>"
         , show cs', show tstyle', show bb', show tps']) (fst debug')
      <> bool mempty
        (mconcat $ (\x -> "<p>" <> x <> "</p>") <$>
         [ "<h2>raw chart svg</h2>"
         , Lazy.toStrict $ renderText $ toHtml (chartsvg cs' tstyle' tps' bb')
         ]) (snd debug')
      )
    hmap cs' tstyle' bb' tps' debug' =
      accordion_ "acca" Nothing
      [ ("Svg", cs')
      , ("Style", tstyle')
      , ("Box", bb')
      , ("Example Text", tps')
      , ("Debug", debug')
      ]

-- write "legend.svg" (Point 400 400) (hudSvg (aspect 1) [[l]] [Chart (LineA defaultLineStyle) mempty [SP 0 0, SP 1 1]] :: ChartSvg Double)
-- let l = legend (defaultLegendOptions & #lcharts .~ l1)
-- writeFile "legend.svg" $ renderCharts (defaultChartSvgStyle & #chartAspect .~ 1 & #chartFrame .~ Just (border 0.02 grey 1)) $ hudChart (Area 0 1 0 1) [l] [Chart (LineA defaultLineStyle) mempty [SP 0 0, SP 1 1]]
repLegendT :: (Monad m) => ChartSvgStyle -> Annotation -> HudConfig -> SharedRep m (Text, Text)
repLegendT cscfg a hcfg =
  bimap hmap mmap cs <<*>> ann <<*>> d <<*>> h <<*>> debug
  where
    h = repHudConfig 2 3 defaultAxisConfig (defaultTitle "example title") defaultLegendOptions hcfg
    cs = repChartSvgStyle cscfg
    ann = repAnnotation a
    d = repData "sin"
    debug = bimap (<>) (,)
      (checkbox (Just "show style values" ) True) <<*>>
      checkbox (Just "show chart svg text" ) False
    mmap cs' ann' d' h' debug' =
      ( renderChartWith cs' h'
        [Chart ann' mempty d']
      , bool mempty
        (mconcat $ (\x -> "<p>" <> x <> "</p>") <$>
         [ "<h2>raw values</h2>"
         , show cs', show ann', show h']) (fst debug')
      <> bool mempty
        (mconcat $ (\x -> "<p>" <> x <> "</p>") <$>
         [ "<h2>raw chart svg</h2>"
         , Lazy.toStrict $ renderText $ toHtml (renderChartWith cs' h' [Chart ann' mempty d'])
         ]) (snd debug')
      )
    hmap cs' ann' d' h' debug' =
      accordion_ "acca" Nothing
      [ ("Svg", cs')
      , ("Annotation", ann')
      , ("Data", d')
      , ("Hud", h')
      , ("Debug", debug')
      ]

{-
-- let cs = (stackAnns (defaultLegendOptions & #lcharts .~ l1) :: [Chart Double])
-- let cs' = (cs <> boxes (border 0.005 (PixelRGB8 20 80 100) 0.2) cs <> [Chart (RectA (border 0.01 (PixelRGB8 50 50 50) 1)) mempty [SpotArea (widenProp 1.1 (styleBoxes cs))]] <> [(Chart BlankA mempty [(SA 0 1.0 0 1.0)])])
-- let m = [Chart (GlyphA defaultGlyphStyle) mempty (SpotPoint <$> dataXY sin (Range 0 (2*pi)) 30)]
-- writeFile "legend.svg" $ renderCharts (defaultChartSvgStyle & #chartAspect .~ 1 & #chartFrame .~ Just (border 0.02 grey 1)) $ (move (SP 0.7 0.6) $ projectSpotsWith (Area 0 0.2 0 0.2) (Area 0 1 0 1) $ (#annotation %~ scaleAnn 0.2) <$> cs') <> projectSpots (Area 0 1 0 1) m
repLegendT :: (Monad m) => ChartSvgStyle -> LegendOptions Double -> SharedRep m (Text, Text)
repLegendT cscfg lcfg =
  bimap hmap mmap cs <<*>> l <<*>> debug
  where
    h = repHudConfig 2 3 defaultAxisConfig (defaultTitle "example title") defaultLegendOptions hcfg
    ann = repAnnotation a
    d = repData "sin"
    cs = repChartSvgStyle cscfg
    l = repLegend lcfg
    debug = bimap (<>) (,)
      (checkbox (Just "show style values" ) True) <<*>>
      checkbox (Just "show chart svg text" ) False
    chartsvg cs' l' =
      renderChartWith cs' h'
        [Chart ann' mempty d']
    mmap cs' l' debug' =
      ( chartsvg cs' l'
      , bool mempty
        (mconcat $ (\x -> "<p>" <> x <> "</p>") <$>
         [ "<h2>raw values</h2>"
         , show cs', show l']) (fst debug')
      <> bool mempty
        (mconcat $ (\x -> "<p>" <> x <> "</p>") <$>
         [ "<h2>raw chart svg</h2>"
         , Lazy.toStrict $ renderText $ toHtml (chartsvg cs' l')
         ]) (snd debug')
      )
    hmap cs' l' debug' =
      accordion_ "acca" Nothing
      [ ("Svg", cs')
      , ("Legend", l')
      , ("Debug", debug')
      ]

-}

updateChart :: Engine -> Either Text (HashMap Text Text, Either Text (Text, Text)) -> IO ()
updateChart e (Left err) = append e "debug" ("map error: " <> err)
updateChart e (Right (_,Left err)) = append e "debug" ("parse error: " <> err)
updateChart e (Right (_, Right (c,d))) = do
  replace e "output" c
  replace e "debug" d

midShared ::
  SharedRep IO (Text, Text) ->
  Application -> Application
midShared sr = start $ \e ->
  void $ runOnEvent
  sr
  (zoom _2 . initChartRender e)
  (updateChart e)
  (bridge e)

initChartRender
  :: Engine
  -> Rep (Text, Text)
  -> StateT (HashMap Text Text) IO ()
initChartRender e r =
  void $ oneRep r
  (\(Rep h fa) m -> do
      append e "input" (toText h)
      replace e "output" (either id fst . snd $ fa m)
      replace e "debug" (either id snd . snd $ fa m))

renderChart :: Double -> Double -> ChartSvg Double -> Text
renderChart x y =
  xmlToText . renderXml (Point x y)

main :: IO ()
main =
  scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "other")
    middleware
      (midShared
       (repChoice 2
        [("main",
         repMain defaultChartSvgStyle (GlyphA defaultGlyphStyle)
         defaultHudConfig),
         ("bounding box", repTextBB defaultChartSvgStyle),
         ("legend test", repLegendT defaultChartSvgStyle
           (GlyphA defaultGlyphStyle)
           (defaultHudConfig & #hudLegends .~ [defaultLegendOptions & #lcharts .~ l1]))
        ]))
    servePageWith "" defaultPageConfig
      (chartStyler True)

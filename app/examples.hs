{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import Chart
import Chart.Bar
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

repTextBB :: (Monad m) => ChartSvgStyle -> SharedRep m (Text, Text)
repTextBB css =
  bimap hmap mmap rcss <<*>> rtstyle <<*>> rbox <<*>> rtps <<*>> debugFlags
  where
    rcss = repChartSvgStyle css
    rtstyle = repTextStyle defaultTextStyle
    rbox = repRectStyle (border 0.002 (PixelRGB8 115 36 163) 0.5)
    rtps =
      second (fmap (second SpotPoint)) $
        listRep
          (Just "text examples")
          "te"
          (checkbox Nothing)
          repTextPoint
          5
          ("another example", Point 0 0)
          [("12345678901234567890", Point 0 0), ("test2", Point 1 1)]
    repTextPoint (t, p) =
      bimap (<>) (,) (textbox Nothing t)
        <<*>> repPoint (Point (Range 0 1) (Range 0 1)) (Point 0.01 0.01) p
    txtchart ts tps =
      [Chart (TextA ts (fst <$> tps)) (snd <$> tps)]
    boxed ts tps bb =
      txtchart ts tps
        <> boxes bb (txtchart ts tps)
    chartsvg cs ts tps bb =
      renderHudChartWith
        cs
        defaultHudConfig
        (boxed ts tps bb)
    mmap cs ts bb tps debug =
      ( chartsvg cs ts tps bb,
        debugHtml debug cs defaultHudConfig (boxed ts tps bb)
      )
    hmap cs ts bb tps debug =
      accordion_
        "acca"
        Nothing
        [ ("Svg", cs),
          ("Style", ts),
          ("Box", bb),
          ("Example Text", tps),
          ("Debug", debug)
        ]

l1 :: [(Annotation, Text)]
l1 =
  [ (GlyphA defaultGlyphStyle, "glyph"),
    (RectA defaultRectStyle, "rect"),
    (TextA (defaultTextStyle & #anchor .~ AnchorStart) ["content"], "text"),
    (LineA defaultLineStyle, "line"),
    (BlankA, "blank")
  ]

l2 :: [(Annotation, Text)]
l2 =
  [ (GlyphA defaultGlyphStyle, "abcdefghijklmnopqrst")
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
                      ( "glyphs",
                        repEx
                          ( makeExample
                              defaultHudConfig
                              glyphs
                          )
                      ),
                      ( "mathjax",
                        repEx
                          ( makeExample
                              sinHudConfig
                              glyphsChart &
                              #excss . #escapeText .~ False
                          )
                      ),
                      ("boundText", repEx (makeExample defaultHudConfig boundText)),
                      ( "label",
                        repEx
                          ( makeExample
                              defaultHudConfig
                              (label <> [Chart BlankA [SP 0 (0 :: Double)]])
                          )
                      ),
                      ("glines", repEx (makeExample defaultHudConfig glines)),
                      ("lglyph", repEx (makeExample defaultHudConfig lglyph)),
                      ( "glines <> lglyph",
                        repEx
                          ( makeExample
                              defaultHudConfig
                              (lglyph <> glines)
                          )
                      ),
                      ("bar", repBarChart defaultChartSvgStyle barDataExample (defaultBarOptions (fromMaybe [] (barDataExample ^. #barColumnLabels))))
                    ]
                ),
                ( "main",
                  repMain
                    defaultChartSvgStyle
                    (GlyphA defaultGlyphStyle)
                    defaultHudConfig
                ),
                ( "bounding box",
                  repTextBB defaultChartSvgStyle
                ),
                ( "legend test",
                  repNoData
                    defaultChartSvgStyle
                    BlankA
                    ( defaultHudConfig
                        & #hudLegend
                        .~ Just
                          ( LegendManual l1,
                            (defaultLegendOptions :: LegendOptions Double)
                              & #scale .~ (0.3 :: Double)
                              & #lplace .~ PlaceAbsolute (Point (0.0 :: Double) 0.0)
                              & #lsize .~ (0.12 :: Double)
                              & #ltext . #size .~ 0.16
                          )
                    )
                ),
                ( "legend row hori bug",
                  repNoData
                    defaultChartSvgStyle
                    BlankA
                    ( defaultHudConfig
                        & #hudLegend
                        .~ Just
                          ( LegendManual l2,
                            (defaultLegendOptions :: LegendOptions Double)
                              & #scale .~ (0.3 :: Double)
                              & #lplace .~ PlaceAbsolute (Point (0.0 :: Double) 0.0)
                              & #lsize .~ (0.12 :: Double)
                              & #ltext . #size .~ 0.16
                          )
                    )
                )
              ]
          )
      )
    servePageWith
      ""
      (defaultPageConfig "default")
      (chartStyler True)

-- let (Ex css' hc' _ ann' sp') = makeExample sinHudConfig glyphsChart & #excss . #escapeText .~ False in renderPageHtmlToFile "other/mj2.html" (defaultPageConfig "blank") $ chartStyler False & #htmlBody %~ (<> (p_ "\\(x \\over \\pi \\)") <> (toHtmlRaw $ renderHudChartWith css' hc' (zipWith Chart ann' sp')))

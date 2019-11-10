{-# LANGUAGE ApplicativeDo #-}
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

repTextBB :: (Monad m) => ChartSvgStyle -> SharedRep m (Text, Text)
repTextBB css =
  bimap hmap mmap rcss <<*>> rtstyle <<*>> rbox <<*>> rtps <<*>> debugFlags
  where
    rcss = repChartSvgStyle css
    rtstyle = repTextStyle defaultTextStyle
    rbox = repRectStyle (border 0.002 (PixelRGB8 115 36 163) 0.5)
    rtps =
      second (fmap (second SpotPoint)) $
        listifyMaybe'
          (Just "text examples")
          "te"
          (checkbox Nothing)
          repTextPoint
          5
          ("another example", Point 0 0)
          [("test1", Point 0 0), ("test2", Point 1 1)]
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
                      ("glyph", repEx (makeExample mempty glyphs)),
                      ("glyphChart", repEx (makeExample mempty glyphsChart)),
                      ("glyphChart", repEx (makeExample mempty glyphsChart)),
                      ("boundText", repEx (makeExample mempty boundText)),
                      ("label", repEx (makeExample mempty label)),
                      ("glines", repEx (makeExample mempty glines)),
                      ("lglyph", repEx (makeExample mempty lglyph))
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
                  repMain
                    ( defaultChartSvgStyle
                        & #sizex .~ 450
                        & #sizey .~ 360
                    )
                    (GlyphA defaultGlyphStyle)
                    ( defaultHudConfig & #hudLegends
                        .~ [(LegendManual l1, defaultLegendOptions)]
                    )
                )
              ]
          )
      )
    servePageWith
      ""
      defaultPageConfig
      (chartStyler True)

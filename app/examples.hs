{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import Chart
import Chart.Examples
import Control.Lens
import Control.Monad (void)
import Control.Monad.Trans.State.Lazy
import Data.Biapplicative
import Data.Bool
import Data.Text (Text)
import qualified Data.Text as Text
import Network.Wai.Middleware.Static ((>->), addBase, noDots, staticPolicy)
import Web.Page
import Web.Scotty
import Prelude

repMain :: (Monad m) => ChartSvgStyle -> Annotation -> HudConfig -> SharedRep m (Text, Text)
repMain cscfg a hcfg =
  bimap hmap mmap cs <<*>> ann <<*>> d <<*>> h <<*>> debug
  where
    h =
      repHudConfig
        2
        3
        defaultAxisConfig
        (defaultTitle "example title")
        defaultLegendOptions
        (LegendFromChart ["example"])
        BlankA
        ""
        hcfg
    cs = repChartSvgStyle cscfg
    ann = repAnnotation a
    d = repData "sin"
    debug =
      bimap
        (<>)
        (,)
        (checkbox (Just "show style values") True)
        <<*>> checkbox (Just "show chart svg text") False
    mmap cs' ann' d' h' debug' =
      ( let ch = [Chart ann' d'] in renderHudChartWith cs' h' ch,
        bool
          mempty
          ( mconcat $
              (\x -> "<p>" <> x <> "</p>")
                <$> [ "<h2>raw values</h2>",
                      (Text.pack . show) cs',
                      (Text.pack . show) ann',
                      (Text.pack . show) h'
                    ]
          )
          (fst debug')
          <> bool
            mempty
            ( mconcat $
                (\x -> "<p>" <> x <> "</p>")
                  <$> [ "<h2>raw chart svg</h2>",
                        renderHudChartWith cs' h' [Chart ann' d']
                      ]
            )
            (snd debug')
      )
    hmap cs' ann' d' h' debug' =
      accordion_
        "acca"
        Nothing
        [ ("Svg", cs'),
          ("Annotation", ann'),
          ("Data", d'),
          ("Hud", h'),
          ("Debug", debug')
        ]

repTextBB :: (Monad m) => ChartSvgStyle -> SharedRep m (Text, Text)
repTextBB cscfg =
  bimap hmap mmap cs <<*>> txtstyle <<*>> bb' <<*>> tps <<*>> debug
  where
    cs = repChartSvgStyle cscfg
    txtstyle = repTextStyle defaultTextStyle
    bb' = repRectStyle (border 0.002 (PixelRGB8 115 36 163) 0.5)
    tps =
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
    debug =
      bimap
        (<>)
        (,)
        (checkbox (Just "show style values") True)
        <<*>> checkbox (Just "show chart svg text") False
    txtchart tstyle' tps' =
      [Chart (TextA tstyle' (fst <$> tps')) (snd <$> tps')]
    chartsvg cs' tstyle' tps' bb'' =
      renderHudChartWith
        cs'
        defaultHudConfig
        ( txtchart tstyle' tps'
            <> boxes bb'' (txtchart tstyle' tps')
        )
    mmap cs' tstyle' bb'' tps' debug' =
      ( chartsvg cs' tstyle' tps' bb'',
        bool
          mempty
          ( mconcat $
              (\x -> "<p>" <> x <> "</p>")
                <$> [ "<h2>raw values</h2>",
                      (Text.pack . show) cs',
                      (Text.pack . show) tstyle',
                      (Text.pack . show) bb'',
                      (Text.pack . show) tps'
                    ]
          )
          (fst debug')
          <> bool
            mempty
            ( mconcat $
                (\x -> "<p>" <> x <> "</p>")
                  <$> [ "<h2>raw chart svg</h2>",
                        chartsvg cs' tstyle' tps' bb''
                      ]
            )
            (snd debug')
      )
    hmap cs' tstyle' bb'' tps' debug' =
      accordion_
        "acca"
        Nothing
        [ ("Svg", cs'),
          ("Style", tstyle'),
          ("Box", bb''),
          ("Example Text", tps'),
          ("Debug", debug')
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
repEx (Ex css' hc' maxcs' anns' spots') =
  bimap hmap mmap cssr <<*>> annsr
    <<*>> bipure (const (mempty :: Text)) spots'
    <<*>> hr
    <<*>> debug
  where
    hr =
      repHudConfig
        2
        3
        defaultAxisConfig
        (defaultTitle "Single Chart Example")
        defaultLegendOptions
        (LegendFromChart ["example"])
        BlankA
        ""
        hc'
    cssr = repChartSvgStyle css'
    annsr =
      listifyMaybe'
        (Just "Annotations")
        "annz"
        (checkbox Nothing)
        repAnnotation
        maxcs'
        BlankA
        anns'
    debug =
      bimap
        (\a b c -> a <> b <> c)
        (,,)
        (checkbox (Just "show style values") True)
        <<*>> checkbox (Just "show chart svg text") False
        <<*>> checkbox (Just "show Chart value") False
    mmap cs' ann' d' h' debug' =
      ( let ch = zipWith Chart ann' d' in renderHudChartWith cs' h' ch,
        bool
          mempty
          ( mconcat $
              (\x -> "<p>" <> x <> "</p>")
                <$> [ "<h2>raw values</h2>",
                      (Text.pack . show) cs',
                      (Text.pack . show) ann',
                      (Text.pack . show) h'
                    ]
          )
          ((\(a, _, _) -> a) debug')
          <> bool
            mempty
            ( mconcat $
                (\x -> "<p>" <> x <> "</p>")
                  <$> [ "<h2>raw chart svg</h2>",
                        renderHudChartWith cs' h' (zipWith Chart ann' d')
                      ]
            )
            ((\(_, a, _) -> a) debug')
          <> bool
            mempty
            ( mconcat $
                (\x -> "<p>" <> x <> "</p>")
                  <$> [ "<h2>raw Chart value</h2>",
                        Text.pack $ show (zipWith Chart ann' d')
                      ]
            )
            ((\(_, _, a) -> a) debug')
      )
    hmap cs' ann' _ h' debug' =
      accordion_
        "acca"
        Nothing
        [ ("Svg", cs'),
          ("Annotations", ann'),
          ("Hud", h'),
          ("Debug", debug')
        ]

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
                ("bounding box", repTextBB defaultChartSvgStyle),
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

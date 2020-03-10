{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import Chart
import Chart.Examples hiding (ts)
import Control.Lens
import Control.Monad (void)
import Control.Monad.Trans.State.Lazy
import Data.Biapplicative
import Data.Text (Text)
import Network.Wai.Middleware.Static ((>->), addBase, noDots, staticPolicy)
import Web.Page
import Web.Scotty
import Protolude hiding ((<<*>>), Rep, replace)

repMain :: (Monad m) => SvgOptions -> Annotation -> HudOptions -> SharedRep m (Text, Text)
repMain css ann hc =
  repChartsWithSharedData css hc 10 [Chart ann []] (fmap (: []) <$> const (repData "sin"))

repNoData :: (Monad m) => SvgOptions -> Annotation -> HudOptions -> SharedRep m (Text, Text)
repNoData css ann hc =
  repChartsWithStaticData css hc 10 [Chart ann [SR (-0.5) 0.5 (-0.5) 0.5]]

repBarChart :: (Monad m) => SvgOptions -> BarData -> BarOptions -> SharedRep m (Text, Text)
repBarChart css bd bo = bimap hmap mmap rcss <<*>> rbd <<*>> rbo <<*>> debugFlags
  where
    rcss = repSvgOptions css
    rbo = repBarOptions 5 defaultRectStyle defaultTextStyle bo
    rbd = repBarData bd
    barchartsvg css' bd' bo' =
      let (hc', cs') = barChart bo' bd'
       in renderHudOptionsChart css' hc' [] cs'
    mmap css' bd' bo' debug =
      ( barchartsvg css' bd' bo',
        debugHtml debug css' (bo' ^. #barHudOptions) (bars bo' bd')
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

repPixelChart ::
  (Monad m) =>
  (SvgOptions, PixelOptions, HudOptions, PixelLegendOptions, Point Double -> Double) ->
  SharedRep m (Text, Text)
repPixelChart (css, po, hc, plo, f) = bimap hmap mmap rcss <<*>> rpo <<*>> rhc <<*>> rplo <<*>> debugFlags
  where
    rcss = repSvgOptions css
    rpo = repPixelOptions po
    rhc = repHudOptionsDefault hc
    rplo = repPixelLegendOptions plo
    mmap rcss' rpo' rhc' rplo' debug =
      let (cs, hs) = pixelfl f rpo' rplo'
       in ( renderHudOptionsChart rcss' rhc' hs cs,
            debugHtml debug rcss' rhc' []
          )
    hmap rcss' rpo' rhc' rplo' debug =
      accordion_
        "accpc"
        Nothing
        [ ("Svg", rcss'),
          ("Hud", rhc'),
          ("Pixel Options", rpo'),
          ("Pixel Legend Options", rplo'),
          ("Debug", debug)
        ]

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
                    [ ("hockey", repEx hockey),
                      ("unit", repEx oneExample),
                      ("hud", repEx (Ex defaultSvgOptions defaultHudOptions 1 [] [])),
                      ("rect", repEx normExample),
                      ("text", repEx textExample),
                      ("glyphs", repEx (makeExample defaultHudOptions glyphs)),
                      ("boundText", repEx (makeExample defaultHudOptions boundText)),
                      ("label", repEx (makeExample defaultHudOptions label)),
                      ("glines", repEx (makeExample defaultHudOptions glines)),
                      ("lglyph", repEx (makeExample defaultHudOptions lglyph)),
                      ("glines <> lglyph", repEx (makeExample defaultHudOptions (lglyph <> glines))),
                      ("bar", repBarChart defaultSvgOptions barDataExample defaultBarOptions)
                    ]
                ),
                ( "main",
                  repMain defaultSvgOptions (GlyphA defaultGlyphStyle) (defaultHudOptions & #hudTitles .~ [defaultTitle "chart-svg"])
                ),
                ( "pixel",
                  repPixelChart
                    (defaultSvgOptions, defaultPixelOptions & #poGrain .~ Point 100 100 & #poRange .~ Rect 1 2 1 2, defaultHudOptions, defaultPixelLegendOptions "pixel test", f1)
                ),
                ( "legend test",
                  repNoData defaultSvgOptions BlankA legendTest
                )
              ]
          )
      )
    servePageWith "" (defaultPageConfig "default") (chartStyler True)

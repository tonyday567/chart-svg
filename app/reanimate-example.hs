{-# LANGUAGE IncoherentInstances #-}
#!/usr/bin/env stack
-- stack runghc --package reanimate

{- | reanimate example

To run this example:

stack runghc --package reanimate app/reanimate-example.hs

and wait for the browser to open ...

-}

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Chart hiding (renderChartsWith)
import Chart.Examples
-- import Chart.Render (charts)
import Control.Lens hiding (transform)
import NumHask.Prelude hiding (fold)
import Reanimate hiding (scale)
import qualified Reanimate as Re
import Chart.Reanimate
import qualified Graphics.SvgTree.Types as Tree
import qualified Graphics.SvgTree as Tree
import Reanimate.Render

main :: IO ()
main =
  -- proj (const lineExample)
  proj (palette1Example 8 0.7 1.0 "chart-svg")
-- render (stdChartAnimation (const lineExample)) "other/aline.gif" RasterInkscape RenderGif 480 270 10 False

palette1Example :: Int -> Double -> Double -> Text Double -> Double -> ChartSvg
palette1Example n mino maxo t s x = mempty & #chartList .~ take n cs <> ((\x -> Chart (RectA (defaultRectStyle & #color %~ setOpac f')) [RectXY x]) <$> maybeToList (styleBoxes cs))
  where
    cs =
      zipWith (\c x -> Chart (TextA (defaultTextStyle &
                                    #size .~ s & #color .~ setOpac f' c)
                              [t]) [P 0 x])
      p
      (grid OuterPos (one :: Range Double) (fromIntegral l) :: [Double])
    l = length p
    p = [grey, dark, light, black, white] <> palette1
    f' = mino + f * (maxo - mino)

proj :: (Double -> ChartSvg) -> IO ()
proj cs =
  reanimate (stdChartAnimation cs)

stdChartAnimation :: (Double -> ChartSvg) -> Animation
stdChartAnimation cs = pauseAtEnd 2 (mkAnimation 5 (Re.scaleXY 4 (-4) . animChartSvg 5 cs))

animChartSvg :: Double -> (Double -> ChartSvg) -> Double -> Tree.Tree
animChartSvg d cs x =
  groupTree $
  (\(ts,_,_) -> ts) $
  chartSvgTrees $
  mempty &
  #svgOptions . #chartAspect .~ FixedAspect (16/9) &
  #hudOptions .~ dimHudOptions (cs x & view #hudOptions) x &
  #chartList .~ ((#annotation %~ (\ann -> dimAnn ann x)) <$> view #chartList (cs (project (Range zero d) one x)))

animCharts :: Double -> (Double -> [Chart Double]) -> Double -> Tree.Tree
animCharts d cs x =
  groupTree $
  renderChartsWith (defaultSvgOptions & #chartAspect .~ FixedAspect (16/9)) $
  (#annotation %~ flip dimAnn x) <$> cs (project (Range zero d) one x)

tweak :: (Double -> Chart Double) -> Range Double -> Double -> Chart Double
tweak mut (Range l u) x =
  mut x'
  where
    x' = l + (u - l) * x

opac' :: Double -> Colour -> Colour
opac' x (Colour r g b o') = Colour r g b (o'*x)

dimAnn :: Annotation -> Double -> Annotation
dimAnn (RectA s) x = RectA s'
  where
    s' = s & #color %~ opac' x & #borderColor %~ opac' x
dimAnn (TextA s ts) x = TextA s' ts
  where
    s' = s & #color %~ opac' x
dimAnn (LineA s) x = LineA s'
  where
    s' = s & #color %~ opac' x
dimAnn(GlyphA s) x = GlyphA s'
  where
    s' = s & #color %~ opac' x & #borderColor %~ opac' x
dimAnn (PathA s pis) x = PathA s' pis
  where
    s' = s & #color %~ opac' x & #borderColor %~ opac' x
dimAnn BlankA _ = BlankA


dimHudOptions :: HudOptions -> Double -> HudOptions
dimHudOptions ho o =
  ho &
  #hudCanvas %~ fmap (#color %~ opac' o) &
  #hudTitles %~ fmap (#style . #color %~ opac' o) &
  #hudAxes %~ fmap (#axisBar %~ fmap (#rstyle . #color %~ opac' o)) &
  #hudAxes %~ fmap (#axisTick . #gtick %~ fmap (first ((#color %~ opac' o) . (#borderColor %~ opac' o)))) &
  #hudAxes %~ fmap (#axisTick . #ttick %~ fmap (first (#color %~ opac' o))) &
  #hudAxes %~ fmap (#axisTick . #ltick %~ fmap (first (#color %~ opac' o))) &
  #hudLegend %~ fmap (first (#ltext %~ (#color %~ opac' o))) &
  #hudLegend %~ fmap (first (#legendFrame %~ fmap ((#color %~ opac' o) . (#borderColor %~ opac' o)))) &
  #hudLegend %~ fmap (second (fmap (first (\x -> dimAnn x o))))

norm' :: Double -> (Double -> [Chart Double]) -> Double -> Tree.Tree
norm' d f x =
  Main.chartSvg $
  mempty &
  #chartList .~ f (project (Range zero d) one x)


groupTree :: [Tree.Tree] -> Tree.Tree
groupTree x =
     Tree.GroupTree $
     Tree.Group
     mempty
     x
     Nothing
     (Tree.PreserveAspectRatio True Tree.AlignNone Nothing)

chartSvg :: ChartSvg -> Tree.Tree
chartSvg cs = groupTree $ (\(xs,_,_) -> xs) $ chartSvgTrees cs


proj' :: Double -> Range Double -> (Double -> Tree.Tree) -> IO ()
proj' s (Range x1 x2) mk =
  reanimate .
  pauseAtEnd 5 $
  mkAnimation s
  (Re.scaleXY x1 x2 . mk)

circle :: Polar Double Double -> Double -> Colour -> Chart Double
circle pos s c = Chart (GlyphA $ defaultGlyphStyle & set #color c & set #size s) [PointXY $ coord pos]

data Circle = Circle { circlePos :: Polar Double Double, circleSize :: Double, circleColor :: Colour} deriving (Eq, Show, Generic)

circles :: [Circle]
circles = [
  Circle (Polar 1 (pi/4)) 0.05 (Colour 0.4 0.6 0.9 0.4)
          ]

fromFile :: FilePath -> IO Tree.Tree
fromFile fp = do
  t <- Tree.loadSvgFile fp
  pure $ maybe Tree.None Re.unbox t


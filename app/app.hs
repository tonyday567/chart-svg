{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import Prelude
import Chart
import Control.Lens
import Graphics.Svg.CssTypes hiding (Point)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text (Text)
import Data.Maybe

ropts :: [RectStyle]
ropts =
  [ blob (PixelRGB8 93 165 218) 0.5
  , blob (PixelRGB8 120 80 60) 0.5
  ]

rss :: [[Rect Double]]
rss =
  [ gridR (\x -> exp (-(x ** 2) / 2)) (Range -5 5) 50
  , gridR (\x -> 0.5 * exp (-(x ** 2) / 8)) (Range -5 5) 50
  ]

rs :: RectStyle
rs = RectStyle 0.1 (PixelRGB8 102 102 102) 0.5 (PixelRGB8 102 5 102) 0.5

rs' :: RectStyle
rs' = rs &  #opacity .~ 0.1 & #borderOpacity .~ 0.1

oneChart :: Chart Double
oneChart = Chart (RectA rs) [SpotRect unitRect]

oneChart' :: Chart Double
oneChart' = Chart (RectA rs') [SpotRect unitRect]


rotateOne :: ChartSvg Double
rotateOne = defaultFrame $
  chartSvg unitRect [showOrigin] <>
  chartSvg unitRect [oneChart'] <>
  rotateSvg 30 (chartSvg unitRect [oneChart])

translateOne :: ChartSvg Double
translateOne = defaultFrame $
  chartSvg unitRect [showOrigin] <>
  chartSvg unitRect [oneChart'] <>
  translateSvg (Point 1 1) (rotateSvg 30 (chartSvg unitRect [oneChart]))

rectChart :: [Chart Double]
rectChart = take 1 rectCharts

rectCharts :: [Chart Double]
rectCharts =
  zipWith (\s xs -> Chart (RectA s) (SpotRect <$> xs)) ropts rss

-- * text
ts :: [(Text.Text, Point Double)]
ts = zip
  (map Text.singleton ['a' .. 'y'])
  [Point (sin (x * 0.1)) x | x <- [0 .. 25]]

textChart :: Chart Double
textChart =
  Chart
  (TextA (defaultTextStyle & #size .~ (1.0 :: Double)) ["abcdefghij"])
  [p0]

textsChart :: Chart Double
textsChart =
  Chart
  ( TextA
    (defaultTextStyle &
    #size .~ 0.2)
    (fst <$> ts)
  )
  (SpotPoint . snd <$> ts)

circle' :: Chart Double
circle' =
     Chart
      ( GlyphA (defaultGlyphStyle &
        #size .~ 1 &
        #borderSize .~ 0.2))
      [p0]

smiley :: Chart Double
smiley =
     Chart
      ( GlyphA (defaultGlyphStyle &
        #size .~ 1 &
        #borderSize .~ (0.02 :: Double) &
        #shape .~ SmileyGlyph))
      [p0]

glyphs :: [Chart Double]
glyphs = zipWith
     (\(sh, bs) p ->
         Chart
         ( GlyphA (defaultGlyphStyle &
                    #size .~ (0.2 :: Double) &
                    #borderSize .~ bs &
                    #shape .~ sh))
         [p])
     [ (CircleGlyph, 0.01 :: Double)
     , (SquareGlyph, 0.01)
     , (RectSharpGlyph 0.75, 0.01)
     , (RectRoundedGlyph 0.75 0.01 0.01, 0.01)
     , (EllipseGlyph 0.75, 0)
     , (VLineGlyph 0.02, 0)
     , (HLineGlyph 0.02, 0)
     , (SmileyGlyph, 0.01)
     ]
     [SP x 0 | x <- [0..7]]


gdata :: [[Point Double]]
gdata =
  [ gridP sin (Range 0 (2*pi)) 30
  , gridP cos (Range 0 (2*pi)) 30
  ]

gopts :: [GlyphStyle]
gopts =
  [ #borderSize .~ 0.001 $
    #size .~ 0.1 $
    defaultGlyphStyle
  , #borderSize .~ 0.001 $
    #size .~ 0.1 $
    #color .~ PixelRGB8 100 30 30 $
    #shape .~ RectRoundedGlyph 1.5 0.01 (0.01 :: Double) $
    defaultGlyphStyle
  ]

glyphsChart :: [Chart Double]
glyphsChart = zipWith (\d s -> Chart (GlyphA s) (SpotPoint <$> d)) gdata gopts

-- textual
boundText :: [Chart Double]
boundText =
  [ Chart (RectA defaultRectStyle) (SpotRect <$> catMaybes (styleBox <$> cs))
  , Chart a1 ps
  ]
  where
  t1 = fst <$> ts
  ps = projectTo (aspect 3) $ SpotPoint . snd <$> ts
  t1s = TextA (defaultTextStyle & #size .~ 0.2) . (:[]) <$> t1
  cs = zipWith (\x y -> Chart x y) t1s ((:[]) <$> ps)
  a1 = TextA (defaultTextStyle & #size .~ 0.2) t1

pixel' :: (Point Double -> Double) -> [Chart Double]
pixel' f =
  (\(r,c) -> Chart (RectA (RectStyle 0 black 0 c 1)) [SpotRect r]) <$>
  pixelate f (fmap (pi*) unitRect) (Point 100 100) blue grey

f1 :: (Floating a) => Point a -> a
f1 (Point x y) = sin (cos (tan x)) * sin (cos (tan y))

cssCrisp :: CssRule
cssCrisp = CssRule [] [CssDeclaration "shape-rendering" [[CssString "crispEdges"]]]

label :: [Chart Double]
label =
  [ placedLabel (Point (1.0 :: Double) 1.0) (45.0 :: Double) "text at (1,1) rotated by 45 degrees"]

-- * lines
ls :: [[Point Double]]
ls =
  map (uncurry Point) <$>
  [ [(0.0, 1.0), (1.0, 1.0), (2.0, 5.0)]
  , [(0.0, 0.0), (3.0, 3.0)]
  , [(0.5, 4.0), (0.5, 0)]
  ]

lopts :: [LineStyle]
lopts =
  zipWith (\w c -> defaultLineStyle & #color .~ c & #width .~ w)
  [0.015, 0.03, 0.01]
  [ PixelRGB8 197 140 75
  , PixelRGB8 60 127 43
  , PixelRGB8 56 42 140
  ]

lines' :: [Chart Double]
lines' = zipWith (\d s -> Chart (LineA s) (SpotPoint <$> d)) ls lopts

-- gline
gopts3 :: [GlyphStyle]
gopts3 =
  zipWith
  (\x y ->
     #color .~ x $
     #borderColor .~ x $
     #borderSize .~ 0.005 $
     #shape .~ y $
     #size .~ 0.08 $
     defaultGlyphStyle)
  [ PixelRGB8 120 67 30
  , PixelRGB8 30 48 130
  , PixelRGB8 60 60 60
  ]
  [EllipseGlyph 1.5, SquareGlyph, CircleGlyph]

glines :: [Chart Double]
glines = cs <> gs where
  cs = zipWith (\d s -> Chart (LineA s) (SpotPoint <$> d)) ls lopts
  gs = zipWith (\d s -> Chart (GlyphA s) (SpotPoint <$> d)) ls gopts3

lgdata :: [(Text.Text, Point Double)]
lgdata =
  (\p@(Point x y) -> (Text.pack (show x <> "," <> show y), fromIntegral <$> p)) <$>
    (Point <$> [0 .. 5] <*> [0 .. 5] :: [Point Int])

lglyph :: [Chart Double]
lglyph = txt <> gly where
  txt = (\(t, p) -> Chart (TextA
    ( defaultTextStyle &
      #opacity .~ 0.2 &
      #translate ?~ Point 0 0.04) [t])
    (SpotPoint <$> [p]))
    <$> lgdata
  gly = (\d -> Chart (GlyphA
    ( defaultGlyphStyle &
      #size .~ 0.01 &
      #borderSize .~ 0 &
      #color .~ black))
      (SpotPoint <$> [d])) <$> (snd <$> lgdata)

def :: Text
def =
  renderHudChartWith
  defaultChartSvgStyle
  defaultHudConfig
  [Chart (GlyphA defaultGlyphStyle) (SpotPoint <$> gridP sin (Range 0 (2*pi)) 30)]

bb :: Text
bb =
  renderHudChartWith
  defaultChartSvgStyle
  defaultHudConfig $
  (\tps -> let cs = [Chart (TextA defaultTextStyle (fst <$> tps)) (snd <$> tps)] in
      cs <> boxes defaultRectStyle cs) [("text1", SP 0 0),("text2", SP 1 1)]

leg :: ChartSvg Double
leg = hudChartSvg (aspect 1) [[l]] [Chart (LineA defaultLineStyle) [SP 0 0, SP 1 1]]
  where
    l = legend (defaultLegendOptions & #lcharts .~ l1)
    l1 = [ (GlyphA defaultGlyphStyle, "glyph")
         , (RectA defaultRectStyle, "rect")
         , (TextA (defaultTextStyle & #anchor .~ AnchorStart) ["content"], "text")
         , (LineA defaultLineStyle, "line")
         , (BlankA, "blank")
         ]

main :: IO ()
main = do
  writeChart "other/one.svg"
    [Chart (RectA defaultRectStyle)
     [SpotRect (unitRect :: Rect Double)]]
  writeChartSvg "other/rotateOne.svg" (Point 200 200) rotateOne
  writeChartSvg "other/translateOne.svg" (Point 200 200) translateOne
  writeChart "other/rectChart.svg" rectChart
  writeChart "other/rectCharts.svg" rectCharts
  writeWithXml "other/pixel.svg" (Point 200.0 200.0)
    Map.empty "" [cssCrisp] (chartSvg unitRect (pixel' f1))
  writeChart "other/textChart.svg" [textChart]
  writeChart "other/textsChart.svg" [textsChart]
  writeChart "other/boundText.svg" boundText
  writeChart "other/label.svg" label
  writeChart "other/circle.svg" [circle']
  writeChart "other/glyphs.svg" glyphs
  writeChart "other/smiley.svg" [smiley]
  writeChart "other/glyphsChart.svg" glyphsChart
  writeChart "other/lglyph.svg" lglyph
  writeChart "other/lines.svg" lines'
  writeChart "other/glines.svg" glines
  writeChart "other/compound.svg" (lglyph <> glines)
  Text.writeFile "other/def.svg" def
  Text.writeFile "other/bb.svg" bb
  writeChartSvg "other/leg.svg" (Point 400 400) leg
  putStrLn " 👍"

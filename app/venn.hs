{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Chart
import Control.Category (id)
import Control.Lens
import Data.Generics.Labels ()
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import Protolude
import Lucid
import Lucid.Base

xs :: Map.Map Text (Point Double)
xs =
  Map.fromList
    [ ("origin", Point 0 0), -- origin
      ("circle1", Point 0.5 (-0.5 + cos (pi / 6))), -- center of circle 1
      ("circle2", Point 0 (-0.5)), -- center of circle 2
      ("circle3", Point (-0.5) ((-0.5) + cos (pi / 6))), -- center of circle 3
      ("corner1", Point 0 ((-0.5) + 2 * cos (pi / 6))), -- corner 1
      ("corner2", Point 1 (-0.5)), -- corner 2
      ("corner3", Point (-1) (-0.5)) -- corner 3
    ]

vennps :: Text -> (Double, Double)
vennps k = let (Point x y) = xs Map.! k in (x, - y)

moveA :: Double -> Double -> Text
moveA x y = "M" <> show x <> "," <> show y

data Arc = Arc {arcXr :: Double, arcYr :: Double, arcRot :: Double, arcLargeArcFlag :: Bool, arcSweepFlag :: Bool, arcX :: Double, arcY :: Double} deriving (Eq, Show, Generic)

arcA_ :: Arc -> Text
arcA_ a = show (view #arcXr a) <> " " <> show (view #arcYr a) <> " " <> show (view #arcRot a) <> " " <> bool "0" "1" (view #arcLargeArcFlag a) <> " " <> bool "0" "1" (view #arcSweepFlag a) <> " " <> show (view #arcX a) <> "," <> show (view #arcY a)

arcA :: [Arc] -> Text
arcA as = "A" <> Text.intercalate " " (arcA_ <$> as)

outerseg1 :: Text
outerseg1 =
  Text.intercalate
    " "
    [ uncurry moveA (vennps "corner1"),
      arcA
        [ uncurry (Arc 0.5 0.5 0 True True) (vennps "corner2"),
          uncurry (Arc 1 1 0 False False) (vennps "circle1"),
          uncurry (Arc 1 1 0 False False) (vennps "corner1")
        ],
      "Z"
    ]

outerseg2 :: Text
outerseg2 =
  Text.intercalate
    " "
    [ uncurry moveA (vennps "corner3"),
      arcA
        [ uncurry (Arc 0.5 0.5 0 True False) (vennps "corner2"),
          uncurry (Arc 1 1 0 False True) (vennps "circle2"),
          uncurry (Arc 1 1 0 False True) (vennps "corner3")
        ],
      "Z"
    ]

outerseg3 :: Text
outerseg3 =
  Text.intercalate
    " "
    [ uncurry moveA (vennps "corner3"),
      arcA
        [ uncurry (Arc 0.5 0.5 0 True True) (vennps "corner1"),
          uncurry (Arc 1 1 0 False False) (vennps "circle3"),
          uncurry (Arc 1 1 0 False False) (vennps "corner3")
        ],
      "Z"
    ]

innerseg :: Text
innerseg =
  Text.intercalate
    " "
    [ uncurry moveA (vennps "circle1"),
      arcA
        [ uncurry (Arc 1 1 0 False True) (vennps "circle2"),
          uncurry (Arc 1 1 0 False True) (vennps "circle3"),
          uncurry (Arc 1 1 0 False True) (vennps "circle1")
        ],
      "Z"
    ]

midseg1 :: Text
midseg1 =
  Text.intercalate
    " "
    [ uncurry moveA (vennps "corner1"),
      arcA
        [ uncurry (Arc 1 1 0 False True) (vennps "circle1"),
          uncurry (Arc 1 1 0 False False) (vennps "circle3"),
          uncurry (Arc 1 1 0 False True) (vennps "corner1")
        ],
      "Z"
    ]

midseg2 :: Text
midseg2 =
  Text.intercalate
    " "
    [ uncurry moveA (vennps "circle1"),
      arcA
        [ uncurry (Arc 1 1 0 False True) (vennps "corner2"),
          uncurry (Arc 1 1 0 False True) (vennps "circle2"),
          uncurry (Arc 1 1 0 False False) (vennps "circle1")
        ],
      "Z"
    ]

midseg3 :: Text
midseg3 =
  Text.intercalate
    " "
    [ uncurry moveA (vennps "circle2"),
      arcA
        [ uncurry (Arc 1 1 0 False True) (vennps "corner3"),
          uncurry (Arc 1 1 0 False True) (vennps "circle3"),
          uncurry (Arc 1 1 0 False False) (vennps "circle2")
        ],
      "Z"
    ]

vennGlyphs :: [Text]
vennGlyphs = [outerseg1, outerseg2, outerseg3, midseg1, midseg2, midseg3, innerseg]

seg :: Text -> Colour -> GlyphStyle
seg p c = defaultGlyphStyle & set #shape (PathGlyph p) & set #color c & set #borderColor white & set #borderSize 0.06

venns :: [Chart Double]
venns = zipWith (\p c -> Chart (GlyphA $ seg p c) [SP 0.0 0.0]) vennGlyphs palette

phrases :: [Chart Double]
phrases = phraseChart <$> mainPhrases

data Phrase
  = Phrase
      { phraseText :: Text,
        phrasePosition :: Point Double,
        phraseSize :: Double,
        phraseRotation :: Double,
        phraseColor :: Colour,
        phraseTag :: Text,
        phraseLevel :: Int
      }
  deriving (Eq, Show, Generic)

phraseChart :: Phrase -> Chart Double
phraseChart p = Chart (TextA a [view #phraseText p]) [SpotPoint (view #phrasePosition p)]
  where
    a =
      defaultTextStyle
        & set #size (view #phraseSize p)
        & set #rotation (Just $ view #phraseRotation p)
        & set #color (view #phraseColor p)

mainPhrases :: [Phrase]
mainPhrases =
  [ Phrase "Composable" (Point 0.9 0.7) 0.16 60 c "composable" 1,
    Phrase "Functional" (Point 0 (-1)) 0.16 0 c "functional" 1,
    Phrase "Open" (Point (-1) 0.55) 0.16 (-60) c "open" 1,
    Phrase "Accurate" (Point 0.6 (-0.4)) 0.16 0 c "accurate" 1,
    Phrase "Dynamic" (Point (-0.6) (-0.4)) 0.16 0 c "dynamic" 1,
    Phrase "Modern" (Point 0 0.7) 0.16 0 c "modern" 1,
    Phrase "chart-svg" (Point 0 0) 0.2 0 c "chart-svg" 1
  ]
  where
    c = black

renderToSvgt :: CssOptions -> Point Double -> Rect Double -> [Chart Double] -> [(TextStyle, Text)] -> Html ()
renderToSvgt csso (Point w' h') (Rect x z y w) cs tts =
  with
  (svg2_ (bool id (cssCrisp <>) (csso == UseCssCrisp) $
          chartDefs cs <>
          mconcat (zipWith svgt cs tts)))
  [width_ (show w'),
   height_ (show h'),
   makeAttribute "viewBox" (show x <> " " <> show (- w) <> " " <> show (z - x) <> " " <> show (w - y))]

writeVennWords :: IO ()
writeVennWords =
  writeFile "other/venn2.svg" $ Lazy.toStrict $ renderText $
  renderToSvgt NoCssOptions (Point 300 300) (Rect (-2) 2 (-2) 2)
  (phrases <> venns <> [Chart BlankA [SR (-2.0) 2.0 (-2.0) 2.0]]) $
  (defaultTextStyle & set #color colorText,) <$>
  (replicate 7 "" <> (phraseText <$> mainPhrases) <> [""])

writeVenn :: [Colour] -> IO ()
writeVenn cs = writeChartsWith "other/venn.svg" (defaultSvgOptions & set #scaleCharts' NoScaleCharts & set #svgAspect ChartAspect & set #svgHeight 100) ([phraseChart (Phrase "λ" (Point 0 (-0.2)) 0.8 0 (Colour 0.1 0 0.2 1) "chart-svg" 1)] <> (zipWith (\p c -> Chart (GlyphA $ seg p c) [SP 0.0 0.0]) [outerseg1, outerseg2, outerseg3, midseg1, midseg2, midseg3] cs) <> [Chart BlankA [SR (-1.5) 1.5 (-1.5) 1.5]])

main :: IO ()
main = do
  writeVennWords
  writeVenn palette

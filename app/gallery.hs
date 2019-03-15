{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}
  
import Chart.Svg
import NumHask.Prelude hiding (Text)
import Lens.Micro 
import qualified Data.Text as Text
import Data.Generics.Product (field)
import Chart.Hud
import Chart.Core
import Chart.Spot
import Data.List (zipWith3)
import Graphics.Svg.Types (TextAnchor(..))

tri1 :: Int -> Int -> Double -> [Chart Double]
tri1 a b s =
  [ Chart
    (GlyphA
      (defaultGlyphStyle &
        field @"shape" .~ TriangleGlyph (Point 0 0)
        (Point (fromIntegral a) 0)
        (Point (fromIntegral a) (fromIntegral b)) &
        field @"borderSize" .~ 0.01 & field @"size" .~ s))
    (translateDA c)
    [SpotPoint $ Point (fromIntegral a) (fromIntegral b)]
  ]
  where
    c :: Point Double
    c = Point (-s*(fromIntegral a*2)/3) (-s*fromIntegral b/3)

tri2 :: Integer -> Integer -> Double -> Double -> Double -> Double -> [Chart Double]
tri2 a b s gap bs txts =
  [ Chart
    (GlyphA
      (defaultGlyphStyle &
        field @"shape" .~ TriangleGlyph (Point 0 0)
        (Point (fromIntegral a) 0)
        (Point (fromIntegral a) (fromIntegral b)) &
        field @"borderSize" .~ bs & field @"size" .~ s))
    (translateDA c)
    [SpotPoint $ Point (fromIntegral a) (fromIntegral b)]
  ] <>
  zipWith3
  (\side bump ts' -> Chart
    (TextA ts' (show <$> [side]))
    (translateDA $ c + bump)
    [SpotPoint $ Point (fromIntegral a) (fromIntegral b)])
  [a,b,h]
  [ Point (s * fromIntegral a / 2) ((-gap) -
              0.65 *
              fromRational (ts ^. field @"vsize") *
              fromRational (ts ^. field @"size"))
  , Point (s * fromIntegral a + gap) (s * fromIntegral b / 2)
  , Point (s * fromIntegral a / 2 - gap * ((fromIntegral b) ** (2.0 :: Double) / (fromIntegral h) ** (2 :: Double))) (s * fromIntegral b / 2 + gap * ((fromIntegral a) ** (2.0 :: Double) / (fromIntegral h) ** (2 :: Double)))
  ]
  [ ts
  , ts & field @"alignH" .~ TextAnchorStart
  , ts & field @"rotation" .~ Just (-(atan (fromIntegral b/fromIntegral a)) * (180 / pi))
  ]
  where
    c :: Point Double
    c = Point (-s*(fromIntegral a*2)/3) (-s*fromIntegral b/3)
    h :: Integer
    h = round $ sqrt (fromIntegral (a^(2::Integer)+b^(2::Integer)) :: Float) :: Integer
    ts = defaultTextStyle & field @"size" .~ txts

tri3 :: Integer -> Integer -> Double -> Double -> Double -> Double -> [Chart Double]
tri3 a b s gap bs txts =
  [ Chart
    (GlyphA
      (defaultGlyphStyle &
        field @"shape" .~ TriangleGlyph (Point 0 0)
        (Point (fromIntegral a / ns) 0)
        (Point (fromIntegral a / ns) (fromIntegral b / ns)) &
        field @"borderSize" .~ bs & field @"size" .~ s))
    (translateDA ((/ns) <$> c))
    [SpotPoint $ Point (fromIntegral a) (fromIntegral b)]
  ] <>
  zipWith3
  (\side bump ts' -> Chart
    (TextA ts' (show <$> [side]))
    (translateDA $ (/ns) <$> (c + bump))
    [SpotPoint $ Point (fromIntegral a) (fromIntegral b)])
  [a,b,h]
  [ Point (s * fromIntegral a / 2) ((-gap) -
              0.65 *
              fromRational (ts ^. field @"vsize") *
              fromRational (ts ^. field @"size"))
  , Point (s * fromIntegral a + gap) (s * fromIntegral b / 2)
  , Point (s * fromIntegral a / 2 - gap * ((fromIntegral b) ** (2.0 :: Double) / (fromIntegral h) ** (2 :: Double))) (s * fromIntegral b / 2 + gap * ((fromIntegral a) ** (2.0 :: Double) / (fromIntegral h) ** (2 :: Double)))
  ]
  [ ts
  , ts & field @"alignH" .~ TextAnchorStart
  , ts & field @"rotation" .~ Just (-(atan (fromIntegral b/fromIntegral a)) * (180 / pi))
  ]
  where
    c :: Point Double
    c = Point (-s*(fromIntegral a*2)/3) (-s*fromIntegral b/3)
    h :: Integer
    h = round $ sqrt (fromIntegral (a^(2::Integer)+b^(2::Integer)) :: Float) :: Integer
    ts = defaultTextStyle & field @"size" .~ txts
    ns = log $ sqrt $ fromIntegral a * fromIntegral b / 2

tri2s :: Double -> Double -> Double -> Double -> [(Integer,Integer)] -> [Chart Double]
tri2s s gap bs txts ab =
  mconcat $ (\(a, b) -> tri2 a b s gap bs txts) <$> ab

tri2ss :: Double -> Double -> Double -> Double -> Maybe (Area Double) -> Integer -> ChartSvg Double
tri2ss s gap bs txts r n =
  hudSvgWith one area hud1 (tri2s s gap bs txts psf)
  where
    ps = euclid n <> ((\(a,b) -> (a,-b)) <$> euclid n)
    psf = maybe ps (\(Area x z y w) -> filter (\(a,b) -> fromIntegral a >= x && fromIntegral a <= z && fromIntegral b >= y && fromIntegral b <= w) ps) r
    area = maybe (toArea $ mconcat $ (\(a,b) -> SpotPoint (Point (fromIntegral a) (fromIntegral b))) <$> ps) identity r

tri3s :: Double -> Double -> Double -> Double -> [(Integer,Integer)] -> [Chart Double]
tri3s s gap bs txts ab =
  mconcat $ (\(a, b) -> tri3 a b s gap bs txts) <$> ab

tri3ss :: Double -> Double -> Double -> Double -> Maybe (Area Double) -> Integer -> ChartSvg Double
tri3ss s gap bs txts r n =
  hudSvgWith one area hud1 (tri3s s gap bs txts psf)
  where
    ps = euclid n <> ((\(a,b) -> (a,-b)) <$> euclid n)
    psf = maybe ps (\(Area x z y w) -> filter (\(a,b) -> fromIntegral a >= x && fromIntegral a <= z && fromIntegral b >= y && fromIntegral b <= w) ps) r
    area = maybe (toArea $ mconcat $ (\(a,b) -> SpotPoint (Point (fromIntegral a) (fromIntegral b))) <$> ps) identity r

corners :: Area Double -> Double -> [Chart Double]
corners (Area x z y w) s =
  [Chart
  (GlyphA $
   field @"borderSize" .~ 0 $
    field @"size" .~ s $
    defaultGlyphStyle)
  mempty
  [SP x y, SP x w, SP z y, SP z w]]

hud1 :: [Hud Double]
hud1 =
  [ -- canvas (blob grey 0.05) mempty
    tick defaultTick mempty <>
    tick ((field @"place" .~ PlaceLeft :: Tick Double -> Tick Double)
          defaultTick) mempty
  ]

euclid :: Integer -> [(Integer,Integer)]
euclid x = filter (\(a,b) -> a/=0 && b/=0) $ (\m n -> (m*m - n*n, 2*m*n)) <$> [1..x] <*> [1..x] :: [(Integer,Integer)]

main :: IO ()
main = do
  write "other/tri1.svg" (Point 400 400)
    (pad 1.1 $ hudSvg one hud1 (tri1 3 4 0.1 <> corners (Area 0 3 0 4) 0.1))
  write "other/tri2.svg" (Point 400 400)
    (pad 1.1 $ hudSvgWith one (Area 0 20 0 20) hud1 (tri2 5 12 0.05 0.025 0.01 0.01))
  write "other/tri2s.svg" (Point 400 400)
    (pad 1.1 (tri2ss 0.00004 0.0001 0 0 (Just (Area 0 3000 0 3000)) 60))
  write "other/tri3s.svg" (Point 400 400)
    (pad 1.1 (tri3ss 0.0001 0.0001 0 0 (Just (Area 0 4000 0 4000)) 100))
  putStrLn (" 👍" :: Text.Text)


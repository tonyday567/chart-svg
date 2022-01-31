{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Colour representations and combinations, based on oklab
module Data.Colour.Mix
  where

import Graphics.Color.Model as M hiding (LCH)
import qualified Graphics.Color.Space as S
import System.Random
import System.Random.Stateful
import Data.Bool (bool)
import Data.Colour
import Data.Text (Text)
import NeatInterpolation
import qualified Data.Text as Text
import Chart.Data
import NumHask.Algebra.Metric
import NumHask.Array.Fixed
import Data.Functor.Rep
import GHC.Exts
import Optics.Core
import qualified Data.List as List

-- $setup
--
-- >>> :set -XOverloadedLabels
-- >>> :set -XOverloadedStrings
-- >>> import Chart
-- >>> import Optics.Core

-- | LCH colour representation
--
newtype LCH a = LCH' { lchArray :: Array '[3] a } deriving (Eq, Show, IsList, Functor)

pattern LCH :: a -> a -> a -> LCH a
pattern LCH l c h <- LCH' [l,c,h] where
  LCH l c h = LCH' [l,c,h]
{-# COMPLETE LCH #-}

l' :: Lens' (LCH Double) Double
l' = lens (\(LCH l _ _) -> l) (\(LCH _ c h) l -> LCH l c h)

c' :: Lens' (LCH Double) Double
c' = lens (\(LCH _ c _) -> c) (\(LCH l _ h) c -> LCH l c h)

h' :: Lens' (LCH Double) Double
h' = lens (\(LCH _ _ h) -> h) (\(LCH l c _) h -> LCH l c h)

-- | LCHA representation
data LCHA = LCHA' { _lch :: LCH Double, _alpha :: Double } deriving (Eq, Show)

lch' :: Lens' LCHA (LCH Double)
lch' = lens (\(LCHA' lch _) -> lch) (\(LCHA' _ a) lch -> LCHA' lch a)

alpha' :: Lens' LCHA Double
alpha' = lens (\(LCHA' _ a) -> a) (\(LCHA' lch _) a -> LCHA' lch a)

pattern LCHA :: Double -> Double -> Double -> Double -> LCHA
pattern LCHA l c h a <- LCHA' (LCH' [l,c,h]) a where
  LCHA l c h a = LCHA' (LCH' [l,c,h]) a
{-# COMPLETE LCHA #-}

-- * RGB colour representation
newtype RGB3 a = RGB3' { rgb3Array :: Array '[3] a } deriving (Eq, Show, IsList, Functor)

pattern RGB3 :: a -> a -> a -> RGB3 a
pattern RGB3 r g b <- RGB3' [r,g,b] where
  RGB3 r g b = RGB3' [r,g,b]
{-# COMPLETE RGB3 #-}

rgbd' :: Iso' (RGB3 Double) (RGB3 Word8)
rgbd' = iso (fmap (floor . (*256))) (fmap (\x -> fromIntegral x / 256.0))

rgb32colour' :: Iso' (RGB3 Double, Double) Colour
rgb32colour' = iso (\(RGB3 r g b, a) -> Colour r g b a) (\(Colour r g b a) -> (RGB3 r g b, a))

-- * LAB colour representation
newtype LAB a = LAB' { labArray :: Array '[3] a } deriving (Eq, Show, IsList, Functor)

pattern LAB :: a -> a -> a -> LAB a
pattern LAB l a b <- LAB' [l,a,b] where
  LAB l a b = LAB' [l,a,b]
{-# COMPLETE LAB #-}

-- * Colour conversions

-- * lcha to colour

-- | LCHA to Colour lens
--
-- >>> c0 = Colour 0.78 0.36 0.02 1.00
-- >>> view (re lcha2colour') c0
-- LCHA' {_lch = LCH' {lchArray = [0.5969891006896103, 0.15793931531669247, 49.191113810479784]}, _alpha = 1.0}
--
-- >>> view (re lcha2colour' % lcha2colour') c0
-- Colour 0.78 0.36 0.02 1.00
--
-- >>> c1 = Colour 0.49 0.14 0.16 1
-- >>> view (re lcha2colour') c1
-- LCHA' {_lch = LCH' {lchArray = [0.40115567099848914, 0.12279066817938503, 21.51476756026837]}, _alpha = 1.0}
--
-- >>> view (re lcha2colour' % lcha2colour') c1
-- Colour 0.49 0.14 0.16 1.00
lcha2colour' :: Iso' LCHA Colour
lcha2colour' =
  iso
  (\(LCHA' lch a) -> let (RGB3 r g b) = view (re lab2lch' % re rgb2lab') lch in Colour r g b a)
  (\c@(Colour _ _ _ a) -> LCHA' (view (re rgb32colour' % _1 % rgb2lab' % lab2lch') c) a)

-- * lab to lch
xy2ch' :: Iso' (Double, Double) (Double, Double)
xy2ch' =
  iso
  (\(x,y) -> (norm (Point x y), 180 / pi * mod_ (angle (Point x y)) (2 * pi)))
  (\(c,h) -> let (Point x y) = coord (Polar c (pi / 180 * h)) in (x,y))

mod_ :: Double -> Double -> Double
mod_ x d = x - fromIntegral (floor (x / d)) * d

lab2lch' :: Iso' (LAB Double) (LCH Double)
lab2lch' =
  iso
    (\(LAB l a b) -> let (c,h) = view xy2ch' (a,b) in LCH l c h)
    (\(LCH l c h) -> let (a,b) = view (re xy2ch') (c,h) in LAB l a b)

-- * rgb to lab
rgb2lab' :: Iso' (RGB3 Double) (LAB Double)
rgb2lab' =
  iso
    (\(RGB3' a) -> LAB' . xyz2lab_ . rgb2xyz_ $ a)
    (\(LAB' a) -> RGB3' . xyz2rgb_ . lab2xyz_ $ a)

-- * rgb to xyz
xyz2rgb_ :: Array '[3] Double -> Array '[3] Double
xyz2rgb_ a = fromList [r,g,b]
  where
    (S.ColorSRGB r g b) = S.xyz2rgb (S.ColorXYZ (a `index` [0]) (a `index` [1]) (a `index` [2])) :: Color (S.SRGB 'S.NonLinear) Double

-- >>> rgb2xyz_ [1,1,1]
-- [0.9505, 1.0, 1.089]
rgb2xyz_ :: Array '[3] Double -> Array '[3] Double
rgb2xyz_ a = fromList [x,y,z]
  where
    (S.ColorXYZ x y z) = S.rgb2xyz (S.ColorSRGB (a `index` [0]) (a `index` [1]) (a `index` [2])) :: Color (S.XYZ S.D65) Double

-- * xyz to lab

m1 :: Array '[3,3] Double
m1 =
  [ 0.8189330101,0.3618667424,-0.1288597137,
    0.0329845436,0.9293118715,0.0361456387,
    0.0482003018,0.2643662691,0.6338517070
  ]

m2 :: Array '[3,3] Double
m2 =
  [ 0.2104542553,0.7936177850,-0.0040720468,
    1.9779984951,-2.4285922050,0.4505937099,
    0.0259040371,0.7827717662,-0.8086757660
  ]

cubicroot :: (Floating a, Ord a) => a -> a
cubicroot x = bool (-1*(-x)**(1/3.0)) (x**(1/3.0)) (x>=0)

-- >>> xyz2lab_ [0.95, 1, 1.089]
-- [0.9999686754143632, -2.580058168537569e-4, -1.1499756458199784e-4]
--
-- >>> xyz2lab_ [1,0,0]
-- [0.4499315814860224, 1.2357102101076207, -1.9027581087245393e-2]
--
-- >>> xyz2lab_ [0,1,0]
-- [0.921816758286376, -0.6712376131199635, 0.2633235500611929]
--
-- >>> xyz2lab_ [1,0,1]
-- [0.5081033967278659, 1.147837087146462, -0.36768466477695416]
--
-- >>> xyz2lab_ [0,0,1]
-- [0.15260258004008057, -1.4149965510120839, -0.4489272035597538]
xyz2lab_ :: Array '[3] Double -> Array '[3] Double
xyz2lab_ xyz =
  dot sum (*) m2 (cubicroot <$> dot sum (*) m1 xyz)

m1' :: Array '[3,3] Double
m1' = [ 1.227013851103521026, -0.5577999806518222383, 0.28125614896646780758,
        -0.040580178423280593977, 1.1122568696168301049, -0.071676678665601200577,
        -0.076381284505706892869, -0.42148197841801273055, 1.5861632204407947575
      ]

m2' :: Array '[3,3] Double
m2' = [ 0.99999999845051981432, 0.39633779217376785678, 0.21580375806075880339,
        1.0000000088817607767, -0.1055613423236563494, -0.063854174771705903402,
        1.0000000546724109177, -0.089484182094965759684, -1.2914855378640917399
      ]

lab2xyz_ :: Array '[3] Double -> Array '[3] Double
lab2xyz_ lab =
  dot sum (*) m1' ((**3.0) <$> dot sum (*) m2' lab)

-- * mixins
-- | mix 2 colours, using the oklch model.
--
mix :: Double -> Colour -> Colour -> Colour
mix x c0 c1 = view lcha2colour' (mixLCHA x (review lcha2colour' c0) (review lcha2colour' c1))

mixLCHA :: Double -> LCHA -> LCHA -> LCHA
mixLCHA x (LCHA l c h a) (LCHA l' c' h' a') = LCHA l'' c'' h'' a''
  where
    l'' = l + x * (l' - l)
    c'' = c + x * (c' - c)
    h'' = h + x * (h' - h)
    a'' = a + x * (a' - a)

-- | interpolate across a list of Colours, with input being in Range 0 1
--
-- >>> mixes 0 [black, (Colour 0.2 0.6 0.8 0.5), white]
-- Colour 0.00 0.00 0.00 1.00
--
-- >>> mixes 1 [black, (Colour 0.2 0.6 0.8 0.5), white]
-- Colour 0.99 0.99 0.99 1.00
--
-- >>> mixes 0.6 [black, (Colour 0.2 0.6 0.8 0.5), white]
-- Colour 0.42 0.67 0.86 0.60
mixes :: Double -> [Colour] -> Colour
mixes _ [] = light
mixes _ [c] = c
mixes x cs = mix r (cs List.!! i) (cs List.!! (i + 1))
  where
    l = length cs - 1
    x' = x * fromIntegral l
    i = max 0 (min (floor x') (l - 1))
    r = x' - fromIntegral i

-- * Colour manipulation

-- | convert a colour to grayscale with the same lightness.
--
-- >>> greyed (Colour 0.4 0.7 0.8 0.4)
-- Colour 0.65 0.65 0.65 0.40
greyed :: Colour -> Colour
greyed = over chroma' (const 0)

-- | lightness lens
--
-- >>> over lightness' (*0.8) (Colour 0.4 0.7 0.8 0.4)
-- Colour 0.22 0.52 0.62 0.40
lightness' :: Lens' Colour Double
lightness' = re lcha2colour' % lch' % l'

-- | chroma lens
--
-- >>> over chroma' (*0.8) (Colour 0.4 0.7 0.8 0.4)
-- Colour 0.46 0.69 0.77 0.40
chroma' :: Lens' Colour Double
chroma' = re lcha2colour' % lch' % c'

-- | hue lens
--
-- >>> over hue' (+180) (Colour 0.4 0.7 0.8 0.4)
-- Colour 0.83 0.58 0.49 0.40
hue' :: Lens' Colour Double
hue' = re lcha2colour' % lch' % h'

-- | html element to display colours
--
-- >>> showSwatch "swatch" dark
-- "<div class=swatch style=\"background:rgba(5%, 5%, 5%, 1.00);\">swatch</div>"
showSwatch:: Text -> Colour -> Text
showSwatch label c =
      [trimming|<div class=swatch style="background:$rgba;">$label</div>|]
        where
          rgba = showRGBA c

-- | show multiple colors with embedded text.
showSwatches :: Text -> Text -> [(Text, Colour)] -> Text
showSwatches pre suff hs =
  [trimming|<div>
$pre
$divs
$suff
</div>
|]
    where
      divs = Text.intercalate "\n" (uncurry showSwatch <$> hs)

-- * random colors
instance (Elevator e, UniformRange e) => Uniform (Color (S.XYZ i) e)
  where
    uniformM g = do
      x <- uniformRM (minValue, maxValue) g
      y <- uniformRM (minValue, maxValue) g
      z <- uniformRM (minValue, maxValue) g
      pure (S.ColorXYZ x y z)

instance Uniform (RGB3 Double)
  where
    uniformM = fmap ((\(ColorRGB r g b) -> RGB3 r g b) . S.unColorRGB . (S.xyz2rgb :: Color (S.XYZ S.D65) Double -> Color (S.SRGB 'S.NonLinear) Double)) . uniformM

rRGB3s_ :: [RGB3 Double]
rRGB3s_ = go g0
  where
    g0 = mkStdGen 42
    go g = let (x,g') = uniform g in x:go g'

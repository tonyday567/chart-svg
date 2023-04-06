{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Colour representations and combinations.
module Data.Colour
  ( -- * Colour
    Colour,
    pattern Colour,
    validColour,
    validate,
    trimColour,
    showRGBA,
    showRGB,
    showOpacity,
    opac',
    opac,
    hex,
    rgb,
    toHex,
    fromHex,
    unsafeFromHex,

    -- * Palette colours
    palette1,
    palette1a,
    transparent,
    black,
    white,
    light,
    dark,
    grey,

    -- * LCH model
    LCH (..),
    pattern LCH,
    lLCH',
    cLCH',
    hLCH',
    LCHA (..),
    pattern LCHA,
    lch',
    alpha',
    RGB3 (..),
    pattern RGB3,
    rgbd',
    rgb32colour',
    LAB (..),
    pattern LAB,
    lcha2colour',
    xy2ch',

    -- * mixins
    mix,
    mixTrim,
    mixLCHA,
    mixes,
    greyed,
    lightness',
    chroma',
    hue',
    showSwatch,
    showSwatches,
    rvRGB3,
    rvColour,
    paletteR,
  )
where

import Chart.Data
import qualified Data.Attoparsec.Text as A
import Data.Bifunctor
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.Char
import Data.Either
import Data.FormatN
import Data.Functor.Rep
import qualified Data.List as List
import Data.String.Interpolate
import Data.Text (Text, pack)
import qualified Data.Text as Text
import GHC.Exts
import GHC.Generics hiding (prec)
import Graphics.Color.Model as M hiding (LCH)
import qualified Graphics.Color.Space as S
import NumHask.Algebra.Metric
import NumHask.Array.Fixed
import Optics.Core
import System.Random
import System.Random.Stateful

-- $setup
--
-- >>> :set -XOverloadedLabels
-- >>> :set -XOverloadedStrings
-- >>> import Chart
-- >>> import Optics.Core

-- | Colour type for the library, wrapping 'Color'.
newtype Colour = Colour'
  { color' :: Color (Alpha RGB) Double
  }
  deriving (Eq, Generic)

-- | Constructor pattern.
--
-- > Colour red green blue alpha
pattern Colour :: Double -> Double -> Double -> Double -> Colour
pattern Colour r g b a = Colour' (ColorRGBA r g b a)

{-# COMPLETE Colour #-}

instance Show Colour where
  show (Colour r g b a) =
    Text.unpack $
      "Colour "
        <> fixed (Just 2) r
        <> " "
        <> fixed (Just 2) g
        <> " "
        <> fixed (Just 2) b
        <> " "
        <> fixed (Just 2) a

-- | CSS-style representation
showRGBA :: Colour -> ByteString
showRGBA (Colour r' g' b' a') =
  [i|rgba(#{r}, #{g}, #{b}, #{a})|]
  where
    r = percent (fixedSF (Just 0)) (Just 2) r'
    g = percent (fixedSF (Just 0)) (Just 2) g'
    b = percent (fixedSF (Just 0)) (Just 2) b'
    a = fixed (Just 2) a'

-- | CSS-style representation
showRGB :: Colour -> ByteString
showRGB (Colour r' g' b' _) =
  [i|rgb(#{r}, #{g}, #{b})|]
  where
    r = percent (fixedSF (Just 0)) (Just 2) r'
    g = percent (fixedSF (Just 0)) (Just 2) g'
    b = percent (fixedSF (Just 0)) (Just 2) b'

-- | Is Colour in-gamut?
--
-- >>> validColour (Colour 1 1 1.01 1)
-- False
validColour :: Colour -> Bool
validColour (Colour r g b o) = r >= 0 && r <= 1 && g >= 0 && g <= 1 && b >= 0 && b <= 1 && o >= 0 && o <= 1

-- | Trim colour back to gamut.
--
-- >>> trimColour (Colour 1 1 1.01 1)
-- Colour 1.00 1.00 1.00 1.00
trimColour :: Colour -> Colour
trimColour (Colour r g b a) = Colour (trim r) (trim g) (trim b) (trim a)
  where
    trim x = max 0 $ min 1 x

-- | Validate that the Colout is in-gamut.
--
-- >>> validate (Colour 1 1 1.01 1)
-- Nothing
validate :: Colour -> Maybe Colour
validate c = bool Nothing (Just c) (validColour c)

-- | Opacity or alpha
opac :: Colour -> Double
opac (Colour _ _ _ o) = o

-- | CSS-style representation
showOpacity :: Colour -> ByteString
showOpacity c =
  [i|#{o}|]
  where
    o = formatOrShow (FixedStyle 2) Nothing (opac c)

-- | lens for opacity (or alpha channel)
opac' :: Lens' Colour Double
opac' = lens opac (\(Colour r g b _) o -> Colour r g b o)

-- | Convert to CSS hex representation.
hex :: Colour -> Text
hex c = toHex c

-- | Sets RGB color but not opacity
rgb :: Colour -> Colour -> Colour
rgb (Colour r g b _) (Colour _ _ _ o) = Colour r g b o

-- | Parse CSS hex text.
parseHex :: A.Parser (Color RGB Double)
parseHex =
  fmap toDouble
    . ( \((r, g), b) ->
          ColorRGB (fromIntegral r) (fromIntegral g) (fromIntegral b) :: Color RGB Word8
      )
    . (\(f, b) -> (f `divMod` (256 :: Int), b))
    . (`divMod` 256)
    <$> (A.string "#" *> A.hexadecimal)

-- | Convert CSS hex to Colour
fromHex :: Text -> Either Text (Color RGB Double)
fromHex = first pack . A.parseOnly parseHex

-- | Convert CSS hex to Colour, unsafely.
unsafeFromHex :: Text -> Color RGB Double
unsafeFromHex t = fromRight (ColorRGB 0 0 0) $ A.parseOnly parseHex t

-- | Convert from 'Colour' to CSS hex (#xxxxxx)
toHex :: Colour -> Text
toHex c =
  "#"
    <> Text.justifyRight 2 '0' (hex' r)
    <> Text.justifyRight 2 '0' (hex' g)
    <> Text.justifyRight 2 '0' (hex' b)
  where
    (ColorRGBA r g b _) = fromIntegral . toWord8 <$> color' c

hex' :: Int -> Text
hex' n
  | n < 0 = "-" <> go (-n)
  | otherwise = go n
  where
    go n'
      | n' < 16 = hexDigit n'
      | otherwise = go (n' `quot` 16) <> hexDigit (n' `rem` 16)

hexDigit :: Int -> Text
hexDigit n
  | n <= 9 = Text.singleton $! i2d n
  | otherwise = Text.singleton $! toEnum (n + 87)

i2d :: Int -> Char
i2d x = chr (ord '0' + x)

-- | Select a Colour from the palette
--
-- >>> palette1 0
-- Colour 0.02 0.73 0.80 1.00
--
-- ![wheel](other/wheel.svg)
palette1 :: Int -> Colour
palette1 x = cycle palette1_ List.!! x

palette1LCHA_ :: [LCHA]
palette1LCHA_ = [LCHA 0.72 0.123 207 1, LCHA 0.40 0.10 246 1, LCHA 0.50 0.21 338 1, LCHA 0.8 0.15 331 1, LCHA 0.83 0.14 69 1, LCHA 0.57 0.15 50 1, LCHA 0.38 0.085 128 1, LCHA 0.60 0.08 104 1]

-- | Finite list of Colours
--
-- Swatched to the oklab color model:
--
-- ![palette1](other/palette1.svg)
palette1_ :: [Colour]
palette1_ = trimColour . view lcha2colour' <$> palette1LCHA_

-- | Select a Colour from the palette with a specified opacity
--
-- >>> palette1a 0 0.5
-- Colour 0.02 0.73 0.80 0.50
palette1a :: Int -> Double -> Colour
palette1a x a = set opac' a $ cycle palette1_ List.!! x

-- | black
--
-- >>> black
-- Colour 0.00 0.00 0.00 1.00
black :: Colour
black = Colour 0 0 0 1

-- | white
--
-- >>> white
-- Colour 0.99 0.99 0.99 1.00
white :: Colour
white = Colour 0.99 0.99 0.99 1

-- | light
--
-- For lighter huds against a dark background ...
--
-- > colourHudOptions light defaultHudOptions
--
-- >>> light
-- Colour 0.94 0.94 0.94 1.00
light :: Colour
light = Colour 0.94 0.94 0.94 1

-- | dark
--
-- dark is hardcoded in most of the default options.
--
-- >>> dark
-- Colour 0.05 0.05 0.05 1.00
dark :: Colour
dark = Colour 0.05 0.05 0.05 1

-- | Grey(scale) colour inputting lightness and opacity.
--
-- >>> grey 0.5 0.4
-- Colour 0.50 0.50 0.50 0.40
grey :: Double -> Double -> Colour
grey g a = Colour g g g a

-- | Zero-opacity black
--
-- >>> transparent
-- Colour 0.00 0.00 0.00 0.00
transparent :: Colour
transparent = Colour 0 0 0 0

-- | LCH colour representation
--
-- oklab is a colour space being written into CSS specifications, that attempts to be ok at human-consistent colour representation. See:
--
-- - <https://bottosson.github.io/posts/oklab/ A perceptual color space for image processing>
-- - <https://www.w3.org/TR/css-color-5/#colorcontrast CSS Color Module Level 5>
-- - <https://www.w3.org/TR/css-color-4/#rgb-functions CSS Color Module Level 4>
--
-- The type is represented by three elements:
--
-- L: Lightness ranging from 0 (@LCH 0 _ _@ is black) to 1 (@LCH 1 _ _@ is white)
--
-- C: Chromacity, which ranges from 0 to around 0.32 or so.
--
-- H: Hue, which ranges from 0 to 360
newtype LCH a = LCH' {lchArray :: Array '[3] a} deriving (Eq, Show, IsList, Functor)

-- | LCH colour pattern
pattern LCH :: a -> a -> a -> LCH a
pattern LCH l c h <-
  LCH' [l, c, h]
  where
    LCH l c h = LCH' [l, c, h]

{-# COMPLETE LCH #-}

-- | Lightness lens for LCH
lLCH' :: Lens' (LCH Double) Double
lLCH' = lens (\(LCH l _ _) -> l) (\(LCH _ c h) l -> LCH l c h)

-- | Chromacity lens for LCH
cLCH' :: Lens' (LCH Double) Double
cLCH' = lens (\(LCH _ c _) -> c) (\(LCH l _ h) c -> LCH l c h)

-- | Hue lens for LCH
hLCH' :: Lens' (LCH Double) Double
hLCH' = lens (\(LCH _ _ h) -> h) (\(LCH l c _) h -> LCH l c h)

-- | LCHA representation, including an alpha channel.
data LCHA = LCHA' {_lch :: LCH Double, _alpha :: Double} deriving (Eq, Show)

-- | LCH lens for LCHA
lch' :: Lens' LCHA (LCH Double)
lch' = lens (\(LCHA' lch _) -> lch) (\(LCHA' _ a) lch -> LCHA' lch a)

-- | Alpha lens for LCHA
alpha' :: Lens' LCHA Double
alpha' = lens (\(LCHA' _ a) -> a) (\(LCHA' lch _) a -> LCHA' lch a)

-- | LCHA pattern
pattern LCHA :: Double -> Double -> Double -> Double -> LCHA
pattern LCHA l c h a <-
  LCHA' (LCH' [l, c, h]) a
  where
    LCHA l c h a = LCHA' (LCH' [l, c, h]) a

{-# COMPLETE LCHA #-}

-- * RGB colour representation

-- | A type to represent the RGB triple, useful as an intermediary between 'Colour' and 'LCHA'
newtype RGB3 a = RGB3' {rgb3Array :: Array '[3] a} deriving (Eq, Show, IsList, Functor)

-- | The RGB3 pattern
pattern RGB3 :: a -> a -> a -> RGB3 a
pattern RGB3 r g b <-
  RGB3' [r, g, b]
  where
    RGB3 r g b = RGB3' [r, g, b]

{-# COMPLETE RGB3 #-}

-- | Lens for conversion between Double and Word8 RGB triples.
rgbd' :: Iso' (RGB3 Double) (RGB3 Word8)
rgbd' = iso (fmap (floor . (* 256))) (fmap (\x -> fromIntegral x / 256.0))

-- | Lens for conversion between an (RGB3, alpha) pair and Colour
rgb32colour' :: Iso' (RGB3 Double, Double) Colour
rgb32colour' = iso (\(RGB3 r g b, a) -> Colour r g b a) (\(Colour r g b a) -> (RGB3 r g b, a))

-- * LAB colour representation

-- | LAB colour representation. a is green-red and b is blue-yellow
newtype LAB a = LAB' {labArray :: Array '[3] a} deriving (Eq, Show, IsList, Functor)

-- | LAB pattern
pattern LAB :: a -> a -> a -> LAB a
pattern LAB l a b <-
  LAB' [l, a, b]
  where
    LAB l a b = LAB' [l, a, b]

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

-- | Lens between generic XY color representations and CH ones, which are polar version of the XY.
xy2ch' :: Iso' (Double, Double) (Double, Double)
xy2ch' =
  iso
    (\(x, y) -> (norm (Point x y), 180 / pi * mod_ (angle (Point x y)) (2 * pi)))
    (\(c, h) -> let (Point x y) = coord (Polar c (pi / 180 * h)) in (x, y))

mod_ :: Double -> Double -> Double
mod_ x d = x - fromIntegral (floor (x / d) :: Integer) * d

-- | Lens between LAB and LCH
lab2lch' :: Iso' (LAB Double) (LCH Double)
lab2lch' =
  iso
    (\(LAB l a b) -> let (c, h) = view xy2ch' (a, b) in LCH l c h)
    (\(LCH l c h) -> let (a, b) = view (re xy2ch') (c, h) in LAB l a b)

-- * rgb to lab

-- | Lens between RGB3 and LAB
rgb2lab' :: Iso' (RGB3 Double) (LAB Double)
rgb2lab' =
  iso
    (\(RGB3' a) -> LAB' . xyz2lab_ . rgb2xyz_ $ a)
    (\(LAB' a) -> RGB3' . xyz2rgb_ . lab2xyz_ $ a)

-- * rgb to xyz

xyz2rgb_ :: Array '[3] Double -> Array '[3] Double
xyz2rgb_ a = fromList [r, g, b]
  where
    (S.ColorSRGB r g b) = S.xyz2rgb (S.ColorXYZ (a `index` [0]) (a `index` [1]) (a `index` [2])) :: Color (S.SRGB 'S.NonLinear) Double

-- >>> rgb2xyz_ [1,1,1]
-- [0.9505, 1.0, 1.089]
rgb2xyz_ :: Array '[3] Double -> Array '[3] Double
rgb2xyz_ a = fromList [x, y, z]
  where
    (S.ColorXYZ x y z) = S.rgb2xyz (S.ColorSRGB (a `index` [0]) (a `index` [1]) (a `index` [2])) :: Color (S.XYZ S.D65) Double

-- * xyz to lab

m1 :: Array '[3, 3] Double
m1 =
  [ 0.8189330101,
    0.3618667424,
    -0.1288597137,
    0.0329845436,
    0.9293118715,
    0.0361456387,
    0.0482003018,
    0.2643662691,
    0.6338517070
  ]

m2 :: Array '[3, 3] Double
m2 =
  [ 0.2104542553,
    0.7936177850,
    -0.0040720468,
    1.9779984951,
    -2.4285922050,
    0.4505937099,
    0.0259040371,
    0.7827717662,
    -0.8086757660
  ]

cubicroot :: (Floating a, Ord a) => a -> a
cubicroot x = bool (-1 * (-x) ** (1 / 3.0)) (x ** (1 / 3.0)) (x >= 0)

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

m1' :: Array '[3, 3] Double
m1' =
  [ 1.227013851103521026,
    -0.5577999806518222383,
    0.28125614896646780758,
    -0.040580178423280593977,
    1.1122568696168301049,
    -0.071676678665601200577,
    -0.076381284505706892869,
    -0.42148197841801273055,
    1.5861632204407947575
  ]

m2' :: Array '[3, 3] Double
m2' =
  [ 0.99999999845051981432,
    0.39633779217376785678,
    0.21580375806075880339,
    1.0000000088817607767,
    -0.1055613423236563494,
    -0.063854174771705903402,
    1.0000000546724109177,
    -0.089484182094965759684,
    -1.2914855378640917399
  ]

lab2xyz_ :: Array '[3] Double -> Array '[3] Double
lab2xyz_ lab =
  dot sum (*) m1' ((** 3.0) <$> dot sum (*) m2' lab)

-- * mixins

-- | Mix 2 colours, using the oklch model.
--
-- This may not always be what you expect. One example is mixing black and another colour:
--
-- >>> mix 0.8 (Colour 0 0 0 1) (Colour 0.2 0.6 0.8 0.5)
-- Colour -0.09 0.48 0.45 0.60
--
-- The mix has gone out of gamut because we are swishing through hue mixes.
--
-- In this case, settting the hue on the black colour within the LCH contruct helps:
--
-- >>> betterblack = set (lch' % hLCH') (view hue' (Colour 0.2 0.6 0.8 0.5)) (review lcha2colour' black)
-- >>> view lcha2colour' $ mixLCHA 0.8 betterblack (review lcha2colour' $ Colour 0.2 0.6 0.8 0.5)
-- Colour 0.14 0.44 0.59 0.60
mix :: Double -> Colour -> Colour -> Colour
mix x c0 c1 = view lcha2colour' (mixLCHA x (review lcha2colour' c0) (review lcha2colour' c1))

-- | Mix 2 colours, using the oklch model, trimming the reult back to in-gamut.
--
-- >>> mixTrim 0.8 (Colour 0 0 0 1) (Colour 0.2 0.6 0.8 0.5)
-- Colour 0.00 0.48 0.45 0.60
mixTrim :: Double -> Colour -> Colour -> Colour
mixTrim x c0 c1 = trimColour (mix x c0 c1)

-- | Mix two LCHA specified colours.
mixLCHA :: Double -> LCHA -> LCHA -> LCHA
mixLCHA x (LCHA l c h a) (LCHA l' c' h' a') = LCHA l'' c'' h'' a''
  where
    l'' = l + x * (l' - l)
    c'' = c + x * (c' - c)
    h'' = h + x * (h' - h)
    a'' = a + x * (a' - a)

-- | Interpolate across a list of Colours, with input being in Range 0 1
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
mixes x cs = mix r (cs List.!! i') (cs List.!! (i' + 1))
  where
    l = length cs - 1
    x' = x * fromIntegral l
    i' = max 0 (min (floor x') (l - 1))
    r = x' - fromIntegral i'

-- * Colour manipulation

-- | Convert a colour to grayscale with the same lightness.
--
-- >>> greyed (Colour 0.4 0.7 0.8 0.4)
-- Colour 0.65 0.65 0.65 0.40
greyed :: Colour -> Colour
greyed = over chroma' (const 0)

-- | Lightness lens
--
-- >>> over lightness' (*0.8) (Colour 0.4 0.7 0.8 0.4)
-- Colour 0.22 0.52 0.62 0.40
lightness' :: Lens' Colour Double
lightness' = re lcha2colour' % lch' % lLCH'

-- | Chromacity lens
--
-- >>> over chroma' (*0.8) (Colour 0.4 0.7 0.8 0.4)
-- Colour 0.46 0.69 0.77 0.40
chroma' :: Lens' Colour Double
chroma' = re lcha2colour' % lch' % cLCH'

-- | Hue lens
--
-- >>> over hue' (+180) (Colour 0.4 0.7 0.8 0.4)
-- Colour 0.83 0.58 0.49 0.40
hue' :: Lens' Colour Double
hue' = re lcha2colour' % lch' % hLCH'

-- | Html element to display colours
--
-- >>> showSwatch "swatch" dark
-- "<div class=swatch style=\"background:rgba(5%, 5%, 5%, 1.00);\">swatch</div>"
showSwatch :: Text -> Colour -> Text
showSwatch label c =
  [i|<div class=swatch style="background:#{rgba};">#{label}</div>|]
  where
    rgba = showRGBA c

-- | Show multiple colors with embedded text.
showSwatches :: Text -> Text -> [(Text, Colour)] -> Text
showSwatches pref suff hs =
  [i|<div>
#{pref}
#{divs}
#{suff}
</div>
|]
  where
    divs = Text.intercalate "\n" (uncurry showSwatch <$> hs)

-- * random colors

instance Uniform (RGB3 Double) where
  uniformM gen = do
    r <- uniformRM (0, 1) gen
    g <- uniformRM (0, 1) gen
    b <- uniformRM (0, 1) gen
    pure (RGB3 r g b)

instance Uniform Colour where
  uniformM gen = do
    r <- uniformRM (0, 1) gen
    g <- uniformRM (0, 1) gen
    b <- uniformRM (0, 1) gen
    a <- uniformRM (0, 1) gen
    pure (Colour r g b a)

-- | Random variates of a uniform
rvs :: (Uniform a) => [a]
rvs = go g0
  where
    g0 = mkStdGen 42
    go g = let (x, g') = uniform g in x : go g'

-- | Random list of RGB3s
rvRGB3 :: [RGB3 Double]
rvRGB3 = rvs

-- | Random list of Colours
rvColour :: [Colour]
rvColour = rvs

-- | Random Colours with an opacity of 1 that are not too extreme in terms of lightness or chromacity.
paletteR :: [Colour]
paletteR = go g0
  where
    g0 = mkStdGen 42
    go g = let (x, g') = runStateGen g rvSensible in x : go g'

-- | A random Colour generator that provides a (hopefully) pleasant colour not too light, dark, over-saturated or dull.
rvSensible :: (StatefulGen g m) => g -> m Colour
rvSensible gen = do
  l <- uniformRM (0.3, 0.75) gen
  c <- uniformRM (0.05, 0.24) gen
  h <- uniformRM (0, 360) gen
  pure ((trimColour . view lcha2colour') (LCHA l c h 1))

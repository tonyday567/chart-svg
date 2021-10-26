{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Colour representations and combinations, based on <https://hackage.haskell.org/package/Color>
module Data.Colour
  ( Colour,
    pattern Colour,
    opac,
    opac',
    hex,
    rgb,
    blend,
    blends,
    toHex,
    fromHex,
    unsafeFromHex,
    palette1,
    transparent,
    black,
    white,
    light,
    dark,
  )
where

import qualified Data.Attoparsec.Text as A
import Data.Bifunctor
import Data.Char
import Data.Either
import Data.FormatN
import qualified Data.List as List
import Data.Text (Text, pack)
import qualified Data.Text as Text
import GHC.Generics hiding (prec)
import Graphics.Color.Model
import Data.List.NonEmpty (NonEmpty(..))
import Data.Foldable
import Optics.Core

-- | Wrapper for 'Color'.
newtype Colour = Colour'
  { color' :: Color (Alpha RGB) Double
  }
  deriving (Eq, Generic)

-- | Constructor pattern.
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

-- | opac
opac :: Colour -> Double
opac (Colour _ _ _ o) = o

-- | lens for opacity
opac' :: Lens' Colour Double
opac' = lens opac (\(Colour r g b _) o -> Colour r g b o)

-- |
hex :: Colour -> Text
hex c = toHex c

-- | resets RGB color but not opacity
rgb :: Colour -> Colour -> Colour
rgb (Colour r g b _) (Colour _ _ _ o) = Colour r g b o

-- | interpolate between 2 colors
blend :: Double -> Colour -> Colour -> Colour
blend c (Colour r g b a) (Colour r' g' b' a') = Colour r'' g'' b'' a''
  where
    r'' = r + c * (r' - r)
    g'' = g + c * (g' - g)
    b'' = b + c * (b' - b)
    a'' = a + c * (a' - a)

-- | interpolate across a list of Colours, with input being in Range 0 1
--
-- >>> blends 0 [black, (Colour 0.2 0.6 0.8 0.5), white] == black
-- True
--
-- >>> blends 1 [black, (Colour 0.2 0.6 0.8 0.5), white] == white
-- True
--
-- >>> blends 0.6 [black, (Colour 0.2 0.6 0.8 0.5), white]
-- Colour 0.36 0.68 0.84 0.60
blends :: Double -> [Colour] -> Colour
blends _ [] = light
blends _ [c] = c
blends x cs = blend r (cs List.!! i) (cs List.!! (i + 1))
  where
    l = length cs - 1
    x' = x * fromIntegral l
    i = max 0 (min (floor x') (l - 1))
    r = x' - fromIntegral i

-- |
parseHex :: A.Parser (Color RGB Double)
parseHex =
  fmap toDouble
    . ( \((r, g), b) ->
          ColorRGB (fromIntegral r) (fromIntegral g) (fromIntegral b) :: Color RGB Word8
      )
    . (\(f, b) -> (f `divMod` (256 :: Int), b))
    . (`divMod` 256)
    <$> (A.string "#" *> A.hexadecimal)

-- |
fromHex :: Text -> Either Text (Color RGB Double)
fromHex = first pack . A.parseOnly parseHex

-- |
unsafeFromHex :: Text -> Color RGB Double
unsafeFromHex t = fromRight (ColorRGB 0 0 0) $ A.parseOnly parseHex t

-- | convert from 'Colour' to #xxxxxx
toHex :: Colour -> Text
toHex c =
  "#"
    <> Text.justifyRight 2 '0' (hex' r)
    <> Text.justifyRight 2 '0' (hex' g)
    <> Text.justifyRight 2 '0' (hex' b)
  where
    (ColorRGBA r g b _) = fromIntegral . toWord8 <$> color' c

-- |
hex' :: Int -> Text
hex' i
  | i < 0 = "-" <> go (-i)
  | otherwise = go i
  where
    go n
      | n < 16 = hexDigit n
      | otherwise = go (n `quot` 16) <> hexDigit (n `rem` 16)

-- |
hexDigit :: Int -> Text
hexDigit n
  | n <= 9 = Text.singleton $! i2d n
  | otherwise = Text.singleton $! toEnum (n + 87)

-- |
i2d :: Int -> Char
i2d i = chr (ord '0' + i)

-- | select a Colour from the palette
--
-- >>> palette1 0
-- Colour 0.69 0.35 0.16 1.00
palette1 :: Int -> Colour
palette1 x = cycle (toList palette1_) List.!! x

-- | finite list of Colours
palette1_ :: NonEmpty Colour
palette1_ =
  [ Colour 0.69 0.35 0.16 1.00,
    Colour 0.65 0.81 0.89 1.00,
    Colour 0.12 0.47 0.71 1.00,
    Colour 0.89 0.10 0.11 1.00,
    Colour 0.70 0.87 0.54 1.00,
    Colour 0.20 0.63 0.17 1.00,
    Colour 0.98 0.60 0.60 1.00,
    Colour 0.99 0.75 0.44 1.00,
    Colour 1.00 0.50 0.00 1.00,
    Colour 0.99 0.99 0.99 1.00,
    Colour 0.00 0.00 0.00 1.00,
    Colour 1.00 1.00 0.60 1.00,
    Colour 0.69 0.35 0.16 1.00
  ]

-- |
--
-- >>> black
-- Colour 0.00 0.00 0.00 1.00
black :: Colour
black = Colour 0 0 0 1

-- |
--
-- >>> white
-- Colour 0.99 0.99 0.99 1.00
white :: Colour
white = Colour 0.99 0.99 0.99 1

-- |
--
-- For lighter huds against a dark background ...
--
-- > colourHudOptions light defaultHudOptions
--
-- >>> light
-- Colour 0.94 0.94 0.94 1.00
light :: Colour
light = Colour 0.94 0.94 0.94 1

-- |
--
-- dark is hardcoded in most of the default options.
--
-- >>> dark
-- Colour 0.05 0.05 0.05 1.00
dark :: Colour
dark = Colour 0.05 0.05 0.05 1

-- | zero opacity black
--
-- >>> transparent
-- Colour 0.00 0.00 0.00 0.00
transparent :: Colour
transparent = Colour 0 0 0 0

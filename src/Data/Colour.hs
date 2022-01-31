{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Colour representations and combinations, based on <https://hackage.haskell.org/package/Color>
module Data.Colour
  ( Colour,
    pattern Colour,
    validColour,
    validate,
    showRGBA,
    showRGB,
    opac,
    opac',
    hex,
    rgb,
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
import Optics.Core
import Data.Bool
import NeatInterpolation

-- $setup
--
-- >>> :set -XOverloadedLabels
-- >>> :set -XOverloadedStrings
-- >>> import Chart
-- >>> import Optics.Core

-- | Wrapper for 'Color'.
newtype Colour = Colour'
  { color' :: Color (Alpha RGB) Double
  }
  deriving (Eq, Generic)

-- | Constructor pattern.
pattern Colour :: Double -> Double -> Double -> Double -> Colour
pattern Colour r g b a = Colour' (ColorRGBA r g b a)

{-# COMPLETE Colour #-}

instance Show Colour
  where
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

-- | css representation
showRGBA :: Colour -> Text
showRGBA (Colour r' g' b' a') =
  [trimming|rgba($r, $g, $b, $a)|]
    where
      r = percent (fixed (Just 0)) r'
      g = percent (fixed (Just 0)) g'
      b = percent (fixed (Just 0)) b'
      a = fixed (Just 2) a'

-- | css representation
showRGB :: Colour -> Text
showRGB (Colour r' g' b' _) =
  [trimming|rgb($r, $g, $b)|]
    where
      r = percent (fixed (Just 0)) r'
      g = percent (fixed (Just 0)) g'
      b = percent (fixed (Just 0)) b'

-- >>> validColour (Colour 1 1 1.01 1)
-- False
validColour :: Colour -> Bool
validColour (Colour r g b o) = r >= 0 && r <= 1 && g >= 0 && g <= 1 && b >= 0 && b <= 1 && o >= 0 && o <= 1

-- | Validate that the Colout is in gamut
--
-- >>> validate (Colour 1 1 1.01 1)
-- Nothing
validate :: Colour -> Maybe Colour
validate c = bool Nothing (Just c) (validColour c)

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
palette1 x = cycle palette1_ List.!! x

-- | finite list of Colours
palette1_ :: [Colour]
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

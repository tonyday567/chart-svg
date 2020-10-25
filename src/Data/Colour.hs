{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Colour representations and combinations, based on <https://hackage.haskell.org/package/Color>
module Data.Colour
  ( Colour,
    pattern Colour,
    opac,
    setOpac,
    fromRGB,
    hex,
    palette,
    palette1,
    blend,
    blends,
    toHex,
    fromHex,
    unsafeFromHex,
    grayscale,
    colorText,
    transparent,
    black,
    white,
  )
where

import qualified Data.Attoparsec.Text as A
import Data.FormatN
import Data.Generics.Labels ()
import qualified Data.Text as Text
import Graphics.Color.Model
import NumHask.Prelude as NHP
import qualified Prelude as P

-- | Wrapper for 'Color'.
newtype Colour = Colour' {color' :: Color (Alpha RGB) Double} deriving (Eq, Generic)

-- | Constructor pattern.
pattern Colour :: Double -> Double -> Double -> Double -> Colour
pattern Colour r g b a = Colour' (ColorRGBA r g b a)

{-# COMPLETE Colour #-}

instance Show Colour where
  show (Colour r g b a) =
    Text.unpack $
      "RGBA "
        <> fixed (Just 2) r
        <> " "
        <> fixed (Just 2) g
        <> " "
        <> fixed (Just 2) b
        <> " "
        <> fixed (Just 2) a

-- | get opacity
opac :: Colour -> Double
opac c = getAlpha (color' c)

-- | set opacity
setOpac :: Double -> Colour -> Colour
setOpac o (Colour r g b _) = Colour r g b o

-- |
fromRGB :: Color RGB Double -> Double -> Colour
fromRGB (ColorRGB r b g) o = Colour' $ ColorRGBA r b g o

-- |
hex :: Colour -> Text
hex c = toHex c

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
-- RGBA 0.16 0.48 0.64 0.60
blends :: Double -> [Colour] -> Colour
blends _ [] = black
blends _ [c] = c
blends x cs = blend r (cs P.!! i) (cs P.!! (i+1))
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
unsafeFromHex t = either (const (ColorRGB 0 0 0)) id $ A.parseOnly parseHex t

-- | convert from 'Colour' to #xxxxxx
toHex :: Colour -> Text
toHex c =
  "#"
    <> Text.justifyRight 2 '0' (hex' r)
    <> Text.justifyRight 2 '0' (hex' g)
    <> Text.justifyRight 2 '0' (hex' b)
  where
    (ColorRGBA r g b _) = toWord8 <$> color' c

-- |
hex' :: (FromInteger a, ToIntegral a Integer, Integral a, Ord a, Subtractive a) => a -> Text
hex' i
  | i < 0 = "-" <> go (- i)
  | otherwise = go i
  where
    go n
      | n < 16 = hexDigit n
      | otherwise = go (n `quot` 16) <> hexDigit (n `rem` 16)

-- |
hexDigit :: (Ord a, FromInteger a, ToIntegral a Integer) => a -> Text
hexDigit n
  | n <= 9 = Text.singleton P.$! i2d (fromIntegral n)
  | otherwise = Text.singleton P.$! toEnum (fromIntegral n + 87)

-- |
i2d :: Int -> Char
i2d i = chr (ord '0' + i)

-- | some RGB colors to work with
palette :: [Color RGB Double]
palette = unsafeFromHex <$> ["#a6cee3", "#1f78b4", "#e31a1c", "#b2df8a", "#33a02c", "#fb9a99", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#ffff99", "#b15928"]

-- | some RGBA colors
palette1 :: [Colour]
palette1 = (\c -> fromRGB c 1) <$> palette

-- | gray with 1 opacity
grayscale :: Double -> Color RGB Double
grayscale n = ColorRGB n n n

-- | standard text color
colorText :: Colour
colorText = fromRGB (grayscale 0.2) 1

-- |
black :: Colour
black = fromRGB (grayscale 0) 1

-- |
white :: Colour
white = fromRGB (grayscale 1) 1

-- |
transparent :: Colour
transparent = Colour 0 0 0 0

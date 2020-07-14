{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

module Chart.Color
  ( Colour,
    pattern Colour,
    opac,
    hex,
    palette,
    blend,
    toHex,
    fromHex,
    unsafeFromHex,
    fromHexOpac,

    grayscale,
    colorText,
    transparent,
    black,
    white,

    -- * re-exports
    module Graphics.Color.Model,
  )
where

import Data.Attoparsec.Text hiding (take)
import Data.Generics.Labels ()
import Data.Text (justifyRight, singleton)
import GHC.Base hiding (($!), (.), id)
import Graphics.Color.Model
import NumHask.Prelude as P

type Colour = Color (Alpha RGB) Double

-- | Constructor.
pattern Colour :: Double -> Double -> Double -> Double -> Colour
pattern Colour r g b a = ColorRGBA r g b a

{-# COMPLETE Colour #-}

opac :: Colour -> Double
opac c = getAlpha c

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

parseHex :: Parser Colour
parseHex =
  (`addAlpha` 1) . fmap toDouble
    . ( \((r, g), b) ->
            ColorRGB (fromIntegral r) (fromIntegral g) (fromIntegral b) :: Color RGB Word8
        )
    . (\(f, b) -> (f `divMod` (256 :: Int), b))
    . (`divMod` 256)
    <$> (string "#" *> hexadecimal)

fromHex :: Text -> Either Text Colour
fromHex = first pack . parseOnly parseHex

unsafeFromHex :: Text -> Colour
unsafeFromHex t = either (const transparent) id $ parseOnly parseHex t

-- | convert from 'Colour' to #xxxxxx
toHex :: Colour -> Text
toHex c =
  "#"
    <> justifyRight 2 '0' (hex' r)
    <> justifyRight 2 '0' (hex' g)
    <> justifyRight 2 '0' (hex' b)
  where
    (ColorRGBA r g b _) = toWord8 <$> c

hex' :: (FromInteger a, ToIntegral a Integer, Integral a, Ord a, Subtractive a) => a -> Text
hex' i
  | i < 0 = "-" <> go (- i)
  | otherwise = go i
  where
    go n
      | n < 16 = hexDigit n
      | otherwise = go (n `quot` 16) <> hexDigit (n `rem` 16)

hexDigit :: (Ord a, FromInteger a, ToIntegral a Integer) => a -> Text
hexDigit n
  | n <= 9 = singleton P.$! i2d (fromIntegral n)
  | otherwise = singleton P.$! toEnum (fromIntegral n + 87)

{-# INLINE i2d #-}
i2d :: Int -> Char
i2d (I# i#) = C# (chr# (ord# '0'# +# i#))

fromHexOpac :: Text -> Double -> Colour
fromHexOpac t o = setAlpha (unsafeFromHex t) o

palette :: [Colour]
palette = unsafeFromHex <$> ["#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#6a3d9a","#ffff99","#b15928"]

grayscale :: Double -> Colour
grayscale n = Colour n n n 1

colorText :: Colour
colorText = grayscale 0.2

black :: Colour
black = grayscale 0

white :: Colour
white = grayscale 1

transparent :: Colour
transparent = Colour 0 0 0 0

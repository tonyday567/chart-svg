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

    -- * named colors
    colorText,
    colorPixelMin,
    colorPixelMax,
    colorFrame,
    colorCanvas,
    colorGlyphTick,
    colorLineTick,
    colorTextTick,
    colorGrey,
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

palette :: [Colour]
palette = unsafeFromHex <$> ["#0ea194", "#0a865a", "#9d1102", "#f8a631", "#695b1e", "#31331c", "#454e56", "#94a7b5", "#ab7257", "#001114", "#042f1e", "#033d26", "#034243", "#026062"]

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

-- some colors used
colorText :: Colour
colorText = Colour 0.2 0.2 0.2 1

colorPixelMin :: Colour
colorPixelMin = Colour 0.8 0.8 0.8 1

colorPixelMax :: Colour
colorPixelMax = Colour 0.1 0.1 1 1

colorFrame :: Colour
colorFrame = Colour 0 0 1 0.4

colorCanvas :: Colour
colorCanvas = Colour 0.8 0.8 0.8 0.1

colorGlyphTick :: Colour
colorGlyphTick = Colour 0.34 0.05 0.4 0.5

colorLineTick :: Colour
colorLineTick = Colour 0.5 0.5 0.5 0.1

colorTextTick :: Colour
colorTextTick = Colour 0.2 0.2 0.2 0.8

colorGrey :: Colour
colorGrey = Colour 0.5 0.5 0.5 1

black :: Colour
black = Colour 0 0 0 1

white :: Colour
white = Colour 1 1 1 1

transparent :: Colour
transparent = Colour 0 0 0 0

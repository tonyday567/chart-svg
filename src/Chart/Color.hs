{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
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
import GHC.Exts
import Protolude
import qualified Data.Text.Lazy as Lazy
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Format
import Data.Text (justifyRight)
import Graphics.Color.Model
import Data.Text (pack)

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
palette = unsafeFromHex <$> ["#026062", "#0ea194", "#0a865a", "#9d1102", "#f8a631", "#695b1e", "#31331c", "#454e56", "#94a7b5", "#ab7257", "#001114", "#042f1e", "#033d26", "#034243", "#026062", "#0ea194", "#0a865a", "#9d1102", "#f8a631", "#695b1e"]

-- | interpolate between 2 colors
blend :: Double -> Colour -> Colour -> Colour
blend c (Colour r g b a) (Colour r' g' b' a') = Colour r'' g'' b'' a''
  where
    r'' = r + c * (r' - r)
    g'' = g + c * (g' - g)
    b'' = b + c * (b' - b)
    a'' = a + c * (a' - a)

parseHex :: Parser Colour
parseHex = (\x -> addAlpha x 1) . fmap toDouble <$>
  ( \((r, g), b) ->
      ColorRGB (fromIntegral r) (fromIntegral g) (fromIntegral b) :: Color RGB Word8
  )
    . (\(f, b) -> (f `divMod` (256 :: Int), b))
    . (`divMod` 256)
    <$> (string "#" *> hexadecimal)

fromHex :: Text -> Either Text Colour
fromHex = first pack . parseOnly parseHex

unsafeFromHex :: Text -> Colour
unsafeFromHex t = either (const transparent) (\x -> x) $ parseOnly parseHex t

-- | convert from 'Colour' to #xxxxxx
toHex :: Colour -> Text
toHex c =
  "#"
    <> justifyRight 2 '0' (Lazy.toStrict $ toLazyText $ Data.Text.Format.hex r)
    <> justifyRight 2 '0' (Lazy.toStrict $ toLazyText $ Data.Text.Format.hex g)
    <> justifyRight 2 '0' (Lazy.toStrict $ toLazyText $ Data.Text.Format.hex b)
  where
    (ColorRGBA r g b _) = toWord8 <$> c

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

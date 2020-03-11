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
  ( blue,
    grey,
    black,
    white,
    red,
    toColour,
    fromColour,
    d3Palette1,
    chartPalette,
    blend,
    blend',
    toHex,
    fromHex,
  )
where

import Codec.Picture.Types
import qualified Data.Colour.Palette.ColorSet as C
import qualified Data.Colour.RGBSpace as C
import qualified Data.Colour.SRGB.Linear as C
import Data.Generics.Labels ()
import GHC.Exts
import Protolude
import Web.Page.Html
import Data.Attoparsec.Text hiding (take)

-- * color
-- | the official chart-unit blue
blue :: PixelRGB8
blue = PixelRGB8 93 165 218

-- | the official chart-unit grey
grey :: PixelRGB8
grey = PixelRGB8 102 102 102

-- | black
black :: PixelRGB8
black = PixelRGB8 0 0 0

-- | white
white :: PixelRGB8
white = PixelRGB8 255 255 255

-- | red
red :: PixelRGB8
red = PixelRGB8 255 0 0

-- | convert a 'PixelRGB8' to a 'Colour' representation.
toColour :: PixelRGB8 -> C.Colour Double
toColour (PixelRGB8 r g b) =
  C.rgb (fromIntegral r / 256.0) (fromIntegral g / 256.0) (fromIntegral b / 256.0)

-- | convert a 'Colour' to a 'PixelRGB8' representation.
fromColour :: C.Colour Double -> PixelRGB8
fromColour (C.toRGB -> C.RGB r g b) =
  PixelRGB8 (floor (256 * r)) (floor (256 * g)) (floor (256 * b))

-- | the d3 palette
d3Palette1 :: [PixelRGB8]
d3Palette1 = fromColour . C.d3Colors1 <$> [0 .. 9]

chartPalette :: [PixelRGB8]
chartPalette = rights $ parseOnly fromHex <$> ["#31331c", "#454e56", "#94a7b5", "#ab7257"]

-- | interpolate between 2 colors
blend :: Double -> PixelRGB8 -> PixelRGB8 -> PixelRGB8
blend c = mixWithAlpha f (f (0 :: Int))
  where
    f _ x0 x1 = fromIntegral (round (fromIntegral x0 + c * (fromIntegral x1 - fromIntegral x0)) :: Integer)

-- | interpolate between 2 alpha colors
blend' :: Double -> (PixelRGB8, Double) -> (PixelRGB8, Double) -> (PixelRGB8, Double)
blend' c (c0, o0) (c1, o1) = (blend c c0 c1, f' c o0 o1)
  where
    f' c' x0 x1 = x0 + c' * (x1 - x0)

{-
-- | convert from 'PixelRGB8' to #xxxxxx
toHex :: PixelRGB8 -> Text
toHex (PixelRGB8 r g b) =
  "#"
    <> justifyRight 2 '0' (toStrict $ toLazyText $ hex r)
    <> justifyRight 2 '0' (toStrict $ toLazyText $ hex g)
    <> justifyRight 2 '0' (toStrict $ toLazyText $ hex b)

-}

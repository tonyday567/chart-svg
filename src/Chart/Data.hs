{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}

-- | Data primitives and utilities
module Chart.Data
  ( padRect,
    singletonGuard,
    one,
    zero,
    angle,
    norm,
    abs,
    Rect(..),
    Point(..),
    module NumHask.Space,
    addp,
  ) where

import NumHask.Prelude
import NumHask.Space hiding (Element)

-- | additive padding
padRect :: (Subtractive a) => a -> Rect a -> Rect a
padRect p (Rect x z y w) = Rect (x - p) (z + p) (y - p) (w + p)

-- | pad a Rect to remove singleton dimensions
padSingletons :: Rect Double -> Rect Double
padSingletons (Rect x z y w)
      | x == z && y == w = Rect (x - 0.5) (x + 0.5) (y - 0.5) (y + 0.5)
      | x == z = Rect (x - 0.5) (x + 0.5) y w
      | y == w = Rect x z (y - 0.5) (y + 0.5)
      | otherwise = Rect x z y w

singletonGuard :: Maybe (Rect Double ) -> Rect Double
singletonGuard = maybe one padSingletons

addp :: Point Double -> Point Double -> Point Double
addp = (+)

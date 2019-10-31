{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | a spot on the xy-plane
module Chart.Spot where

import NumHask.Point
import NumHask.Rect
import NumHask.Range
import NumHask.Space
import Protolude
import Algebra.Lattice
import Control.Category (id)

instance Lattice Double where
  (\/) = min
  (/\) = max

-- * primitive Chart elements


-- | unification of a point and rect on the plane
data Spot a =
  SpotPoint (Point a) |
  SpotRect (Rect a)
  deriving (Eq, Show, Functor)

instance (Num a, Fractional a, Spaceable a) => Num (Spot a) where
  SpotPoint (Point x y) + SpotPoint (Point x' y') = SpotPoint (Point (x+x') (y+y'))
  SpotPoint (Point x' y') + SpotRect (Rect x z y w) = SpotRect $ Rect (x+x') (z+x') (y+y') (w+y')
  SpotRect (Rect x z y w) + SpotPoint (Point x' y') = SpotRect $ Rect (x+x') (z+x') (y+y') (w+y')
  SpotRect (Rect x z y w) + SpotRect (Rect x' z' y' w') =
    SpotRect $ Rect (x+x') (z+z') (y+y') (w+w')
  x * y = SpotRect $ toRect x `multRect` toRect y

-- | pattern for SP x y
pattern SP :: a -> a -> Spot a
pattern SP a b = SpotPoint (Point a b)
{-# COMPLETE SP #-}

-- | pattern for SA lowerx upperx lowery uppery
pattern SA :: a -> a -> a -> a -> Spot a
pattern SA a b c d = SpotRect (Rect a b c d)
{-# COMPLETE SA #-}

-- | Convert a spot to an Rect
toRect :: Spot a -> Rect a
toRect (SP x y) = Rect x x y y
toRect (SpotRect a) = a

-- | Convert a spot to a Point
toPoint :: (Fractional a, Spaceable a) => Spot a -> Point a
toPoint (SP x y) = Point x y
toPoint (SpotRect (Ranges x y)) = Point (mid x) (mid y)

instance (Spaceable a) => Semigroup (Spot a) where
  (<>) a b = SpotRect (toRect a `union` toRect b)


-- | project a Spot from one Rect to another, preserving relative position.
projectOn :: (Fractional a, Spaceable a) => Rect a -> Rect a -> Spot a -> Spot a
projectOn new old@(Rect x z y w) po@(SP px py)
  | x==z && y==w = po
  | x==z = SP px py'
  | y==w = SP px' py
  | otherwise = SP px' py'
  where
    (Point px' py') = project old new (toPoint po)
projectOn new old@(Rect x z y w) ao@(SA ox oz oy ow)
  | x==z && y==w = ao
  | x==z = SA ox oz ny nw
  | y==w = SA nx nz oy ow
  | otherwise = SpotRect a
  where
    a@(Rect nx nz ny nw) = projectRect old new (toRect ao)

-- | project a [Spot a] from it's folded space to the given area
projectTo :: (Fractional a, Spaceable a) => Rect a -> [Spot a] -> [Spot a]
projectTo _ [] = []
projectTo vb (x:xs) = projectOn vb (toRect $ sconcat (x :| xs)) <$> (x:xs)

-- | project a [[Spot a]] from its folded space to the given area
projectTo2 :: (Fractional a, Spaceable a) => Rect a -> [[Spot a]] -> [[Spot a]]
projectTo2 vb xss = fmap (maybe id (projectOn vb) (fold $ foldRect . fmap toRect <$> xss)) <$> xss

-- | scale an area
scale :: (Num a) => Point a -> Rect a -> Rect a
scale (Point x' y') (Rect x z y w) = Rect (x*x') (z*x') (y*y') (w*y')

-- | widen an area by an amount
widen :: (Fractional a) => a -> Rect a -> Rect a
widen a (Rect x z y w) = Rect (x-a/2) (z+a/2) (y-a/2) (w+a/2)

-- | widen an area by a relative proportion
widenProp :: (Num a) => a -> Rect a -> Rect a
widenProp p (Rect x z y w) = Rect (x-wid) (z+wid) (y-hei) (w+hei)
  where
    wid = (p - 1) * (z - x)
    hei = (p - 1) * (w - y)

-- | rotate a point by x degrees relative to the origin
rotatePoint :: (Floating a) => a -> Point a -> Point a
rotatePoint d (Point x y) = Point (x * cos d' + y*sin d') (y* cos d'-x*sin d')
  where
    d' = d*pi/180

-- | the 4 corners of an area
pointsRect :: Rect a -> NonEmpty (Point a)
pointsRect (Rect x z y w) =
  Point x y :|
  [ Point x w
  , Point z y
  , Point z w
  ]

-- | rotate an area by x degrees relative to the origin
rotateRect :: (Floating a, Eq a, Lattice a) => a -> Rect a -> Rect a
rotateRect d r =
  sconcat $ (\(Point x y) -> Rect x x y y) . rotatePoint d <$> pointsRect r

-- | translate an area
translateRect :: (Num a) => Point a -> Rect a -> Rect a
translateRect (Point x' y') (Rect x z y w) = Rect (x+x') (z+x') (y+y') (w+y')

-- | Create area data for a formulae y = f(x) across an x range
areaXY :: (Fractional a, Spaceable a) => (a -> a) -> Range a -> Int -> [Rect a]
areaXY f r g = (\x -> Rect (x-tick/2) (x+tick/2) 0 (f x)) <$> grid MidPos r g
  where
    tick = NumHask.Space.width r / fromIntegral g

-- | Create point data for a formulae y = f(x) across an x range
dataXY :: (Fractional a, Spaceable a) => (a -> a) -> Range a -> Int -> [Point a]
dataXY f r g = (\x -> Point x (f x)) <$> grid OuterPos r g

-- | Create area data for a formulae c = f(x,y)
areaF :: (Fractional a, Spaceable a) => (Point a -> b) -> Rect a -> Grid (Rect a) -> [(Rect a, b)]
areaF f r g = (\x -> (x, f (mid x))) <$> gridSpace r g

-- | expand singleton dimensions
singletonUnit :: (Eq a, Fractional a) => Rect a -> Rect a
singletonUnit (Rect x z y w)
    | x == z && y == w = Rect (x - 0.5) (x + 0.5) (y - 0.5) (y + 0.5)
    | x == z = Rect (x - 0.5) (x + 0.5) y w
    | y == w = Rect x z (y - 0.5) (y + 0.5)
    | otherwise = Rect x z y w

defRect :: (Fractional a) => Maybe (Rect a) -> Rect a
defRect = fromMaybe unitRect

defRectS :: (Eq a, Fractional a) => Maybe (Rect a) -> Rect a
defRectS r = maybe unitRect singletonUnit r

addToRect :: (Lattice a, Eq a) => Rect a -> Maybe (Rect a) -> Rect a
addToRect r r' = sconcat $ r :| maybeToList r'

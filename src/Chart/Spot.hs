{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import NumHask.Data.Pair
import NumHask.Prelude as P hiding (Group)
import NumHask.Data.Rect
import NumHask.Analysis.Space

-- * primitive Chart elements

-- | a point on the xy-plane
newtype Point a = Point'
  { getPair :: Pair a
  } deriving (Eq, Show, Functor, Additive)

-- | pattern for Point x y
pattern Point :: a -> a -> Point a
pattern Point a b = Point' (Pair a b)
{-# COMPLETE Point #-}

-- | a rectangular area on the xy-plane
newtype Area a = Area'
  { getRect :: Rect a
  } deriving (Eq, Show, Functor, Space, Semigroup, Monoid, Distributive, Semiring)

instance (Lattice a, Field a, Subtractive a, FromInteger a) => FieldSpace (Area a) where
  type (Grid (Area a)) = Pair Int
  grid o (Area' r) n = grid o r n
  gridSpace (Area' r) n = Area' <$> gridSpace r n

-- | pattern for Area x z y w
pattern Area :: a -> a -> a -> a -> Area a
pattern Area x z y w = Area' (Rect x z y w)
{-# COMPLETE Area #-}

instance (Additive a) => Additive (Area a) where
  (Area x z y w) + (Area x' z' y' w') =
    Area (x+x') (z+z') (y+y') (w+w')

  zero = Area zero zero zero zero

-- | Area projection maths: some sort of affine projection lurking under the hood?
-- > width one = one
-- > mid zero = zero
instance (Lattice a, Subtractive a, Field a) =>
         Multiplicative (Area a) where
  (Area' (Ranges x0 y0)) * (Area' (Ranges x1 y1)) =
    Area' $ Ranges (x0 `rtimes` x1) (y0 `rtimes` y1)
    where
      rtimes a b = bool (Range (m - r/two) (m + r/two)) zero (a == zero || b == zero)
        where
          m = mid a + mid b
          r = width a * width b

  one = Area' $ Ranges rone rone where
    rone = Range (negate half) half

projectArea :: (Lattice a, Subtractive a, Field a) => Area a -> Area a -> Area a -> Area a
projectArea (Area' r0) (Area' r1) (Area' r2) = Area' $ projectRect r0 r1 r2

-- | unification of a point and an area on the plane
data Spot a =
  SpotPoint (Point a) |
  SpotArea (Area a)
  deriving (Eq, Show, Functor)

instance (Additive a) => Additive (Spot a) where
  SpotPoint (Point x y) + SpotPoint (Point x' y') = SpotPoint (Point (x+x') (y+y'))
  SpotPoint (Point x' y') + SpotArea (Area x z y w) = SpotArea $ Area (x+x') (z+x') (y+y') (w+y')
  SpotArea (Area x z y w) + SpotPoint (Point x' y') = SpotArea $ Area (x+x') (z+x') (y+y') (w+y')
  SpotArea (Area x z y w) + SpotArea (Area x' z' y' w') =
    SpotArea $ Area (x+x') (z+z') (y+y') (w+w')

  zero = SpotPoint (Point zero zero)

instance (Lattice a, Subtractive a, Field a) =>
         Multiplicative (Spot a) where
  x * y = SpotArea $ toArea x * toArea y

  one = SpotArea one

-- | pattern for SP x y
pattern SP :: a -> a -> Spot a
pattern SP a b = SpotPoint (Point a b)
{-# COMPLETE SP #-}

-- | pattern for SA lowerx upperx lowery uppery
pattern SA :: a -> a -> a -> a -> Spot a
pattern SA a b c d = SpotArea (Area a b c d)
{-# COMPLETE SA #-}

-- | Convert a spot to an Area
toArea :: Spot a -> Area a
toArea (SP x y) = Area x x y y
toArea (SpotArea a) = a

-- | Convert a spot to a Point
toPoint :: (Lattice a, Field a) => Spot a -> Point a
toPoint (SP x y) = Point x y
toPoint (SpotArea (Area' (Ranges x y))) = Point (mid x) (mid y)

instance (Lattice a) => Semigroup (Spot a) where
  (<>) a b = SpotArea (toArea a `union` toArea b)

instance (BoundedLattice a) => Monoid (Spot a) where
  mempty = SpotArea (Area' mempty)

-- | project a Spot from one Area to another, preserving relative position.
projectOn :: (BoundedLattice a, Lattice a, Field a, Subtractive a) => Area a -> Area a -> Spot a -> Spot a
projectOn new old@(Area x z y w) po@(SP px py)
  | new == mempty = po
  | old == mempty = po
  | x==z && y==w = po
  | x==z = SP px py'
  | y==w = SP px' py
  | otherwise = SP px' py'
  where
    (Pair px' py') = project old new (getPair $ toPoint po)
projectOn new old@(Area x z y w) ao@(SA ox oz oy ow)
  | new == mempty = ao
  | old == mempty = ao
  | x==z && y==w = ao
  | x==z = SA ox oz ny nw
  | y==w = SA nx nz oy ow
  | otherwise = SpotArea a
  where
    a@(Area nx nz ny nw) = projectArea old new (toArea ao)

-- | project a [Spot a] from it's folded space to the given area
projectTo :: (BoundedLattice a, Field a, Subtractive a) => Area a -> [Spot a] -> [Spot a]
projectTo vb xs = projectOn vb (toArea $ fold xs) <$> xs

-- | project a [[Spot a]] from its folded space to the given area
projectTo2 :: (BoundedLattice a, Field a, Subtractive a) => Area a -> [[Spot a]] -> [[Spot a]]
projectTo2 vb xss = fmap (projectOn vb (toArea $ fold $ fold <$> xss)) <$> xss

-- | scale an area
scale :: (Multiplicative a) => Point a -> Area a -> Area a
scale (Point x' y') (Area x z y w) = Area (x*x') (z*x') (y*y') (w*y')

-- | widen an area by an amount
widen :: (Field a, Subtractive a, FromInteger a) => a -> Area a -> Area a
widen a (Area x z y w) = Area (x-a/2) (z+a/2) (y-a/2) (w+a/2)

-- | widen an area by a relative proportion
widenProp :: (Multiplicative a, Subtractive a) => a -> Area a -> Area a
widenProp p (Area x z y w) = Area (x-wid) (z+wid) (y-hei) (w+hei)
  where
    wid = (p - one) * (z - x)
    hei = (p - one) * (w - y)

-- | rotate a point by x degrees relative to the origin
rotatePoint :: (FromInteger a, Subtractive a, TrigField a) => a -> Point a -> Point a
rotatePoint d (Point x y) = Point (x * cos d' + y*sin d') (y* cos d'-x*sin d')
  where
    d' = d*pi/180

-- | the 4 corners of an area
pointsArea :: Area a -> [Point a]
pointsArea (Area x z y w) =
  [ Point x y
  , Point x w
  , Point z y
  , Point z w
  ]

-- | rotate an area by x degrees relative to the origin
rotateArea :: (TrigField a, BoundedLattice a, FromInteger a, Subtractive a) => a -> Area a -> Area a
rotateArea d r =
  fold $ (\(Point x y) -> Area x x y y) . rotatePoint d <$> pointsArea r

-- | translate an area
translateArea :: (Additive a) => Point a -> Area a -> Area a
translateArea (Point x' y') (Area x z y w) = Area (x+x') (z+x') (y+y') (w+y')

-- | Create area data for a formulae y = f(x) across an x range
areaXY :: (Lattice a, Subtractive a, Field a, FromInteger a) => (a -> a) -> Range a -> Int -> [Area a]
areaXY f r g = (\x -> Area (x-tick/two) (x+tick/two) zero (f x)) <$> grid MidPos r g
  where
    tick = NumHask.Analysis.Space.width r / fromIntegral g

-- | Create point data for a formulae y = f(x) across an x range
dataXY :: (Lattice a, Field a, Subtractive a, FromInteger a) => (a -> a) -> Range a -> Int -> [Point a]
dataXY f r g = (\x -> Point x (f x)) <$> grid OuterPos r g

-- | Create area data for a formulae c = f(x,y)
areaF :: (Lattice a, Field a, Subtractive a, FromInteger a) => (Point a -> b) -> Area a -> Grid (Area a) -> [(Area a, b)]
areaF f r g = (\x -> (x, f (Point' $ mid (getRect x)))) <$> gridSpace r g

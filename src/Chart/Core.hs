{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Chart.Core
  ( padChart,
    frameChart,
    projectTo,
    projectSpots,
    projectSpotsWith,
    dataBox,
    toAspect,
    scaleAnn,
    defRect,
    defRectS,
    moveChart,
    hori,
    vert,
    stack,
    addChartBox,
    addChartBoxes,
  )
where

import Chart.Svg (styleBox, styleBoxes)
import Chart.Types
import Control.Category (id)
import Control.Lens hiding (transform)
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Semigroup hiding (getLast)
import NumHask.Space
import Protolude

-- | additively pad a [Chart]
padChart :: Double -> [Chart Double] -> [Chart Double]
padChart p cs = cs <> [Chart BlankA (maybeToList (SpotRect . padRect p <$> styleBoxes cs))]

-- | overlay a frame on some charts with some additive padding between
frameChart :: RectStyle -> Double -> [Chart Double] -> [Chart Double]
frameChart rs p cs = [Chart (RectA rs) (maybeToList (SpotRect . padRect p <$> styleBoxes cs))] <> cs

-- | project a Spot from one Rect to another, preserving relative position.
projectOn :: (Ord a, Fractional a) => Rect a -> Rect a -> Spot a -> Spot a
projectOn new old@(Rect x z y w) po@(SP px py)
  | x == z && y == w = po
  | x == z = SP px py'
  | y == w = SP px' py
  | otherwise = SP px' py'
  where
    (Point px' py') = project old new (toPoint po)
projectOn new old@(Rect x z y w) ao@(SR ox oz oy ow)
  | x == z && y == w = ao
  | x == z = SR ox oz ny nw
  | y == w = SR nx nz oy ow
  | otherwise = SpotRect a
  where
    a@(Rect nx nz ny nw) = projectRect old new (toRect ao)

-- | project a [Spot a] from it's folded space to the given area
projectTo :: (Ord a, Fractional a) => Rect a -> [Spot a] -> [Spot a]
projectTo _ [] = []
projectTo vb (x : xs) = projectOn vb (toRect $ sconcat (x :| xs)) <$> (x : xs)

-- | project a [[Spot a]] from its folded space to the given area
projectTo2 :: (Ord a, Fractional a) => Rect a -> [[Spot a]] -> [[Spot a]]
projectTo2 vb xss = fmap (maybe id (projectOn vb) (fold $ foldRect . fmap toRect <$> xss)) <$> xss

defRect :: (Fractional a) => Maybe (Rect a) -> Rect a
defRect = fromMaybe unitRect

defRectS :: (Eq a, Fractional a) => Maybe (Rect a) -> Rect a
defRectS r = maybe unitRect singletonUnit r
  where
    singletonUnit :: (Eq a, Fractional a) => Rect a -> Rect a
    singletonUnit (Rect x z y w)
      | x == z && y == w = Rect (x - 0.5) (x + 0.5) (y - 0.5) (y + 0.5)
      | x == z = Rect (x - 0.5) (x + 0.5) y w
      | y == w = Rect x z (y - 0.5) (y + 0.5)
      | otherwise = Rect x z y w

projectSpots :: (Chartable a) => Rect a -> [Chart a] -> [Chart a]
projectSpots a cs = cs'
  where
    xss = projectTo2 a (spots <$> cs)
    ss = annotation <$> cs
    cs' = zipWith Chart ss xss

projectSpotsWith :: (Chartable a) => Rect a -> Rect a -> [Chart a] -> [Chart a]
projectSpotsWith new old cs = cs'
  where
    xss = fmap (projectOn new old) . spots <$> cs
    ss = annotation <$> cs
    cs' = zipWith Chart ss xss

toAspect :: (Fractional a) => Rect a -> a
toAspect (Rect x z y w) = (z - x) / (w - y)

-- |
dataBox :: Chartable a => [Chart a] -> Maybe (Rect a)
dataBox cs = foldRect . mconcat $ fmap toRect <$> (spots <$> cs)

scaleAnn :: Double -> Annotation -> Annotation
scaleAnn x (LineA a) = LineA $ a & #width %~ (* x)
scaleAnn x (RectA a) = RectA $ a & #borderSize %~ (* x)
scaleAnn x (TextA a txs) = TextA (a & #size %~ (* x)) txs
scaleAnn x (GlyphA a) = GlyphA (a & #size %~ (* x))
scaleAnn x (PixelA a) = PixelA $ a & #pixelRectStyle . #borderSize %~ (* x)
scaleAnn _ BlankA = BlankA

moveChart :: Chartable a => Spot a -> [Chart a] -> [Chart a]
moveChart sp cs = fmap (#spots %~ fmap (sp +)) cs

-- horizontally stack a list of list of charts (proceeding to the right) with a gap between
hori :: Double -> [[Chart Double]] -> [Chart Double]
hori _ [] = []
hori gap cs = foldl step [] cs
  where
    step x a = x <> (a & fmap (#spots %~ fmap (\s -> SP (z x) 0 - SP (origx x) 0 + s)))
    z xs = maybe 0 (\(Rect _ z' _ _) -> z' + gap) (styleBoxes xs)
    origx xs = maybe 0 (\(Rect x' _ _ _) -> x') (styleBoxes xs)

-- vertically stack a list of charts (proceeding upwards), aligning them to the left
vert :: Double -> [[Chart Double]] -> [Chart Double]
vert _ [] = []
vert gap cs = foldl step [] cs
  where
    step x a = x <> (a & fmap (#spots %~ fmap (\s -> SP (origx x - origx a) (w x) + s)))
    w xs = maybe 0 (\(Rect _ _ _ w') -> w' + gap) (styleBoxes xs)
    origx xs = maybe 0 (\(Rect x' _ _ _) -> x') (styleBoxes xs)

-- stack a list of charts horizontally, then vertically
stack :: Int -> Double -> [[Chart Double]] -> [Chart Double]
stack _ _ [] = []
stack n gap cs = vert gap (hori gap <$> group' cs [])
  where
    group' [] acc = reverse acc
    group' x acc = group' (drop n x) (take n x : acc)

addChartBox :: Chart Double -> Rect Double -> Rect Double
addChartBox c r = sconcat (r :| maybeToList (styleBox c))

addChartBoxes :: [Chart Double] -> Rect Double -> Rect Double
addChartBoxes c r = sconcat (r :| maybeToList (styleBoxes c))

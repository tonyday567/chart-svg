{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wall #-}

module Chart.Core
  ( pShow',
    frame,
    pad,
    projectTo,
    projectSpots,
    projectSpotsWith,
    dataBox,
    toAspect,
    showOrigin,
    showOriginWith,
    defaultOrigin,
    blue,
    grey,
    red,
    black,
    white,
    blend,
    blend',
    scaleAnn,
    placedLabel,
    defRect,
    defRectS,
    addToRect,
    moveChart,
  )
where

import Chart.Types
import Codec.Picture.Types
import Control.Category (id)
import Control.Lens hiding (transform)
import Data.Foldable
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Data.Monoid
import Data.Semigroup hiding (getLast)
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import NumHask.Space
import Protolude
import Text.Pretty.Simple (pShowNoColor)

pShow' :: (Show a) => a -> Text
pShow' = toStrict . pShowNoColor

-- | A framing chart.
frame :: RectStyle -> Rect a -> Chart a
frame o r = Chart (RectA o) [SpotRect r]

-- | widen a Rect by a fraction.
pad :: (Chartable a) => a -> Rect a -> Rect a
pad p r = fmap (p *) r

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

addToRect :: (Ord a) => Rect a -> Maybe (Rect a) -> Rect a
addToRect r r' = sconcat $ r :| maybeToList r'

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

-- | include a circle at the origin with size and color
showOriginWith :: (Chartable a) => GlyphStyle -> Chart a
showOriginWith c =
  Chart
    (GlyphA c)
    [SP 0 0]

defaultOrigin :: GlyphStyle
defaultOrigin = GlyphStyle 0.05 red 0.5 grey 0 0 CircleGlyph Nothing Nothing

-- | include a red circle at the origin
showOrigin :: (Chartable a) => Chart a
showOrigin = showOriginWith defaultOrigin

scaleAnn :: Double -> Annotation -> Annotation
scaleAnn x (LineA a) = LineA $ a & #width %~ (* x)
scaleAnn x (RectA a) = RectA $ a & #borderSize %~ (* x)
scaleAnn x (TextA a txs) = TextA (a & #size %~ (* x)) txs
scaleAnn x (GlyphA a) = GlyphA (a & #size %~ (* x))
scaleAnn x (PixelA a) = PixelA $ a & #pixelRectStyle . #borderSize %~ (* x)
scaleAnn _ BlankA = BlankA

placedLabel :: (Chartable a) => Point a -> a -> Text.Text -> Chart a
placedLabel p d t =
  Chart
    ( TextA
        ( defaultTextStyle
            & #rotation ?~ realToFrac d
        )
        [t]
    )
    [SpotPoint p]

moveChart :: Chartable a => Spot a -> [Chart a] -> [Chart a]
moveChart sp cs = fmap (#spots %~ fmap (sp +)) cs

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

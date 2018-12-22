{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RebindableSyntax #-}

module Chart.Hud where

-- import qualified Text.Blaze as B
import qualified Data.Text as Text
-- import qualified Data.Text.Lazy.IO as Lazy
import NumHask.Prelude as P hiding (rotate, singleton, Group)
-- import qualified Data.Colour.SRGB as C
-- import Control.Monad.State.Lazy
import Graphics.Svg.Types as Svg
import Graphics.Svg as Svg
import NumHask.Data.Rect
import NumHask.Data.Pair
import NumHask.Data.Range
import NumHask.Analysis.Space
import qualified Data.Map as Map
import Lens.Micro
import Codec.Picture.Types
import Data.Generics.Product (field)
import Data.Generics.Sum
import Linear.V2
-- import Data.Colour
import Control.Exception
import Chart.Svg
import Chart.Spot
import Chart.Core
import Formatting
import Data.Scientific
import Data.List (nub)

newtype Hud a = Hud { unhud :: ViewBox a -> Area a -> [Chart a] }

instance Semigroup (Hud a) where
  (<>) (Hud h1) (Hud h2) = Hud $ \vb a -> h1 vb a <> h2 vb a

instance Monoid (Hud a) where
  mempty = Hud (const (const []))

layer :: Chartable a => Hud a -> Hud a -> Hud a
layer (Hud h1) (Hud h2) =
  Hud $ \vb@(ViewBox asp) a -> h1 vb a <> h2 (ViewBox $ asp <> styleBoxes (h1 vb a)) a

hudSvg :: (ToRatio a, FromRatio a, Subtractive a, Field a, BoundedLattice a) =>
  ViewBox a -> [Hud a] -> [Chart a] -> ChartSvg a
hudSvg (ViewBox asp) hs cs =
  chartSvg_ (ViewBox (styleBoxes (cs' <> h vb' xs))) (cs' <> h vb' xs)
  where
    (Hud h) = foldl layer mempty hs
    cs' = projectSpots asp cs
    vb' = ViewBox $ styleBoxes cs'
    xs = toArea $ fold $ fold (spots <$> cs)

-- | Orientation for an element.  Watch this space for curvature!
data Orientation
  = Hori
  | Vert
  deriving (Show, Eq, Generic)

-- | Placement of elements around (what is implicity but maybe shouldn't just be) a rectangular canvas
data Place
  = PlaceLeft
  | PlaceRight
  | PlaceTop
  | PlaceBottom
  deriving (Show, Eq, Generic)

data Bar a = Bar
  { place :: Place
  , rstyle :: RectStyle
  , wid :: a
  , buff :: a
  } deriving (Show, Eq, Generic)

canvas :: RectStyle -> DrawAttributes -> Hud a
canvas s das = Hud $ \(ViewBox vb) _ -> [Chart (RectA s) das [SpotArea vb]]

bar :: (Chartable a) => Bar a -> DrawAttributes -> Hud a
bar b das = Hud $ \(ViewBox (Area x z y w)) _ -> (:[]) $ case b ^. field @"place" of
  PlaceLeft ->
    Chart (RectA (rstyle b)) das
    [SA x z
     (y - (b ^. (field @"wid")) - (b ^. (field @"buff")))
     (y - (b ^. (field @"buff")))
    ]
  PlaceRight -> Chart (RectA (rstyle b)) das
    [SA x z (w + (b ^. (field @"wid")) + (b ^. (field @"buff")))
     (w + (b ^. (field @"buff")))]
  PlaceTop -> Chart (RectA (rstyle b)) das
    [SA (z + (b ^. (field @"wid")) + (b ^. (field @"buff")))
     (z + (b ^. (field @"buff"))) y w]
  PlaceBottom -> Chart (RectA (rstyle b)) das
    [SA (x - (b ^. (field @"wid")) - (b ^. (field @"buff")))
     (x - (b ^. (field @"buff"))) y w]


bars :: (Chartable a) => [Bar a] -> DrawAttributes -> Hud a
bars bs das = mconcat ((\b -> bar b das) <$> bs)

data Tick a = Tick
  { place :: Place
  , gstyle :: GlyphStyle
  , wid :: a
  , buff :: a
  , tstyle :: TickStyle a
  } deriving (Show, Eq, Generic)

-- | Style of tick marks on an axis.
data TickStyle a
  = TickNone -- ^ no ticks on axis
  | TickLabels [P.Text] -- ^ specific labels
  | TickRound Int -- ^ sensibly rounded ticks and a guide to how many
  | TickExact Int -- ^ exactly n equally spaced ticks
  | TickPlaced [(a, P.Text)] -- ^ specific labels and placement
  deriving (Show, Eq, Generic)

-- | compute tick values and labels given options and ranges
computeTicks :: (RealFloat a, Ord a, ExpField a, QuotientField a Integer, FromInteger a, Chartable a) => TickStyle a -> Range a -> Range a -> ([a], [P.Text])
computeTicks s r asp =
    case s of
      TickNone -> ([], [])
      TickRound n -> (project r asp <$> ticks0, precision 0 ticks0)
        where ticks0 = gridSensible OuterPos r (fromIntegral n)
      TickExact n -> (project r asp <$> ticks0, precision 3 ticks0)
        where ticks0 = grid OuterPos r n
      TickLabels ls ->
          ( project (Range 0 (fromIntegral $ length ls)) asp <$>
            ((\x -> x - 0.5) . fromIntegral <$> [1 .. length ls])
          , ls)
      TickPlaced xs -> (project r asp . fst <$> xs, snd <$> xs)

-- | Provide formatted text for a list of numbers so that they are just distinguished.  'precision 2 ticks' means give the tick labels as much precision as is needed for them to be distinguished, but with at least 2 significant figues.
precision :: (Ord a, FromInteger a, FromRatio a, QuotientField a Integer, RealFloat a) => Int -> [a] -> [P.Text]
precision n0 xs
  | foldr max 0 xs < 0.01 = precLoop expt' n0 (fromFloatDigits <$> xs)
  | foldr max 0 xs > 100000 = precLoop expt' n0 (fromFloatDigits <$> xs)
  | foldr max 0 xs > 1000 =
    precLoopInt (const Formatting.commas) n0 (floor <$> xs :: [Integer])
  | otherwise = precLoop fixed n0 xs
  where
    expt' x = scifmt Exponent (Just x)
    precLoop f n xs' =
      let s = sformat (f n) <$> xs'
      in if s == nub s
           then s
           else precLoop f (n + 1) xs'
    precLoopInt f n xs' =
      let s = sformat (f n) <$> xs'
      in if s == nub s
           then s
           else precLoopInt f (n + 1) xs'


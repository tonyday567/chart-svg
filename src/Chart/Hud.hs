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

bar PlaceBottom (Rect x z y w) rs das wid buff =
  Chart (RectA rs) das [SA x z (y - wid - buff) (y - buff)]
bar PlaceTop (Rect x z y w) rs das wid buff =
  Chart (RectA rs) das [SA x z (w + buff) (w + buff + wid)]
bar PlaceLeft (Rect x z y w) rs das wid buff =
  Chart (RectA rs) das [SA (x - wid - buff) (x - buff) y w]
bar PlaceLeft (Rect x z y w) rs das wid buff =
  Chart (RectA rs) das [SA (z + buff) (z + wid + buff) y w]




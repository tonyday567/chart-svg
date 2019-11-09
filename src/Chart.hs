{-# OPTIONS_GHC -Wall #-}

-- | A haskell Charting library targetting SVG
module Chart
  ( -- * Types
    -- $types
    module Chart.Types,
    module Chart.Core,
    module Chart.Svg,
    module Chart.Hud,
    module Chart.Page,
    module NumHask.Space,
    PixelRGB8 (..),
  )
where

import Chart.Core
import Chart.Hud
import Chart.Page
import Chart.Svg
import Chart.Types
import Codec.Picture.Types
import NumHask.Space

-- $types
-- There are some.

{-# OPTIONS_GHC -Wall #-}

-- | A haskell Charting library targetting SVG
module Chart
  ( -- * Types
    -- $types
    module Chart.Types,
    module Chart.Color,
    module Chart.Core,
    module Chart.Svg,
    module Chart.Hud,
    module Chart.Format,
    module Chart.Page,
    module Chart.Render,
    module Chart.Bar,
    module Chart.Pixel,
    module NumHask.Space,
    PixelRGB8 (..),
  )
where

import Chart.Bar
import Chart.Color
import Chart.Core
import Chart.Format
import Chart.Hud
import Chart.Page
import Chart.Pixel
import Chart.Render
import Chart.Svg
import Chart.Types
import Codec.Picture.Types
import NumHask.Space

-- $types
-- There are some.

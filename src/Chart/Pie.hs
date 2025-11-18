{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pie charts
module Chart.Pie
  ( PieOptions (..),
    defaultPieOptions,
    PieData (..),
    PieLabelPlacement (..),
    PieTickStyle (..),
    pieChart,
  )
where

import Chart.Data
import Chart.Hud
import Chart.Markup
import Chart.Primitive
import Chart.Style
import Data.Bool
import Data.Colour
import Data.Data
import Data.Foldable
import Data.FormatN
import Data.List (transpose)
import Data.Maybe
import Data.Text (Text, pack)
import GHC.Generics
import Optics.Core
import Prelude hiding (abs)

-- $setup
--
-- >>> :set -XOverloadedLabels
-- >>> :set -XOverloadedStrings
-- >>> import Chart
-- >>> import Optics.Core
-- >>> import Data.Text (pack)

-- | Typical bar chart options.
--
-- The internal model for a bar chart (across the x-axis for a vertical bar chart) is:
--
-- - half the outerGap at the start and the end.
--
-- - each row collection of bars, including the outerGap and innerGaps has a value of 1.
--
-- - the entire x range of the chart isequal to the number of rows in the bar data.
--
-- - The value of inner and outer gaps are relative to this model.
--
-- >>> let barDataExample = BarData [[1, 2, 3, 5, 8, 0, -2, 11, 2, 1], [1 .. 10]] (("row " <>) . pack . show <$> [1 .. 11]) (("column " <>) . pack . show <$> [1 .. 2])
-- >>> let barExample = barChart defaultBarOptions barDataExample
--
-- > writeChartOptions "other/bar.svg" barExample
--
-- ![bar chart example](other/bar.svg)
data PieOptions = PieOptions
  { glyph :: Glyph,
    innerGlyph :: Maybe Glyph,
    -- | origin point for secant creation.
    origin :: Point Double,
    -- | options for label placement
    pieLabelPlacement :: PieLabelPlacement,
    pieTickStyle :: PieTickStyle,
    pieSecantOptions :: [SecantOptions],
    pieLabelStyles :: [Style],
    -- | offset distance from origin
    pieOffsets :: [Double],
    -- | offset angle relative to mid-point line of secant
    pieOffsetAngles :: [Double]
 } deriving (Eq, Show, Generic, Data)


data PieSecantOptions = PieSecantOptions
  { pieSecantStyle :: Style,
    pieLabelStyle :: Style,
    -- | offset distance from origin
    offset :: [Double],
    -- | offset angle relative to mid-point line of secant

  }


data PieLabelPlacement = PieLabelInside | PieLabelOutside PieTickStyle
  deriving (Eq, Show, Generic, Data)

data PieTickStyle = PieTickHori | PieTickDefault | PieTickHori
  deriving (Eq, Show, Generic, Data)

-- | The official pie options.
defaultPieOptions :: PieOptions
defaultPieOptions =
  PieOptions
    CircleGlyph
    Nothing
    zero
    PieLabelInside
    gs
    ts
    os
    oas
  where
    gs = (\x -> defaultGlyphStyle & set #borderSize 0.005 & set #borderColor (palette x) & set #color (paletteO x 0.7)) <$> [1, 2, 6, 7, 5, 3, 4, 0]
    ts = (\x -> defaultTextStyle & set #color (palette x) & set #size 0.03) <$> [1, 2, 6, 7, 5, 3, 4, 0]
    os = replicate 8 zero
    oas = replicate 8 zero

pieChart :: PieOptions -> [(Text, Double)] -> Chart
pieChart os xs = undefined

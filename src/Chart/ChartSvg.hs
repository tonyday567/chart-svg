module Chart.ChartSvg where

import NumHask.Space hiding (Element)
import Protolude
import Chart.Types
import Graphics.Svg (Element, Tree(..))
import qualified Data.Map.Strict as Map

-- | Svg of a Chart consists of a viewBox expressed as a Rect, an Svg `Tree` list and any named elements for the definitions section.
data ChartSvg a
  = ChartSvg
      { vbox :: Rect a,
        chartTrees :: [Tree],
        chartDefs :: Map.Map Text Element
      }
  deriving (Show)

instance (Ord a) => Semigroup (ChartSvg a) where
  (ChartSvg a b c) <> (ChartSvg a' b' c') = ChartSvg (a <> a') (b <> b') (c <> c')

instance (Chartable a) => Monoid (ChartSvg a) where
  mempty = ChartSvg unitRect mempty mempty

-- | widen a ChartSvg Rect by a fraction of the size.
pad :: (Chartable a) => a -> ChartSvg a -> ChartSvg a
pad p (ChartSvg vb s defs) = ChartSvg (fmap (p *) vb) s defs

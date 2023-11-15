{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Combine charts that share a common canvas.
module Chart.Compound
  ( runHudCompoundWith,
    addHudCompound,
    compoundMerge,
    writeChartOptionsCompound,
    encodeChartOptionsCompound,
    markupChartOptionsCompound,
  )
where

import Chart.Data
import Chart.Hud
import Chart.Markup
import Chart.Primitive
import Control.Monad.State.Lazy
import Data.ByteString.Char8 qualified as C
import Data.List qualified as List
import Data.Maybe
import MarkupParse
import Optics.Core
import Prelude

-- | Write multiple charts to a single file sharing the canvas.
writeChartOptionsCompound :: FilePath -> [ChartOptions] -> IO ()
writeChartOptionsCompound fp cs = C.writeFile fp (encodeChartOptionsCompound cs)

-- | Encode multiple charts.
encodeChartOptionsCompound :: [ChartOptions] -> C.ByteString
encodeChartOptionsCompound [] = mempty
encodeChartOptionsCompound cs@(c0 : _) =
  markdown_ (view (#markupOptions % #renderStyle) c0) Xml (markupChartOptionsCompound cs)

-- | Create Markup representing multiple charts sharing a common canvas.
markupChartOptionsCompound :: [ChartOptions] -> Markup
markupChartOptionsCompound [] = mempty
markupChartOptionsCompound cs@(co0 : _) =
  header
    (view (#markupOptions % #markupHeight) co0)
    (fromMaybe one viewbox)
    ( markupCssOptions (view (#markupOptions % #cssOptions) co0)
        <> markupChartTree csAndHuds
    )
  where
    viewbox = padSingletons <$> view styleBox' csAndHuds
    csAndHuds = addHudCompound (zip (view #hudOptions <$> cs) (view #charts <$> cs)) (view (#markupOptions % #chartAspect) co0)

-- | Merge a list of ChartOptions, treating each element as charts to be merged. Note that this routine mempties the hud options and converts them to charts.
compoundMerge :: [ChartOptions] -> ChartOptions
compoundMerge [] = mempty
compoundMerge cs@(c0 : _) =
  ChartOptions
    (view #markupOptions c0)
    mempty
    (addHudCompound (zip (view #hudOptions <$> cs) (view #charts <$> cs)) (view (#markupOptions % #chartAspect) c0))

-- | Decorate a ChartTree with HudOptions, merging the individual hud options.
-- FIXME: align with addHud structure
addHudCompound :: [(HudOptions, ChartTree)] -> ChartAspect -> ChartTree
addHudCompound [] _ = mempty
addHudCompound ts@((_, cs0) : _) asp = undefined
  where

{-
    hss =
      ts &
      fmap (\(db,hs,_) -> fmap (over #hud (withStateT (#dataBox .~ db))) hs) &
      mconcat &
      prioritizeHuds &
      mapM_ (closes . fmap (view #hud)) &
      flip execState (HudChart css mempty undefined) &
      (\x -> group (Just "chart") [view #chart x] <> group (Just "hud") [view #hud x])

    css =
      ts &
      fmap (\(db,_,ct) -> over chart' (projectWith cb db) ct) &
      mconcat

-}
{-
  runHudCompoundWith
    -- FIXME:
    (fromMaybe one $ initialCanvas asp)
    (zip3 dbs hss css)
  where
    hss = zipWith (\i hs -> fmap (over #priority (+Priority (i*0.1))) hs) [0..] (fst <$> huds)
    dbs = snd <$> huds
    css = snd <$> ts -- <> (blank <$> dbs)
    huds = (\(ho, cs) -> toHuds ho (maybe one padSingletons (view box' cs))) <$> ts

-}

-- | Combine a collection of chart trees that share a canvas box.
runHudCompoundWith ::
  -- | canvas
  CanvasBox ->
  -- | databox-huds-chart tuples representing independent chart trees occupying the same canvas space
  [(DataBox, [Hud], ChartTree)] ->
  -- | integrated chart tree
  ChartTree
runHudCompoundWith cb ts = undefined

{-
hss
  where
    hss =
      ts &
      fmap (\(db,hs,_) -> fmap (over #hud (withStateT (#dataBox .~ db))) hs) &
      mconcat &
      prioritizeHuds &
      mapM_ (closes . fmap (view #hud)) &
      flip execState (HudChart css mempty undefined) &
      (\x -> group (Just "chart") [view #chart x] <> group (Just "hud") [view #hud x])

    css =
      ts &
      fmap (\(db,_,ct) -> over chart' (projectWith cb db) ct) &
      mconcat

-}

prioritizeHuds :: [Hud] -> [[Hud]]
prioritizeHuds hss =
  hss
    & List.sortOn (view #priority)
    & List.groupBy (\a b -> view #priority a == view #priority b)

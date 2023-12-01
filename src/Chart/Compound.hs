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
import Data.Foldable
import Data.Bool
import Chart.Style

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
    viewbox
    ( markupCssOptions (view (#markupOptions % #cssOptions) co0)
        <> markupChartTree ctFinal
    )
  where
    viewbox = maybe one padSingletons (view styleBox' ctFinal)
    ctFinal =
      projectChartCompoundWith
      (view (#markupOptions % #chartAspect) co0)
      (zip (view #hudOptions <$> cs) (view #chartTree <$> cs))

projectChartCompoundWith :: ChartAspect -> [(HudOptions,ChartTree)] -> ChartTree
projectChartCompoundWith asp css = ctFinal
  where
    csAndHud = addHudCompound asp css
    viewbox = finalCanvas asp (Just csAndHud)
    ctFinal = set styleBox' (Just viewbox) csAndHud

-- | Merge a list of ChartOptions, treating each element as charts to be merged. Note that this routine mempties the hud options and converts them to charts.
compoundMerge :: [ChartOptions] -> ChartOptions
compoundMerge [] = mempty
compoundMerge cs@(c0 : _) =
  ChartOptions
    (view #markupOptions c0)
    mempty
    (addHudCompound (view (#markupOptions % #chartAspect) c0) (zip (view #hudOptions <$> cs) (view #chartTree <$> cs)))

-- | Decorate a ChartTree with HudOptions, merging the individual hud options.
addHudCompound :: ChartAspect -> [(HudOptions, ChartTree)] -> ChartTree
addHudCompound _ [] = mempty
addHudCompound asp ts@((_, cs0) : _) =
  runHudCompoundWith
    (initialCanvas asp (Just cs0))
    (zip3 dbs' hss css')
  where
    css :: [ChartTree]
    css = snd <$> ts
    hos = fst <$> ts
    dbs = maybe one padSingletons . view box' <$> css
    huds = zipWith toHuds hos dbs
    mdbs = fst <$> huds
    hss = snd <$> huds
    dbs' = zipWith fromMaybe dbs mdbs
    css' :: [ChartTree]
    css' = zipWith3 (\cs mdb db -> cs <> maybe mempty (\r -> bool (named "datapadding" [BlankChart defaultStyle [r]]) mempty (r == db)) mdb) css mdbs dbs

-- | Combine a collection of chart trees that share a canvas box.
runHudCompoundWith ::
  -- | initial canvas
  CanvasBox ->
  -- | databox-huds-chart tuples representing independent chart trees occupying the same canvas space
  [(DataBox, [Hud], ChartTree)] ->
  -- | integrated chart tree
  ChartTree
runHudCompoundWith cb ts = hss
  where
    hss =
      ts &
      fmap (\(_,hs,_) -> hs) &
      mconcat &
      prioritizeHuds &
      fmap (fmap (view (#phud % #item))) &
      foldl' (\x a -> makeHuds a x) hc0 &
      fromHudChart
    css =
      ts &
      fmap (\(db,_,ct) -> over chart' (projectWith cb db) ct) &
      mconcat
    hc0 = HudChart (css & set styleBox' (Just cb)) mempty

prioritizeHuds :: [Hud] -> [[Hud]]
prioritizeHuds hss =
  hss
    & List.sortOn (view (#phud % #priority))
    & List.groupBy (\a b -> view (#phud % #priority) a == view (#phud % #priority) b)

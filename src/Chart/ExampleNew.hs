{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Chart API
module Chart.ExampleNew where

import Chart.Style
import Chart.Chart
import Chart.Svg
import Chart.Hud
import Data.Generics.Labels ()
import NumHask.Prelude
import NumHask.Space as NH hiding (Element)
import Data.List.NonEmpty as NonEmpty
import GHC.OverloadedLabels
import Control.Lens
import Data.Colour

-- * Examples

-- | unit example()
--
-- ![unit example](other/unit.svg)
unitExample :: ChartSvg
unitExample = mempty & #chartTree .~ [RectChart defaultRectStyle (one:|[])]

-- |x <> () rect example
--
-- ![rect example](other/rect.svg())
rectExample :: ChartSvg
rectExample =
  mempty &
  (#hudOptions .~ (defaultHudOptions &
    #hudAxes .~ [defaultAxisOptions & #axisTick . #ltick .~ Nothing] &
    #hudCanvas .~ Nothing)) &
  #chartTree .~ NumHask.Prelude.zipWith RectChart ropts rss

rss :: [NonEmpty (Rect Double)]
rss =
  [ NonEmpty.fromList $ gridR (\x -> exp (-(x ** 2) / 2)) (Range -5 5) 50,
    NonEmpty.fromList $ gridR (\x -> 0.5 * exp (-(x ** 2) / 8)) (Range -5 5) 50
  ]

ropts :: [RectStyle]
ropts =
  [ blob (setOpac 0.3 (palette1 3)),
    blob (setOpac 0.3 (palette1 5))
  ]

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Chart.Page
  where

import Chart.Core
import Chart.Hud
import Codec.Picture.Types
import Control.Category (id)
import Control.Lens
import Data.Attoparsec.Text
import Data.Generics.Labels ()
import Graphics.Svg.Types (TextAnchor(..))
import Lucid hiding (b_)
import Protolude
import Web.Page
import Web.Page.Bootstrap
import Web.Page.Html.Input
import Web.Page.Bridge
import Web.Page.Rep
import qualified Box ()

repGlyphShape :: (Monad m) => SharedRep m GlyphShape
repGlyphShape = toGlyph <$>
  dropdown takeText show "Shape"
  [ "Circle"
  , "Triangle"
  , "Square"
  , "Ellipse"
  , "Rectangle"
  , "Rounded Rectangle"
  , "Verticle Line"
  , "Horizontal Line"
  , "Smiley Face"
  ] "Circle"

repGlyphStyle :: (Monad m) => SharedRep m GlyphStyle
repGlyphStyle = first (cardify [("style", "width: 10 rem;")] mempty (Just "Glyph Style")) $ do
  sh <- repGlyphShape
  sz <- slider "Size" 0 0.2 0.01 (0.04 :: Double)
  gc <- Web.Page.Rep.color "Color"
    (defaultGlyphStyle ^. #color)
  go <- slider "Opacity" 0 1 0.1 (1 :: Double)
  bsz <- slider "Border Size" 0 0.02 0.001 (0.004 :: Double)
  gbc <- Web.Page.Rep.color "Border Color"
    (PixelRGB8 64 64 64)
  gbo <- slider "Border Opacity" 0 1 0.1 (1 :: Double)
  pure (GlyphStyle sz gc go gbc gbo bsz sh)

repTitle :: (Monad m) => Text -> Title Double -> SharedRep m (Title Double)
repTitle txt cfg = do
  ttext <- textbox "text" txt
  ts <- repTextStyle (cfg^. #style)
  tp <- repPlace (cfg ^. #place)
  ta <- repAnchor (cfg ^. #align)
  b <- slider "buffer" 0 0.2 0.01 (cfg ^. #buff)
  pure $ Title ttext ts tp ta b

repTextStyle :: (Monad m) => TextStyle -> SharedRep m TextStyle
repTextStyle s = do
  ts <- slider "size" 0.02 0.3 0.01 (s ^. #size)
  tc <- Web.Page.Rep.color "color" (s ^. #color)
  to' <- slider "opacity" 0 1 0.1 (s ^. #opacity)
  ta <- repAnchor (s ^. #alignH)
  th <- slider "hsize" 0.2 1 0.05 (s ^. #hsize)
  tv <- slider "vsize" 0.5 2 0.05 (s ^. #vsize)
  tn <- slider "nudge1" (-0.5) 0.5 0.05 (s ^. #nudge1)
  trc <- maybeRep "rotation" (maybe False  (const True) (s ^. #rotation))
    (slider "rotation" (-180) 180 10 (maybe 0 identity (s ^. #rotation)))
  pure $ TextStyle ts tc to' ta th tv tn trc

repPlace :: (Show a, Monad m) => Place a -> SharedRep m (Place a)
repPlace =
  dropdown (toPlace <$> takeText) fromPlace "Placement"
  [ "Top"
  , "Bottom"
  , "Left"
  , "Right"
  ]

repAnchor :: (Monad m) => TextAnchor -> SharedRep m TextAnchor
repAnchor =
    dropdown
    (toTextAnchor <$> takeText)
    anchorToString
    "Anchor"
    (fromTextAnchor <$> [TextAnchorStart, TextAnchorMiddle, TextAnchorEnd])

repCanvasConfig :: (Monad m) => CanvasConfig -> SharedRep m CanvasConfig
repCanvasConfig cfg = do
  canvasc <- Web.Page.Rep.color "Canvas Color" (cfg ^. #color)
  canvaso <- slider "Canvas Opacity" 0 0.2 0.01 (cfg ^. #opacity)
  pure $ CanvasConfig canvasc canvaso

repRectStyle :: (Monad m) => RectStyle -> SharedRep m RectStyle
repRectStyle s = do
  bs <- slider "border size" 0.02 0.3 0.01 (s ^. #borderSize)
  bc <- Web.Page.Rep.color "border color" (s ^. #borderColor)
  bo <- slider "border opacity" 0 1 0.1 (s ^. #borderOpacity)
  c <- Web.Page.Rep.color "color" (s ^. #color)
  o <- slider "opacity" 0 1 0.1 (s ^. #opacity)
  pure $ RectStyle bs bc bo c o

repBar :: (Monad m) => Bar Double -> SharedRep m (Bar Double)
repBar cfg = do
  p <- repPlace (cfg ^. #place)
  r <- repRectStyle (cfg ^. #rstyle)
  w <- slider "width" 0 0.1 0.01 (cfg ^. #wid)
  b <- slider "buffer" 0 0.2 0.01 (cfg ^. #buff)
  pure $ Bar p r w b

repAxisConfig :: (Monad m) => AxisConfig Double -> SharedRep m (AxisConfig Double)
repAxisConfig cfg = do
  b <-
    maybeRep
    "axis bar"
    (isJust (cfg ^. #abar))
    (repBar (maybe defaultBar identity (cfg ^. #abar)))
  hauto <- checkbox "auto" (cfg^. #hasAuto)
  tn <- sliderI "no. ticks" 0 20 1 (cfg ^. #tickN)
  ts <- slider "tick size" 0 0.2 0.01 (cfg ^. #tickSize)
  p <- repPlace (cfg ^. #place)
  pure $ AxisConfig b hauto tn ts p

repHudConfig :: (Monad m) => HudConfig Double -> SharedRep m (HudConfig Double)
repHudConfig cfg = do
  t <- maybeRep "title" (isJust (cfg ^. #title1)) $
    repTitle "label for title" (maybe (defaultTitle "actual title") id (cfg ^. #title1))
  can <- maybeRep "canvas" (isJust (cfg ^. #canvas1)) $
    repCanvasConfig (maybe defaultCanvasConfig id (cfg ^. #canvas1))
  ax <- maybeRep "axis" (isJust (cfg ^. #axis1)) $
    repAxisConfig (maybe defaultAxisConfig id (cfg ^. #axis1))
  pure (HudConfig can t ax)

repChart :: (Monad m) => HudConfig Double -> SharedRep m (GlyphStyle, HudConfig Double)
repChart cfg = SharedRep $ do
  t@(Rep rt _) <- unrep $ repTitle "label for title" (maybe (defaultTitle "actual title") id (cfg ^. #title1))
  can@(Rep rcan _) <- unrep $ repCanvasConfig (maybe defaultCanvasConfig id (cfg ^. #canvas1))
  ax@(Rep rax _) <- unrep $ repAxisConfig (maybe defaultAxisConfig id (cfg ^. #axis1))
  gs@(Rep rgs _) <- unrep repGlyphStyle
  acc <- zoom _1 $
    accordion "accRepChart" (Just "canvas") [("glyph style", rgs), ("canvas", rcan), ("title", rt), ("axis", rax)]
  pure (first
        (const acc) $
        (\a b c d -> (a, HudConfig (Just b) (Just c) (Just d))) <$> gs <*> can <*> t <*> ax)

chartStylePage :: Page
chartStylePage =
  showJs <>
  bootstrapPage <>
  bridgePage &
  #htmlHeader .~ title_ "chart styling" &
  #htmlBody .~ b_ "container"
  (mconcat
    [ b_ "row" (h1_ "repTestPage")
    , b_ "row" (b_ "col-sm" (h2_ "inputs" <> with form_ [id_ "inputs"] mempty) <>
           b_ "col-sm" (with div_ [id_ "output"]
                  (h2_ "output" <>
                   with div_ [id_ "results"] mempty)))
    , b_ "row" (with div_ [id_ "log"] (h2_ "server log"))
    ])

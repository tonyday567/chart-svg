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
import Chart.Svg
import Chart.Spot
import Control.Category (id)
import Control.Lens
import Data.Attoparsec.Text
import Data.Generics.Labels ()
import Graphics.Svg.Types (TextAnchor(..))
import Lucid
import Protolude hiding ((<<*>>))
import Web.Page
import qualified Box ()
import NumHask.Data.Range
import qualified Data.Text as Text
import Data.Biapplicative

repChart :: (Monad m) => Chart a -> SharedRep m (Chart a)
repChart c = do
  ann <- repAnnotation (c ^. #annotation)
  pure $ Chart ann (c ^. #drawatts) (c ^. #spots)

repAnnotation :: (Monad m) => Annotation -> SharedRep m Annotation
repAnnotation ann = SharedRep $ do
  (Rep hrect frect) <- unrep $ repRectStyle defRect
  (Rep htext ftext) <- unrep $ repTextStyle defText
  (Rep hglyph fglyph) <- unrep $ repGlyphStyle defGlyph
  (Rep hline fline) <- unrep $ repLineStyle defLine
  (Rep ha fa) <- unrep $ dropdownSum takeText id "Chart Annotation"
    ["RectA", "TextA", "GlyphA", "LineA"]
    (annotationText ann)
  pure $ Rep (ha <>
              with hrect [ class__ "subtype "
                      , data_ "sumtype" "RectA"
                      , style_
                   ("display:" <> bool "block" "none" (annotationText ann /= "RectA"))] <>
              with htext [ class__ "subtype "
                      , data_ "sumtype" "TextA"
                      , style_
                   ("display:" <> bool "block" "none" (annotationText ann /= "TextA"))] <>
              with hglyph [ class__ "subtype "
                      , data_ "sumtype" "GlyphA"
                      , style_
                   ("display:" <> bool "block" "none" (annotationText ann /= "GlyphA"))] <>
              with hline [ class__ "subtype "
                      , data_ "sumtype" "LineA"
                      , style_
                   ("display:" <> bool "block" "none" (annotationText ann /= "LineA"))])
    (\m -> let (m', c) = fa m in
            case c of
              Left e -> (m', Left e)
              Right "RectA" -> second (second RectA) (frect m')
              Right "TextA" -> second (second (\x -> TextA x texts)) (ftext m')
              Right "GlyphA" -> second (second GlyphA) (fglyph m')
              Right "LineA" -> second (second LineA) (fline m')
              Right _ -> (m', Left "bad sumtype text"))
  where
    defRect = case ann of
      RectA s -> s
      _ -> defaultRectStyle
    (defText, texts) = case ann of
      TextA s xs -> (s, xs)
      _ -> (defaultTextStyle, Text.singleton <$> ['a'..'z'])
    defGlyph = case ann of
      GlyphA s -> s
      _ -> defaultGlyphStyle
    defLine = case ann of
      LineA s -> s
      _ -> defaultLineStyle

repLineStyle :: (Monad m) => LineStyle -> SharedRep m LineStyle
repLineStyle s = do
  w <- slider "width" 0.000 0.2 0.001 (s ^. #width)
  c <- colorPicker "color" (s ^. #color)
  o <- slider "opacity" 0 1 0.1 (s ^. #opacity)
  pure $ LineStyle w c o

repGlyphShape :: (Monad m) => GlyphShape -> SharedRep m GlyphShape
repGlyphShape d = toGlyph <$>
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
  ] (fromGlyph d)

repGlyphStyle :: (Monad m) => GlyphStyle -> SharedRep m GlyphStyle
repGlyphStyle gs = first (cardify [style_ "width: 10 rem;"] mempty (Just "Glyph Style")) $ do
  sh <- repGlyphShape (gs ^. #shape)
  sz <- slider "Size" 0 0.2 0.001 (gs ^. #size)
  gc <- colorPicker "Color"
    (gs ^. #color)
  go <- slider "Opacity" 0 1 0.1 (gs ^. #opacity)
  bsz <- slider "Border Size" 0 0.02 0.001 (gs ^. #borderSize)
  gbc <- colorPicker "Border Color" (gs ^. #borderColor)
  gbo <- slider "Border Opacity" 0 1 0.1 (gs ^. #borderOpacity)
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
  tc <- colorPicker "color" (s ^. #color)
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
  canvasc <- colorPicker "Canvas Color" (cfg ^. #color)
  canvaso <- slider "Canvas Opacity" 0 0.2 0.01 (cfg ^. #opacity)
  pure $ CanvasConfig canvasc canvaso

repRectStyle :: (Monad m) => RectStyle -> SharedRep m RectStyle
repRectStyle s = do
  bs <- slider "border size" 0.02 0.3 0.01 (s ^. #borderSize)
  bc <- colorPicker "border color" (s ^. #borderColor)
  bo <- slider "border opacity" 0 1 0.1 (s ^. #borderOpacity)
  c <- colorPicker "color" (s ^. #color)
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
repHudConfig cfg = bimap hmap mmap can <<*>> ts <<*>> axs <<*>> auto
  where
  can = maybeRep "canvas" (isJust (cfg ^. #hudCanvas)) $
    repCanvasConfig (maybe defaultCanvasConfig id (cfg ^. #hudCanvas))
  ts = listify' (Just "titles") "tz" repTitle (cfg ^. #hudTitles)
  axs = listify' (Just "axes") "axz" (const repAxisConfig) (cfg ^. #hudAxes)
  auto = checkbox "auto" (cfg ^. #hudAuto)
  hmap can' ts' axs' auto' = auto' <> accordion_ "accc" Nothing
                      [("Axes", axs'), ("Canvas", can'), ("Titles", ts')]
  mmap can' ts' axs' auto' = HudConfig can' ts' axs' auto'

repChartSvgStyle :: (Monad m) => ChartSvgStyle -> SharedRep m ChartSvgStyle
repChartSvgStyle s = do
  x <- slider "sizex" 0 1000 1 (s ^. #sizex)
  y <- slider "sizex" 0 1000 1 (s ^. #sizey)
  a <- slider "aspect" 0.1 10 0.1 (s ^. #chartAspect)
  op' <- maybeRep "outer pad" (maybe False (const True) (s ^. #outerPad))
    (slider "pad" 1 1.2 0.01 (maybe 0 identity (s ^. #outerPad)))
  ip <- maybeRep "inner pad" (maybe False (const True) (s ^. #innerPad))
    (slider "pad" 1 1.2 0.01 (maybe 0 identity (s ^. #innerPad)))
  fr <- maybeRep "frame" (maybe False (const True) (s ^. #chartFrame))
    (repRectStyle (maybe defaultRectStyle id (s ^. #chartFrame)))
  orig <- maybeRep "origin" (maybe False (const True) (s ^. #orig))
    ((\(os,c) -> (,) <$> slider "size" 0 0.1 0.001 os <*> colorPicker "color" c) (maybe (0.04, red) id (s ^. #orig)))
  pure $ ChartSvgStyle x y a op' ip fr orig

repData :: (Monad m) => SharedRep m [Spot Double]
repData = do
  a <- dropdown takeText show "type"
    [ "sin"
    , "line"
    ] "sin"
  pure (case a of
          "sin" -> SpotPoint <$> dataXY sin (Range 0 (2*pi)) 30
          "line" -> SpotPoint <$> dataXY sin (Range 0 (2*pi)) 30
          _ -> SpotPoint <$> dataXY sin (Range 0 (2*pi)) 30
       )

repTickFormat :: (Monad m) => TickFormat -> SharedRep m TickFormat
repTickFormat cfg =
  bimap hmap mmap tf <<*>>
  sliderI "prec" 0 20 1 (defInt cfg) <<*>>
  sliderI "prec" 0 20 1 (defInt cfg)
  where
    tf = dropdownSum takeText id "Tick Format"
      ["TickFormatDefault", "TickFormatCommas", "TickFormatFixed", "TickFormatDollars"]
      (tickFormatText cfg)
    defInt tf' = case tf' of
      TickFormatCommas n -> n
      TickFormatFixed n -> n
      _ -> 8
    hmap tformat tcommas tfixed =
      tformat <>
      with tcommas [ class__ "subtype "
              , data_ "sumtype" "TickFormatCommas"
              , style_
                ("display:" <> bool "block" "none" (tickFormatText cfg /= "TickFormatCommas"))] <>
      with tfixed [ class__ "subtype "
              , data_ "sumtype" "TickFormatFixed"
              , style_
                ("display:" <> bool "block" "none" (tickFormatText cfg /= "TickFormatFixed"))]
    mmap tformat tcommas tfixed = case tformat of
      "TickFormatDefault" -> TickFormatDefault
      "TickFormatCommas" -> TickFormatCommas tcommas
      "TickFormatFixed" -> TickFormatFixed tfixed
      "TickFormatDollars" -> TickFormatDollars
      _ -> TickFormatDefault

repTickStyle :: (Monad m) => TickStyle Double -> SharedRep m (TickStyle Double)
repTickStyle cfg =
  bimap hmap mmap ts <<*>> ls <<*>> tr <<*>> te <<*>> tplaced
  where
    ts = dropdownSum takeText id "Tick Style"
      ["TickNone", "TickLabels", "TickRound", "TickExact", "TickPlaced"]
      (tickStyleText cfg)
    ls = accordionListify (Just "tick labels") "tick-style-labels" Nothing textbox (defaultListifyLabels (length defLabels)) defLabels
    tr = (,) <$> repTickFormat defTf <*> sliderI "round" 0 20 1 defTn
    te = (,) <$> repTickFormat defTf <*> sliderI "exact" 0 20 1 defTn
    tplaced = accordionListify (Just "placed ticks") "tick-style-placed" Nothing dt (defaultListifyLabels (length dtDef)) dtDef
    hmap ts' ls' tr' te' tplaced' =
      ts' <>
      with ls' [ class__ "subtype "
              , data_ "sumtype" "TickLabels"
              , style_
                ("display:" <> bool "block" "none" (tickStyleText cfg /= "TickLabels"))] <>
      with tr' [ class__ "subtype "
              , data_ "sumtype" "TickRound"
              , style_
                ("display:" <> bool "block" "none" (tickStyleText cfg /= "TickRound"))] <>
      with te' [ class__ "subtype "
              , data_ "sumtype" "TickExact"
              , style_
                ("display:" <> bool "block" "none" (tickStyleText cfg /= "TickExact"))] <>
      with tplaced' [ class__ "subtype "
              , data_ "sumtype" "TickPlaced"
              , style_
                ("display:" <> bool "block" "none" (tickStyleText cfg /= "TickPlaced"))]
    mmap ts' ls' tr' te' tplaced' = case ts' of
      "TickNone" -> TickNone
      "TickLabels" -> TickLabels ls'
      "TickRound" -> TickRound tr'
      "TickExact" -> TickExact te'
      "TickPlaced" -> TickPlaced tplaced'
      _ -> TickNone
    dtDef = case cfg of
      TickPlaced x -> x
      _ -> zip [0..5] (show <$> [0..5])
    dt _ (x, l) = (,) <$> slider "placement" 0 1 0.01 x <*> textbox "label" l
    defLabels = case cfg of
      TickLabels xs -> xs
      _ -> replicate 5 ""
    defTn = case cfg of
      TickRound (_, x) -> x
      TickExact (_, x) -> x
      _ -> 8
    defTf = case cfg of
      TickRound (x, _) -> x
      TickExact (x, _) -> x
      _ -> TickFormatDefault

repTick :: (Monad m) => Tick Double -> SharedRep m (Tick Double)
repTick cfg = do
  p <- repPlace (cfg ^. #place)
  gs <- repGlyphStyle (cfg ^. #gstyle)
  ts <- repTextStyle (cfg ^. #textStyle)
  b <- slider "buffer" 0 0.2 0.01 (cfg ^. #buff)
  tb <- slider "text buffer" 0 0.2 0.01 (cfg ^. #textBuff)
  ts' <- repTickStyle (cfg ^. #tstyle)
  pure $ Tick p gs ts b tb ts'

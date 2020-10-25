{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | Representations of chart elements for use with the "Web.Rep" library.
module Chart.Page
  ( repAnnotation,
    repRectStyle,
    repTextStyle,
    repGlyphStyle,
    repLineStyle,
    repPlace,
    repAnchor,
    repAxisBar,
    repAdjustments,
    repTitle,
    repHudOptions,
    repAxisOptions,
    repSvgOptions,
    repData,
    repFormatN,
    repTickStyle,
    repTick,
    repPoint,
    repPointI,
    repRect,
    repGlyphShape,
    repLegendOptions,
    repChartsWithSharedData,
    repChartsWithStaticData,
    repChartSvg,
    repHudOptionsDefault,
    repBarOptions,
    repBarData,
    repBarChart,
    repSurfaceOptions,
    repSurfaceLegendOptions,
    repSurfaceChart,
    repNoData,
  )
where

import Chart.Bar
import Chart.Render (ChartSvg (..), chartSvg)
import Chart.Surface
import Chart.Types
import Control.Lens
import Data.Attoparsec.Text
import Data.Colour
import Data.FormatN
import qualified Data.Text as Text
import Lucid
import NumHask.Prelude
import NumHask.Space
import Text.Pretty.Simple (pShowNoColor)
import Web.Rep

pShow' :: (Show a) => a -> Text
pShow' = toStrict . pShowNoColor

-- | Represent an Annotation.
repAnnotation :: (Monad m) => Annotation -> SharedRep m Annotation
repAnnotation initann = bimap hmap mmap rann <<*>> rs <<*>> ts <<*>> gs <<*>> ls
  where
    rann =
      dropdownSum
        takeText
        id
        (Just "Chart Annotation")
        ["RectA", "TextA", "GlyphA", "LineA", "BlankA"]
        (annotationText initann)
    rs = repRectStyle defRectStyle
    ts = repTextStyle defText
    gs = repGlyphStyle defGlyph
    ls = repLineStyle defLine
    hmap ann rs ts gs ls =
      ann
        <> subtype rs (annotationText initann) "RectA"
        <> subtype ts (annotationText initann) "TextA"
        <> subtype gs (annotationText initann) "GlyphA"
        <> subtype ls (annotationText initann) "LineA"
    mmap ann rs ts gs ls =
      case ann of
        "RectA" -> RectA rs
        "TextA" -> TextA ts texts
        "GlyphA" -> GlyphA gs
        "LineA" -> LineA ls
        "BlankA" -> BlankA
        _ -> BlankA
    defRectStyle = case initann of
      RectA s -> s
      _ -> defaultRectStyle
    (defText, texts) = case initann of
      TextA s xs -> (s, xs)
      _ -> (defaultTextStyle, Text.singleton <$> ['a' .. 'z'])
    defGlyph = case initann of
      GlyphA s -> s
      _ -> defaultGlyphStyle
    defLine = case initann of
      LineA s -> s
      _ -> defaultLineStyle

-- | Represent a RectStyle.
repRectStyle :: (Monad m) => RectStyle -> SharedRep m RectStyle
repRectStyle s = do
  bs <- slider (Just "border size") 0.0 0.1 0.001 (s ^. #borderSize)
  bc <- colorPicker (Just "border color") (hex $ s ^. #borderColor)
  bo <- slider (Just "border opacity") 0 1 0.1 (opac $ s ^. #borderColor)
  c <- colorPicker (Just "color") (hex $ s ^. #color)
  o <- slider (Just "opacity") 0 1 0.1 (opac $ s ^. #color)
  pure $ RectStyle bs (fromRGB (unsafeFromHex bc) bo) (fromRGB (unsafeFromHex c) o)

-- | Represent a GlyphStyle
repGlyphStyle :: (Monad m) => GlyphStyle -> SharedRep m GlyphStyle
repGlyphStyle gs = first (\x -> cardify (mempty, [style_ "width: 10 rem;"]) Nothing (x, [])) $ do
  sh <- repGlyphShape (gs ^. #shape)
  sz <- slider (Just "Size") 0 0.2 0.001 (gs ^. #size)
  gc <-
    colorPicker
      (Just "Color")
      (toHex $ gs ^. #color)
  go <- slider (Just "Opacity") 0 1 0.1 (opac $ gs ^. #color)
  bsz <- slider (Just "Border Size") 0 0.02 0.001 (gs ^. #borderSize)
  gbc <- colorPicker (Just "Border Color") (toHex $ gs ^. #borderColor)
  gbo <- slider (Just "Border Opacity") 0 1 0.1 (opac $ gs ^. #borderColor)
  tr <-
    maybeRep
      (Just "rotation")
      (isJust (gs ^. #rotation))
      (slider (Just "rotation") (-180) 180 10 (fromMaybe 0 (gs ^. #rotation)))
  tt <-
    maybeRep
      (Just "translate")
      (isJust (gs ^. #translate))
      ( repPoint
          (Point (Range 0 1) (Range 0 1))
          (Point 0.001 0.001)
          (Point 0 0)
      )
  pure (GlyphStyle sz (fromRGB (unsafeFromHex gc) go) (fromRGB (unsafeFromHex gbc) gbo) bsz sh tr tt)

-- | Represent a TextStyle
repTextStyle :: (Monad m) => TextStyle -> SharedRep m TextStyle
repTextStyle s = do
  ts <- slider (Just "size") 0.02 0.2 0.01 (s ^. #size)
  tc <- colorPicker (Just "color") (toHex $ s ^. #color)
  to' <- slider (Just "opacity") 0 1 0.1 (opac $ s ^. #color)
  ta <- repAnchor (s ^. #anchor)
  th <- slider (Just "hsize") 0.2 1 0.05 (s ^. #hsize)
  tv <- slider (Just "vsize") 0.5 2 0.05 (s ^. #vsize)
  tn <- slider (Just "nudge1") (-0.5) 0.5 0.05 (s ^. #nudge1)
  tr <-
    maybeRep
      (Just "rotation")
      (isJust (s ^. #rotation))
      (slider (Just "rotation") (-180) 180 10 (fromMaybe 0 (s ^. #rotation)))
  tt <-
    maybeRep
      (Just "translate")
      (isJust (s ^. #translate))
      ( repPoint
          (Point (Range 0 1) (Range 0 1))
          (Point 0.001 0.001)
          (Point 0 0)
      )
  pure $ TextStyle ts (fromRGB (unsafeFromHex tc) to') ta th tv tn tr tt

-- | Represent a LineStyle
repLineStyle :: (Monad m) => LineStyle -> SharedRep m LineStyle
repLineStyle s = do
  w <- slider (Just "width") 0.000 0.05 0.001 (s ^. #width)
  c <- colorPicker (Just "color") (toHex $ s ^. #color)
  o <- slider (Just "opacity") 0 1 0.1 (opac $ s ^. #color)
  pure $ LineStyle w (fromRGB (unsafeFromHex c) o)

-- | Represent a Place
repPlace :: (Monad m) => Place -> SharedRep m Place
repPlace initpl = bimap hmap mmap rplace <<*>> rp
  where
    rplace =
      dropdownSum
        takeText
        id
        (Just "Placement")
        [ "Bottom",
          "Left",
          "Top",
          "Right",
          "Absolute"
        ]
        (placeText initpl)
    rp = repPoint (Point (Range 0 1) (Range 0 1)) (Point 0.01 0.01) (defPoint initpl)
    defPoint pl = case pl of
      PlaceAbsolute p -> p
      _ -> Point 0.0 0.0
    hmap rplace rp =
      div_
        ( rplace
            <> subtype rp (placeText initpl) "Absolute"
        )
    mmap rplace rp = case rplace of
      "Top" -> PlaceTop
      "Bottom" -> PlaceBottom
      "Left" -> PlaceLeft
      "Right" -> PlaceRight
      "Absolute" -> PlaceAbsolute rp
      _ -> PlaceBottom

-- | Represent an Anchor
repAnchor :: (Monad m) => Anchor -> SharedRep m Anchor
repAnchor a =
  toAnchor
    <$> dropdown
      takeText
      id
      (Just "Anchor")
      (fromAnchor <$> [AnchorStart, AnchorMiddle, AnchorEnd])
      (fromAnchor a)

-- | Represent a Direction
repDirection :: (Monad m) => Direction -> SharedRep m Direction
repDirection a =
  toDirection
    <$> dropdown
      takeText
      id
      (Just "Direction")
      (fromDirection <$> [Vert, Hori])
      (fromDirection a)

-- | Represent an AxisBar
repAxisBar :: (Monad m) => AxisBar -> SharedRep m AxisBar
repAxisBar cfg = do
  r <- repRectStyle (cfg ^. #rstyle)
  w <- slider (Just "width") 0 0.04 0.001 (cfg ^. #wid)
  b <- slider (Just "buffer") 0 0.08 0.001 (cfg ^. #buff)
  pure $ AxisBar r w b

-- | Represent Adjustments
repAdjustments :: (Monad m) => Adjustments -> SharedRep m Adjustments
repAdjustments a = do
  maxx <- slider (Just "maximum x ratio") 0.000 0.2 0.001 (a ^. #maxXRatio)
  maxy <- slider (Just "maximum y ratio") 0.000 0.2 0.001 (a ^. #maxYRatio)
  angle <- slider (Just "angle ratio") 0.000 1 0.001 (a ^. #angledRatio)
  diag <- checkbox (Just "allow diagonal text") (a ^. #allowDiagonal)
  pure $ Adjustments maxx maxy angle diag

-- | Represent a Title
repTitle :: (Monad m) => Title -> SharedRep m Title
repTitle cfg = do
  ttext <- textbox (Just "text") (cfg ^. #text)
  ts <- repTextStyle (cfg ^. #style)
  tp <- repPlace (cfg ^. #place)
  ta <- repAnchor (cfg ^. #anchor)
  b <- slider (Just "buffer") 0 0.2 0.01 (cfg ^. #buff)
  pure $ Title ttext ts tp ta b

-- | Represent HudOptions
repHudOptions ::
  (Monad m) =>
  Int ->
  Int ->
  Int ->
  AxisOptions ->
  Title ->
  LegendOptions ->
  [(Annotation, Text)] ->
  Annotation ->
  Text ->
  HudOptions ->
  SharedRep m HudOptions
repHudOptions naxes ntitles nlegendRows defaxis deftitle deflegend deflrs defann deflabel cfg =
  bimap hmap (\a b c d -> HudOptions a b c d) can
    <<*>> ts
    <<*>> axs
    <<*>> l
  where
    can =
      maybeRep (Just "canvas") (isJust (cfg ^. #hudCanvas)) $
        repRectStyle (fromMaybe defaultCanvas (cfg ^. #hudCanvas))
    ts =
      listRep
        (Just "titles")
        "tz"
        (checkbox Nothing)
        repTitle
        ntitles
        deftitle
        (cfg ^. #hudTitles)
    axs =
      listRep
        (Just "axes")
        "axz"
        (checkbox Nothing)
        repAxisOptions
        naxes
        defaxis
        (cfg ^. #hudAxes)
    labelsc =
      listRep
        (Just "labelsc")
        "labelscz"
        (checkbox Nothing)
        (textbox Nothing)
        nlegendRows
        deflabel
        (snd <$> deflrs)
    anns =
      listRep
        (Just "annotations")
        "annsz"
        (checkbox Nothing)
        repAnnotation
        nlegendRows
        defann
        (fst <$> deflrs)
    l =
      maybeRep
        (Just "legend")
        (isJust $ cfg ^. #hudLegend)
        ( (,)
            <$> repLegendOptions (maybe deflegend fst (cfg ^. #hudLegend))
            <*> (zip <$> anns <*> labelsc)
        )
    hmap can' ts' axs' l' =
      accordion_
        "accc"
        Nothing
        [ ("Axes", axs'),
          ("Canvas", can'),
          ("Titles", ts'),
          ("Legend", l')
        ]

-- | Represent AxisOptions
repAxisOptions :: (Monad m) => AxisOptions -> SharedRep m AxisOptions
repAxisOptions cfg = bimap hmap AxisOptions b <<*>> adj <<*>> t <<*>> p
  where
    b =
      maybeRep
        (Just "axis bar")
        (isJust (cfg ^. #abar))
        (repAxisBar (fromMaybe defaultAxisBar (cfg ^. #abar)))
    adj =
      maybeRep
        (Just "adjustments")
        (isJust (cfg ^. #adjust))
        (repAdjustments (fromMaybe defaultAdjustments (cfg ^. #adjust)))
    t = repTick (cfg ^. #atick)
    p = repPlace (cfg ^. #place)
    hmap b' hauto' t' p' =
      accordion_
        "accaxis"
        Nothing
        [ ("Bar", b'),
          ("Auto", hauto'),
          ("Ticks", t'),
          ("Place", p')
        ]

-- | Represent an SvgAspect
repSvgAspect :: (Monad m) => SvgAspect -> Double -> SharedRep m SvgAspect
repSvgAspect sa ddef =
  bimap hmap toSvgAspect sa' <<*>> td
  where
    sa' =
      dropdownSum
        takeText
        id
        (Just "Aspect")
        [ "ManualAspect",
          "ChartsAspect"
        ]
        (fromSvgAspect sa)
    td = slider (Just "aspect scale") 0 10 0.01 (defD ddef)
    defD d' = case sa of
      ManualAspect d -> d
      ChartAspect -> d'
    hmap sa'' td' =
      div_
        ( sa''
            <> subtype td' (fromSvgAspect sa) "ManualAspect"
        )

-- | Represent SvgOptions
repSvgOptions :: (Monad m) => SvgOptions -> SharedRep m SvgOptions
repSvgOptions s =
  bimap
    hmap
    SvgOptions
    h'
    <<*>> op'
    <<*>> ip
    <<*>> fr
    <<*>> esc
    <<*>> csso
    <<*>> scalec
    <<*>> svga
  where
    svga = repSvgAspect (s ^. #svgAspect) 1.33
    h' = slider (Just "height") 1 1000 1 (s ^. #svgHeight)
    op' =
      maybeRep
        (Just "outer pad")
        (isJust (s ^. #outerPad))
        (slider Nothing 0 0.1 0.01 (fromMaybe 1 (s ^. #outerPad)))
    ip =
      maybeRep
        (Just "inner pad")
        (isJust (s ^. #innerPad))
        (slider Nothing 0 0.1 0.01 (fromMaybe 1 (s ^. #innerPad)))
    fr =
      maybeRep
        (Just "frame")
        (isJust (s ^. #chartFrame))
        (repRectStyle (fromMaybe defaultSvgFrame (s ^. #chartFrame)))
    esc =
      bool NoEscapeText EscapeText
        <$> checkbox (Just "escape text") (EscapeText == s ^. #escapeText)
    csso =
      bool NoCssOptions UseCssCrisp
        <$> checkbox (Just "Use CssCrisp") (UseCssCrisp == s ^. #useCssCrisp)
    scalec =
      bool NoScaleCharts ScaleCharts
        <$> checkbox (Just "Scale Charts") (ScaleCharts == s ^. #scaleCharts')
    hmap h' op'' ip' fr' esc' csso' scalec' svga' =
      accordion_
        "accsvg"
        Nothing
        [ ("Aspect", svga' <> h'),
          ("Padding", op'' <> ip'),
          ("Frame", fr'),
          ("Escape", esc'),
          ("Css", csso'),
          ("Scale", scalec')
        ]

-- | Represent XY Data
repData :: (Monad m) => Text -> SharedRep m [XY Double]
repData d = do
  a <-
    dropdown
      takeText
      id
      (Just "type")
      [ "sin",
        "line",
        "one",
        "dist"
      ]
      d
  pure
    ( case a of
        "sin" -> PointXY <$> gridP sin (Range 0 (2 * pi)) 30
        "line" ->
          PointXY . uncurry Point
            <$> [(0.0, 1.0), (1.0, 1.0), (2.0, 5.0)]
        "one" -> [R 0 1 0 1]
        "dist" -> RectXY <$> gridR (\x -> exp (- (x ** 2) / 2)) (Range (-5) 5) 50
        _ -> PointXY <$> gridP sin (Range 0 (2 * pi)) 30
    )

-- | Represent a FormatN
repFormatN :: (Monad m) => FormatN -> SharedRep m FormatN
repFormatN tf = bimap hmap mmap tformat <<*>> tcommas <<*>> tfixed <<*>> texpt <<*>> tpercent <<*>> tprec <<*>> tdecimal <<*>> tdollar
  where
    tformat =
      dropdownSum
        takeText
        id
        (Just "Format")
        [ "Comma",
          "Fixed",
          "Decimal",
          "Prec",
          "Expt",
          "Dollar",
          "Percent",
          "None"
        ]
        (fromFormatN tf)
    tcommas = maybeRep Nothing (defSig tf) (sliderI (Just "prec") 0 8 1 (defInt tf))
    tfixed = maybeRep Nothing (defSig tf) $ sliderI (Just "prec") 0 8 1 (defInt tf)
    texpt = maybeRep Nothing (defSig tf) $ sliderI (Just "prec") 0 8 1 (defInt tf)
    tpercent = maybeRep Nothing (defSig tf) $ sliderI (Just "prec") 0 8 1 (defInt tf)
    tprec = maybeRep Nothing (defSig tf) $ sliderI (Just "prec") 0 8 1 (defInt tf)
    tdecimal = maybeRep Nothing (defSig tf) $ sliderI (Just "prec") 0 8 1 (defInt tf)
    tdollar = maybeRep Nothing (defSig tf) $ sliderI (Just "prec") 0 8 1 (defInt tf)
    defInt tf' = case tf' of
      FormatComma (Just n) -> n
      FormatFixed (Just n) -> n
      FormatDecimal (Just n) -> n
      FormatPrec (Just n) -> n
      FormatExpt (Just n) -> n
      FormatDollar (Just n) -> n
      FormatPercent (Just n) -> n
      _ -> 3
    defSig tf' = case tf' of
      FormatComma (Just _) -> True
      FormatFixed (Just _) -> True
      FormatDecimal (Just _) -> True
      FormatPrec (Just _) -> True
      FormatExpt (Just _) -> True
      FormatDollar (Just _) -> True
      FormatPercent (Just _) -> True
      _ -> False
    hmap tformat' tcommas' tfixed' texpt' tpercent' tprec' tdecimal' tdollar' =
      div_
        ( tformat'
            <> subtype tcommas' (fromFormatN tf) "Comma"
            <> subtype tfixed' (fromFormatN tf) "Fixed"
            <> subtype texpt' (fromFormatN tf) "Expt"
            <> subtype tpercent' (fromFormatN tf) "Percent"
            <> subtype tprec' (fromFormatN tf) "Prec"
            <> subtype tdecimal' (fromFormatN tf) "Decimal"
            <> subtype tdollar' (fromFormatN tf) "Dollar"
        )
    mmap tformat' tcommas' tfixed' texpt' tpercent' tprec' tdecimal' tdollar' = case tformat' of
      "Comma" -> FormatComma tcommas'
      "Fixed" -> FormatFixed tfixed'
      "Decimal" -> FormatDecimal tdecimal'
      "Prec" -> FormatPrec tprec'
      "Expt" -> FormatExpt texpt'
      "Dollar" -> FormatDollar tdollar'
      "Percent" -> FormatPercent tpercent'
      "None" -> FormatNone
      _ -> FormatNone

-- | Represent a TickStyle
repTickStyle :: (Monad m) => TickStyle -> SharedRep m TickStyle
repTickStyle cfg =
  bimap hmap mmap ts <<*>> ls <<*>> tr <<*>> te <<*>> tplaced
  where
    ts =
      dropdownSum
        takeText
        id
        (Just "Tick Style")
        ["TickNone", "TickLabels", "TickRound", "TickExact", "TickPlaced"]
        (tickStyleText cfg)
    ls =
      accordionList
        (Just "tick labels")
        "tick-style-labels"
        Nothing
        (textbox . Just)
        (defaultListLabels (length defLabels))
        defLabels
    tr =
      (,,)
        <$> sliderI (Just "Number of ticks") 0 20 1 defTn
        <*> repFormatN defTf
        <*> ( bool NoTickExtend TickExtend
                <$> checkbox (Just "extend") defExtend
            )
    te =
      (,)
        <$> sliderI (Just "Number of ticks") 0 20 1 defTn
        <*> repFormatN defTf
    tplaced =
      accordionList
        (Just "placed ticks")
        "tick-style-placed"
        Nothing
        dt
        (defaultListLabels (length dtDef))
        dtDef
    hmap ts' ls' tr' te' tplaced' =
      div_
        ( ts'
            <> subtype ls' (tickStyleText cfg) "TickLabels"
            <> subtype tr' (tickStyleText cfg) "TickRound"
            <> subtype te' (tickStyleText cfg) "TickExact"
            <> subtype tplaced' (tickStyleText cfg) "TickPlaced"
        )
    mmap ts' ls' (tri, trf, tre) (tei, tef) tplaced' = case ts' of
      "TickNone" -> TickNone
      "TickLabels" -> TickLabels ls'
      "TickRound" -> TickRound trf tri tre
      "TickExact" -> TickExact tef tei
      "TickPlaced" -> TickPlaced tplaced'
      _ -> TickNone
    dtDef = case cfg of
      TickPlaced x -> x
      _ -> zip [0 .. 5] (pack . show <$> [0 .. 5 :: Int])
    dt _ (x, l) = (,) <$> slider (Just "placement") 0 1 0.01 x <*> textbox (Just "label") l
    defLabels = case cfg of
      TickLabels xs -> xs
      _ -> replicate 5 ""
    defTn = case cfg of
      TickRound _ x _ -> x
      TickExact _ x -> x
      _ -> 8
    defTf = case cfg of
      TickRound x _ _ -> x
      TickExact x _ -> x
      _ -> FormatPrec (Just 3)
    defExtend = case cfg of
      TickRound _ _ e -> e == TickExtend
      _ -> True

-- | Represent a Tick
repTick :: (Monad m) => Tick -> SharedRep m Tick
repTick cfg = bimap hmap Tick ts <<*>> gt <<*>> tt <<*>> lt
  where
    hmap ts' gt' tt' lt' =
      accordion_
        "acctick"
        Nothing
        [ ("style", ts'),
          ("glyph", gt'),
          ("text", tt'),
          ("line", lt')
        ]
    ts = repTickStyle (cfg ^. #tstyle)
    gt =
      maybeRep Nothing (isJust (cfg ^. #gtick)) $
        bimap
          (<>)
          (,)
          (repGlyphStyle (maybe defaultGlyphTick fst (cfg ^. #gtick)))
          <<*>> slider (Just "buffer") 0 0.05 0.001 (maybe 0.05 snd (cfg ^. #gtick))
    tt =
      maybeRep Nothing (isJust (cfg ^. #ttick)) $
        bimap
          (<>)
          (,)
          (repTextStyle (maybe defaultTextTick fst (cfg ^. #ttick)))
          <<*>> slider (Just "buffer") 0 0.05 0.001 (maybe 0.05 snd (cfg ^. #ttick))
    lt =
      maybeRep Nothing (isJust (cfg ^. #ltick)) $
        bimap
          (<>)
          (,)
          (repLineStyle (maybe defaultLineTick fst (cfg ^. #ltick)))
          <<*>> slider (Just "buffer") (-0.1) 0.1 0.001 (maybe 0 snd (cfg ^. #ltick))

-- | Represent a Point Double
repPoint ::
  (Monad m) =>
  Point (Range Double) ->
  Point Double ->
  Point Double ->
  SharedRep m (Point Double)
repPoint (Point (Range xmin xmax) (Range ymin ymax)) (Point xstep ystep) (Point x y) =
  bimap
    (<>)
    Point
    (slider (Just "x") xmin xmax xstep x)
    <<*>> slider (Just "y") ymin ymax ystep y

-- | Represent a Point Integer
repPointI ::
  (Monad m) =>
  Point (Range Int) ->
  Point Int ->
  Point Int ->
  SharedRep m (Point Int)
repPointI (Point (Range xmin xmax) (Range ymin ymax)) (Point xstep ystep) (Point x y) =
  bimap
    (<>)
    Point
    (sliderI (Just "x") xmin xmax xstep x)
    <<*>> sliderI (Just "y") ymin ymax ystep y

-- | Represent a Rect Double
repRect :: (Monad m) => Rect (Range Double) -> Rect Double -> Rect Double -> SharedRep m (Rect Double)
repRect (Rect (Range xmin xmax) (Range zmin zmax) (Range ymin ymax) (Range wmin wmax)) (Rect xstep zstep ystep wstep) (Rect x z y w) =
  bimap
    (\a b c d -> a <> b <> c <> d)
    Rect
    (slider (Just "x") xmin xmax xstep x)
    <<*>> slider (Just "z") zmin zmax zstep z
    <<*>> slider (Just "y") ymin ymax ystep y
    <<*>> slider (Just "w") wmin wmax wstep w

-- | Represent a Rounded Rect
repRounded :: (Monad m) => (Double, Double, Double) -> SharedRep m (Double, Double, Double)
repRounded (a, b, c) =
  bimap
    (\a' b' c' -> a' <> b' <> c')
    (,,)
    (slider Nothing 0 1 0.001 a)
    <<*>> slider Nothing 0 1 0.001 b
    <<*>> slider Nothing 0 1 0.001 c

-- | Represent a Triple
repTriple :: (Monad m) => (a, a, a) -> (a -> SharedRep m a) -> SharedRep m (a, a, a)
repTriple (a, b, c) sr =
  bimap (\a' b' c' -> a' <> b' <> c') (,,) (sr a) <<*>> sr b <<*>> sr c

-- | Represent a GlyphShape
repGlyphShape :: (Monad m) => GlyphShape -> SharedRep m GlyphShape
repGlyphShape sh = bimap hmap mmap sha <<*>> ell <<*>> rsharp <<*>> rround <<*>> tri <<*>> p <<*>> lwidth
  where
    sha =
      dropdownSum
        takeText
        id
        Nothing
        [ "Circle",
          "Square",
          "Triangle",
          "Ellipse",
          "RectSharp",
          "RectRounded",
          "VLine",
          "HLine",
          "Path"
        ]
        (glyphText sh)
    ell = slider Nothing 0.5 2 0.01 defRatio
    rsharp = slider Nothing 0.5 2 0.01 defRatio
    rround = repRounded defRounded
    lwidth = slider (Just "width") 0.001 0.02 0.001 defLwidth
    tri =
      repTriple
        defTriangle
        ( repPoint
            (Point (Range 0 1) (Range 0 1))
            (Point 0.001 0.001)
        )
    p = textbox (Just "path") defP
    hmap sha' ell' rsharp' rround' tri' p' lwidth' =
      sha'
        <> subtype ell' (glyphText sh) "Ellipse"
        <> subtype rsharp' (glyphText sh) "RectSharp"
        <> subtype rround' (glyphText sh) "RectRounded"
        <> subtype tri' (glyphText sh) "Triangle"
        <> subtype lwidth' (glyphText sh) "VLine"
        <> subtype lwidth' (glyphText sh) "HLine"
        <> subtype p' (glyphText sh) "Path"
    mmap sha' ell' rsharp' rround' tri' p' lwidth' =
      case sha' of
        "Circle" -> CircleGlyph
        "Square" -> SquareGlyph
        "Ellipse" -> EllipseGlyph ell'
        "RectSharp" -> RectSharpGlyph rsharp'
        "RectRounded" -> (\(a, b, c) -> RectRoundedGlyph a b c) rround'
        "Triangle" -> (\(a, b, c) -> TriangleGlyph a b c) tri'
        "VLine" -> VLineGlyph lwidth'
        "HLine" -> HLineGlyph lwidth'
        "Path" -> PathGlyph p'
        _ -> CircleGlyph
    defP = case sh of
      PathGlyph p -> p
      _ -> mempty
    defRatio = case sh of
      EllipseGlyph r -> r
      RectSharpGlyph r -> r
      _ -> 1.5
    defLwidth = case sh of
      VLineGlyph r -> r
      HLineGlyph r -> r
      _ -> 0.005
    defRounded = case sh of
      RectRoundedGlyph a b c -> (a, b, c)
      _ -> (0.884, 2.7e-2, 5.0e-2)
    defTriangle = case sh of
      TriangleGlyph a b c -> (a, b, c)
      _ -> (Point 0.0 0.0, Point 1 1, Point 1 0)

-- | Represent LegendOptions
repLegendOptions :: (Monad m) => LegendOptions -> SharedRep m LegendOptions
repLegendOptions initl =
  bimap
    hmap
    LegendOptions
    lsize'
    <<*>> vgap'
    <<*>> hgap'
    <<*>> ltext'
    <<*>> lmax'
    <<*>> innerPad'
    <<*>> outerPad'
    <<*>> legendFrame'
    <<*>> lplace'
    <<*>> scale'
  where
    lsize' = slider (Just "element size") 0.000 1 0.001 (initl ^. #lsize)
    hgap' = slider (Just "horizontal gap") 0.000 0.5 0.001 (initl ^. #hgap)
    vgap' = slider (Just "vertical gap") 0.000 0.5 0.001 (initl ^. #vgap)
    ltext' = repTextStyle (initl ^. #ltext)
    lmax' = sliderI (Just "max entries") 0 10 1 (initl ^. #lmax)
    innerPad' =
      slider
        (Just "inner padding")
        0
        0.2
        0.001
        (initl ^. #innerPad)
    outerPad' =
      slider
        (Just "outer padding")
        0
        0.2
        0.001
        (initl ^. #outerPad)
    legendFrame' =
      maybeRep
        (Just "frame")
        (isJust (initl ^. #legendFrame))
        (repRectStyle (fromMaybe defaultSvgFrame (initl ^. #legendFrame)))
    lplace' = repPlace (initl ^. #lplace)
    scale' = slider (Just "scale") 0.01 1 0.001 (initl ^. #lscale)
    hmap lsize'' vgap'' hgap'' ltext'' lmax'' innerPad'' outerPad'' legendFrame'' lplace'' scale'' =
      accordion_
        "accleg"
        Nothing
        [ ("Scale", scale'' <> lsize''),
          ("Pads", innerPad'' <> outerPad'' <> vgap'' <> hgap''),
          ("Text", ltext''),
          ("Frame", legendFrame''),
          ("Place", lplace''),
          ("Max Elements", lmax'')
        ]

-- | Represent a chart with a supplied 'SharedRep' for the data.
repChartsWithSharedData ::
  (Monad m) =>
  SvgOptions ->
  HudOptions ->
  Int ->
  [Chart Double] ->
  ([[XY Double]] -> SharedRep m [[XY Double]]) ->
  SharedRep m (Text, Text)
repChartsWithSharedData css' hc' maxcs' cs' sxys =
  bimap
    hmap
    mmap
    cssr
    <<*>> annsr
    <<*>> sxys xys'
    <<*>> hr
    <<*>> debugFlags
  where
    xys' = view #xys <$> cs'
    anns' = view #annotation <$> cs'
    hr =
      repHudOptions
        2
        3
        5
        defaultAxisOptions
        (defaultTitle "default")
        (maybe defaultLegendOptions fst (hc' ^. #hudLegend))
        (maybe [] snd (hc' ^. #hudLegend))
        BlankA
        ""
        hc'
    cssr = repSvgOptions css'
    annsr =
      listRep
        (Just "Annotations")
        "annz"
        (checkbox Nothing)
        repAnnotation
        maxcs'
        BlankA
        anns'
    mmap css'' ann' d' h' debug' =
      let ch = zipWith Chart ann' d'
       in ( chartSvg (mempty & #svgOptions .~ css'' & #hudOptions .~ h' & #chartList .~ ch),
            debugHtml debug' css'' h' ch
          )
    hmap css'' ann' _ h' debug' =
      accordion_
        "acca"
        Nothing
        [ ("Svg", css''),
          ("Annotations", ann'),
          ("Hud", h'),
          ("Debug", debug')
        ]

-- | Represent a Chart with no representation of the underlying data.
repChartsWithStaticData ::
  (Monad m) =>
  SvgOptions ->
  HudOptions ->
  Int ->
  [Chart Double] ->
  SharedRep m (Text, Text)
repChartsWithStaticData css' hc' maxcs' cs' =
  repChartsWithSharedData css' hc' maxcs' cs' (bipure mempty)

-- | Representation of a ChartSvg.  The hud list cannot be represented and is ignored. Text output if Html representation and debug info.
repChartSvg :: (Monad m) => Int -> ChartSvg -> SharedRep m (Text, Text)
repChartSvg maxn cs =
  repChartsWithStaticData (cs ^. #svgOptions) (cs ^. #hudOptions) maxn (cs ^. #chartList)

debugHtml :: (Bool, Bool, Bool) -> SvgOptions -> HudOptions -> [Chart Double] -> Text
debugHtml debug css hc cs =
  bool
    mempty
    ( mconcat $
        (\x -> "<p style='white-space: pre'>" <> x <> "</p>")
          <$> [ "<h2>config values</h2>",
                pShow' css,
                pShow' hc
              ]
    )
    ((\(a, _, _) -> a) debug)
    <> bool
      mempty
      ( mconcat
          [ "<h2>chart svg</h2>",
            "<xmp>",
            chartSvg
              ( mempty
                  & #svgOptions .~ css
                  & #hudOptions .~ hc
                  & #chartList .~ cs
              ),
            "</xmp>"
          ]
      )
      ((\(_, a, _) -> a) debug)
    <> bool
      mempty
      ( mconcat $
          (\x -> "<p style='white-space: pre'>" <> x <> "</p>")
            <$> [ "<h2>chart value</h2>",
                  pack $ show cs
                ]
      )
      ((\(_, _, a) -> a) debug)

debugFlags :: (Monad m) => SharedRepF m (Html ()) (Bool, Bool, Bool)
debugFlags =
  bimap
    (\a b c -> a <> b <> c)
    (,,)
    (checkbox (Just "show hudOptions values") False)
    <<*>> checkbox (Just "show chart svg") False
    <<*>> checkbox (Just "show Chart values") False

-- | Represent HudOptions with some sane default values.
repHudOptionsDefault :: Monad m => HudOptions -> SharedRep m HudOptions
repHudOptionsDefault hc =
  repHudOptions
    2
    3
    5
    defaultAxisOptions
    (defaultTitle "default")
    defaultLegendOptions
    []
    BlankA
    ""
    hc

-- | Represent BarOptions
repBarOptions ::
  (Monad m) =>
  Int ->
  RectStyle ->
  TextStyle ->
  BarOptions ->
  SharedRep m BarOptions
repBarOptions nrows defrs defts cfg =
  bimap hmap BarOptions rs
    <<*>> ts
    <<*>> og
    <<*>> ig
    <<*>> tg
    <<*>> tgn
    <<*>> dv
    <<*>> fn
    <<*>> av
    <<*>> or
    <<*>> ho
  where
    rs =
      listRep
        (Just "bar styles")
        "rs"
        (checkbox Nothing)
        repRectStyle
        nrows
        defrs
        (cfg ^. #barRectStyles)
    ts =
      listRep
        (Just "text styles")
        "ts"
        (checkbox Nothing)
        repTextStyle
        nrows
        defts
        (cfg ^. #barTextStyles)
    og = slider (Just "outer gap") 0.0 1.0 0.001 (cfg ^. #outerGap)
    ig = slider (Just "inner gap") (-1.0) 1 0.001 (cfg ^. #innerGap)
    tg = slider (Just "text gap") (-0.05) 0.05 0.001 (cfg ^. #textGap)
    tgn = slider (Just "neg. text gap") 0 0.1 0.001 (cfg ^. #textGapNegative)
    dv = checkbox (Just "display values") (cfg ^. #displayValues)
    fn = repFormatN (cfg ^. #valueFormatN)
    av = checkbox (Just "accumulate values") (cfg ^. #accumulateValues)
    or = repDirection (cfg ^. #barOrientation)
    ho =
      repHudOptions
        2
        3
        5
        defaultAxisOptions
        (defaultTitle "bar options")
        (maybe defaultLegendOptions fst (cfg ^. #barHudOptions . #hudLegend))
        (maybe [] snd (cfg ^. #barHudOptions . #hudLegend))
        BlankA
        ""
        (cfg ^. #barHudOptions)
    hmap rs' ts' og' ig' tg' tgn' dv' fn' av' or' ho' =
      accordion_
        "accbo"
        Nothing
        [ ("Bar Styles", rs'),
          ("Text Styles", ts'),
          ("Gaps", og' <> ig' <> tg' <> tgn'),
          ("Style", dv' <> fn' <> av' <> or'),
          ("Hud", ho')
        ]

-- | Represent BarData
repBarData ::
  (Monad m) =>
  BarData ->
  SharedRep m BarData
repBarData initbd =
  bimap hmap BarData bd
    <<*>> rl
    <<*>> cl
  where
    rl =
      maybeRep
        Nothing
        (isJust (initbd ^. #barRowLabels))
        ( either (const []) id
            <$> readTextbox (Just "row labels") (fromMaybe [] (initbd ^. #barRowLabels))
        )
    cl =
      maybeRep
        Nothing
        (isJust (initbd ^. #barColumnLabels))
        ( either (const []) id
            <$> readTextbox (Just "column labels") (fromMaybe [] (initbd ^. #barColumnLabels))
        )
    bd =
      either (const (pure [])) id
        <$> readTextbox (Just "bar data") (initbd ^. #barData)
    hmap rl' cl' bd' = rl' <> cl' <> bd'

-- | Represent a SurfaceStyle
repSurfaceStyle ::
  (Monad m) =>
  SurfaceStyle ->
  SharedRep m SurfaceStyle
repSurfaceStyle cfg =
  bimap hmap mmap pcs
    <<*>> pos
    <<*>> prs
  where
    pcs =
      listRep
      (Just "RGB colors")
      "pcs"
      (checkbox Nothing)
      (colorPicker Nothing)
      4
      (toHex black)
      (toHex <$> cfg ^. #surfaceColors)
    pos = listRep (Just "opacities") "pos" (checkbox Nothing) (slider Nothing 0.0 1.0 0.001) 4 0 (opac <$> cfg ^. #surfaceColors)
    prs = repRectStyle (cfg ^. #surfaceRectStyle)
    hmap pcs' pos' prs' =
      pcs' <> pos' <> prs'
    mmap pcs' pos' prs' =
      SurfaceStyle (zipWith fromRGB (unsafeFromHex <$> pcs') pos') prs'

-- | Represent SurfaceOptions
repSurfaceOptions ::
  (Monad m) =>
  SurfaceOptions ->
  SharedRep m SurfaceOptions
repSurfaceOptions cfg =
  bimap hmap SurfaceOptions ps
    <<*>> pg
    <<*>> pr
  where
    ps = repSurfaceStyle (cfg ^. #soStyle)
    pg = repPointI (Point (Range 1 100) (Range 1 100)) (Point 1 1) (cfg ^. #soGrain)
    pr = repRect (Rect (Range 0 5) (Range 0 5) (Range 0 5) (Range 0 5)) (Rect 0.01 0.01 0.01 0.01) (cfg ^. #soRange)
    hmap ps' pg' pr' =
      accordion_
        "accsurface"
        Nothing
        [ ("Grain", pg'),
          ("Range", pr'),
          ("Style", ps')
        ]

-- | Represent SurfaceLegendOptions
repSurfaceLegendOptions ::
  (Monad m) =>
  SurfaceLegendOptions ->
  SharedRep m SurfaceLegendOptions
repSurfaceLegendOptions cfg =
  bimap hmap SurfaceLegendOptions ps
    <<*>> pt
    <<*>> pw
    <<*>> pa
    <<*>> pl
  where
    ps = repSurfaceStyle (cfg ^. #sloStyle)
    pt = textbox (Just "title") (cfg ^. #sloTitle)
    pw = slider (Just "width") 0.0 0.3 0.001 (cfg ^. #sloWidth)
    pa = repAxisOptions (cfg ^. #sloAxisOptions)
    pl = repLegendOptions (cfg ^. #sloLegendOptions)
    hmap ps' pt' pw' pa' pl' =
      accordion_
        "accslo"
        Nothing
        [ ("Style", ps'),
          ("Title", pt'),
          ("Width", pw'),
          ("Axis", pa'),
          ("Legend", pl')
        ]

-- | Represent a BarChart
repBarChart :: (Monad m) => SvgOptions -> BarData -> BarOptions -> SharedRep m (Text, Text)
repBarChart css bd bo = bimap hmap mmap rcss <<*>> rbd <<*>> rbo <<*>> debugFlags
  where
    rcss = repSvgOptions css
    rbo = repBarOptions 5 defaultRectStyle defaultTextStyle bo
    rbd = repBarData bd
    barchartsvg css' bd' bo' =
      let (hc', cs') = barChart bo' bd'
       in chartSvg (mempty & #svgOptions .~ css' & #hudOptions .~ hc' & #chartList .~ cs')
    mmap css' bd' bo' debug =
      ( barchartsvg css' bd' bo',
        debugHtml debug css' (bo' ^. #barHudOptions) (bars bo' bd')
      )
    hmap css' bd' bo' debug =
      accordion_
        "accbc"
        Nothing
        [ ("Svg", css'),
          ("Bar Data", bd'),
          ("Bar Options", bo'),
          ("Debug", debug)
        ]

-- | Represent a SurfaceChart
repSurfaceChart ::
  (Monad m) =>
  (SvgOptions, SurfaceOptions, HudOptions, SurfaceLegendOptions, Point Double -> Double) ->
  SharedRep m (Text, Text)
repSurfaceChart (css, po, hc, slo, f) = bimap hmap mmap rcss <<*>> rpo <<*>> rhc <<*>> rslo <<*>> debugFlags
  where
    rcss = repSvgOptions css
    rpo = repSurfaceOptions po
    rhc = repHudOptionsDefault hc
    rslo = repSurfaceLegendOptions slo
    mmap rcss' rpo' rhc' rslo' debug =
      let (cs, hs) = surfacefl f rpo' rslo'
       in ( chartSvg (ChartSvg rcss' rhc' hs cs),
            debugHtml debug rcss' rhc' []
          )
    hmap rcss' rpo' rhc' rslo' debug =
      accordion_
        "accpc"
        Nothing
        [ ("Svg", rcss'),
          ("Hud", rhc'),
          ("Surface Options", rpo'),
          ("Surface Legend Options", rslo'),
          ("Debug", debug)
        ]

-- | Represent a chart with no data.
repNoData :: (Monad m) => SvgOptions -> Annotation -> HudOptions -> SharedRep m (Text, Text)
repNoData css ann hc =
  repChartsWithStaticData css hc 10 [Chart ann [one]]

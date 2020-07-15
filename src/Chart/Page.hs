{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Chart.Page
  ( repChartStaticData,
    repAnnotation,
    repRectStyle,
    repTextStyle,
    repGlyphStyle,
    repLineStyle,
    repPlace,
    repAnchor,
    repBar,
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
    repRectOne,
    repRounded,
    repTriple,
    repGlyphShape,
    repLegendOptions,
    repChartsWithSharedData,
    repChartsWithStaticData,
    debugHtml,
    debugFlags,
    repHudOptionsDefault,
    repBarOptions,
    repBarData,
    repBarChart,
    repPixelOptions,
    repPixelLegendOptions,
    repPixelChart,
    repNoData,
  )
where

import Chart.Bar
import Chart.Pixel
import Chart.Render (renderHudOptionsChart)
import Chart.Types
import Control.Lens
import Data.Attoparsec.Text
import qualified Data.Text as Text
import Lucid
import NumHask.Prelude
import NumHask.Space
import Text.Pretty.Simple (pShowNoColor)
import Web.Rep

pShow' :: (Show a) => a -> Text
pShow' = toStrict . pShowNoColor

repChartStaticData :: (Monad m) => Chart a -> SharedRep m (Chart a)
repChartStaticData c = do
  ann <- repAnnotation (c ^. #annotation)
  pure $ Chart ann (c ^. #spots)

repAnnotation :: (Monad m) => Annotation -> SharedRep m Annotation
repAnnotation initann = bimap hmap mmap rann <<*>> rs <<*>> ts <<*>> gs <<*>> ls <<*>> ps
  where
    rann =
      dropdownSum
        takeText
        id
        (Just "Chart Annotation")
        ["RectA", "TextA", "GlyphA", "LineA", "BlankA", "PixelA"]
        (annotationText initann)
    rs = repRectStyle defRectStyle
    ts = repTextStyle defText
    gs = repGlyphStyle defGlyph
    ls = repLineStyle defLine
    ps = repPixelStyle defPixel
    hmap ann rs ts gs ls ps =
      ann
        <> subtype rs (annotationText initann) "RectA"
        <> subtype ts (annotationText initann) "TextA"
        <> subtype gs (annotationText initann) "GlyphA"
        <> subtype ls (annotationText initann) "LineA"
        <> subtype ps (annotationText initann) "PixelA"
    mmap ann rs ts gs ls ps =
      case ann of
        "RectA" -> RectA rs
        "TextA" -> TextA ts texts
        "GlyphA" -> GlyphA gs
        "LineA" -> LineA ls
        "BlankA" -> BlankA
        "PixelA" -> PixelA ps
        _ -> BlankA
    defRectStyle = case initann of
      RectA s -> s
      _ -> defaultRectStyle
    defPixel = case initann of
      PixelA s -> s
      _ -> defaultPixelStyle
    (defText, texts) = case initann of
      TextA s xs -> (s, xs)
      _ -> (defaultTextStyle, Text.singleton <$> ['a' .. 'z'])
    defGlyph = case initann of
      GlyphA s -> s
      _ -> defaultGlyphStyle
    defLine = case initann of
      LineA s -> s
      _ -> defaultLineStyle

repRectStyle :: (Monad m) => RectStyle -> SharedRep m RectStyle
repRectStyle s = do
  bs <- slider (Just "border size") 0.0 0.1 0.001 (s ^. #borderSize)
  bc <- colorPicker (Just "border color") (hex $ s ^. #borderColor)
  bo <- slider (Just "border opacity") 0 1 0.1 (opac $ s ^. #borderColor)
  c <- colorPicker (Just "color") (hex $ s ^. #color)
  o <- slider (Just "opacity") 0 1 0.1 (opac $ s ^. #color)
  pure $ RectStyle bs (fromRGB (unsafeFromHex bc) bo) (fromRGB (unsafeFromHex c) o)

repPixelStyle ::
  (Monad m) =>
  PixelStyle ->
  SharedRep m PixelStyle
repPixelStyle cfg =
  bimap hmap mmap (unsafeFromHex <$> pcmin)
    <<*>> pomin
    <<*>> (unsafeFromHex <$> pcmax)
    <<*>> pomax
    <<*>> pd
    <<*>> prs
    <<*>> pt
  where
    pcmax = colorPicker (Just "high color") (toHex $ cfg ^. #pixelColorMax)
    pcmin = colorPicker (Just "low color") (toHex $ cfg ^. #pixelColorMin)
    pomax = slider (Just "high opacity") 0.0 1.0 0.001 (opac $ cfg ^. #pixelColorMax)
    pomin = slider (Just "low opacity") 0.0 1.0 0.001 (opac $ cfg ^. #pixelColorMin)
    pd = slider (Just "gradient direction") 0.0 (2 * pi) 0.001 (cfg ^. #pixelGradient)
    prs = repRectStyle (cfg ^. #pixelRectStyle)
    pt = textbox (Just "texture id") (cfg ^. #pixelTextureId)
    hmap pcmin' pomin' pcmax' pomax' pd' prs' pt' =
      pcmin' <> pomin' <> pcmax' <> pomax' <> pd' <> prs' <> pt'
    mmap pcmin' pomin' pcmax' pomax' pd' prs' pt' =
      PixelStyle (fromRGB pcmin' pomin') (fromRGB pcmax' pomax') pd' prs' pt'

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
  tm <- checkbox (Just "mathjax") (s ^. #hasMathjax)
  pure $ TextStyle ts (fromRGB (unsafeFromHex tc) to') ta th tv tn tr tt tm

repLineStyle :: (Monad m) => LineStyle -> SharedRep m LineStyle
repLineStyle s = do
  w <- slider (Just "width") 0.000 0.05 0.001 (s ^. #width)
  c <- colorPicker (Just "color") (toHex $ s ^. #color)
  o <- slider (Just "opacity") 0 1 0.1 (opac $ s ^. #color)
  pure $ LineStyle w (fromRGB (unsafeFromHex c) o)

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

repAnchor :: (Monad m) => Anchor -> SharedRep m Anchor
repAnchor a =
  toAnchor
    <$> dropdown
      takeText
      id
      (Just "Anchor")
      (fromAnchor <$> [AnchorStart, AnchorMiddle, AnchorEnd])
      (fromAnchor a)

repDirection :: (Monad m) => Direction -> SharedRep m Direction
repDirection a =
  toDirection
    <$> dropdown
      takeText
      id
      (Just "Direction")
      (fromDirection <$> [Vert, Hori])
      (fromDirection a)

repBar :: (Monad m) => Bar -> SharedRep m Bar
repBar cfg = do
  r <- repRectStyle (cfg ^. #rstyle)
  w <- slider (Just "width") 0 0.04 0.001 (cfg ^. #wid)
  b <- slider (Just "buffer") 0 0.08 0.001 (cfg ^. #buff)
  pure $ Bar r w b

repAdjustments :: (Monad m) => Adjustments -> SharedRep m Adjustments
repAdjustments a = do
  maxx <- slider (Just "maximum x ratio") 0.000 0.2 0.001 (a ^. #maxXRatio)
  maxy <- slider (Just "maximum y ratio") 0.000 0.2 0.001 (a ^. #maxYRatio)
  angle <- slider (Just "angle ratio") 0.000 1 0.001 (a ^. #angledRatio)
  diag <- checkbox (Just "allow diagonal text") (a ^. #allowDiagonal)
  pure $ Adjustments maxx maxy angle diag

repTitle :: (Monad m) => Title -> SharedRep m Title
repTitle cfg = do
  ttext <- textbox (Just "text") (cfg ^. #text)
  ts <- repTextStyle (cfg ^. #style)
  tp <- repPlace (cfg ^. #place)
  ta <- repAnchor (cfg ^. #anchor)
  b <- slider (Just "buffer") 0 0.2 0.01 (cfg ^. #buff)
  pure $ Title ttext ts tp ta b

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

repAxisOptions :: (Monad m) => AxisOptions -> SharedRep m AxisOptions
repAxisOptions cfg = bimap hmap AxisOptions b <<*>> adj <<*>> t <<*>> p
  where
    b =
      maybeRep
        (Just "axis bar")
        (isJust (cfg ^. #abar))
        (repBar (fromMaybe defaultBar (cfg ^. #abar)))
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

repData :: (Monad m) => Text -> SharedRep m [Spot Double]
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
        "sin" -> SpotPoint <$> gridP sin (Range 0 (2 * pi)) 30
        "line" ->
          SpotPoint . uncurry Point
            <$> [(0.0, 1.0), (1.0, 1.0), (2.0, 5.0)]
        "one" -> [SpotRect (Rect 0 1 0 1)]
        "dist" -> SpotRect <$> gridR (\x -> exp (- (x ** 2) / 2)) (Range (-5) 5) 50
        _ -> SpotPoint <$> gridP sin (Range 0 (2 * pi)) 30
    )

repFormatN :: (Monad m) => FormatN -> SharedRep m FormatN
repFormatN tf = bimap hmap mmap tformat <<*>> tcommas <<*>> tfixed <<*>> texpt <<*>> tpercent
  where
    tformat =
      dropdownSum
        takeText
        id
        (Just "Format")
        [ "Comma",
          "Fixed",
          "Expt",
          "Dollar",
          "Percent",
          "None"
        ]
        (fromFormatN tf)
    tcommas = sliderI (Just "prec") 0 8 1 (defInt tf)
    tfixed = sliderI (Just "prec") 0 8 1 (defInt tf)
    texpt = sliderI (Just "prec") 0 8 1 (defInt tf)
    tpercent = sliderI (Just "prec") 0 8 1 (defInt tf)
    defInt tf' = case tf' of
      FormatComma n -> n
      FormatFixed n -> n
      _ -> 3
    hmap tformat' tcommas' tfixed' texpt' tpercent' =
      div_
        ( tformat'
            <> subtype tcommas' (fromFormatN tf) "Comma"
            <> subtype tfixed' (fromFormatN tf) "Fixed"
            <> subtype texpt' (fromFormatN tf) "Expt"
            <> subtype tpercent' (fromFormatN tf) "Percent"
        )
    mmap tformat' tcommas' tfixed' texpt' tpercent' = case tformat' of
      "Comma" -> FormatComma tcommas'
      "Fixed" -> FormatFixed tfixed'
      "Expt" -> FormatExpt texpt'
      "Dollar" -> FormatDollar
      "Percent" -> FormatPercent tpercent'
      "None" -> FormatNone
      _ -> FormatNone

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
      _ -> FormatComma 2
    defExtend = case cfg of
      TickRound _ _ e -> e == TickExtend
      _ -> True


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

repRect :: (Monad m) => Rect (Range Double) -> Rect Double -> Rect Double -> SharedRep m (Rect Double)
repRect (Rect (Range xmin xmax) (Range zmin zmax) (Range ymin ymax) (Range wmin wmax)) (Rect xstep zstep ystep wstep) (Rect x z y w) =
  bimap
    (\a b c d -> a <> b <> c <> d)
    Rect
    (slider (Just "x") xmin xmax xstep x)
    <<*>> slider (Just "z") zmin zmax zstep z
    <<*>> slider (Just "y") ymin ymax ystep y
    <<*>> slider (Just "w") wmin wmax wstep w

repRectOne :: (Monad m) => Rect Double -> SharedRep m (Rect Double)
repRectOne a = repRect (Rect (Range 0 1) (Range 0 1) (Range 0 1) (Range 0 1)) (Rect 0.01 0.01 0.01 0.01) a

repRounded :: (Monad m) => (Double, Double, Double) -> SharedRep m (Double, Double, Double)
repRounded (a, b, c) =
  bimap
    (\a' b' c' -> a' <> b' <> c')
    (,,)
    (slider Nothing 0 1 0.001 a)
    <<*>> slider Nothing 0 1 0.001 b
    <<*>> slider Nothing 0 1 0.001 c

repTriple :: (Monad m) => (a, a, a) -> (a -> SharedRep m a) -> SharedRep m (a, a, a)
repTriple (a, b, c) sr =
  bimap (\a' b' c' -> a' <> b' <> c') (,,) (sr a) <<*>> sr b <<*>> sr c

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

repChartsWithSharedData ::
  (Monad m) =>
  SvgOptions ->
  HudOptions ->
  Int ->
  [Chart Double] ->
  ([[Spot Double]] -> SharedRep m [[Spot Double]]) ->
  SharedRep m (Text, Text)
repChartsWithSharedData css' hc' maxcs' cs' sspots =
  bimap
    hmap
    mmap
    cssr
    <<*>> annsr
    <<*>> sspots spots'
    <<*>> hr
    <<*>> debugFlags
  where
    spots' = view #spots <$> cs'
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
       in ( renderHudOptionsChart css'' h' [] ch,
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

repChartsWithStaticData ::
  (Monad m) =>
  SvgOptions ->
  HudOptions ->
  Int ->
  [Chart Double] ->
  SharedRep m (Text, Text)
repChartsWithStaticData css' hc' maxcs' cs' =
  repChartsWithSharedData css' hc' maxcs' cs' (bipure mempty)

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
      ( mconcat $
          [ "<h2>chart svg</h2>",
            "<xmp>",
            renderHudOptionsChart css hc [] cs,
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

repPixelOptions ::
  (Monad m) =>
  PixelOptions ->
  SharedRep m PixelOptions
repPixelOptions cfg =
  bimap hmap PixelOptions ps
    <<*>> pg
    <<*>> pr
  where
    ps = repPixelStyle (cfg ^. #poStyle)
    pg = repPointI (Point (Range 1 100) (Range 1 100)) (Point 1 1) (cfg ^. #poGrain)
    pr = repRect (Rect (Range 0 5) (Range 0 5) (Range 0 5) (Range 0 5)) (Rect 0.01 0.01 0.01 0.01) (cfg ^. #poRange)
    hmap ps' pg' pr' =
      accordion_
        "accpixel"
        Nothing
        [ ("Grain", pg'),
          ("Range", pr'),
          ("Style", ps')
        ]

repPixelLegendOptions ::
  (Monad m) =>
  PixelLegendOptions ->
  SharedRep m PixelLegendOptions
repPixelLegendOptions cfg =
  bimap hmap PixelLegendOptions ps
    <<*>> pt
    <<*>> pw
    <<*>> pa
    <<*>> pl
  where
    ps = repPixelStyle (cfg ^. #ploStyle)
    pt = textbox (Just "title") (cfg ^. #ploTitle)
    pw = slider (Just "width") 0.0 0.3 0.001 (cfg ^. #ploWidth)
    pa = repAxisOptions (cfg ^. #ploAxisOptions)
    pl = repLegendOptions (cfg ^. #ploLegendOptions)
    hmap ps' pt' pw' pa' pl' =
      accordion_
        "accplo"
        Nothing
        [ ("Style", ps'),
          ("Title", pt'),
          ("Width", pw'),
          ("Axis", pa'),
          ("Legend", pl')
        ]

repBarChart :: (Monad m) => SvgOptions -> BarData -> BarOptions -> SharedRep m (Text, Text)
repBarChart css bd bo = bimap hmap mmap rcss <<*>> rbd <<*>> rbo <<*>> debugFlags
  where
    rcss = repSvgOptions css
    rbo = repBarOptions 5 defaultRectStyle defaultTextStyle bo
    rbd = repBarData bd
    barchartsvg css' bd' bo' =
      let (hc', cs') = barChart bo' bd'
       in renderHudOptionsChart css' hc' [] cs'
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

repPixelChart ::
  (Monad m) =>
  (SvgOptions, PixelOptions, HudOptions, PixelLegendOptions, Point Double -> Double) ->
  SharedRep m (Text, Text)
repPixelChart (css, po, hc, plo, f) = bimap hmap mmap rcss <<*>> rpo <<*>> rhc <<*>> rplo <<*>> debugFlags
  where
    rcss = repSvgOptions css
    rpo = repPixelOptions po
    rhc = repHudOptionsDefault hc
    rplo = repPixelLegendOptions plo
    mmap rcss' rpo' rhc' rplo' debug =
      let (cs, hs) = pixelfl f rpo' rplo'
       in ( renderHudOptionsChart rcss' rhc' hs cs,
            debugHtml debug rcss' rhc' []
          )
    hmap rcss' rpo' rhc' rplo' debug =
      accordion_
        "accpc"
        Nothing
        [ ("Svg", rcss'),
          ("Hud", rhc'),
          ("Pixel Options", rpo'),
          ("Pixel Legend Options", rplo'),
          ("Debug", debug)
        ]

repNoData :: (Monad m) => SvgOptions -> Annotation -> HudOptions -> SharedRep m (Text, Text)
repNoData css ann hc =
  repChartsWithStaticData css hc 10 [Chart ann [SpotRect (Rect (-0.5) 0.5 (-0.5) 0.5)]]

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Chart.Page
  ( chartStyler,
    repChartStaticData,
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
    repHudConfig,
    repAxisConfig,
    repChartSvgStyle,
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
    repChoice,
    repLegendOptions,
    repLegendRows,
    repChartsWithSharedData,
    repChartsWithStaticData,
    debugHtml,
    debugFlags,
    repHudConfigDefault,
    repBarOptions,
    repBarData,
    repPixelOptions,
    repPixelLegendOptions,
  )
where

import Chart.Core
import Chart.Hud
import Chart.Types
import Chart.Format
import Chart.Bar
import Chart.Pixel
import Control.Lens
import Data.Attoparsec.Text
import Data.Biapplicative
import Data.Bool
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Lucid
import NumHask.Space
import Web.Page
import Prelude

chartStyler :: Bool -> Page
chartStyler doDebug =
  mathjaxSvgPage "hasmathjax" <>
  bootstrapPage
    <> bridgePage
    & #htmlHeader .~ title_ "chart styler"
    & #htmlBody
      .~ divClass_
        "container"
        ( divClass_
            "row d-flex justify-content-between"
            ( sec "col4" "input"
                <> sec "col8" "output"
            )
            <> bool mempty (divClass_ "row" (with div_ [id_ "debug"] mempty)) doDebug
        )
  where
    sec d n = divClass_ d (with div_ [id_ n] mempty)

subtype :: With a => a -> Text -> Text -> a
subtype h origt t =
  with
    h
    [ class__ "subtype ",
      data_ "sumtype" t,
      style_ ("display:" <> bool "block" "none" (origt /= t))
    ]

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
  bc <- colorPicker (Just "border color") (s ^. #borderColor)
  bo <- slider (Just "border opacity") 0 1 0.1 (s ^. #borderOpacity)
  c <- colorPicker (Just "color") (s ^. #color)
  o <- slider (Just "opacity") 0 1 0.1 (s ^. #opacity)
  pure $ RectStyle bs bc bo c o

repPixelStyle ::
  (Monad m) =>
  PixelStyle ->
  SharedRep m PixelStyle
repPixelStyle cfg =
  bimap hmap PixelStyle pcmin
    <<*>> pomin
    <<*>> pcmax
    <<*>> pomax
    <<*>> pd
    <<*>> prs
    <<*>> pt
  where
    pcmax = colorPicker (Just "high color") (cfg ^. #pixelColorMax)
    pcmin = colorPicker (Just "low color") (cfg ^. #pixelColorMin)
    pomax = slider (Just "high opacity") 0.0 1.0 0.001 (cfg ^. #pixelOpacityMax)
    pomin = slider (Just "low opacity") 0.0 1.0 0.001 (cfg ^. #pixelOpacityMin)
    pd = slider (Just "gradient direction") 0.0 (2*pi) 0.001 (cfg ^. #pixelGradient)
    prs = repRectStyle (cfg ^. #pixelRectStyle)
    pt = textbox (Just "texture id") (cfg ^. #pixelTextureId)

    hmap pcmin' pomin' pcmax' pomax' pd' prs' pt' =
      pcmin' <> pomin' <> pcmax' <> pomax' <> pd' <> prs' <> pt'

repGlyphStyle :: (Monad m) => GlyphStyle -> SharedRep m GlyphStyle
repGlyphStyle gs = first (\x -> cardify (mempty, [style_ "width: 10 rem;"]) Nothing (x, [])) $ do
  sh <- repGlyphShape (gs ^. #shape)
  sz <- slider (Just "Size") 0 0.2 0.001 (gs ^. #size)
  gc <-
    colorPicker
      (Just "Color")
      (gs ^. #color)
  go <- slider (Just "Opacity") 0 1 0.1 (gs ^. #opacity)
  bsz <- slider (Just "Border Size") 0 0.02 0.001 (gs ^. #borderSize)
  gbc <- colorPicker (Just "Border Color") (gs ^. #borderColor)
  gbo <- slider (Just "Border Opacity") 0 1 0.1 (gs ^. #borderOpacity)
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
  pure (GlyphStyle sz gc go gbc gbo bsz sh tr tt)

repTextStyle :: (Monad m) => TextStyle -> SharedRep m TextStyle
repTextStyle s = do
  ts <- slider (Just "size") 0.02 0.3 0.01 (s ^. #size)
  tc <- colorPicker (Just "color") (s ^. #color)
  to' <- slider (Just "opacity") 0 1 0.1 (s ^. #opacity)
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
  pure $ TextStyle ts tc to' ta th tv tn tr tt tm

repLineStyle :: (Monad m) => LineStyle -> SharedRep m LineStyle
repLineStyle s = do
  w <- slider (Just "width") 0.000 0.05 0.001 (s ^. #width)
  c <- colorPicker (Just "color") (s ^. #color)
  o <- slider (Just "opacity") 0 1 0.1 (s ^. #opacity)
  pure $ LineStyle w c o

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

repOrientation :: (Monad m) => Orientation -> SharedRep m Orientation
repOrientation a =
  toOrientation
    <$> dropdown
      takeText
      id
      (Just "Orientation")
      (fromOrientation <$> [Vert, Hori])
      (fromOrientation a)

repBar :: (Monad m) => Bar -> SharedRep m Bar
repBar cfg = do
  r <- repRectStyle (cfg ^. #rstyle)
  w <- slider (Just "width") 0 0.1 0.01 (cfg ^. #wid)
  b <- slider (Just "buffer") 0 0.2 0.01 (cfg ^. #buff)
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

repHudConfig ::
  (Monad m) =>
  Int ->
  Int ->
  AxisConfig ->
  Title ->
  LegendOptions ->
  LegendRows ->
  Annotation ->
  Text ->
  HudConfig ->
  SharedRep m HudConfig
repHudConfig naxes ntitles defaxis deftitle deflegend deflrs defann deflabel cfg =
  bimap hmap (\a b c d -> HudConfig a b c d) can
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
        repAxisConfig
        naxes
        defaxis
        (cfg ^. #hudAxes)
    l =
      maybeRep
        (Just "legend")
        (isJust $ cfg ^. #hudLegend)
        ((,) <$>
         repLegendRows 5 (maybe deflrs fst (cfg ^. #hudLegend)) defann deflabel <*>
         repLegendOptions (maybe deflegend snd (cfg ^. #hudLegend)))
    hmap can' ts' axs' l' =
      accordion_
        "accc"
        Nothing
        [ ("Axes", axs'),
          ("Canvas", can'),
          ("Titles", ts'),
          ("Legend", l')
        ]

repAxisConfig :: (Monad m) => AxisConfig -> SharedRep m AxisConfig
repAxisConfig cfg = bimap hmap AxisConfig b <<*>> adj <<*>> t <<*>> p
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

repChartSvgStyle :: (Monad m) => ChartSvgStyle -> SharedRep m ChartSvgStyle
repChartSvgStyle s =
  bimap
    hmap
    ChartSvgStyle
    x
    <<*>> y
    <<*>> a
    <<*>> op'
    <<*>> ip
    <<*>> fr
    <<*>> orig'
    <<*>> esc
    <<*>> crisp
  where
    x = slider (Just "sizex") 0 1000 1 (s ^. #sizex)
    y = slider (Just "sizey") 0 1000 1 (s ^. #sizey)
    a = slider (Just "aspect") 0.2 5 0.1 (s ^. #chartAspect)
    op' =
      maybeRep
        (Just "outer pad")
        (isJust (s ^. #outerPad))
        (slider Nothing 1 1.2 0.01 (fromMaybe 1 (s ^. #outerPad)))
    ip =
      maybeRep
        (Just "inner pad")
        (isJust (s ^. #innerPad))
        (slider Nothing 1 1.2 0.01 (fromMaybe 1 (s ^. #innerPad)))
    fr =
      maybeRep
        (Just "frame")
        (isJust (s ^. #chartFrame))
        (repRectStyle (fromMaybe defaultSvgFrame (s ^. #chartFrame)))
    orig' =
      maybeRep
        (Just "origin")
        (isJust (s ^. #orig))
        (repGlyphStyle (fromMaybe defaultOrigin (s ^. #orig)))
    esc = checkbox (Just "escape text") (s ^. #escapeText)
    crisp = checkbox (Just "Use CssCrisp") (s ^. #useCssCrisp)
    hmap x' y' a' op'' ip' fr' orig'' esc' crisp' =
      accordion_
        "accsvg"
        Nothing
        [ ("Sizing", x' <> y' <> a'),
          ("Padding", op'' <> ip'),
          ("Frame", fr'),
          ("Origin", orig''),
          ("Escape", esc'),
          ("CssCrisp", crisp')
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
        "one" -> [SR 0 1 0 1]
        "dist" -> SpotRect <$> gridR (\x -> exp (- (x ** 2) / 2)) (Range (-5) 5) 50
        _ -> SpotPoint <$> gridP sin (Range 0 (2 * pi)) 30
    )

repFormatN :: (Monad m) => FormatN -> SharedRep m FormatN
repFormatN tf = bimap hmap mmap tformat <<*>> tcommas <<*>> tfixed <<*>> texpt
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
          "None"
        ]
        (fromFormatN tf)
    tcommas = sliderI (Just "prec") 0 8 1 (defInt tf)
    tfixed = sliderI (Just "prec") 0 8 1 (defInt tf)
    texpt = sliderI (Just "prec") 0 8 1 (defInt tf)
    defInt tf' = case tf' of
      FormatComma n -> n
      FormatFixed n -> n
      _ -> 3
    hmap tformat' tcommas' tfixed' texpt'=
      div_
        ( tformat'
            <> subtype tcommas' (fromFormatN tf) "Comma"
            <> subtype tfixed' (fromFormatN tf) "Fixed"
            <> subtype texpt' (fromFormatN tf) "Expt"
        )
    mmap tformat' tcommas' tfixed' texpt' = case tformat' of
      "Comma" -> FormatComma tcommas'
      "Fixed" -> FormatFixed tfixed'
      "Expt" -> FormatExpt texpt'
      "Dollar" -> FormatDollar
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
      _ -> zip [0 .. 5] (Text.pack . show <$> [0 .. 5 :: Int])
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
repTick cfg = SharedRep $ do
  (Rep h fa) <-
    unrep $ bimap hmap Tick ts <<*>> gt <<*>> tt <<*>> lt
  h' <- zoom _1 h
  pure (Rep h' fa)
  where
    hmap ts' gt' tt' lt' =
      accordion
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
repGlyphShape sh = bimap hmap mmap sha <<*>> ell <<*>> rsharp <<*>> vl <<*>> hl <<*>> rround <<*>> tri
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
          "Smiley"
        ]
        (glyphText sh)
    ell = slider Nothing 0.5 2 0.01 defRatio
    rsharp = slider Nothing 0.5 2 0.01 defRatio
    vl = slider Nothing 0.001 0.1 0.0001 defLine
    hl = slider Nothing 0.001 0.1 0.0001 defLine
    rround = repRounded defRounded
    tri =
      repTriple
        defTriangle
        ( repPoint
            (Point (Range 0 1) (Range 0 1))
            (Point 0.001 0.001)
        )
    hmap sha' ell' rsharp' vl' hl' rround' tri' =
      sha'
        <> subtype ell' (glyphText sh) "Ellipse"
        <> subtype rsharp' (glyphText sh) "RectSharp"
        <> subtype vl' (glyphText sh) "VLine"
        <> subtype hl' (glyphText sh) "HLine"
        <> subtype rround' (glyphText sh) "RectRounded"
        <> subtype tri' (glyphText sh) "Triangle"
    mmap sha' ell' rsharp' vl' hl' rround' tri' =
      case sha' of
        "Circle" -> CircleGlyph
        "Square" -> SquareGlyph
        "Ellipse" -> EllipseGlyph ell'
        "RectSharp" -> RectSharpGlyph rsharp'
        "RectRounded" -> (\(a, b, c) -> RectRoundedGlyph a b c) rround'
        "Triangle" -> (\(a, b, c) -> TriangleGlyph a b c) tri'
        "VLine" -> VLineGlyph vl'
        "HLine" -> HLineGlyph hl'
        "Smiley" -> SmileyGlyph
        _ -> SmileyGlyph
    defRatio = case sh of
      EllipseGlyph r -> r
      RectSharpGlyph r -> r
      _ -> 1.5
    defLine = case sh of
      VLineGlyph r -> r
      HLineGlyph r -> r
      _ -> 0.05
    defRounded = case sh of
      RectRoundedGlyph a b c -> (a, b, c)
      _ -> (0.884, 2.7e-2, 5.0e-2)
    defTriangle = case sh of
      TriangleGlyph a b c -> (a, b, c)
      _ -> (Point 0.0 0.0, Point 1 1, Point 1 0)

repChoice :: (Monad m) => Int -> [(Text, SharedRep m (Text, Text))] -> SharedRep m (Text, Text)
repChoice initt xs =
  bimap hmap mmap dd
    <<*>> foldr (\x a -> bimap (:) (:) x <<*>> a) (pure []) cs
  where
    ts = fst <$> xs
    cs = snd <$> xs
    dd = dropdownSum takeText id Nothing ts t0
    t0 = ts !! initt
    hmap dd' cs' =
      div_
        ( dd'
            <> mconcat (zipWith (\c t -> subtype c t0 t) cs' ts)
        )
    mmap dd' cs' = maybe (Data.List.head cs') (cs' !!) (elemIndex dd' ts)

repLegendOptions :: (Monad m) => LegendOptions -> SharedRep m LegendOptions
repLegendOptions initl = LegendOptions <$> lsize' <*> hgap' <*> vgap' <*> ltext' <*> lmax' <*> innerPad' <*> outerPad' <*> legendFrame' <*> lplace' <*> scale'
  where
    lsize' = slider (Just "glyph size") 0.000 0.5 0.001 (initl ^. #lsize)
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
    scale' = slider (Just "scale") 0.01 1 0.001 (initl ^. #scale)

repLegendRows ::
  (Monad m) =>
  Int ->
  LegendRows ->
  Annotation ->
  Text ->
  SharedRep m LegendRows
repLegendRows n initlrs defann deflabel =
  bimap hmap mmap lrows <<*>> labelsc <<*>> labels
    <<*>> anns
  where
    lrows =
      dropdownSum
        takeText
        id
        (Just "LegendRows")
        [ "LegendFromChart",
          "LegendManual"
        ]
        (legendRowsText initlrs)
    labels =
      listRep
        (Just "labels")
        "labelsz"
        (checkbox Nothing)
        (textbox Nothing)
        n
        deflabel
        (defLabels initlrs)
    labelsc =
      listRep
        (Just "labelsc")
        "labelscz"
        (checkbox Nothing)
        (textbox Nothing)
        n
        deflabel
        (defLabels initlrs)
    anns =
      listRep
        (Just "annotations")
        "annsz"
        (checkbox Nothing)
        repAnnotation
        n
        defann
        (defAnns initlrs)
    defLabels lrs'' = case lrs'' of
      LegendFromChart ts -> ts
      LegendManual lrs' -> snd <$> lrs'
    defAnns lrs'' = case lrs'' of
      LegendFromChart _ -> []
      LegendManual lrs' -> fst <$> lrs'
    hmap lrows' labelsc' labels' anns' =
      div_
        ( lrows'
            <> subtype labelsc' (legendRowsText initlrs) "LegendFromChart"
            <> subtype labels' (legendRowsText initlrs) "LegendManual"
            <> subtype anns' (legendRowsText initlrs) "LegendManual"
        )
    mmap lrows' labelsc' labels' anns' = case lrows' of
      "LegendFromChart" -> LegendFromChart labelsc'
      "LegendManual" -> LegendManual (zip anns' labels')
      _ -> LegendManual (zip anns' labels')

repChartsWithSharedData ::
  (Monad m) =>
  ChartSvgStyle ->
  HudConfig ->
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
      repHudConfig
        2
        3
        defaultAxisConfig
        (defaultTitle "default")
        defaultLegendOptions
        (LegendFromChart ["default"])
        BlankA
        ""
        hc'
    cssr = repChartSvgStyle css'
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
       in ( renderHudChartWith css'' h' ch,
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
  ChartSvgStyle ->
  HudConfig ->
  Int ->
  [Chart Double] ->
  SharedRep m (Text, Text)
repChartsWithStaticData css' hc' maxcs' cs' =
  repChartsWithSharedData css' hc' maxcs' cs' (bipure mempty)

debugHtml :: (Bool, Bool, Bool) -> ChartSvgStyle -> HudConfig -> [Chart Double] -> Text
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
          (\x -> "<p style='white-space: pre'>" <> x <> "</p>")
            <$> [ "<h2>chart svg</h2>",
                  renderHudChartWith css hc cs
                ]
      )
      ((\(_, a, _) -> a) debug)
    <> bool
      mempty
      ( mconcat $
          (\x -> "<p style='white-space: pre'>" <> x <> "</p>")
            <$> [ "<h2>chart value</h2>",
                  Text.pack $ show cs
                ]
      )
      ((\(_, _, a) -> a) debug)

debugFlags :: (Monad m) => SharedRepF m (Html ()) (Bool, Bool, Bool)
debugFlags =
  bimap
    (\a b c -> a <> b <> c)
    (,,)
    (checkbox (Just "show hudConfig values") False)
    <<*>> checkbox (Just "show chart svg") False
    <<*>> checkbox (Just "show Chart values") False

repHudConfigDefault :: Monad m => HudConfig -> SharedRep m HudConfig
repHudConfigDefault hc =
  repHudConfig
    2
    3
    defaultAxisConfig
    (defaultTitle "default")
    defaultLegendOptions
    (LegendFromChart ["insert names here"])
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
    dv = checkbox (Just "display values") (cfg ^. #displayValues)
    fn = repFormatN (cfg ^. #valueFormatN)
    av = checkbox (Just "accumulate values") (cfg ^. #accumulateValues)
    or = repOrientation (cfg ^. #orientation)
    ho = repHudConfig
        2
        3
        defaultAxisConfig
        (defaultTitle "bar options")
        (maybe defaultLegendOptions snd (cfg ^. #barHudConfig . #hudLegend))
        (maybe (LegendManual []) fst (cfg ^. #barHudConfig . #hudLegend))
        BlankA
        ""
        (cfg ^. #barHudConfig)

    hmap rs' ts' og' ig' tg' dv' fn' av' or' ho' =
      accordion_
        "accbo"
        Nothing
        [ ("Bar Styles", rs'),
          ("Text Styles", ts'),
          ("Gaps", og' <> ig' <> tg'),
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
      maybeRep Nothing (isJust (initbd ^. #barRowLabels))
      (either (const []) id <$>
       readTextbox (Just "row labels") (fromMaybe [] (initbd ^. #barRowLabels)))
    cl =
      maybeRep Nothing (isJust (initbd ^. #barColumnLabels))
      (either (const []) id <$>
       readTextbox (Just "column labels") (fromMaybe [] (initbd ^. #barColumnLabels)))
    bd =
      either (const (pure [])) id <$>
      readTextbox (Just "bar data") (initbd ^. #barData)
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

-- PixelLegendOptions
--      {ploStyle :: PixelStyle, ploTitle :: Text, ploWidth :: Double, ploAxisConfig :: AxisConfig, ploLegendOptions :: LegendOptions}

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
    pa = repAxisConfig (cfg ^. #ploAxisConfig)
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

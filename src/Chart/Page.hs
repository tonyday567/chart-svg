{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wall #-}

module Chart.Page
  ( repChart
  , repAnnotation
  , repRectStyle
  , repTextStyle
  , repGlyphStyle
  , repPlace
  , repAnchor
  , repBar
  , repAdjustments
  , repTitle
  , repHudConfig
  , repAxisConfig
  , repChartSvgStyle
  , repData
  , repTickFormat
  , repTickStyle
  , repTick
  , repPoint
  , repRect
  , repRectOne
  , repRounded
  , repTriple
  , repGlyphShape
  , repChoice
  , repLegend
  )
  where

import Chart.Core
import Chart.Hud
import Chart.Types
import NumHask.Space
import Control.Lens
import Data.Attoparsec.Text
import Lucid
import Prelude
import Web.Page
import qualified Box ()
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Biapplicative
import Data.List
import Data.Bool
import Data.Maybe

subtype :: With a => a -> Text -> Text -> a
subtype h origt t =
  with h
  [ class__ "subtype "
  , data_ "sumtype" t
  , style_ ("display:" <> bool "block" "none" (origt /= t))
  ]

repChart :: (Monad m) => Chart a -> SharedRep m (Chart a)
repChart c = do
  ann <- repAnnotation (c ^. #annotation)
  pure $ Chart ann (c ^. #spots)

repAnnotation :: (Monad m) => Annotation -> SharedRep m Annotation
repAnnotation ann = bimap hmap mmap a <<*>> rs <<*>> ts <<*>> gs <<*>> ls
  where
    a = dropdownSum takeText id (Just "Chart Annotation")
      ["RectA", "TextA", "GlyphA", "LineA", "BlankA"]
      (annotationText ann)
    rs = repRectStyle defRectStyle
    ts = repTextStyle defText
    gs = repGlyphStyle defGlyph
    ls = repLineStyle defLine
    hmap ann' rs' ts' gs' ls' =
      ann' <>
      subtype rs' (annotationText ann) "RectA" <>
      subtype ts' (annotationText ann) "TextA" <>
      subtype gs' (annotationText ann) "GlyphA" <>
      subtype ls' (annotationText ann) "LineA"
    mmap ann' rs' ts' gs' ls' =
      case ann' of
        "RectA" -> RectA rs'
        "TextA" -> TextA ts' texts
        "GlyphA" -> GlyphA gs'
        "LineA" -> LineA ls'
        "BlankA" -> BlankA
        _ -> BlankA
    defRectStyle = case ann of
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

repRectStyle :: (Monad m) => RectStyle -> SharedRep m RectStyle
repRectStyle s = do
  bs <- slider (Just "border size") 0.0 0.1 0.001 (s ^. #borderSize)
  bc <- colorPicker (Just "border color") (s ^. #borderColor)
  bo <- slider (Just "border opacity") 0 1 0.1 (s ^. #borderOpacity)
  c <- colorPicker (Just "color") (s ^. #color)
  o <- slider (Just "opacity") 0 1 0.1 (s ^. #opacity)
  pure $ RectStyle bs bc bo c o



repGlyphStyle :: (Monad m) => GlyphStyle -> SharedRep m GlyphStyle
repGlyphStyle gs = first (\x -> cardify (mempty, [style_ "width: 10 rem;"]) Nothing (x,[])) $ do
  sh <- repGlyphShape (gs ^. #shape)
  sz <- slider (Just "Size") 0 0.2 0.001 (gs ^. #size)
  gc <- colorPicker (Just "Color")
    (gs ^. #color)
  go <- slider (Just "Opacity") 0 1 0.1 (gs ^. #opacity)
  bsz <- slider (Just "Border Size") 0 0.02 0.001 (gs ^. #borderSize)
  gbc <- colorPicker (Just "Border Color") (gs ^. #borderColor)
  gbo <- slider (Just "Border Opacity") 0 1 0.1 (gs ^. #borderOpacity)
  tr <- maybeRep (Just "rotation") (isJust (gs ^. #rotation))
    (slider (Just "rotation") (-180) 180 10 (fromMaybe 0 (gs ^. #rotation)))
  tt <- maybeRep (Just "translate") (isJust (gs ^. #translate))
    (repPoint (Point (Range 0 1) (Range 0 1))
                                 (Point 0.001 0.001) (Point 0 0))
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
  tr <- maybeRep (Just "rotation") (isJust (s ^. #rotation))
    (slider (Just "rotation") (-180) 180 10 (fromMaybe 0 (s ^. #rotation)))
  tt <- maybeRep (Just "translate") (isJust (s ^. #translate))
    (repPoint (Point (Range 0 1) (Range 0 1))
                                 (Point 0.001 0.001) (Point 0 0))
  pure $ TextStyle ts tc to' ta th tv tn tr tt

repLineStyle :: (Monad m) => LineStyle -> SharedRep m LineStyle
repLineStyle s = do
  w <- slider (Just "width") 0.000 0.05 0.001 (s ^. #width)
  c <- colorPicker (Just "color") (s ^. #color)
  o <- slider (Just "opacity") 0 1 0.1 (s ^. #opacity)
  pure $ LineStyle w c o

repPlace :: (Monad m) => Place Double -> SharedRep m (Place Double)
repPlace p = bimap hmap mmap splace <<*>> sp
  where
    splace =
      dropdownSum takeText id (Just "Placement")
      [ "Bottom"
      , "Left"
      , "Top"
      , "Right"
      , "Absolute"
      ] 
      (placeText p)
    sp = repPoint (Point (Range 0 1) (Range 0 1)) (Point 0.01 0.01) (defPoint p)
    defPoint p'' = case p'' of
      PlaceAbsolute p' -> p'
      _ -> Point 0.0 0.0
    hmap splace' sp' =
      div_
      (splace' <>
       subtype sp' (placeText p) "Absolute"
      )
    mmap splace' sp' = case splace' of
      "Top" -> PlaceTop
      "Bottom" -> PlaceBottom
      "Left" -> PlaceLeft
      "Right" -> PlaceRight
      "Absolute" -> PlaceAbsolute sp'
      _ -> PlaceBottom


repAnchor :: (Monad m) => Anchor -> SharedRep m Anchor
repAnchor a = toAnchor <$>
    dropdown
    takeText
    id
    (Just "Anchor")
    (fromAnchor <$> [AnchorStart, AnchorMiddle, AnchorEnd])
    (fromAnchor a)

repBar :: (Monad m) => Bar Double -> SharedRep m (Bar Double)
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

repTitle :: (Monad m) => Title Double -> SharedRep m (Title Double)
repTitle cfg = do
  ttext <- textbox (Just "text") (cfg ^. #text)
  ts <- repTextStyle (cfg^. #style)
  tp <- repPlace (cfg ^. #place)
  ta <- repAnchor (cfg ^. #anchor)
  b <- slider (Just "buffer") 0 0.2 0.01 (cfg ^. #buff)
  pure $ Title ttext ts tp ta b

repHudConfig :: (Monad m) => Int -> Int -> AxisConfig Double -> Title Double -> LegendOptions Double ->
  HudConfig -> SharedRep m HudConfig
repHudConfig naxes ntitles defaxis deftitle deflegend cfg =
  bimap hmap HudConfig can <<*>> ts <<*>> axs <<*>> ls
  where
    can = maybeRep (Just "canvas") (isJust (cfg ^. #hudCanvas)) $
      repRectStyle (fromMaybe defaultCanvas (cfg ^. #hudCanvas))
    ts = listifyMaybe' (Just "titles") "tz" (checkbox Nothing) repTitle
      ntitles deftitle (cfg ^. #hudTitles)
    axs = listifyMaybe' (Just "axes") "axz" (checkbox Nothing) repAxisConfig
      naxes defaxis (cfg ^. #hudAxes)
    ls = listifyMaybe' (Just "legends") "lz" (checkbox Nothing) repLegend
      naxes deflegend (cfg ^. #hudLegends)
    hmap can' ts' axs' ls' =
      accordion_ "accc" Nothing
      [ ("Axes", axs')
      , ("Canvas", can')
      , ("Titles", ts')
      , ("Legends", ls')
      ]

repAxisConfig :: (Monad m) => AxisConfig Double -> SharedRep m (AxisConfig Double)
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
    hmap b' hauto' t' p' = accordion_ "accaxis" Nothing
      [ ("Bar", b')
      , ("Auto", hauto')
      , ("Ticks", t')
      , ("Place", p')
      ]


repChartSvgStyle :: (Monad m) => ChartSvgStyle -> SharedRep m ChartSvgStyle
repChartSvgStyle s =
  bimap hmap ChartSvgStyle x <<*>> y <<*>> a <<*>>
  op' <<*>> ip <<*>> fr <<*>> orig'
  where
    x = slider (Just "sizex") 0 1000 1 (s ^. #sizex)
    y = slider (Just "sizey") 0 1000 1 (s ^. #sizey)
    a = slider (Just "aspect") 0.2 5 0.1 (s ^. #chartAspect)
    op' = maybeRep (Just "outer pad")
      (isJust (s ^. #outerPad))
      (slider Nothing 1 1.2 0.01 (fromMaybe 1 (s ^. #outerPad)))
    ip = maybeRep (Just "inner pad")
      (isJust (s ^. #innerPad))
      (slider Nothing 1 1.2 0.01 (fromMaybe 1 (s ^. #innerPad)))
    fr = maybeRep (Just "frame") (isJust (s ^. #chartFrame))
      (repRectStyle (fromMaybe defaultSvgFrame (s ^. #chartFrame)))
    orig' = maybeRep (Just "origin") (isJust (s ^. #orig))
      (repGlyphStyle (fromMaybe defaultOrigin (s ^. #orig)))
    hmap x' y' a' op'' ip' fr' orig'' = accordion_ "accsvg" Nothing
      [ ("Sizing", x' <> y' <> a')
      , ("Padding", op'' <> ip')
      , ("Frame", fr')
      , ("Origin", orig'')
      ]

repData :: (Monad m) => Text -> SharedRep m [Spot Double]
repData d = do
  a <- dropdown takeText id (Just "type")
    [ "sin"
    , "line"
    , "one"
    , "dist"
    ] d
  pure (case a of
          "sin" -> SpotPoint <$> gridP sin (Range 0 (2*pi)) 30
          "line" -> SpotPoint . uncurry Point <$>
            [(0.0, 1.0), (1.0, 1.0), (2.0, 5.0)]
          "one" -> [SR 0 1 0 1]
          "dist" -> SpotRect <$> gridR (\x -> exp (-(x ** 2) / 2)) (Range -5 5) 50
          _ -> SpotPoint <$> gridP sin (Range 0 (2*pi)) 30
       )

repTickFormat :: (Monad m) => TickFormat -> SharedRep m TickFormat
repTickFormat tf = bimap hmap mmap tformat <<*>> tcommas <<*>> tfixed
  where
    tformat = dropdownSum takeText id (Just "Tick Format")
      [ "TickFormatDefault"
      , "TickFormatCommas"
      , "TickFormatFixed"
      , "TickFormatDollars"
      ]
      (tickFormatText tf)
    tcommas = sliderI (Just "prec") 0 8 1 (defInt tf)
    tfixed = sliderI (Just "prec") 0 8 1 (defInt tf)
    defInt tf' = case tf' of
      TickFormatCommas n -> n
      TickFormatFixed n -> n
      _ -> 3
    hmap tformat' tcommas' tfixed' =
      div_
      (tformat' <>
       subtype tcommas' (tickFormatText tf) "TickFormatCommas" <>
       subtype tfixed' (tickFormatText tf) "TickFormatFixed"
      )
    mmap tformat' tcommas' tfixed' = case tformat' of
      "TickFormatDefault" -> TickFormatDefault
      "TickFormatCommas" -> TickFormatCommas tcommas'
      "TickFormatFixed" -> TickFormatFixed tfixed'
      "TickFormatDollars" -> TickFormatDollars
      _ -> TickFormatDefault

repTickStyle :: (Monad m) => TickStyle Double -> SharedRep m (TickStyle Double)
repTickStyle cfg =
  bimap hmap mmap ts <<*>> ls <<*>> tr <<*>> te <<*>> tplaced
  where
    ts = dropdownSum takeText id (Just "Tick Style")
      ["TickNone", "TickLabels", "TickRound", "TickExact", "TickPlaced"]
      (tickStyleText cfg)
    ls = accordionListify (Just "tick labels") "tick-style-labels" Nothing
      (textbox . Just) (defaultListifyLabels (length defLabels)) defLabels
    tr = (,,) <$> sliderI (Just "Number of ticks") 0 20 1 defTn <*>
      repTickFormat defTf <*>
      (bool NoTickExtend TickExtend <$>
       checkbox (Just "extend") defExtend)
    te = (,) <$> sliderI (Just "Number of ticks") 0 20 1 defTn <*>
      repTickFormat defTf
    tplaced = accordionListify (Just "placed ticks") "tick-style-placed"
      Nothing dt (defaultListifyLabels (length dtDef)) dtDef
    hmap ts' ls' tr' te' tplaced' =
      div_
      (ts' <>
       subtype ls' (tickStyleText cfg) "TickLabels" <>
       subtype tr' (tickStyleText cfg) "TickRound" <>
       subtype te' (tickStyleText cfg) "TickExact" <>
       subtype tplaced' (tickStyleText cfg) "TickPlaced"
      )
    mmap ts' ls' (tri,trf,tre) (tei,tef) tplaced' = case ts' of
      "TickNone" -> TickNone
      "TickLabels" -> TickLabels ls'
      "TickRound" -> TickRound trf tri tre
      "TickExact" -> TickExact tef tei
      "TickPlaced" -> TickPlaced tplaced'
      _ -> TickNone
    dtDef = case cfg of
      TickPlaced x -> x
      _ -> zip [0..5] (Text.pack . show <$> [0..5::Int])
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
      _ -> TickFormatDefault
    defExtend = case cfg of
      TickRound _ _ e -> e == TickExtend
      _ -> True

repTick :: (Monad m) => Tick Double -> SharedRep m (Tick Double)
repTick cfg = bimap hmap Tick ts <<*>> gt <<*>> tt <<*>> lt
  where
    ts = repTickStyle (cfg ^. #tstyle)
    gt = maybeRep Nothing (isJust (cfg ^. #gtick)) $ bimap (<>) (,) (repGlyphStyle (maybe defaultGlyphTick fst (cfg ^. #gtick))) <<*>>
      slider (Just "buffer") 0 0.05 0.001 (maybe 0.05 snd (cfg ^. #gtick))
    tt = maybeRep Nothing (isJust (cfg ^. #ttick)) $ bimap (<>) (,) (repTextStyle (maybe defaultTextTick fst (cfg ^. #ttick))) <<*>>
      slider (Just "buffer") 0 0.05 0.001 (maybe 0.05 snd (cfg ^. #ttick))
    lt = maybeRep Nothing (isJust (cfg ^. #ltick)) $ bimap (<>) (,) (repLineStyle (maybe defaultLineTick fst (cfg ^. #ltick))) <<*>>
      slider (Just "buffer") -0.1 0.1 0.001 (maybe 0 snd (cfg ^. #ltick))
    hmap ts' gt' tt' lt' =
      accordion_ "acctick" Nothing
      [ ("style", ts')
      , ("glyph", gt')
      , ("text", tt')
      , ("line", lt')
      ]

repPoint :: (Monad m) => Point (Range Double) -> Point Double -> Point Double -> SharedRep m (Point Double)
repPoint (Point (Range xmin xmax) (Range ymin ymax)) (Point xstep ystep) (Point x y) =
  bimap (<>) Point
  (slider (Just "x") xmin xmax xstep x) <<*>>
  slider (Just "y") ymin ymax ystep y

repRect :: (Monad m) => Rect (Range Double) -> Rect Double -> Rect Double -> SharedRep m (Rect Double)
repRect (Rect (Range xmin xmax) (Range zmin zmax) (Range ymin ymax) (Range wmin wmax)) (Rect xstep zstep ystep wstep) (Rect x z y w) =
  bimap (\a b c d -> a <> b <> c <> d) Rect
  (slider (Just "x") xmin xmax xstep x) <<*>>
  slider (Just "z") zmin zmax zstep z <<*>>
  slider (Just "y") ymin ymax ystep y <<*>>
  slider (Just "w") wmin wmax wstep w

repRectOne :: (Monad m) => Rect Double -> SharedRep m (Rect Double)
repRectOne a = repRect (Rect (Range 0 1) (Range 0 1) (Range 0 1) (Range 0 1)) (Rect 0.01 0.01 0.01 0.01) a

repRounded :: (Monad m) => (Double, Double, Double) -> SharedRep m (Double, Double, Double)
repRounded (a,b,c) =
  bimap (\a' b' c' -> a' <> b' <> c') (,,)
  (slider Nothing 0 1 0.001 a) <<*>>
  slider Nothing 0 1 0.001 b <<*>>
  slider Nothing 0 1 0.001 c

repTriple :: (Monad m) => (a, a, a) -> (a -> SharedRep m a) -> SharedRep m (a,a,a)
repTriple (a,b,c) sr =
  bimap (\a' b' c' -> a' <> b' <> c') (,,) (sr a) <<*>> sr b <<*>> sr c

repGlyphShape :: (Monad m) => GlyphShape -> SharedRep m GlyphShape
repGlyphShape sh = bimap hmap mmap sha <<*>> ell <<*>> rsharp <<*>> vl <<*>> hl <<*>> rround <<*>> tri
  where
    sha = dropdownSum takeText id Nothing
      [ "Circle"
      , "Square"
      , "Triangle"
      , "Ellipse"
      , "RectSharp"
      , "RectRounded"
      , "VLine"
      , "HLine"
      , "Smiley"
      ]
      (glyphText sh)
    ell = slider Nothing 0.5 2 0.01 defRatio
    rsharp = slider Nothing 0.5 2 0.01 defRatio
    vl = slider Nothing 0.001 0.1 0.0001 defLine
    hl = slider Nothing 0.001 0.1 0.0001 defLine
    rround = repRounded defRounded
    tri = repTriple defTriangle (repPoint (Point (Range 0 1) (Range 0 1))
                                 (Point 0.001 0.001))
    hmap sha' ell' rsharp' vl' hl' rround' tri' =
      sha' <>
      subtype ell' (glyphText sh) "Ellipse" <>
      subtype rsharp' (glyphText sh) "RectSharp" <>
      subtype vl' (glyphText sh) "VLine" <>
      subtype hl' (glyphText sh) "HLine" <>
      subtype rround' (glyphText sh) "RectRounded" <>
      subtype tri' (glyphText sh) "Triangle"
    mmap sha' ell' rsharp' vl' hl' rround' tri' =
      case sha' of
        "Circle" -> CircleGlyph
        "Square" -> SquareGlyph
        "Ellipse" -> EllipseGlyph ell'
        "RectSharp" -> RectSharpGlyph rsharp'
        "RectRounded" -> (\(a,b,c) -> RectRoundedGlyph a b c) rround'
        "Triangle" -> (\(a,b,c) -> TriangleGlyph a b c) tri'
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
      RectRoundedGlyph a b c -> (a,b,c)
      _ -> (0.884, 2.7e-2, 5.0e-2)
    defTriangle = case sh of
      TriangleGlyph a b c -> (a,b,c)
      _ -> (Point 0.0 0.0, Point 1 1, Point 1 0)

repChoice :: (Monad m) => Int -> [(Text, SharedRep m (Text, Text))] -> SharedRep m (Text, Text)
repChoice initt xs = bimap hmap mmap dd <<*>>
  foldr (\x a -> bimap (:) (:) x <<*>> a) (pure []) cs
  where
    ts = fst <$> xs
    cs = snd <$> xs
    dd = dropdownSum takeText id (Just "Chart Family") ts t0
    t0 = ts !! initt
    hmap dd' cs' =
      div_ (dd' <>
      mconcat (zipWith (\c t -> subtype c t0 t) cs' ts))
    mmap dd' cs' = maybe (Data.List.head cs') (cs'!!) (elemIndex dd' ts)

repLegend :: (Monad m) => LegendOptions Double -> SharedRep m (LegendOptions Double)
repLegend initl = LegendOptions <$> lcharts' <*> lsize' <*> hgap' <*> vgap' <*> ltext' <*> lmax' <*> innerPad' <*> outerPad' <*> legendFrame' <*> lplace' <*> scale'
  where
    -- lentry :: (Annotation, Text) -> SharedRep m (Annotation, Text)
    lentry (a, t) = (,) <$> repAnnotation a <*> textbox Nothing t
    -- lcharts :: SharedRep m [(Annotation, Text)]
    lcharts' =
      listifyMaybe' (Just "legends") "lz" (checkbox Nothing)
      lentry
      (initl ^. #lmax)
      (BlankA, "") (initl ^. #lcharts)
    lsize' = slider (Just "size") 0.000 0.1 0.001 (initl ^. #lsize)
    hgap' = slider (Just "horizontal gap") 0.000 0.1 0.001 (initl ^. #hgap)
    vgap' = slider (Just "vertical gap") 0.000 0.1 0.001 (initl ^. #vgap)
    ltext' = repTextStyle (initl ^. #ltext)
    lmax' = sliderI (Just "max entries") 0 10 1 (initl ^. #lmax)
    -- orig = maybeRep (Just "origin") (isJust (s ^. #orig))
    --   (repGlyphStyle (maybe defaultOrigin id (s ^. #orig)))
    innerPad' =
      maybeRep Nothing (isJust (initl ^. #innerPad))
      (slider (Just "inner padding") 1 1.2 0.001
       (fromMaybe 1 (initl ^. #innerPad)))
    outerPad' =
      maybeRep Nothing (isJust (initl ^. #outerPad))
      (slider (Just "outer padding") 1 1.2 0.001
       (fromMaybe 1 (initl ^. #outerPad)))
    legendFrame' =
      maybeRep (Just "frame") (isJust (initl ^. #legendFrame))
      (repRectStyle (fromMaybe defaultSvgFrame (initl ^. #legendFrame)))
    lplace' = repPlace (initl ^. #lplace)
    scale' = slider (Just "scale") 0.01 1 0.001 (initl ^. #scale)





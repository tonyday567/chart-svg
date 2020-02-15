{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Chart.Core
  ( pShow',
    renderChartWith,
    renderChart,
    renderChartSvgWith,
    writeChartWith,
    writeChart,
    writeChartSvgWith,
    chartSvg_,
    chartSvg,
    chartSvgWith,
    fittedSvg,
    frame,
    defaultFrame,
    projectTo,
    projectSpots,
    projectSpotsWith,
    dataBox,
    styleBox,
    styleBoxes,
    styleBoxText,
    styleBoxGlyph,
    addChartBox,
    addChartBoxes,
    showOrigin,
    showOriginWith,
    defaultOrigin,
    blue,
    grey,
    red,
    black,
    white,
    blend,
    pixelate,
    boxes,
    scaleAnn,
    pad,
    placedLabel,
    defRect,
    defRectS,
    addToRect,
    p0,
    hori,
    vert,
    stack,
    moveChart,
    fixed,
    commas,
    formatN,
  )
where

import Chart.Svg
import Chart.Types
import Codec.Picture.Types
import Control.Lens hiding (transform)
import Data.Foldable
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Data.Monoid
import Data.Semigroup hiding (getLast)
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import qualified Data.Text.IO as Text
import NumHask.Space
import Prelude
import Text.Pretty.Simple (pShowNoColor)
import Text.Printf

pShow' :: (Show a) => a -> Text
pShow' = toStrict . pShowNoColor

renderChartWith :: ChartSvgStyle -> [Chart Double] -> Text.Text
renderChartWith scfg cs =
  renderChartSvg (scfg ^. #sizex) (scfg ^. #sizey)
    . maybe id pad (scfg ^. #outerPad)
    . maybe id (\x c -> frame x c <> c) (scfg ^. #chartFrame)
    . maybe id pad (scfg ^. #innerPad)
    . chartSvg (aspect (scfg ^. #chartAspect))
    $ cs
      <> maybe mempty (\g -> [showOriginWith g]) (scfg ^. #orig)

renderChart :: [Chart Double] -> Text.Text
renderChart = renderChartWith defaultChartSvgStyle

renderChartSvgWith :: ChartSvgStyle -> (Rect Double -> ChartSvg Double) -> Text.Text
renderChartSvgWith scfg f =
  renderChartSvg (scfg ^. #sizex) (scfg ^. #sizey)
    . maybe id pad (scfg ^. #outerPad)
    . maybe id (\x c -> frame x c <> c) (scfg ^. #chartFrame)
    . maybe id pad (scfg ^. #innerPad)
    $ (f (aspect (scfg ^. #chartAspect))
      <> chartSvg (aspect (scfg ^. #chartAspect))
       (maybe mempty (\g -> [showOriginWith g]) (scfg ^. #orig)))

writeChartWith :: FilePath -> ChartSvgStyle -> [Chart Double] -> IO ()
writeChartWith fp scfg cs = Text.writeFile fp (renderChartWith scfg cs)

writeChartSvgWith :: FilePath -> ChartSvgStyle -> (Rect Double -> ChartSvg Double) -> IO ()
writeChartSvgWith fp scfg cs = Text.writeFile fp (renderChartSvgWith scfg cs)

-- | write a ChartSvg to an svg file.
writeChart :: FilePath -> [Chart Double] -> IO ()
writeChart fp cs = writeChartWith fp defaultChartSvgStyle cs

-- | create a ChartSvg from a [Chart] and a Rect without any scaling
chartSvg_ ::
  (Chartable a) =>
  Rect a ->
  [Chart a] ->
  ChartSvg a
chartSvg_ a cs = ChartSvg a (tree <$> cs)

-- | convert a [Chart] to a ChartSvg, projecting Chart data to the supplied Rect, and expanding the Rect for chart style if necessary
chartSvg ::
  (Chartable a) =>
  Rect a ->
  [Chart a] ->
  ChartSvg a
chartSvg a cs = chartSvg_ (defRect $ styleBoxes cs') cs'
  where
    cs' = projectSpots a cs

-- | convert a [Chart] to a ChartSvg, projecting Chart data from a specified Rect range to the supplied Rect, and expanding the Rect for chart style if necessary
chartSvgWith ::
  (Chartable a) =>
  Rect a ->
  Rect a ->
  [Chart a] ->
  ChartSvg a
chartSvgWith new old cs = chartSvg_ (addToRect new (styleBoxes cs')) cs'
  where
    cs' = projectSpotsWith new old cs

-- | convert a [Chart] to a ChartSvg, setting the Rect equal to the Chart data area
fittedSvg ::
  (Chartable a) =>
  [Chart a] ->
  ChartSvg a
fittedSvg cs =
  chartSvg (defRect $ styleBoxes cs) cs

-- | add an enclosing fitted frame to a ChartSvg
frame :: (Chartable a) => RectStyle -> ChartSvg a -> ChartSvg a
frame o (ChartSvg vb _) =
  ChartSvg
    vb
    ((: []) . tree $ Chart (RectA o) [SpotRect vb])

-- | a default frame
defaultFrame :: (Chartable a) => ChartSvg a -> ChartSvg a
defaultFrame ch = frame (border 0.01 blue 1.0) ch <> ch

p0 :: (Num a) => Spot a
p0 = SP 0 0

-- | project a Spot from one Rect to another, preserving relative position.
projectOn :: (Ord a, Fractional a) => Rect a -> Rect a -> Spot a -> Spot a
projectOn new old@(Rect x z y w) po@(SP px py)
  | x == z && y == w = po
  | x == z = SP px py'
  | y == w = SP px' py
  | otherwise = SP px' py'
  where
    (Point px' py') = project old new (toPoint po)
projectOn new old@(Rect x z y w) ao@(SR ox oz oy ow)
  | x == z && y == w = ao
  | x == z = SR ox oz ny nw
  | y == w = SR nx nz oy ow
  | otherwise = SpotRect a
  where
    a@(Rect nx nz ny nw) = projectRect old new (toRect ao)

-- | project a [Spot a] from it's folded space to the given area
projectTo :: (Ord a, Fractional a) => Rect a -> [Spot a] -> [Spot a]
projectTo _ [] = []
projectTo vb (x : xs) = projectOn vb (toRect $ sconcat (x :| xs)) <$> (x : xs)

-- | project a [[Spot a]] from its folded space to the given area
projectTo2 :: (Ord a, Fractional a) => Rect a -> [[Spot a]] -> [[Spot a]]
projectTo2 vb xss = fmap (maybe id (projectOn vb) (fold $ foldRect . fmap toRect <$> xss)) <$> xss

defRect :: (Fractional a) => Maybe (Rect a) -> Rect a
defRect = fromMaybe unitRect

defRectS :: (Eq a, Fractional a) => Maybe (Rect a) -> Rect a
defRectS r = maybe unitRect singletonUnit r
  where
    singletonUnit :: (Eq a, Fractional a) => Rect a -> Rect a
    singletonUnit (Rect x z y w)
      | x == z && y == w = Rect (x - 0.5) (x + 0.5) (y - 0.5) (y + 0.5)
      | x == z = Rect (x - 0.5) (x + 0.5) y w
      | y == w = Rect x z (y - 0.5) (y + 0.5)
      | otherwise = Rect x z y w

addToRect :: (Ord a) => Rect a -> Maybe (Rect a) -> Rect a
addToRect r r' = sconcat $ r :| maybeToList r'

projectSpots :: (Chartable a) => Rect a -> [Chart a] -> [Chart a]
projectSpots a cs = cs'
  where
    xss = projectTo2 a (spots <$> cs)
    ss = annotation <$> cs
    cs' = zipWith Chart ss xss

projectSpotsWith :: (Chartable a) => Rect a -> Rect a -> [Chart a] -> [Chart a]
projectSpotsWith new old cs = cs'
  where
    xss = fmap (projectOn new old) . spots <$> cs
    ss = annotation <$> cs
    cs' = zipWith Chart ss xss

-- |
dataBox :: Chartable a => [Chart a] -> Maybe (Rect a)
dataBox cs = foldRect . mconcat $ fmap toRect <$> (spots <$> cs)

-- | a Rect that bounds the geometric attributes of a 'DrawAttributes'
-- only handles stroke width and transformations, referencing a point to calculate relative rotation from
daBox :: (Chartable a) => DrawAttributes -> Spot a -> Rect a -> Rect a
daBox da s r = transformRect s da (strokeRect da r)

-- | the geometric dimensions of a Chart inclusive of style geometry
styleBox :: (Real a, Chartable a) => Chart a -> Maybe (Rect a)
styleBox (Chart (TextA s ts) xs) =
  foldRect $
    zipWith
      ( \t x ->
          daBox
            (dagText s)
            x
            (styleBoxText s t (toPoint x))
      )
      ts
      xs
styleBox (Chart (GlyphA s) xs) =
  foldRect $
    ( \x ->
        daBox
          (dagGlyph s)
          x
          (move (toPoint x) (styleBoxGlyph s))
    )
      <$> xs
styleBox (Chart (RectA s) xs) =
  foldRect
    ((\x -> daBox (dagRect s) x (toRect x)) <$> xs)
styleBox (Chart (LineA s) xs) =
  foldRect
    ((\x -> daBox (dagLine s) x (toRect x)) <$> xs)
styleBox (Chart BlankA xs) =
  foldRect
    ((\x -> daBox mempty x (toRect x)) <$> xs)

-- | the extra area from text styling
styleBoxText ::
  (Ord a, Fractional a) =>
  TextStyle ->
  Text.Text ->
  Point a ->
  Rect a
styleBoxText o t p = move p $ realToFrac <$> maybe flat (`rotateRect` flat) (realToFrac <$> o ^. #rotation)
  where
    flat = Rect ((- x' / 2) + x' * a') (x' / 2 + x' * a') ((- y' / 2) - n1') (y' / 2 - n1')
    s = o ^. #size
    h = o ^. #hsize
    v = o ^. #vsize
    n1 = o ^. #nudge1
    x' = s * h * realToFrac (Text.length t)
    y' = s * v
    n1' = s * n1
    a' = case o ^. #anchor of
      AnchorStart -> 0.5
      AnchorEnd -> -0.5
      AnchorMiddle -> 0.0

-- | the extra area from glyph styling
styleBoxGlyph :: (Chartable a) => GlyphStyle -> Rect a
styleBoxGlyph s = realToFrac <$> case sh of
  EllipseGlyph a -> scale (Point sz (a * sz)) unitRect
  RectSharpGlyph a -> scale (Point sz (a * sz)) unitRect
  RectRoundedGlyph a _ _ -> scale (Point sz (a * sz)) unitRect
  VLineGlyph a -> scale (Point (a * sz) sz) unitRect
  HLineGlyph a -> scale (Point sz (a * sz)) unitRect
  TriangleGlyph a b c -> (sz *) <$> sconcat (toRect . SpotPoint <$> (a :| [b, c]) :: NonEmpty (Rect Double))
  _ -> (sz *) <$> unitRect
  where
    sh = s ^. #shape
    sz = s ^. #size

-- | the extra geometric dimensions of a [Chart]
styleBoxes :: (Chartable a) => [Chart a] -> Maybe (Rect a)
styleBoxes xss = foldRect $ catMaybes (styleBox <$> xss)

addChartBox :: (Chartable a) => Chart a -> Rect a -> Rect a
addChartBox c r = sconcat (r :| maybeToList (styleBox c))

addChartBoxes :: (Chartable a) => [Chart a] -> Rect a -> Rect a
addChartBoxes c r = sconcat (r :| maybeToList (styleBoxes c))

-- | include a circle at the origin with size and color
showOriginWith :: (Chartable a) => GlyphStyle -> Chart a
showOriginWith c =
  Chart
    (GlyphA c)
    [p0]

defaultOrigin :: GlyphStyle
defaultOrigin = GlyphStyle 0.05 red 0.5 grey 0 0 CircleGlyph Nothing Nothing

-- | include a red circle at the origin
showOrigin :: (Chartable a) => Chart a
showOrigin = showOriginWith defaultOrigin

-- | interpolate between 2 colors
blend :: Double -> PixelRGB8 -> PixelRGB8 -> PixelRGB8
blend c = mixWithAlpha f (f (0 :: Int))
  where
    f _ x0 x1 = fromIntegral (round (fromIntegral x0 + c * (fromIntegral x1 - fromIntegral x0)) :: Integer)

-- | create pixel data from a function on a Point
pixelate ::
  (Chartable a) =>
  (Point a -> Double) ->
  Rect a ->
  Grid (Rect a) ->
  PixelRGB8 ->
  PixelRGB8 ->
  [(Rect a, PixelRGB8)]
pixelate f r g c0 c1 = (\(x, y) -> (x, blend y c0 c1)) <$> ps'
  where
    ps = gridF f r g
    rs = snd <$> ps
    rs' = project (space1 rs :: Range Double) (Range 0 1) <$> rs
    ps' = zip (fst <$> ps) rs'

-- deconstruct a chart into a chart for every spot
decons :: Chart a -> [Chart a]
decons (Chart (TextA ts txts) spts) = zipWith (\t s -> Chart (TextA ts [t]) [s]) txts spts
decons (Chart ann spts) = (\s -> Chart ann [s]) <$> spts

-- take a chart and produce a RectA chart of all the bounding style boxes of each point
boxes :: (Chartable a) => RectStyle -> [Chart a] -> [Chart a]
boxes rs cs = mconcat $ fmap (Chart (RectA rs) . maybeToList . fmap SpotRect . styleBox) . decons <$> cs

scaleAnn :: Double -> Annotation -> Annotation
scaleAnn x (LineA a) = LineA $ a & #width %~ (* x)
scaleAnn x (RectA a) = RectA $ a & #borderSize %~ (* x)
scaleAnn x (TextA a txs) = TextA (a & #size %~ (* x)) txs
scaleAnn x (GlyphA a) = GlyphA (a & #size %~ (* x))
scaleAnn _ BlankA = BlankA

-- | widen a ChartSvg Rect by a fraction of the size.
pad :: (Chartable a) => a -> ChartSvg a -> ChartSvg a
pad p (ChartSvg vb s) = ChartSvg (fmap (p *) vb) s

placedLabel :: (Chartable a) => Point a -> a -> Text.Text -> Chart a
placedLabel p d t =
  Chart
    ( TextA
        (defaultTextStyle
            & #rotation ?~ realToFrac d
        )
        [t]
    )
    [SpotPoint p]

-- horizontally stack a list of list of charts (proceeding to the right) with a gap between
hori :: Chartable a => a -> [[Chart a]] -> [Chart a]
hori _ [] = []
hori gap cs = foldl step [] cs
  where
    step x a = x <> (a & fmap (#spots %~ fmap (\s -> SP (z x) 0 - SP (origx x) 0 + s)))
    z xs = maybe 0 (\(Rect _ z' _ _) -> z' + gap) (styleBoxes xs)
    origx xs = maybe 0 (\(Rect x' _ _ _) -> x') (styleBoxes xs)

-- vertically stack a list of charts (proceeding upwards)
vert :: Chartable a => a -> [[Chart a]] -> [Chart a]
vert _ [] = []
vert gap cs = foldl step [] cs
  where
    step x a = x <> (a & fmap (#spots %~ fmap (\s -> SP 0 (w x) + s)))
    w xs = maybe 0 (\(Rect _ _ _ w') -> w' + gap) (styleBoxes xs)

-- stack a list of charts horizontally, then vertically
stack :: Chartable a => Int -> a -> [[Chart a]] -> [Chart a]
stack _ _ [] = []
stack n gap cs = vert gap (hori gap <$> group' cs [])
  where
    group' [] acc = reverse acc
    group' x acc = group' (drop n x) (take n x:acc)

moveChart :: Chartable a => Spot a -> [Chart a] -> [Chart a]
moveChart sp cs = fmap (#spots %~ fmap (sp +)) cs

commas :: (RealFrac a, PrintfArg a) => Int -> a -> Text
commas n a
  | a < 1000 = fixed n a
  | otherwise = go (floor a) ""
  where
    go :: Int -> Text -> Text
    go x t
      | x < 0 = "-" <> go (- x) ""
      | x < 1000 = Text.pack (show x) <> t
      | otherwise =
        let (d, m) = divMod x 1000
         in go d ("," <> Text.pack (show m))

fixed :: (PrintfArg a) => Int -> a -> Text
fixed n a = Text.pack $ printf ("%." <> show n <> "f") a

formatN :: (PrintfArg a, RealFrac a, Show a) => FormatN -> a -> Text
formatN (FormatFixed n) x = fixed n x
formatN (FormatComma n) x = commas n x
formatN FormatNone x = Text.pack (show x)

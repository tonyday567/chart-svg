{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | Various common chart patterns.
module Chart.Various
  ( -- * sub-chart patterns
    xify,
    yify,
    addLineX,
    addLineY,
    stdLineChart,
    stdLines,
    lineLegend,
    tsAxes,
    titlesHud,
    gpalette,
    gpaletteStyle,
    blendMidLineStyles,

    -- * chart patterns
    quantileChart,
    digitChart,
    scatterChart,
    histChart,
    quantileHistChart,
    digitSurfaceChart,
    tableChart,
  )
where

import Chart.Bar
import Chart.Chart
import Chart.Style
import Chart.Hud
import Chart.Surface
import Chart.Svg
import Control.Lens
import Data.Bifunctor
import Data.Colour
import Data.Foldable
import Data.FormatN
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Text (Text)
import Data.Time (UTCTime (..))
import NumHask.Prelude (fromList)
import NumHask.Space hiding (singleton)

-- | convert from [a] to [Point a], by adding the index as the x axis
xify :: [Double] -> [Point Double]
xify ys =
  zipWith Point [0 ..] ys

-- | convert from [a] to [Point a], by adding the index as the y axis
yify :: [Double] -> [Point Double]
yify xs =
  zipWith Point xs [0 ..]

-- | add a horizontal line at y
addLineX :: Double -> LineStyle -> [Chart Double] -> [Chart Double]
addLineX y ls cs = cs <> [l]
  where
    l = LineChart ls (fromList [Point lx y, Point ux y])
    (Rect lx ux _ _) = sboxes cs

-- | add a verticle line at x
addLineY :: Double -> LineStyle -> [Chart Double] -> [Chart Double]
addLineY x ls cs = cs <> [zeroLine]
  where
    zeroLine = LineChart ls (fromList [Point x ly, Point x uy])
    (Rect _ _ ly uy) = sboxes cs

-- | interpret a [[Double]] as a series of lines with x coordinates of [0..]
stdLineChart :: Double -> [Colour] -> [[Double]] -> [Chart Double]
stdLineChart w p xss =
  zipWith
    ( \c xs ->
        LineChart
          (defaultLineStyle & #color .~ c & #width .~ w)
          (fromList (xify xs))
    )
    p
    xss

-- | Can of the main palette
stdLines :: Double -> [LineStyle]
stdLines w = (\c -> defaultLineStyle & #color .~ c & #width .~ w) <$> palette1_

-- | Legend template for a line chart.
lineLegend :: Double -> [Text] -> [Colour] -> (LegendOptions, [(Styles, Text)])
lineLegend w rs cs =
  ( defaultLegendOptions
      & #ltext . #size .~ 0.3
      & #lplace .~ PlaceBottom
      & #legendFrame .~ Just (RectStyle 0.02 (palette1 5) (palette1 4)),
    zipWith
      (\a r -> (LineA a, r))
      ((\c -> defaultLineStyle & #color .~ c & #width .~ w) <$> cs)
      rs
  )

-- | Create a hud that has time as the x-axis, based on supplied days, and a rounded yaxis.
tsAxes :: [UTCTime] -> [AxisOptions]
tsAxes ds =
  [ defaultAxisOptions
      & #axisTick . #tstyle .~ TickRound (FormatPrec (Just 3)) 6 TickExtend
      & #place .~ PlaceLeft,
    defaultAxisOptions & #axisTick . #tstyle
      .~ TickPlaced
        ( first fromIntegral
            <$> makeTickDates PosIncludeBoundaries Nothing 8 ds
        )
  ]

-- | common pattern of chart title, x-axis title and y-axis title
titlesHud :: Text -> Text -> Text -> HudOptions
titlesHud t x y =
  defaultHudOptions
    & #hudTitles
    .~ [ defaultTitle t,
         defaultTitle x & #place .~ PlaceBottom & #style . #size .~ 0.08,
         defaultTitle y & #place .~ PlaceLeft & #style . #size .~ 0.08
       ]

-- | GlyphStyle palette
gpaletteStyle :: Double -> [GlyphStyle]
gpaletteStyle s = zipWith (\c g -> defaultGlyphStyle & #size .~ s & #color .~ c & #shape .~ fst g & #borderSize .~ snd g) palette1_ gpalette

-- | Glyph palette
gpalette :: [(GlyphShape, Double)]
gpalette =
  [ (CircleGlyph, 0.01 :: Double),
    (SquareGlyph, 0.01),
    (RectSharpGlyph 0.75, 0.01),
    (RectRoundedGlyph 0.75 0.01 0.01, 0.01),
    (EllipseGlyph 0.75, 0),
    (VLineGlyph 0.005, 0.01),
    (HLineGlyph 0.005, 0.01),
    (TriangleGlyph (Point 0.0 0.0) (Point 1 1) (Point 1 0), 0.01),
    (PathGlyph "M0.05,-0.03660254037844387 A0.1 0.1 0.0 0 1 0.0,0.05 0.1 0.1 0.0 0 1 -0.05,-0.03660254037844387 0.1 0.1 0.0 0 1 0.05,-0.03660254037844387 Z", 0.01)
  ]

-- * charts

-- | Chart template for quantiles.
quantileChart ::
  Text ->
  [Text] ->
  [LineStyle] ->
  [AxisOptions] ->
  [[Double]] ->
  ChartSvg
quantileChart title names ls as xs =
  mempty & #hudOptions .~ hudOptions & #chartTree .~ chart'
  where
    hudOptions =
      defaultHudOptions
        & #hudTitles .~ [defaultTitle title]
        & ( #hudLegend
              .~ Just
                ( defaultLegendOptions
                    & #ltext . #size .~ 0.1
                    & #vgap .~ 0.05
                    & #innerPad .~ 0.2
                    & #lplace .~ PlaceRight,
                  first LineA <$> zip ls names
                )
          )
        & #hudAxes .~ as

    chart' =
      zipWith (\s d -> LineChart s (fromList d))
        ls
        (zipWith Point [0 ..] <$> xs)

-- | /blendMidLineStyle n w/ produces n lines of width w interpolated between two colors.
blendMidLineStyles :: Int -> Double -> (Colour, Colour) -> [LineStyle]
blendMidLineStyles l w (c1, c2) = lo
  where
    m = (fromIntegral l - 1) / 2 :: Double
    cs = (\x -> 1 - abs (fromIntegral x - m) / m) <$> [0 .. (l - 1)]
    bs = (\x -> blend x c1 c2) <$> cs
    lo = (\c -> defaultLineStyle & #width .~ w & #color .~ c) <$> bs

-- | No idea what this is really.
digitChart ::
  Text ->
  [UTCTime] ->
  [Double] ->
  ChartSvg
digitChart title utcs xs =
  mempty & #hudOptions .~ hudOptions & #chartTree .~ [c]
  where
    hudOptions =
      defaultHudOptions
        & #hudTitles .~ [defaultTitle title]
        & #hudAxes .~ tsAxes utcs
    c =
      GlyphChart
            ( defaultGlyphStyle
                & #color .~ Colour 0 0 1 1
                & #shape .~ CircleGlyph
                & #size .~ 0.01
            )
        (fromList (xify xs))

-- | scatter chart
scatterChart ::
  [[Point Double]] ->
  [Chart Double]
scatterChart xss = zipWith (\gs xs -> GlyphChart gs (fromList xs)) (gpaletteStyle 0.02) xss

-- | histogram chart
histChart ::
  Text ->
  Maybe [Text] ->
  Range Double ->
  Int ->
  [Double] ->
  ChartSvg
histChart title names r g xs =
  barChart defaultBarOptions barData
    & (#hudOptions . #hudTitles .~ [defaultTitle title])
  where
    barData = BarData [hr] names Nothing
    hcuts = grid OuterPos r g
    h = fill hcuts xs
    hr =
      (\(Rect x x' _ _) -> (x + x') / 2)
        <$> makeRects (IncludeOvers (NumHask.Space.width r / fromIntegral g)) h

-- | a chart drawing a histogram based on quantile information
quantileHistChart ::
  Text ->
  Maybe [Text] ->
  -- | quantiles
  [Double] ->
  -- | quantile values
  [Double] ->
  ChartSvg
quantileHistChart title names qs vs =
  mempty & #hudOptions .~ hudOptions & #chartTree .~ [chart']
  where
    hudOptions =
      defaultHudOptions
        & #hudTitles
        .~ [defaultTitle title]
        & #hudAxes
        .~ [ maybe
               ( defaultAxisOptions & #axisTick . #tstyle
                   .~ TickRound (FormatPrec (Just 3)) 8 TickExtend
               )
               ( \x ->
                   defaultAxisOptions & #axisTick . #tstyle
                     .~ TickPlaced (zip vs x)
               )
               names
           ]
    chart' = RectChart defaultRectStyle (fromList hr)
    hr =
      zipWith
        (\(y, w) (x, z) -> Rect x z 0 ((w - y) / (z - x)))
        (zip qs (drop 1 qs))
        (zip vs (drop 1 vs))

-- | pixel chart of digitized vs digitized counts
digitSurfaceChart ::
  SurfaceStyle ->
  SurfaceLegendOptions ->
  (Text, Text, Text) ->
  [Text] ->
  [(Int, Int)] ->
  [Chart Double]
digitSurfaceChart pixelStyle plo ts names ps =
  runHud (aspect 1) (hs0 <> hs1) (cs0 <> cs1)
  where
    l = length names
    pts = Point l l
    gr :: Rect Double
    gr = fromIntegral <$> Rect 0 l 0 l
    mapCount = foldl' (\m x -> HashMap.insertWith (+) x 1.0 m) HashMap.empty ps
    f :: Point Double -> Double
    f (Point x y) = fromMaybe 0 $ HashMap.lookup (floor x, floor y) mapCount
    (hs0, cs0) = makeHud gr (qvqHud ts names)
    (cs1, hs1) =
      surfacefl
        f
        (SurfaceOptions pixelStyle pts gr)
        plo

-- style helpers
qvqHud :: (Text, Text, Text) -> [Text] -> HudOptions
qvqHud ts labels =
  defaultHudOptions
    & #hudTitles .~ makeTitles ts
    & #hudAxes
      .~ [ defaultAxisOptions
             & #axisTick . #tstyle .~ TickPlaced (zip ((0.5 +) <$> [0 ..]) labels)
             & #place .~ PlaceLeft,
           defaultAxisOptions
             & #axisTick . #tstyle .~ TickPlaced (zip ((0.5 +) <$> [0 ..]) labels)
             & #place .~ PlaceBottom
         ]

makeTitles :: (Text, Text, Text) -> [Title]
makeTitles (t, xt, yt) =
  reverse
    [ defaultTitle t,
      defaultTitle xt & #place .~ PlaceBottom & #style . #size .~ 0.06,
      defaultTitle yt & #place .~ PlaceLeft & #style . #size .~ 0.06
    ]

-- | Chart for double list of Text.
tableChart :: [[Text]] -> [Chart Double]
tableChart tss = zipWith (\ts x -> TextChart defaultTextStyle (fromList $ zip ts (Point x <$> take (length ts) [0 ..]))) tss [0 ..]

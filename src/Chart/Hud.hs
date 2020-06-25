{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | A hud (heads-up display) are decorations in and around a chart that assist with data interpretation.
module Chart.Hud
  ( runHudWith,
    runHud,
    makeHud,
    freezeTicks,
    flipAxis,
    canvas,
    title,
    tick,
    precision,
    adjustTick,
    makeTickDates,
    makeTickDatesContinuous,
    legendHud,
    legendEntry,
    legendChart,
    legendFromChart,
  )
where

import Chart.Core
import Chart.Format
import Chart.Svg (styleBox, styleBoxText, styleBoxes)
import Chart.Types
import Control.Category (id)
import qualified Control.Foldl as L
import Control.Lens
import Control.Monad.Trans.State.Lazy
import Data.Time
import GHC.Generics
import NumHask.Space
import Protolude

-- | combine huds and charts to form a new Chart using the supplied
-- initial canvas and data dimensions.
-- Note that chart data is transformed by this computation.
-- used once in makePixelTick
runHudWith ::
  -- | initial canvas dimension
  Rect Double ->
  -- | initial data dimension
  Rect Double ->
  -- | huds to add
  [Hud Double] ->
  -- | underlying chart
  [Chart Double] ->
  -- | chart list
  [Chart Double]
runHudWith ca xs hs cs =
  flip evalState (ChartDims ca' da' xs) $
    (unhud $ mconcat hs) cs'
  where
    da' = defRect $ dataBox cs'
    ca' = defRect $ styleBoxes cs'
    cs' = projectSpotsWith ca xs cs

-- | Combine huds and charts to form a new [Chart] using the supplied canvas and the actual data dimension.
-- Note that the original chart data are transformed and irrevocably lost by this computation.
-- used once in renderHudChart
runHud :: Rect Double -> [Hud Double] -> [Chart Double] -> [Chart Double]
runHud ca hs cs = runHudWith ca (defRectS $ dataBox cs) hs cs

-- | Make huds from a HudOptions
-- Some huds, such as the creation of tick values, can extend the data dimension of a chart, so we also return a blank chart with the new data dimension.
-- The complexity internally is due to the creation of ticks and, specifically, gridSensible, which is not idempotent. As a result, a tick calculation that does extends the data area, can then lead to new tick values when applying TickRound etc.
makeHud :: Rect Double -> HudOptions -> ([Hud Double], [Chart Double])
makeHud xs cfg =
  (haxes <> [can] <> titles <> [l], [xsext])
  where
    can = maybe mempty (\x -> canvas x) (cfg ^. #hudCanvas)
    titles = title <$> (cfg ^. #hudTitles)
    newticks = (\a -> freezeTicks (a ^. #place) xs (a ^. #atick . #tstyle)) <$> (cfg ^. #hudAxes)
    axes' = zipWith (\c t -> c & #atick . #tstyle .~ fst t) (cfg ^. #hudAxes) newticks
    xsext = Chart BlankA (SpotRect <$> catMaybes (snd <$> newticks))
    haxes = (\x -> maybe mempty (\a -> bar (x ^. #place) a) (x ^. #abar) <> adjustedTickHud x) <$> axes'
    l = maybe mempty (\(lo, ats) -> legendHud lo (legendChart ats lo)) (cfg ^. #hudLegend)

-- convert TickRound to TickPlaced
freezeTicks :: Place -> Rect Double -> TickStyle -> (TickStyle, Maybe (Rect Double))
freezeTicks pl xs' ts@TickRound {} = maybe (ts, Nothing) (\x -> (TickPlaced (zip ps ls), Just x)) ((\x -> replaceRange pl x xs') <$> ext)
  where
    (TickComponents ps ls ext) = makeTicks ts (placeRange pl xs')
    replaceRange :: Place -> Range Double -> Rect Double -> Rect Double
    replaceRange pl' (Range a0 a1) (Rect x z y w) = case pl' of
      PlaceRight -> Rect x z a0 a1
      PlaceLeft -> Rect x z a0 a1
      _ -> Rect a0 a1 y w
freezeTicks _ _ ts = (ts, Nothing)

flipAxis :: AxisOptions -> AxisOptions
flipAxis ac = case ac ^. #place of
  PlaceBottom -> ac & #place .~ PlaceLeft
  PlaceTop -> ac & #place .~ PlaceRight
  PlaceLeft -> ac & #place .~ PlaceBottom
  PlaceRight -> ac & #place .~ PlaceTop
  PlaceAbsolute _ -> ac

addToRect :: (Ord a) => Rect a -> Maybe (Rect a) -> Rect a
addToRect r r' = sconcat $ r :| maybeToList r'

canvas :: (Monad m) => RectStyle -> HudT m Double
canvas s = Hud $ \cs -> do
  a <- use #canvasDim
  let c = Chart (RectA s) [SpotRect a]
  #canvasDim .= addToRect a (styleBox c)
  pure $ c : cs

bar_ :: Place -> Bar -> Rect Double -> Rect Double -> Chart Double
bar_ pl b (Rect x z y w) (Rect x' z' y' w') =
  case pl of
    PlaceTop ->
      Chart
        (RectA (rstyle b))
        [ SR
            x
            z
            (w' + b ^. #buff)
            (w' + b ^. #buff + b ^. #wid)
        ]
    PlaceBottom ->
      Chart
        (RectA (rstyle b))
        [ SR
            x
            z
            (y' - b ^. #wid - b ^. #buff)
            (y' - b ^. #buff)
        ]
    PlaceLeft ->
      Chart
        (RectA (rstyle b))
        [ SR
            (x' - b ^. #wid - b ^. #buff)
            (x' - b ^. #buff)
            y
            w
        ]
    PlaceRight ->
      Chart
        (RectA (rstyle b))
        [ SR
            (z' + (b ^. #buff))
            (z' + (b ^. #buff) + (b ^. #wid))
            y
            w
        ]
    PlaceAbsolute (Point x'' _) ->
      Chart
        (RectA (rstyle b))
        [ SR
            (x'' + (b ^. #buff))
            (x'' + (b ^. #buff) + (b ^. #wid))
            y
            w
        ]

bar :: (Monad m) => Place -> Bar -> HudT m Double
bar pl b = Hud $ \cs -> do
  da <- use #chartDim
  ca <- use #canvasDim
  let c = bar_ pl b ca da
  #chartDim .= addChartBox c da
  pure $ c : cs

title_ :: Title -> Rect Double -> Chart Double
title_ t a =
  Chart
    ( TextA
        ( style'
            & #translate ?~ (realToFrac <$> (placePos' a + alignPos a))
            & #rotation ?~ rot
        )
        [t ^. #text]
    )
    [SP 0 0]
  where
    style'
      | t ^. #anchor == AnchorStart =
        #anchor .~ AnchorStart $ t ^. #style
      | t ^. #anchor == AnchorEnd =
        #anchor .~ AnchorEnd $ t ^. #style
      | otherwise = t ^. #style
    rot
      | t ^. #place == PlaceRight = 90.0
      | t ^. #place == PlaceLeft = -90.0
      | otherwise = 0
    placePos' (Rect x z y w) = case t ^. #place of
      PlaceTop -> Point ((x + z) / 2.0) (w + (t ^. #buff))
      PlaceBottom ->
        Point
          ((x + z) / 2.0)
          ( y - (t ^. #buff)
              - 0.5
              * realToFrac (t ^. #style . #vsize)
              * realToFrac (t ^. #style . #size)
          )
      PlaceLeft -> Point (x - (t ^. #buff)) ((y + w) / 2.0)
      PlaceRight -> Point (z + (t ^. #buff)) ((y + w) / 2.0)
      PlaceAbsolute p -> p
    alignPos (Rect x z y w)
      | t ^. #anchor == AnchorStart
          && t ^. #place `elem` [PlaceTop, PlaceBottom] =
        Point ((x - z) / 2.0) 0.0
      | t ^. #anchor == AnchorStart
          && t ^. #place == PlaceLeft =
        Point 0.0 ((y - w) / 2.0)
      | t ^. #anchor == AnchorStart
          && t ^. #place == PlaceRight =
        Point 0.0 ((w - y) / 2.0)
      | t ^. #anchor == AnchorEnd
          && t ^. #place `elem` [PlaceTop, PlaceBottom] =
        Point ((- x + z) / 2.0) 0.0
      | t ^. #anchor == AnchorEnd
          && t ^. #place == PlaceLeft =
        Point 0.0 ((- y + w) / 2.0)
      | t ^. #anchor == AnchorEnd
          && t ^. #place == PlaceRight =
        Point 0.0 ((y - w) / 2.0)
      | otherwise = Point 0.0 0.0

-- | Add a title to a chart. The logic used to work out placement is flawed due to being able to freely specify text rotation.  It works for specific rotations (Top, Bottom at 0, Left at 90, Right @ 270)
title :: (Monad m) => Title -> HudT m Double
title t = Hud $ \cs -> do
  ca <- use #chartDim
  let c = title_ t ca
  #chartDim .= addChartBox c ca
  pure $ c : cs

placePos :: Place -> Double -> Rect Double -> Point Double
placePos pl b (Rect x z y w) = case pl of
  PlaceTop -> Point 0 (w + b)
  PlaceBottom -> Point 0 (y - b)
  PlaceLeft -> Point (x - b) 0
  PlaceRight -> Point (z + b) 0
  PlaceAbsolute p -> p

placeRot :: Place -> Maybe Double
placeRot pl = case pl of
  PlaceRight -> Just (-90.0)
  PlaceLeft -> Just (-90.0)
  _ -> Nothing

textPos :: Place -> TextStyle -> Double -> Point Double
textPos pl tt b = case pl of
  PlaceTop -> Point 0 b
  PlaceBottom -> Point 0 (- b - 0.5 * realToFrac (tt ^. #vsize) * realToFrac (tt ^. #size))
  PlaceLeft ->
    Point
      (- b)
      (realToFrac (tt ^. #nudge1) * realToFrac (tt ^. #vsize) * realToFrac (tt ^. #size))
  PlaceRight ->
    Point
      b
      (realToFrac (tt ^. #nudge1) * realToFrac (tt ^. #vsize) * realToFrac (tt ^. #size))
  PlaceAbsolute p -> p

placeRange :: Place -> Rect Double -> Range Double
placeRange pl (Rect x z y w) = case pl of
  PlaceRight -> Range y w
  PlaceLeft -> Range y w
  _ -> Range x z

placeOrigin :: Place -> Double -> Point Double
placeOrigin pl x
  | pl `elem` [PlaceTop, PlaceBottom] = Point x 0
  | otherwise = Point 0 x

placeTextAnchor :: Place -> (TextStyle -> TextStyle)
placeTextAnchor pl
  | pl == PlaceLeft = #anchor .~ AnchorEnd
  | pl == PlaceRight = #anchor .~ AnchorStart
  | otherwise = id

placeGridLines :: Place -> Rect Double -> Double -> Double -> [Spot Double]
placeGridLines pl (Rect x z y w) a b
  | pl `elem` [PlaceTop, PlaceBottom] = [SP a (y - b), SP a (w + b)]
  | otherwise = [SP (x - b) a, SP (z + b) a]

-- | compute tick values and labels given options, ranges and formatting
ticksR :: TickStyle -> Range Double -> Range Double -> [(Double, Text)]
ticksR s d r =
  case s of
    TickNone -> []
    TickRound f n e -> zip (project r d <$> ticks0) (formatNs f ticks0)
      where
        ticks0 = gridSensible OuterPos (e == NoTickExtend) r (fromIntegral n :: Integer)
    TickExact f n -> zip (project r d <$> ticks0) (formatNs f ticks0)
      where
        ticks0 = grid OuterPos r n
    TickLabels ls ->
      zip
        ( project (Range 0 (fromIntegral $ length ls)) d
            <$> ((\x -> x - 0.5) . fromIntegral <$> [1 .. length ls])
        )
        ls
    TickPlaced xs -> zip (project r d . fst <$> xs) (snd <$> xs)

data TickComponents
  = TickComponents
      { positions :: [Double],
        labels :: [Text],
        extension :: Maybe (Range Double)
      }
  deriving (Eq, Show, Generic)

-- | compute tick components given style, ranges and formatting
makeTicks :: TickStyle -> Range Double -> TickComponents
makeTicks s r =
  case s of
    TickNone -> TickComponents [] [] Nothing
    TickRound f n e ->
      TickComponents
        ticks0
        (formatNs f ticks0)
        (bool (Just $ space1 ticks0) Nothing (e == NoTickExtend))
      where
        ticks0 = gridSensible OuterPos (e == NoTickExtend) r (fromIntegral n :: Integer)
    TickExact f n -> TickComponents ticks0 (formatNs f ticks0) Nothing
      where
        ticks0 = grid OuterPos r n
    TickLabels ls ->
      TickComponents
        ( project (Range 0 (fromIntegral $ length ls)) r
            <$> ((\x -> x - 0.5) . fromIntegral <$> [1 .. length ls])
        )
        ls
        Nothing
    TickPlaced xs -> TickComponents (fst <$> xs) (snd <$> xs) Nothing

-- | compute tick values given placement, canvas dimension & data range
ticksPlaced :: TickStyle -> Place -> Rect Double -> Rect Double -> TickComponents
ticksPlaced ts pl d xs = TickComponents (project (placeRange pl xs) (placeRange pl d) <$> ps) ls ext
  where
    (TickComponents ps ls ext) = makeTicks ts (placeRange pl xs)

tickGlyph_ :: Place -> (GlyphStyle, Double) -> TickStyle -> Rect Double -> Rect Double -> Rect Double -> Chart Double
tickGlyph_ pl (g, b) ts ca da xs =
  Chart
    (GlyphA (g & #rotation .~ (realToFrac <$> placeRot pl)))
    ( SpotPoint . (placePos pl b ca +) . placeOrigin pl
        <$> positions
          (ticksPlaced ts pl da xs)
    )

-- | aka marks
tickGlyph ::
  (Monad m) =>
  Place ->
  (GlyphStyle, Double) ->
  TickStyle ->
  HudT m Double
tickGlyph pl (g, b) ts = Hud $ \cs -> do
  a <- use #chartDim
  d <- use #canvasDim
  xs <- use #dataDim
  let c = tickGlyph_ pl (g, b) ts a d xs
  #chartDim .= addToRect a (styleBox c)
  pure $ c : cs

tickText_ ::
  Place ->
  (TextStyle, Double) ->
  TickStyle ->
  Rect Double ->
  Rect Double ->
  Rect Double ->
  [Chart Double]
tickText_ pl (txts, b) ts ca da xs =
  zipWith
    ( \txt sp ->
        Chart
          ( TextA
              (placeTextAnchor pl txts)
              [txt]
          )
          [SpotPoint sp]
    )
    (labels $ ticksPlaced ts pl da xs)
    ( (placePos pl b ca + textPos pl txts b +) . placeOrigin pl
        <$> positions (ticksPlaced ts pl da xs)
    )

-- | aka tick labels
tickText ::
  (Monad m) =>
  Place ->
  (TextStyle, Double) ->
  TickStyle ->
  HudT m Double
tickText pl (txts, b) ts = Hud $ \cs -> do
  ca <- use #chartDim
  da <- use #canvasDim
  xs <- use #dataDim
  let c = tickText_ pl (txts, b) ts ca da xs
  #chartDim .= addChartBoxes c ca
  pure $ c <> cs

-- | aka grid lines
tickLine ::
  (Monad m) =>
  Place ->
  (LineStyle, Double) ->
  TickStyle ->
  HudT m Double
tickLine pl (ls, b) ts = Hud $ \cs -> do
  da <- use #canvasDim
  xs <- use #dataDim
  let c =
        Chart (LineA ls) . (\x -> placeGridLines pl da x b)
          <$> positions (ticksPlaced ts pl da xs)
  #chartDim %= addChartBoxes c
  pure $ c <> cs

-- | Create tick glyphs (marks), lines (grid) and text (labels)
tick ::
  (Monad m) =>
  Place ->
  Tick ->
  HudT m Double
tick pl t =
  maybe mempty (\x -> tickGlyph pl x (t ^. #tstyle)) (t ^. #gtick)
    <> maybe mempty (\x -> tickText pl x (t ^. #tstyle)) (t ^. #ttick)
    <> maybe mempty (\x -> tickLine pl x (t ^. #tstyle)) (t ^. #ltick)
    <> extendData pl t

-- | compute an extension to the Range if a tick went over the data bounding box
computeTickExtension :: TickStyle -> Range Double -> Maybe (Range Double)
computeTickExtension s r =
  case s of
    TickNone -> Nothing
    TickRound _ n e -> bool Nothing (Just (space1 ticks0 <> r)) (e == TickExtend)
      where
        ticks0 = gridSensible OuterPos (e == NoTickExtend) r (fromIntegral n :: Integer)
    TickExact _ _ -> Nothing
    TickLabels _ -> Nothing
    TickPlaced xs -> Just $ r <> space1 (fst <$> xs)

-- | Create a style extension for the data, if ticks extend beyond the existing range
tickExtended ::
  Place ->
  Tick ->
  Rect Double ->
  Rect Double
tickExtended pl t xs =
  maybe
    xs
    (\x -> rangeext xs x)
    (computeTickExtension (t ^. #tstyle) (ranged xs))
  where
    ranged xs' = case pl of
      PlaceTop -> rangex xs'
      PlaceBottom -> rangex xs'
      PlaceLeft -> rangey xs'
      PlaceRight -> rangey xs'
      PlaceAbsolute _ -> rangex xs'
    rangex (Rect x z _ _) = Range x z
    rangey (Rect _ _ y w) = Range y w
    rangeext (Rect x z y w) (Range a0 a1) = case pl of
      PlaceTop -> Rect a0 a1 y w
      PlaceBottom -> Rect a0 a1 y w
      PlaceLeft -> Rect x z a0 a1
      PlaceRight -> Rect x z a0 a1
      PlaceAbsolute _ -> Rect a0 a1 y w

extendData :: (Monad m) => Place -> Tick -> HudT m Double
extendData pl t = Hud $ \cs -> do
  #dataDim %= tickExtended pl t
  pure cs

-- | adjust Tick for sane font sizes etc
adjustTick ::
  Adjustments ->
  Rect Double ->
  Rect Double ->
  Place ->
  Tick ->
  Tick
adjustTick (Adjustments mrx ma mry ad) vb cs pl t
  | pl `elem` [PlaceBottom, PlaceTop] = case ad of
    False -> t & #ttick . _Just . _1 . #size %~ (/ adjustSizeX)
    True ->
      case adjustSizeX > 1 of
        True ->
          ( case pl of
              PlaceBottom -> #ttick . _Just . _1 . #anchor .~ AnchorEnd
              PlaceTop -> #ttick . _Just . _1 . #anchor .~ AnchorStart
              _ -> #ttick . _Just . _1 . #anchor .~ AnchorEnd
          )
            . (#ttick . _Just . _1 . #size %~ (/ adjustSizeA))
            $ (#ttick . _Just . _1 . #rotation ?~ (-45)) t
        False -> (#ttick . _Just . _1 . #size %~ (/ adjustSizeA)) t
  | otherwise = -- pl `elem` [PlaceLeft, PlaceRight]
    (#ttick . _Just . _1 . #size %~ (/ adjustSizeY)) t
  where
    max' [] = 1
    max' xs = maximum xs
    ra (Rect x z y w)
      | pl `elem` [PlaceTop, PlaceBottom] = Range x z
      | otherwise = Range y w
    asp = ra vb
    r = ra cs
    tickl = snd <$> ticksR (t ^. #tstyle) asp r
    maxWidth :: Double
    maxWidth =
      maybe
        1
        ( \tt ->
            max' $
              (\(Rect x z _ _) -> z - x)
                . (\x -> styleBoxText (fst tt) x (Point 0 0)) <$> tickl
        )
        (t ^. #ttick)
    maxHeight =
      maybe
        1
        ( \tt ->
            max' $
              (\(Rect _ _ y w) -> w - y)
                . (\x -> styleBoxText (fst tt) x (Point 0 0)) <$> tickl
        )
        (t ^. #ttick)
    adjustSizeX :: Double
    adjustSizeX = max' [(maxWidth / realToFrac (upper asp - lower asp)) / mrx, 1]
    adjustSizeY = max' [(maxHeight / realToFrac (upper asp - lower asp)) / mry, 1]
    adjustSizeA = max' [(maxHeight / realToFrac (upper asp - lower asp)) / ma, 1]

adjustedTickHud :: (Monad m) => AxisOptions -> HudT m Double
adjustedTickHud c = Hud $ \cs -> do
  vb <- use #chartDim
  xs <- use #dataDim
  let adjTick =
        maybe
          (c ^. #atick)
          (\x -> adjustTick x vb xs (c ^. #place) (c ^. #atick))
          (c ^. #adjust)
  unhud (tick (c ^. #place) adjTick) cs

-- | Convert a UTCTime list into sensible ticks, placed exactly
makeTickDates :: PosDiscontinuous -> Maybe Text -> Int -> [UTCTime] -> [(Int, Text)]
makeTickDates pc fmt n dates =
  lastOnes (\(_, x0) (_, x1) -> x0 == x1) . fst $ placedTimeLabelDiscontinuous pc fmt n dates
  where
    lastOnes :: (a -> a -> Bool) -> [a] -> [a]
    lastOnes _ [] = []
    lastOnes _ [x] = [x]
    lastOnes f (x : xs) = L.fold (L.Fold step (x, []) (\(x0, x1) -> reverse $ x0 : x1)) xs
      where
        step (a0, rs) a1 = if f a0 a1 then (a1, rs) else (a1, a0 : rs)

-- | Convert a UTCTime list into sensible ticks, placed on the (0,1) space
makeTickDatesContinuous :: PosDiscontinuous -> Maybe Text -> Int -> [UTCTime] -> [(Double, Text)]
makeTickDatesContinuous pc fmt n dates = placedTimeLabelContinuous pc fmt n (l,u)
  where
    l = minimum dates
    u = maximum dates

legendHud :: LegendOptions -> [Chart Double] -> Hud Double
legendHud l lcs = Hud $ \cs -> do
  ca <- use #chartDim
  let cs' = cs <> movedleg ca scaledleg
  #chartDim .= defRect (styleBoxes cs')
  pure cs'
  where
    scaledleg =
      (#annotation %~ scaleAnn (realToFrac $ l ^. #lscale))
        . (#spots %~ fmap (fmap (* l ^. #lscale)))
        <$> lcs
    movedleg ca' leg =
      maybe id (moveChart . SpotPoint . placel (l ^. #lplace) ca') (styleBoxes leg) leg
    placel pl (Rect x z y w) (Rect x' z' y' w') =
      case pl of
        PlaceTop -> Point ((x + z) / 2.0) (w + (w' - y') / 2.0)
        PlaceBottom -> Point ((x + z) / 2.0) (y - (w' - y' / 2.0))
        PlaceLeft -> Point (x - (z' - x') / 2.0) ((y + w) / 2.0)
        PlaceRight -> Point (z + (z' - x') / 2.0) ((y + w) / 2.0)
        PlaceAbsolute p -> p

legendEntry ::
  LegendOptions ->
  Annotation ->
  Text ->
  (Chart Double, Chart Double)
legendEntry l a t =
  ( Chart ann sps,
    Chart (TextA (l ^. #ltext & #anchor .~ AnchorStart) [t]) [SP 0 0]
  )
  where
    (ann, sps) = case a of
      RectA rs ->
        ( RectA rs,
          [SR 0 (l ^. #lsize) 0 (l ^. #lsize)]
        )
      PixelA ps ->
        ( PixelA ps,
          [SR 0 (l ^. #lsize) 0 (l ^. #lsize)]
        )
      TextA ts txts ->
        ( TextA (ts & #size .~ realToFrac (l ^. #lsize)) (take 1 txts),
          [SP 0 0]
        )
      GlyphA gs ->
        ( GlyphA (gs & #size .~ realToFrac (l ^. #lsize)),
          [SP (0.5 * l ^. #lsize) (0.33 * l ^. #lsize)]
        )
      LineA ls ->
        ( LineA (ls & #width %~ (/ (realToFrac $ l ^. #lscale))),
          [SP 0 (0.33 * l ^. #lsize), SP (2 * l ^. #lsize) (0.33 * l ^. #lsize)]
        )
      BlankA ->
        ( BlankA,
          [SP 0 0]
        )

legendChart :: [(Annotation, Text)] -> LegendOptions -> [Chart Double]
legendChart lrs l =
  padChart (l ^. #outerPad)
    . maybe id (\x -> frameChart x (l ^. #innerPad)) (l ^. #legendFrame)
    . vert (l ^. #hgap)
    $ (\(a, t) -> hori ((l ^. #vgap) + twidth - gapwidth t) [[t], [a]])
      <$> es
  where
    es = reverse $ uncurry (legendEntry l) <$> lrs
    twidth = maybe 0 (\(Rect _ z _ _) -> z) . foldRect $ catMaybes (styleBox . snd <$> es)
    gapwidth t = maybe 0 (\(Rect _ z _ _) -> z) (styleBox t)

legendFromChart :: [Text] -> [Chart Double] -> [(Annotation, Text)]
legendFromChart = zipWith (\t c -> (c ^. #annotation, t))

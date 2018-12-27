{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

module Chart.Core
  ( Chart(..)
  , Chartable
  , Annotation(..)
  , DrawAttributes(..)
  , rotateChart
  , translateChart
  , RectStyle(..)
  , defaultRectStyle
  , blob
  , clear
  , border
  , TextStyle(..)
  , defaultTextStyle
  , GlyphStyle(..)
  , defaultGlyphStyle
  , GlyphShape(..)
  , LineStyle(..)
  , defaultLineStyle
  , styleBoxText
  , styleBoxGlyph
  , styleBoxDA
  , daRect
  , daText
  , daGlyph
  , daLine
  , styleBox
  , styleBoxes
  , projectWithStyle
  , projectWithStyles
  , showOrigin
  , showOriginWith
  , blue
  , grey
  , transparent
  , red
  , black
  , white
  , blend
  , pixelate
  ) where

import Codec.Picture.Types
import Control.Exception
import Data.Generics.Product (field)
import Graphics.Svg as Svg hiding (Point, toPoint)
import Lens.Micro
import NumHask.Data.Pair
import NumHask.Prelude as P hiding (Group)
import qualified Data.Text as Text
import Data.List (zipWith3)
import Chart.Spot

-- * Chart
-- | A `Chart` consists of
-- - a list of spots on the xy-plane
-- - svg draw attributes for the chart
-- - specific style of representation for each spot (an Annotation)
data Chart a = Chart
  { annotation :: Annotation
  , drawatts :: DrawAttributes
  , spots :: [Spot a]
  } deriving (Eq, Show, Generic)

-- | the aspects a number needs to be to form the data for a chart
type Chartable a =
  ( ToRatio a
  , FromRatio a
  , Subtractive a
  , Field a
  , BoundedJoinSemiLattice a
  , BoundedMeetSemiLattice a)

-- | a piece of chart structure
-- | The use of field @"rowName" with these Annotation collection doesn't seem to mesh well with polymorphism, so a switch to concrete types (which fit it with svg-tree methods) occurs at this layer, and the underlying data structure is a lot of Doubles
data Annotation
  = RectA RectStyle
  | TextA TextStyle [Text.Text]
  | GlyphA GlyphStyle
  | LineA LineStyle
  deriving (Eq, Show, Generic)

-- * transformations
-- | rotate a Chart by x degrees. This does not touch the underlying data but instead adds a draw attribute to the styling.
-- Multiple rotations will expand the bounding box conservatively.
rotateChart :: (ToRatio a) => a -> Chart a -> Chart a
rotateChart r c = c & field @"drawatts" %~ (<> rot (fromRational r))
  where
    rot r' = mempty & transform .~ Just [Rotate r' Nothing]

-- | translate a Chart by a Point
translateChart :: (ToRatio a) => Pair a -> Chart a -> Chart a
translateChart p c =
  c &
  field @"drawatts" %~
  (<> (mempty & transform .~ Just [Translate x (-y)]))
  where
   (Pair x y) = fromRational <$> p 

-- | Rectangle styling
data RectStyle = RectStyle
  { borderSize :: Double
  , borderColor :: PixelRGBA8
  , borderOpacity :: Double
  , color :: PixelRGBA8
  , opacity :: Double
  } deriving (Show, Eq, Generic)

-- | the official style
defaultRectStyle :: RectStyle
defaultRectStyle = RectStyle 0.005 grey 0.5 blue 0.5

daRect :: RectStyle -> DrawAttributes
daRect o =
  mempty &
  (strokeWidth .~ Last (Just $ Num (fromRational $ o ^. field @"borderSize"))) .
  (strokeColor .~ Last (Just $ ColorRef (o ^. field @"borderColor"))) .
  (strokeOpacity .~ Just (fromRational $ o ^. field @"borderOpacity")) .
  (fillColor .~ Last (Just $ ColorRef (o ^. field @"color"))) .
  (fillOpacity .~ Just (fromRational $ o ^. field @"opacity")) 

-- | solid rectangle, no border
blob :: PixelRGBA8 -> Double -> RectStyle
blob = RectStyle zero transparent zero

-- | clear and utrans rect
clear :: RectStyle
clear = RectStyle zero transparent zero transparent zero

-- | transparent rectangle, with border
border :: Double -> PixelRGBA8 -> Double -> RectStyle
border s c o = RectStyle s c o transparent zero

-- | Text styling
data TextStyle = TextStyle
  { size :: Double
  , color :: PixelRGBA8
  , opacity :: Double
  , alignH :: TextAnchor 
  , hsize :: Double
  , vsize :: Double
  , nudge1 :: Double
  } deriving (Show, Eq, Generic)

-- | the offical text style
defaultTextStyle :: TextStyle
defaultTextStyle =
  TextStyle 0.08 grey 1.0 TextAnchorMiddle 0.45 1.1 (-0.25)

daText :: TextStyle -> DrawAttributes
daText o =
  mempty &
  (fontSize .~ Last (Just $ Num (o ^. field @"size"))) .
  (strokeWidth .~ Last (Just $ Num 0)) .
  (strokeColor .~ Last (Just FillNone)) .
  (fillColor .~ Last (Just $ ColorRef (o ^. field @"color"))) .
  (fillOpacity .~ Just (fromRational $ o ^. field @"opacity")) .
  (textAnchor .~ Last (Just (o ^. field @"alignH")))

-- | the extra area from text styling
styleBoxText :: (FromRatio a) =>
  TextStyle -> DrawAttributes -> Text.Text -> Area a
styleBoxText o das t = fromRational <$>
    Area ((-x'/two) + x'*origx) (x'/two + x'*origx) ((-y'/two) - n1') (y'/two - n1')
    where
      das' = das <> daText o
      s = case getLast (das' ^. fontSize) of
        Just (Num n) -> fromRational n
        _ -> 0.0
      h = o ^. field @"hsize"
      v = o ^. field @"vsize"
      n1 = o ^. field @"nudge1"
      x' = s * h * fromRational (Text.length t)
      y' = s * v
      n1' = s * n1
      origx = case das' ^. textAnchor of
        Last (Just TextAnchorStart) -> 0.5
        Last (Just TextAnchorEnd) -> -0.5
        Last (Just TextAnchorMiddle) -> 0.0
        _ -> 0.0

-- | Glyph styling
data GlyphStyle = GlyphStyle
  { size :: Double -- ^ glyph radius
  , color :: PixelRGBA8 -- ^ fill color
  , opacity :: Double
  , borderColor :: PixelRGBA8 -- ^ stroke color
  , borderOpacity :: Double
  , borderSize :: Double -- ^ stroke width (adds a bit to the bounding box)
  , shape :: GlyphShape
  } deriving (Show, Eq, Generic)

-- | the offical circle style
defaultGlyphStyle :: GlyphStyle
defaultGlyphStyle = GlyphStyle 0.03 blue 0.3 grey 0.3 0.015 CircleGlyph

-- | glyph shapes
data GlyphShape
  = CircleGlyph
  | SquareGlyph
  | EllipseGlyph Double
  | RectSharpGlyph Double
  | RectRoundedGlyph Double Double Double
  | VLineGlyph Double
  | HLineGlyph Double
  | SmileyGlyph
  deriving (Show, Eq, Generic)

daGlyph :: GlyphStyle -> DrawAttributes
daGlyph o =
  mempty &
  (strokeWidth .~ Last (Just $ Num (o ^. field @"borderSize"))) .
  (strokeColor .~ Last (Just $ ColorRef (o ^. field @"borderColor"))) .
  (strokeOpacity .~ Just (fromRational $ o ^. field @"borderOpacity")) .
  (fillColor .~ Last (Just $ ColorRef (o ^. field @"color"))) .
  (fillOpacity .~ Just (fromRational $ o ^. field @"opacity"))

-- | the extra area from glyph styling
styleBoxGlyph :: (Chartable a) => GlyphStyle -> Area a
styleBoxGlyph s = fromRational <$> case sh of
  EllipseGlyph a -> scale (Point sz (a*sz)) one
  RectSharpGlyph a -> scale (Point sz (a*sz)) one
  RectRoundedGlyph a _ _ -> scale (Point sz (a*sz)) one
  VLineGlyph a -> scale (Point (a*sz) sz) one
  HLineGlyph a -> scale (Point sz (a*sz)) one
  _ -> (sz*) <$> one
  where
    sh = s ^. field @"shape" 
    sz = s ^. field @"size"

-- | line style
data LineStyle = LineStyle
  { width :: Double
  , color :: PixelRGBA8
  , opacity :: Double
  } deriving (Show, Eq, Generic)

-- | the official default line style
defaultLineStyle :: LineStyle
defaultLineStyle = LineStyle 0.02 blue 0.5

daLine :: LineStyle -> DrawAttributes
daLine o =
  mempty &
  (strokeWidth .~ Last (Just $ Num (o ^. field @"width"))) .
  (strokeColor .~ Last (Just $ ColorRef (o ^. field @"color"))) .
  (strokeOpacity .~ Just (fromRational $ o ^. field @"opacity")) .
  (fillColor .~ Last (Just FillNone))

-- | the extra area from the stroke element of an svg style attribute
styleBoxStroke :: (FromRatio a) => DrawAttributes -> Area a
styleBoxStroke das = fromRational <$> Area (-x/2) (x/2) (-x/2) (x/2)
  where
    x = case das ^. Svg.strokeWidth & getLast of
      Just (Num x') -> x'
      _ -> 0

-- | the extra geometric dimensions of a 'DrawAttributes'
-- only handles stroke width and transformations
styleBoxDA :: (ToRatio a, FromRatio a, Subtractive a) => DrawAttributes -> Area a -> Area a
styleBoxDA da r = fromRational <$> r' where
  r' = foldr tr (fromRational <$> styleBoxStroke da + r)
    (da ^. transform & maybe [] identity)
  tr a x = case a of
    Translate x' y' -> translateArea (Point x' (-y')) x
    TransformMatrix{} ->
      throw (NumHaskException "TransformMatrix transformation not yet implemented")
    Scale s Nothing -> (s*) <$> x
    Scale sx (Just sy) -> scale (Point sx sy) x
    Rotate d Nothing -> rotateArea d x
    Rotate d (Just (x',y')) -> rotateArea d (translateArea (Point x' y') x)
    SkewX _ -> throw (NumHaskException "SkewX transformation not yet implemented")
    SkewY _ -> throw (NumHaskException "SkewY transformation not yet implemented")
    TransformUnknown -> x

-- | the extra geometric dimensions of a Chart (from both style and draw attributes)
styleBox :: (Chartable a) => Chart a -> Area a
styleBox (Chart (TextA s ts) das xs) = fold $ zipWith (\t x ->
  (styleBoxDA (das <> daText s) . translateArea (toPoint x) $ styleBoxText s das t)) ts xs
styleBox (Chart (GlyphA s) das xs) = fold
  (styleBoxDA (das <> daGlyph s) . flip translateArea (styleBoxGlyph s) . toPoint <$> xs)
styleBox (Chart (RectA s) das xs) = fold
  (styleBoxDA (das <> daRect s) . toArea <$> xs)
styleBox (Chart (LineA s) das xs) = fold
  (styleBoxDA (das <> daLine s) . toArea <$> xs)

-- | the extra geometric dimensions of a [Chart]
styleBoxes :: (Chartable a) => [Chart a] -> Area a
styleBoxes xss = fold $ styleBox <$> xss

-- | project data to a ViewBox based on style effects
projectWithStyle :: (Chartable a) =>
  Area a -> Chart a -> Chart a
projectWithStyle vb ch@(Chart s das xs) =
  Chart s das (projectOn vb (styleBox ch) <$> xs)

-- | project data to a ViewBox based on style effects
projectWithStyles :: (Chartable a) =>
  Area a -> [Chart a] -> [Chart a]
projectWithStyles vb chs =
  zipWith3 Chart ss dass (fmap (projectOn vb (styleBoxes chs)) <$> xss)
  where
    ss = (\(Chart s _ _) -> s) <$> chs
    dass = (\(Chart _ s _) -> s) <$> chs
    xss = (\(Chart _ _ xs) -> xs) <$> chs

-- | include a circle at the origin with size and color
showOriginWith :: forall a. (Chartable a) => Double -> PixelRGBA8 -> Chart a
showOriginWith s c =
  Chart
  (GlyphA $
    field @"borderSize" .~ 0.0 $
    field @"size" .~ s $
    field @"color" .~ c $
    defaultGlyphStyle)
  mempty
  [SP zero zero]

-- | include a red circle at the origin
showOrigin :: (Chartable a) => Chart a
showOrigin = showOriginWith 0.1 red

-- * color
-- | the official chart-unit blue
blue :: PixelRGBA8
blue = PixelRGBA8 93 165 218 255

-- | the official chart-unit grey
grey :: PixelRGBA8
grey = PixelRGBA8 102 102 102 255

-- | transparent
transparent :: PixelRGBA8
transparent = PixelRGBA8 0 0 0 0

-- | black
black :: PixelRGBA8
black = PixelRGBA8 0 0 0 255

-- | white
white :: PixelRGBA8
white = PixelRGBA8 255 255 255 255

-- | red
red :: PixelRGBA8
red = PixelRGBA8 255 0 0 255

-- | interpolate between two colors
blend :: Double -> PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8
blend c = mixWithAlpha f (f 0) where
  f _ x0 x1 = fromIntegral (round (fromIntegral x0 + c * (fromIntegral x1 - fromIntegral x0)) :: Integer)

-- | create pixel data from a function on a Point
pixelate :: (Lattice a, Field a, Subtractive a, FromInteger a) =>
  (Point a -> Double) -> Area a -> Point Int -> PixelRGBA8 -> PixelRGBA8 -> [(Area a, PixelRGBA8)]
pixelate f r g c0 c1 = (\(x,y) -> (x, blend y c0 c1)) <$> ps'
  where
    ps = areaF f r g
    rs = snd <$> ps
    rs' = project (space1 rs :: Range Double) (Range 0 1) <$> rs
    ps' = zip (fst <$> ps) rs'

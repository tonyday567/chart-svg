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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

module Chart.Core
  ( Chart(..)
  , Chartable
  , Annotation(..)
  , annotationText
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
  , Anchor(..)
  , fromAnchor
  , toAnchor
  , toTextAnchor
  , GlyphStyle(..)
  , defaultGlyphStyle
  , GlyphShape(..)
  , toGlyph
  , fromGlyph
  , LineStyle(..)
  , defaultLineStyle
  , styleBoxText
  , styleBoxGlyph
  , styleBoxDA
  , daRect
  , daText
  , daGlyph
  , daLine
  , dataBox
  , styleBox
  , addChartBox
  , styleBoxes
  , addChartBoxes
  , projectWithStyle
  , showOrigin
  , showOriginWith
  , defaultOrigin
  , blue
  , grey
  , red
  , black
  , white
  , blend
  , pixelate
  , boxes
  , scaleAnn
  ) where

import Codec.Picture.Types
import Control.Exception
import Data.Generics.Labels ()
import Graphics.Svg as Svg hiding (Point, toPoint, Text)
import Control.Lens hiding (transform)
import NumHask.Point
import NumHask.Range
import NumHask.Rect
import NumHask.Space
import qualified Data.Text as Text
import Chart.Spot
import Protolude
import GHC.Exts
import Control.Category (id)

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
  ( Real a, Fractional a, Spaceable a, RealFrac a, RealFloat a)

-- | a piece of chart structure
-- | The use of #rowName with these Annotation collection doesn't seem to mesh well with polymorphism, so a switch to concrete types (which fit it with svg-tree methods) occurs at this layer, and the underlying data structure is a lot of Doubles
data Annotation
  = RectA RectStyle
  | TextA TextStyle [Text.Text]
  | GlyphA GlyphStyle
  | LineA LineStyle
  | BlankA
  deriving (Eq, Show, Generic)

annotationText :: Annotation -> Text
annotationText (RectA _) = "RectA"
annotationText TextA{} = "TextA"
annotationText (GlyphA _) = "GlyphA"
annotationText (LineA _) = "LineA"
annotationText BlankA = "BlankA"

-- * transformations
-- | rotate a Chart by x degrees. This does not touch the underlying data but instead adds a draw attribute to the styling.
-- Multiple rotations will expand the bounding box conservatively.
rotateChart :: (Real a) => a -> Chart a -> Chart a
rotateChart r c = c & #drawatts %~ (<> rot (realToFrac r))
  where
    rot r' = mempty & transform ?~ [Rotate r' Nothing]

-- | translate a Chart by a Point
translateChart :: (Real a) => Point a -> Chart a -> Chart a
translateChart p c =
  c &
  #drawatts %~
  (<> (mempty & transform ?~ [Translate x (-y)]))
  where
   (Point x y) = realToFrac <$> p

-- | Rectangle styling
data RectStyle = RectStyle
  { borderSize :: Double
  , borderColor :: PixelRGB8
  , borderOpacity :: Double
  , color :: PixelRGB8
  , opacity :: Double
  } deriving (Show, Eq, Generic)

-- | the official style
defaultRectStyle :: RectStyle
defaultRectStyle = RectStyle 0.005 grey 0.5 red 0.5

daRect :: RectStyle -> DrawAttributes
daRect o =
  mempty &
  (strokeWidth .~ Last (Just $ Num (realToFrac $ o ^. #borderSize))) .
  (strokeColor .~ Last (Just $ ColorRef (promotePixel $ o ^. #borderColor))) .
  (strokeOpacity ?~ realToFrac (o ^. #borderOpacity)) .
  (fillColor .~ Last (Just $ ColorRef (promotePixel $ o ^. #color))) .
  (fillOpacity ?~ realToFrac (o ^. #opacity)) 

-- | solid rectangle, no border
blob :: PixelRGB8 -> Double -> RectStyle
blob = RectStyle 0 black 0

-- | clear and utrans rect
clear :: RectStyle
clear = RectStyle 0 black 0 black 0

-- | transparent rectangle, with border
border :: Double -> PixelRGB8 -> Double -> RectStyle
border s c o = RectStyle s c o black 0

-- | Text styling
data TextStyle = TextStyle
  { size :: Double
  , color :: PixelRGB8
  , opacity :: Double
  , anchor :: Anchor
  , hsize :: Double
  , vsize :: Double
  , nudge1 :: Double
  , rotation :: Maybe Double
  } deriving (Show, Eq, Generic)

data Anchor = AnchorMiddle | AnchorStart | AnchorEnd deriving (Eq, Show, Generic)

fromAnchor :: (IsString s) => Anchor -> s
fromAnchor AnchorMiddle = "Middle"
fromAnchor AnchorStart = "Start"
fromAnchor AnchorEnd = "End"

toAnchor :: (Eq s, IsString s) => s -> Anchor
toAnchor "Middle" = AnchorMiddle
toAnchor "Start" = AnchorStart
toAnchor "End" = AnchorEnd
toAnchor _ = AnchorMiddle

toTextAnchor :: Anchor -> TextAnchor
toTextAnchor AnchorMiddle = TextAnchorMiddle
toTextAnchor AnchorStart = TextAnchorStart
toTextAnchor AnchorEnd = TextAnchorEnd

-- | the offical text style
defaultTextStyle :: TextStyle
defaultTextStyle =
  TextStyle 0.08 grey 1.0 AnchorMiddle 0.5 1.45 (-0.4) Nothing

-- | doesn't include the rotation text style which needs to be specified in the same layer as the placement.
daText :: TextStyle -> DrawAttributes
daText o =
  mempty &
  (fontSize .~ Last (Just $ Num (o ^. #size))) .
  (strokeWidth .~ Last (Just $ Num 0)) .
  (strokeColor .~ Last (Just FillNone)) .
  (fillColor .~ Last (Just $ ColorRef (promotePixel $ o ^. #color))) .
  (fillOpacity ?~ realToFrac (o ^. #opacity)) .
  (textAnchor .~ Last (Just (toTextAnchor $ o ^. #anchor)))
  -- maybe identity (\x -> transform .~ Just [Rotate x Nothing]) (o ^. #rotation)

-- | the extra area from text styling
styleBoxText :: (Fractional a) =>
  TextStyle -> DrawAttributes -> Text.Text -> Rect a
styleBoxText o das t = realToFrac <$> maybe flat (`rotateRect` flat) (realToFrac <$> o ^. #rotation)
    where
      flat = Rect ((-x'/2) + x'*origx) (x'/2 + x'*origx) ((-y'/2) - n1') (y'/2 - n1')
      das' = das <> daText o
      s = case getLast (das' ^. fontSize) of
        Just (Num n) -> realToFrac n
        _ -> 0.0
      h = o ^. #hsize
      v = o ^. #vsize
      n1 = o ^. #nudge1
      x' = s * h * realToFrac (Text.length t)
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
  , color :: PixelRGB8 -- ^ fill color
  , opacity :: Double
  , borderColor :: PixelRGB8 -- ^ stroke color
  , borderOpacity :: Double
  , borderSize :: Double -- ^ stroke width (adds a bit to the bounding box)
  , shape :: GlyphShape
  } deriving (Show, Eq, Generic)

-- | the offical circle style
defaultGlyphStyle :: GlyphStyle
defaultGlyphStyle =
  GlyphStyle 0.03 (PixelRGB8 217 151 33) 0.8 (PixelRGB8 44 66 157) 0.4 0.003
  SquareGlyph

-- | glyph shapes
data GlyphShape
  = CircleGlyph
  | SquareGlyph
  | EllipseGlyph Double
  | RectSharpGlyph Double
  | RectRoundedGlyph Double Double Double
  | TriangleGlyph (Point Double) (Point Double) (Point Double)
  | VLineGlyph Double
  | HLineGlyph Double
  | SmileyGlyph
  deriving (Show, Eq, Generic)

toGlyph :: Text -> GlyphShape
toGlyph sh =
  case sh of
    "Circle" -> CircleGlyph
    "Square" -> SquareGlyph
    "Triangle" -> TriangleGlyph (Point (-1) 0) (Point 1 0) (Point 0 1)
    "Ellipse" -> EllipseGlyph 1.5
    "Rectangle" -> RectSharpGlyph 1.5
    "Rounded Rectangle" -> RectRoundedGlyph 1.5 0.1 0.1
    "Verticle Line" -> VLineGlyph 0.01
    "Horizontal Line" -> HLineGlyph 0.01
    "Smiley Face" -> SmileyGlyph
    _ -> CircleGlyph

fromGlyph :: GlyphShape -> Text
fromGlyph sh =
  case sh of
    CircleGlyph -> "Circle"
    SquareGlyph -> "Square"
    TriangleGlyph {} -> "Triangle"
    EllipseGlyph _ -> "Ellipse"
    RectSharpGlyph _ -> "RectSharp"
    RectRoundedGlyph {} -> "RectRounded"
    VLineGlyph _ -> "VLine"
    HLineGlyph _ -> "HLine"
    SmileyGlyph -> "Smiley"

daGlyph :: GlyphStyle -> DrawAttributes
daGlyph o =
  mempty &
  (strokeWidth .~ Last (Just $ Num (o ^. #borderSize))) .
  (strokeColor .~ Last (Just $ ColorRef (promotePixel $ o ^. #borderColor))) .
  (strokeOpacity ?~ realToFrac (o ^. #borderOpacity)) .
  (fillColor .~ Last (Just $ ColorRef (promotePixel $ o ^. #color))) .
  (fillOpacity ?~ realToFrac (o ^. #opacity))

-- | the extra area from glyph styling
styleBoxGlyph :: (Chartable a) => GlyphStyle -> Rect a
styleBoxGlyph s = realToFrac <$> case sh of
  EllipseGlyph a -> scale (Point sz (a*sz)) unitRect
  RectSharpGlyph a -> scale (Point sz (a*sz)) unitRect
  RectRoundedGlyph a _ _ -> scale (Point sz (a*sz)) unitRect
  VLineGlyph a -> scale (Point (a*sz) sz) unitRect
  HLineGlyph a -> scale (Point sz (a*sz)) unitRect
  TriangleGlyph a b c -> (sz*) <$> sconcat (toRect . SpotPoint <$> (a :| [b,c]) :: NonEmpty (Rect Double))
  _ -> (sz*) <$> unitRect
  where
    sh = s ^. #shape 
    sz = s ^. #size

-- | line style
data LineStyle = LineStyle
  { width :: Double
  , color :: PixelRGB8
  , opacity :: Double
  } deriving (Show, Eq, Generic)

-- | the official default line style
defaultLineStyle :: LineStyle
defaultLineStyle = LineStyle 0.02 blue 0.5

daLine :: LineStyle -> DrawAttributes
daLine o =
  mempty &
  (strokeWidth .~ Last (Just $ Num (o ^. #width))) .
  (strokeColor .~ Last (Just $ ColorRef (promotePixel $ o ^. #color))) .
  (strokeOpacity ?~ realToFrac (o ^. #opacity)) .
  (fillColor .~ Last (Just FillNone))

-- | the extra area from the stroke element of an svg style attribute
styleBoxStroke :: (Fractional a) => DrawAttributes -> Rect a
styleBoxStroke das = realToFrac <$> Rect (-x/2) (x/2) (-x/2) (x/2)
  where
    x = case das ^. Svg.strokeWidth & getLast of
      Just (Num x') -> x'
      _ -> 0

data ChartException = NotYetImplementedException deriving Show

instance Exception ChartException

-- | the extra geometric dimensions of a 'DrawAttributes'
-- only handles stroke width and transformations
styleBoxDA :: (Fractional a, Real a) => DrawAttributes -> Rect a -> Rect a
styleBoxDA da r = realToFrac <$> r' where
  r' = foldr tr (realToFrac <$> styleBoxStroke da `addRect` r)
    (da ^. transform & maybe [] identity)
  tr a x = case a of
    Translate x' y' -> translateRect (Point x' (-y')) x
    TransformMatrix{} ->
      throw NotYetImplementedException
    Scale s Nothing -> (s*) <$> x
    Scale sx (Just sy) -> scale (Point sx sy) x
    Rotate d Nothing -> rotateRect d x
    Rotate d (Just (x',y')) -> rotateRect d (translateRect (Point x' y') x)
    SkewX _ -> throw NotYetImplementedException
    SkewY _ -> throw NotYetImplementedException
    TransformUnknown -> x

-- |
dataBox :: Chartable a => [Chart a] -> Maybe (Rect a)
dataBox cs = foldRect $ mconcat $ fmap toRect <$> (spots <$> cs)

-- | the extra geometric dimensions of a Chart (from both style and draw attributes)
styleBox :: (Chartable a) => Chart a -> Maybe (Rect a)
styleBox (Chart (TextA s ts) das xs) = foldRect $ zipWith (\t x ->
  styleBoxDA (das <> daText s) . translateRect (toPoint x) $ styleBoxText s das t) ts xs
styleBox (Chart (GlyphA s) das xs) = foldRect
  (styleBoxDA (das <> daGlyph s) . flip translateRect (styleBoxGlyph s) . toPoint <$> xs)
styleBox (Chart (RectA s) das xs) = foldRect
  (styleBoxDA (das <> daRect s) . toRect <$> xs)
styleBox (Chart (LineA s) das xs) = foldRect
  (styleBoxDA (das <> daLine s) . toRect <$> xs)
styleBox (Chart BlankA das xs) = foldRect
  (styleBoxDA das . toRect <$> xs)

addChartBox :: (Chartable a) => Chart a -> Rect a -> Rect a
addChartBox c r = sconcat (r :| maybeToList (styleBox c))

-- | the extra geometric dimensions of a [Chart]
styleBoxes :: (Chartable a) => [Chart a] -> Maybe (Rect a)
styleBoxes xss = foldRect $ catMaybes (styleBox <$> xss)

addChartBoxes :: (Chartable a) => [Chart a] -> Rect a -> Rect a
addChartBoxes c r = sconcat (r :| maybeToList (styleBoxes c))

-- | project data to a box based on style effects
projectWithStyle :: (Chartable a) =>
  Rect a -> Chart a -> Chart a
projectWithStyle vb ch@(Chart s das xs) =
  Chart s das (maybe id (projectOn vb) (styleBox ch) <$> xs)

-- | include a circle at the origin with size and color
showOriginWith :: forall a. (Chartable a) => GlyphStyle -> Chart a
showOriginWith c =
  Chart
  (GlyphA c)
  mempty
  [SP 0 0]

defaultOrigin :: GlyphStyle
defaultOrigin = GlyphStyle 0.05 red 0.5 grey 0 0 CircleGlyph

-- | include a red circle at the origin
showOrigin :: (Chartable a) => Chart a
showOrigin = showOriginWith defaultOrigin

-- * color
-- | the official chart-unit blue
blue :: PixelRGB8
blue = PixelRGB8 93 165 218

-- | the official chart-unit grey
grey :: PixelRGB8
grey = PixelRGB8 102 102 102

-- | black
black :: PixelRGB8
black = PixelRGB8 0 0 0

-- | white
white :: PixelRGB8
white = PixelRGB8 255 255 255

-- | red
red :: PixelRGB8
red = PixelRGB8 255 0 0

-- | interpolate between 2 colors
blend :: Double -> PixelRGB8 -> PixelRGB8 -> PixelRGB8
blend c = mixWithAlpha f (f 0) where
  f _ x0 x1 = fromIntegral (round (fromIntegral x0 + c * (fromIntegral x1 - fromIntegral x0)) :: Integer)

-- | create pixel data from a function on a Point
pixelate :: (Chartable a) =>
  (Point a -> Double) -> Rect a -> Grid (Rect a) -> PixelRGB8 -> PixelRGB8 -> [(Rect a, PixelRGB8)]
pixelate f r g c0 c1 = (\(x,y) -> (x, blend y c0 c1)) <$> ps'
  where
    ps = areaF f r g
    rs = snd <$> ps
    rs' = project (space1 rs :: Range Double) (Range 0 1) <$> rs
    ps' = zip (fst <$> ps) rs'


-- deconstruct a chart into a chart for every spot
decons :: Chart a -> [Chart a]
decons (Chart (TextA ts txts) das spts) = zipWith (\t s -> Chart (TextA ts [t]) das [s]) txts spts
decons (Chart ann das spts) = (\s -> Chart ann das [s]) <$> spts

-- take a chart and produce a RectA chart of all the bounding style boxes of each point
boxes :: (Chartable a) => RectStyle -> [Chart a] -> [Chart a]
boxes rs cs = mconcat $ fmap (Chart (RectA rs) mempty . maybeToList . fmap SpotRect . styleBox) . decons <$> cs

scaleAnn :: Double -> Annotation -> Annotation
scaleAnn x (LineA a) = LineA $ a & #width %~ (*x)
scaleAnn x (RectA a) = RectA $ a & #borderSize %~ (*x)
scaleAnn x (TextA a txs) = TextA (a & #size %~ (*x)) txs
scaleAnn x (GlyphA a) = GlyphA (a & #size %~ (*x))
scaleAnn _ BlankA = BlankA

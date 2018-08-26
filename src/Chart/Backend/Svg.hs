{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RebindableSyntax #-}

module Chart.Backend.Svg where

-- import qualified Text.Blaze as B
import qualified Data.Text as Text
-- import qualified Data.Text.Lazy.IO as Lazy
import NumHask.Prelude as P hiding (rotate, singleton, Group)
-- import qualified Data.Colour.SRGB as C
-- import Control.Monad.State.Lazy
import Graphics.Svg.Types as Svg
import Graphics.Svg as Svg
import NumHask.Rect
import NumHask.Pair
import NumHask.Range
import NumHask.Space
import qualified Data.Map as Map
import Lens.Micro
import Codec.Picture.Types
import Data.Generics.Product (field)
import Data.Generics.Sum
import Linear.V2
-- import Data.Colour
import Control.Exception

-- * Chart
-- | the aspect of a chart expressed as a ratio of x-plane : y-plane.
newtype ViewBox a = ViewBox { vbRect :: Rect a } deriving (Show, Eq, Semigroup)

aspect :: (CanRange a) => a -> ViewBox a
aspect a = ViewBox $ Ranges ((a *) <$> one) one

data ChartSvg = ChartSvg { chartTrees :: [Tree], vbox :: ViewBox Double }

instance Semigroup ChartSvg where
  (ChartSvg s b) <> (ChartSvg s' b') = ChartSvg (s<>s') (b<>b')

instance Monoid ChartSvg where
  mempty = ChartSvg mempty (ViewBox zero)

-- * primitive Chart elements

-- | a space occupying the x,y-plane
data Spot = SpotPoint (Pair Double) | SpotRect (Rect Double) deriving (Show)

-- * projecting to a ViewBox

-- | bounding box of data (ie ignore styling)
bbox :: Spot -> Rect Double
bbox (SpotRect x) = x
bbox (SpotPoint (Pair x y)) = Rect x x y y

bboxes :: [Spot] -> Rect Double
bboxes = fold . fmap bbox

projectOn :: Rect Double -> Rect Double -> Spot -> Spot
projectOn new old@(Rect x z y w) po@(SpotPoint po'@(Pair px py))
  | x==z && y==w = po
  | x==z = SpotPoint (Pair px py')
  | y==w = SpotPoint (Pair px' py)
  | otherwise = SpotPoint pn
  where
    pn@(Pair px' py') = project old new po'
projectOn new old@(Rect x z y w) ro@(SpotRect ro'@(Rect ox oz oy ow))
  | x==z && y==w = ro
  | x==z = SpotRect (Rect ox oz ny nw)
  | y==w = SpotRect (Rect nx nz oy ow)
  | otherwise = SpotRect n
  where
    n@(Rect nx nz ny nw) = projectRect old new ro'

projectTo :: ViewBox Double -> [Spot] -> [Spot]
projectTo (ViewBox vb) xs = projectOn vb (bboxes xs) <$> xs

projectTo2 :: ViewBox Double -> [[Spot]] -> [[Spot]]
projectTo2 (ViewBox vb) xss = fmap (projectOn vb (fold $ bboxes <$> xss)) <$> xss

-- | a piece of chart structure
data Annotation
  = RectA RectStyle
  | TextA TextStyle [Text.Text]
  | GlyphA GlyphStyle
  | LineA LineStyle
  deriving (Show, Generic)

data Chart = Chart { ann :: Annotation, spots :: [Spot]} deriving (Show, Generic)

-- * RectChart styling

data RectStyle = RectStyle
  { borderSize :: Double
  , borderColor :: PixelRGBA8
  , borderOpacity :: Double
  , color :: PixelRGBA8
  , opacity :: Double
  , rectDA :: DrawAttributes
  } deriving (Show, Eq, Generic)

defaultRectStyle :: RectStyle
defaultRectStyle = RectStyle 0.005 ugrey 0.5 ublue 0.5 mempty

daRect :: RectStyle -> DrawAttributes
daRect o =
  o ^. field @"rectDA" &
  (strokeWidth .~ Last (Just $ Num (o ^. field @"borderSize"))) .
  (strokeColor .~ Last (Just $ ColorRef $ (o ^. field @"borderColor"))) .
  (strokeOpacity .~ Just (fromRational $ o ^. field @"borderOpacity")) .
  (fillColor .~ Last (Just $ ColorRef $ (o ^. field @"color"))) .
  (fillOpacity .~ (Just (fromRational $ o ^. field @"opacity"))) 

-- | solid rectangle, no border
blob ::  PixelRGBA8 -> Double -> RectStyle
blob c o = RectStyle 0 utrans 0 c o mempty

-- | clear and utrans rect
clear :: RectStyle
clear = RectStyle 0 utrans 0 utrans 0 mempty

-- | transparent rectangle, with border
border :: Double -> PixelRGBA8 -> Double -> RectStyle
border s c o = RectStyle s c o utrans 0 mempty


-- * TextChart styling

data TextStyle = TextStyle
  { size :: Double
  , color :: PixelRGBA8
  , opacity :: Double
  , alignH :: TextAnchor 
  , hsize :: Double
  , vsize :: Double
  , nudge1 :: Double
  , textDA :: DrawAttributes
  } deriving (Show, Eq, Generic)

defaultTextStyle :: TextStyle
defaultTextStyle =
  TextStyle 0.08 ugrey 1 TextAnchorMiddle 0.45 1.1 (-0.25) mempty

daText :: TextStyle -> DrawAttributes
daText o =
  o ^. field @"textDA" &
  (fontSize .~ Last (Just $ Num (o ^. field @"size"))) .
  (strokeWidth .~ Last (Just $ Num 0)) .
  (strokeColor .~ Last (Just $ FillNone)) .
  (fillColor .~ Last (Just $ ColorRef $ (o ^. field @"color"))) .
  (fillOpacity .~ (Just (fromRational $ o ^. field @"opacity"))) .
  (textAnchor .~ Last (Just $ (o ^. field @"alignH")))

-- * GlyphChart styling

data GlyphStyle = GlyphStyle
  { size :: Double -- ^ glyph radius
  , color :: PixelRGBA8 -- ^ fill color
  , opacity :: Double
  , borderColor :: PixelRGBA8 -- ^ stroke color
  , borderOpacity :: Double
  , borderSize :: Double -- ^ stroke width (adds a bit to the bounding box)
  , shape :: GlyphShape
  , glyphDA :: DrawAttributes
  } deriving (Show, Eq, Generic)

defaultGlyphStyle :: GlyphStyle
defaultGlyphStyle = GlyphStyle 0.03 ublue 0.3 ugrey 0.3 0.015 CircleGlyph mempty

-- | glyph shape sum type
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
  o ^. field @"glyphDA" &
  (strokeWidth .~ Last (Just $ Num (o ^. field @"borderSize"))) .
  (strokeColor .~ Last (Just $ ColorRef $ (o ^. field @"borderColor"))) .
  (strokeOpacity .~ Just (fromRational $ o ^. field @"borderOpacity")) .
  (fillColor .~ Last (Just $ ColorRef $ (o ^. field @"color"))) .
  (fillOpacity .~ (Just (fromRational $ o ^. field @"opacity")))

data LineStyle = LineStyle
  { width :: Double
  , color :: PixelRGBA8
  , opacity :: Double
  , da :: DrawAttributes
  } deriving (Show, Eq, Generic)

defaultLineStyle :: LineStyle
defaultLineStyle = LineStyle 0.02 ublue 0.5 mempty

daLine :: LineStyle -> DrawAttributes
daLine o =
  o ^. field @"da" &
  (strokeWidth .~ Last (Just $ Num (o ^. field @"width"))) .
  (strokeColor .~ Last (Just $ ColorRef $ (o ^. field @"color"))) .
  (strokeOpacity .~ Just (fromRational $ o ^. field @"opacity")) .
  (fillColor .~ Last (Just FillNone))

-- style boxes
styleBoxRect :: RectStyle -> Rect Double
styleBoxRect s = Rect (-x/2) (x/2) (-x/2) (x/2)
  where
    x = case daRect s ^. Svg.strokeWidth & getLast of
      Just (Num x') -> x'
      _ -> 0

styleBoxText :: TextStyle -> Text.Text -> Rect Double
styleBoxText o t =
    Rect ((-x'/2) + x'*origx) (x'/2 + x'*origx) ((-y'/2) - n1') (y'/2 - n1')
    where
      s = case getLast ((daText o) ^. fontSize) of
        Just (Num n) -> n
        _ -> 0
      h = o ^. field @"hsize"
      v = o ^. field @"vsize"
      n1 = o ^. field @"nudge1"
      x' = s * h * fromIntegral (Text.length t)
      y' = s * v
      n1' = s * n1
      origx = case (daText o ^. textAnchor) of
        Last (Just TextAnchorStart) -> 0.5
        Last (Just TextAnchorEnd) -> -0.5
        Last (Just TextAnchorMiddle) -> 0
        _ -> 0

-- * Rectangle combinators
(+.+) :: (Additive a) => Pair a -> Rect a -> Rect a
(+.+) (Pair x' y') (Rect x z y w) = Rect (x+x') (z+x') (y+y') (w+y')

(*.*) :: (Multiplicative a) => Pair a -> Rect a -> Rect a
(*.*) (Pair x' y') (Rect x z y w) = Rect (x*x') (z*x') (y*y') (w*y')

widen :: (Field a, Subtractive a, FromInteger a) => a -> Rect a -> Rect a
widen a (Rect x z y w) =
  Rect (x-a/2) (z+a/2) (y-a/2) (w+a/2)

styleBoxGlyph :: GlyphStyle -> Rect Double
styleBoxGlyph s = widen e $ case sh of
  EllipseGlyph a -> Pair sz (a*sz) *.* one
  RectSharpGlyph a -> Pair sz (a*sz) *.* one
  RectRoundedGlyph a _ _ -> Pair sz (a*sz) *.* one
  VLineGlyph a -> Pair (a*sz) sz *.* one
  HLineGlyph a -> Pair sz (a*sz) *.* one
  _ -> sz *. one
  where
    sh = s ^. field @"shape" 
    sz = s ^. field @"size"
    e = case daGlyph s ^. strokeWidth & getLast of
          Just (Num x') -> x'
          _ -> 0

styleBoxLine :: LineStyle -> Rect Double
styleBoxLine s = Rect (-x/2) (x/2) (-x/2) (x/2)
  where
    x = case daLine s ^. Svg.strokeWidth & getLast of
      Just (Num x') -> x'
      _ -> 0

plusSpot :: Rect Double -> Spot -> Rect Double
plusSpot (Rect x z y w) (SpotRect (Rect x' z' y' w')) =
  Rect (x+x') (z+z') (y+y') (w+w')
plusSpot (Rect x z y w) (SpotPoint (Pair x' y')) =
  Rect (x+x') (z+x') (y+y') (w+y')

styleBoxDA :: DrawAttributes -> Rect Double -> Rect Double
styleBoxDA da r = r' where
  r' = foldr tr r (da ^. transform & maybe [] identity)
  tr a x = case a of
    Translate x' y' -> (Pair x' (-y')) +.+ x
    TransformMatrix{} -> throw (NumHaskException "TransformMatrix transformation not yet implemented")
    Scale s Nothing -> Pair s s *.* x
    Scale sx (Just sy) -> Pair sx sy *.* x
    Rotate d Nothing -> rotatedRect d x
    Rotate d (Just (x',y')) -> rotatedRect d (Pair x' y' +.+ x)
    SkewX _ -> throw (NumHaskException "SkewX transformation not yet implemented")
    SkewY _ -> throw (NumHaskException "SkewY transformation not yet implemented")
    TransformUnknown -> x

-- FIXME: refactor out the DrawAttribute bits from the Style definitions (rectDA, textDA and glyphDA are all the same)
styleBox :: Chart -> Rect Double
styleBox (Chart (RectA s) xs) = fold $ plusSpot
  (styleBoxDA (s ^. field @"rectDA") (styleBoxRect s)) <$> xs
styleBox (Chart (TextA s ts) xs) = fold $ zipWith plusSpot
  (styleBoxDA (s ^. field @"textDA") <$> styleBoxText s <$> ts) xs
styleBox (Chart (GlyphA s) xs) = fold $ plusSpot
  (styleBoxDA (s ^. field @"glyphDA") (styleBoxGlyph s)) <$> xs
styleBox (Chart (LineA s) xs) = fold $ plusSpot
  (styleBoxDA (s ^. field @"da") (styleBoxLine s)) <$> xs

styleBoxes :: [Chart] -> Rect Double
styleBoxes xss = fold $ styleBox <$> xss

-- | project data to a ViewBox based on style effects
projectToStyle :: ViewBox Double -> Chart -> Chart
projectToStyle (ViewBox vb) ch@(Chart s xs) =
  Chart s (projectOn vb (styleBox ch) <$> xs)

-- | project data to a ViewBox based on style effects
projectToStyle2 :: ViewBox Double -> [Chart] -> [Chart]
projectToStyle2 (ViewBox vb) chs =
  zipWith Chart ss (fmap (projectOn vb (styleBoxes chs)) <$> xss)
  where
    ss = (\(Chart s _) -> s) <$> chs
    xss = (\(Chart _ xs) -> xs) <$> chs

treeRect :: Rect Double -> Tree
treeRect (Rect x z y w) =
  RectangleTree $
  rectUpperLeftCorner .~ (Num x, Num (-w)) $
  rectWidth .~ Num (z-x) $
  rectHeight .~ Num (w-y) $
  defaultSvg

treeText :: P.Text -> Pair Double -> Tree
treeText t (Pair x y) =
  TextTree Nothing (textAt (Num x, Num (-y)) t)

treeShape :: GlyphShape -> Double -> Pair Double -> Tree
treeShape CircleGlyph s (Pair x y) =
  CircleTree $ Circle mempty (Num x, Num (-y)) (Num (s/2))

treeShape SquareGlyph s p = treeRect (plusSpot (s *. one) (SpotPoint p))

treeShape (RectSharpGlyph x') s p  =
  treeRect (plusSpot (Pair s (x'*s) *.* one) (SpotPoint p))

treeShape (RectRoundedGlyph x'' rx ry) s (Pair x' y')  =
  RectangleTree $
  rectUpperLeftCorner .~ (Num (x+x'), Num (-(w+y'))) $
  rectWidth .~ Num (fromRational z-fromRational x) $
  rectHeight .~ Num (fromRational w-fromRational y) $
  rectCornerRadius .~ (Num rx, Num ry) $
  defaultSvg
  where
    (Rect x z y w) = Pair s (x''*s) *.* one

treeShape (EllipseGlyph x') s (Pair x y) =
  EllipseTree $ Ellipse mempty (Num x, Num (-y)) (Num $ s/2) (Num $ (x'*s)/2)

treeShape (VLineGlyph x') s (Pair x y) =
  LineTree $ Line (mempty & strokeWidth .~ Last (Just (Num x')))
  (Num x, Num (y - s/2)) (Num x, Num (y + s/2))

treeShape (HLineGlyph x') s (Pair x y) =
  LineTree $ Line (mempty & strokeWidth .~ Last (Just (Num x')))
  (Num (x - s/2), Num y) (Num (x + s/2), Num y)

treeShape SmileyGlyph s (Pair x y) =
  groupTrees mempty
  [ CircleTree
    $ defaultSvg
    & drawAttr . fillColor .~ (Last $ Just $ ColorRef $ PixelRGBA8 255 255 0 255)
    & circleCenter .~ (Num (0.5 * fromRational s), Num (0.5 * fromRational s))
    & circleRadius .~ Num (0.5 * fromRational s)
  , EllipseTree
    $ defaultSvg
    & drawAttr . fillColor .~ (Last $ Just $ ColorRef $ PixelRGBA8 255 255 255 255)
    & ellipseCenter .~ (Num (0.35 * fromRational s), Num (0.3 * fromRational s))
    & ellipseXRadius .~ Num (0.05 * fromRational s)
    & ellipseYRadius .~ Num (0.1 * fromRational s)
  , EllipseTree
    $ defaultSvg
    & drawAttr . fillColor .~ (Last $ Just $ ColorRef $ PixelRGBA8 255 255 255 255)
    & ellipseCenter .~ (Num (0.65 * fromRational s), Num (0.3 * fromRational s))
    & ellipseXRadius .~ Num (0.05 * fromRational s)
    & ellipseYRadius .~ Num (0.1 * fromRational s)
  , GroupTree $
    defaultSvg &
    groupChildren .~
    [ PathTree $
      defaultSvg
      & pathDefinition .~
      [ MoveTo OriginAbsolute [V2 0 0]
      , EllipticalArc OriginAbsolute [(0.38*s,0.4*s,0.1*s,False,False, V2 (0.65*s) 0)]
      ]
      & drawAttr . fillColor .~ (Last $ Just $ FillNone)
      & drawAttr . strokeColor .~ (Last $ Just $ ColorRef $
                                   PixelRGBA8 0 0 0 255)
      & drawAttr . strokeWidth .~ Last (Just (Num (fromRational s * 0.03)))
      & drawAttr . transform .~ Just [Translate (0.18*s) (0.65*s)]
    ]
  ]
  & drawAttr . transform .~ Just [Translate (x - s/2) (y - s/2)]

treeGlyph :: GlyphStyle -> Pair Double -> Tree
treeGlyph s p =
  treeShape (s ^. field @"shape") (s ^. field @"size") p

treeLine :: [Pair Double] -> Tree
treeLine xs =
  PolyLineTree $
  polyLinePoints .~ ((\(Pair x y) -> V2 x (-y)) <$> xs) $
  defaultSvg

tree_ :: Chart -> Tree
tree_ (Chart (RectA s) xs) =
  groupTrees (daRect s) (treeRect . (\(SpotRect x) -> x) <$> xs)
tree_ (Chart (TextA s ts) xs) =
  groupTrees (daText s) (zipWith (\t (SpotPoint x) -> treeText t x) ts xs)
tree_ (Chart (GlyphA s) xs) =
  groupTrees (daGlyph s) ((\(SpotPoint x) -> treeGlyph s x) <$> xs)
tree_ (Chart (LineA s) xs) =
  groupTrees (daLine s) [treeLine ((\(SpotPoint x) -> x) <$> xs)]

groupTrees :: DrawAttributes -> [Tree] -> Tree
groupTrees da' tree =
  defaultSvg &
  drawAttr %~ (da'<>) &
  groupChildren .~ tree &
  GroupTree

chartSvg :: ViewBox Double -> Chart -> ChartSvg
chartSvg (ViewBox vb) c =
  ChartSvg
  ((:[]) $ tree_ c)
  (ViewBox $ styleBox c <> vb)

chartSvg_ :: ViewBox Double -> Chart -> ChartSvg
chartSvg_ vb c = chartSvg vb (Chart (ann c) (projectTo vb (spots c)))

multiSvg :: ViewBox Double -> [Chart] -> ChartSvg
multiSvg (ViewBox vb) cs =
  ChartSvg
  (tree_ <$> cs)
  (ViewBox $ vb <> styleBoxes cs)

multiSvg_ :: ViewBox Double -> [Chart] -> ChartSvg
multiSvg_ vb chs = multiSvg vb chs'
  where
    xss = projectTo2 vb (spots <$> chs)
    ss = ann <$> chs
    chs' = zipWith Chart ss xss

-- * chartSvg combinators
padRect :: Double -> Rect Double -> Rect Double
padRect p (Rect x z y w) = Rect (x-wid) (z+wid) (y-hei) (w+hei)
  where
    wid = (p - 1) * (z - x)
    hei = (p - 1) * (w - y)

pad :: Double -> ChartSvg -> ChartSvg
pad p (ChartSvg s (ViewBox vb)) = ChartSvg s (ViewBox (padRect p vb))

frame :: RectStyle -> ChartSvg -> ChartSvg
frame o (ChartSvg _ (ViewBox vb)) =
  ChartSvg
  ((:[]) . tree_ $ (Chart (RectA o) [SpotRect vb]))
  (ViewBox vb)

wrapTree :: Pair Double -> ChartSvg -> Document
wrapTree (Pair wid hei) (ChartSvg ts (ViewBox vb)) =
  Document
  ((\(Rect x z y w) -> Just (x,-w,z-x,w-y)) vb)
  (Just (Num wid))
  (Just (Num hei))
  ts (Map.empty) "" [] ""

save :: FilePath -> Pair Double -> ChartSvg -> IO ()
save f sz ts = saveXmlFile f (wrapTree sz ts)


-- * color
-- | the official chart-unit blue
ublue :: PixelRGBA8
ublue = PixelRGBA8 93 165 218 255

-- | the official chart-unit grey
ugrey :: PixelRGBA8
ugrey = PixelRGBA8 102 102 102 255

-- | transparent
utrans :: PixelRGBA8
utrans = PixelRGBA8 0 0 0 0

-- | black
ublack :: PixelRGBA8
ublack = PixelRGBA8 0 0 0 255

-- | white
uwhite :: PixelRGBA8
uwhite = PixelRGBA8 255 255 255 255

ured :: PixelRGBA8
ured = PixelRGBA8 255 0 0 255

-- * transformations

-- | rotate a Chart by x degrees.  Multiple rotations will expand the bounding box conservatively.
rotateDA :: Double -> DrawAttributes
rotateDA r = mempty & transform .~ Just [Rotate r Nothing]

translateDA :: Pair Double -> DrawAttributes
translateDA (Pair x y) = mempty & transform .~ Just [Translate x (-y)]

rotateSvg :: Double -> ChartSvg -> ChartSvg
rotateSvg r (ChartSvg svg (ViewBox vb)) = 
  ChartSvg
  ([groupTrees (rotateDA r) svg])
  (ViewBox $ rotatedRect r vb)

translateSvg :: Pair Double -> ChartSvg -> ChartSvg
translateSvg p (ChartSvg svg (ViewBox vb)) = 
  ChartSvg
  ([groupTrees (translateDA p) svg])
  (ViewBox $ translateRect p vb)

rotatePoint :: (FromInteger a, Subtractive a, TrigField a) => a -> Pair a -> Pair a
rotatePoint d (Pair x y) = Pair (x * cos(d') + y*sin(d')) (y*cos(d')-x*sin(d'))
  where
    d' = d*pi/180

pointsRect :: Rect a -> [Pair a]
pointsRect (Rect x z y w) =
  [ Pair x y
  , Pair x w
  , Pair z y
  , Pair z w
  ]

rotatedRect :: Double -> Rect Double -> Rect Double
rotatedRect d r =
  fold $ (\(Pair x y) -> Rect x x y y) . rotatePoint d <$> (pointsRect r)

translateRect :: Pair Double -> Rect Double -> Rect Double
translateRect (Pair x' y') (Rect x z y w) = Rect (x+x') (z+x') (y+y') (w+y')

showOriginWith :: Double -> PixelRGBA8 -> Chart
showOriginWith s c = 
  Chart
  ( GlyphA $
    field @"borderSize" .~ 0 $
    field @"size" .~ s $
    field @"color" .~ c $
    defaultGlyphStyle)
  [SpotPoint (Pair 0 0)]

showOrigin :: Chart
showOrigin = showOriginWith 0.1 ured

-- | Create rect data for a formulae y = f(x)
rectXY :: (CanRange a) => (a -> a) -> Range a -> Int -> [Rect a]
rectXY f r g = (\x -> Rect (x-tick/(one+one)) (x+tick/(one+one)) zero (f x)) <$> grid MidPos r g
  where
    tick = NumHask.Space.width r / fromIntegral g

-- | Create point data for a formulae y = f(x)
dataXY :: (CanRange a) => (a -> a) -> Range a -> Int -> [Pair a]
dataXY f r g = (\x -> Pair x (f x)) <$> grid OuterPos r g

-- | Create rect data for a formulae c = f(x,y)
rectF :: (Signed a, CanRange a) => (Pair a -> b) -> Rect a -> Pair Int -> [(Rect a, b)]
rectF f r g = (\x -> (x, f (mid x))) <$> gridSpace r g

blend :: Double -> PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8
blend c c0 c1 = mixWithAlpha f fa c0 c1 where
  f _ x0 x1 = fromIntegral $ (round (fromIntegral x0 + c * (fromIntegral x1 - fromIntegral x0)) :: Integer)
  fa x0 x1 = f 0 x0 x1

pixelate :: (Signed a, CanRange a) => (Pair a -> Double) -> Rect a -> Pair Int -> PixelRGBA8 -> PixelRGBA8 -> [(Rect a, PixelRGBA8)]
pixelate f r g c0 c1 = (\(x,y) -> (x, blend y c0 c1)) <$> rectF f r g


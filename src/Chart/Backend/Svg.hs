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
import Linear.V2

-- * helpers
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) f x = fmap f <$> x

-- * Rectangle combinators
(+.+) :: (Additive a) => Pair a -> Rect a -> Rect a
(+.+) (Pair x' y') (Rect x z y w) = Rect (x+x') (z+x') (y+y') (w+y')

(*.*) :: (Multiplicative a) => Pair a -> Rect a -> Rect a
(*.*) (Pair x' y') (Rect x z y w) = Rect (x*x') (z*x') (y*y') (w*y')

widen :: (Field a, FromInteger a) => a -> Rect a -> Rect a
widen a (Rect x z y w) =
  Rect (x-a/2) (z+a/2) (y-a/2) (w+a/2)

padRect :: Double -> Rect Double -> Rect Double
padRect p (Rect x z y w) = Rect (x-wid) (z+wid) (y-hei) (w+hei)
  where
    wid = (p - 1) * (z - x)
    hei = (p - 1) * (w - y)

widthRect :: (AdditiveGroup a) => Rect a -> a
widthRect (Rect x z _ _) = z - x

heightRect :: (AdditiveGroup a) => Rect a -> a
heightRect (Rect _ _ y w) = w - y

-- | Create rect data for a formulae y = f(x)
rectXY :: (BoundedField a, Ord a, FromInteger a) => (a -> a) -> Range a -> Int -> [Rect a]
rectXY f r g = (\x -> Rect (x-tick/(one+one)) (x+tick/(one+one)) zero (f x)) <$> grid MidPos r g
  where
    tick = NumHask.Space.width r / fromIntegral g

-- * Chart
-- | the aspect of a chart expressed as a ratio of x-plane : y-plane.
newtype ViewBox a = ViewBox { vbRect :: Rect a } deriving (Show, Eq, Semigroup)

aspect :: (BoundedField a, Ord a, FromInteger a) => a -> ViewBox a
aspect a = ViewBox $ Ranges ((a *) <$> one) one

data Chart = Chart { chartSvg :: [Tree], vbox :: ViewBox Double }

instance Semigroup Chart where
  (Chart s b) <> (Chart s' b') = Chart (s<>s') (b<>b')

instance Monoid Chart where
  mempty = Chart mempty (ViewBox zero)

-- * Chart ADT
data ChartType
  = RectChart RectStyle [Rect Double]
  | TextChart TextStyle [(Text.Text, Pair Double)]
  | GlyphChart GlyphStyle [Pair Double]
  deriving (Show)

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
  | RectRoundedGlyph Double Double
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

-- * projecting to a ViewBox

-- | range based just on point data (ie ignore styling)
pointBoxes :: ChartType -> Rect Double
pointBoxes (RectChart _ xs) = fold xs
pointBoxes (TextChart _ xs) = fold $ (\(_, Pair x y) -> Rect x x y y) <$> xs
pointBoxes (GlyphChart _ xs) = fold $ (\(Pair x y) -> Rect x x y y) <$> xs

projectPair :: (BoundedField a, Ord a, Signed a, FromInteger a) => Rect a -> Rect a -> Pair a -> Pair a
projectPair new old@(Rect x z y w) po@(Pair px py)
  | x==z && y==w = po
  | x==z = Pair px py'
  | y==w = Pair px' py
  | otherwise = pn
  where
    pn@(Pair px' py') = project old new po

projectRect' :: (BoundedField a, Ord a, Signed a, FromInteger a) => Rect a -> Rect a -> Rect a -> Rect a
projectRect' new old@(Rect x z y w) o@(Rect ox oz oy ow)
  | x==z && y==w = o
  | x==z = Rect ox oz ny nw
  | y==w = Rect nx nz oy ow
  | otherwise = n
  where
    n@(Rect nx nz ny nw) = projectRect old new o

-- | project to a ViewBox based on a specific data range
projectOn :: ViewBox Double -> Rect Double -> ChartType -> ChartType
projectOn (ViewBox vb) r (RectChart s xs) = RectChart s $
  projectRect' vb r <$> xs
projectOn (ViewBox vb) r (TextChart s xs) = TextChart s $
  zip ts ps
  where
    ts = fst <$> xs
    ps = projectPair vb r . snd <$> xs
projectOn (ViewBox vb) r (GlyphChart s xs) = GlyphChart s $ projectPair vb r <$> xs

projectOnPoint :: ViewBox Double -> ChartType -> ChartType
projectOnPoint vb d = projectOn vb (pointBoxes d) d

projectOnPoints :: ViewBox Double -> [ChartType] -> [ChartType]
projectOnPoints vb xss = projectOn vb (fold $ pointBoxes <$> xss) <$> xss

da :: ChartType -> DrawAttributes
da (RectChart s _) = daRect s
da (TextChart s _) = daText s
da (GlyphChart s _) = daGlyph s

styleBoxRect :: RectStyle -> Rect Double -> Rect Double
styleBoxRect s x = widen e x where
  e = case daRect s ^. Svg.strokeWidth & getLast of
        Just (Num x') -> x'
        _ -> 0

styleBoxText :: TextStyle -> (Text.Text, Pair Double) -> Rect Double
styleBoxText o (t, p) =
    (+.+) p
    (Rect ((-x'/2) + x'*origx) (x'/2 + x'*origx) ((-y'/2) - n1') (y'/2 - n1'))
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

styleBoxGlyph :: GlyphStyle -> Pair Double -> Rect Double
styleBoxGlyph s x = (+.+) x $ widen e $ case sh of
  EllipseGlyph a -> Pair sz (a*sz) *.* one
  RectSharpGlyph a -> Pair sz (a*sz) *.* one
  RectRoundedGlyph a _ -> Pair sz (a*sz) *.* one
  VLineGlyph a -> Pair (a*sz) sz *.* one
  HLineGlyph a -> Pair sz (a*sz) *.* one
  _ -> sz *. one
  where
    sh = s ^. field @"shape" 
    sz = s ^. field @"size"
    e = case daGlyph s ^. strokeWidth & getLast of
          Just (Num x') -> x'
          _ -> 0

styleBox :: ChartType -> Rect Double
styleBox (RectChart s xs) = fold $ styleBoxRect s <$> xs
styleBox (TextChart s xs) = fold $ styleBoxText s <$> xs
styleBox (GlyphChart s xs) = fold $ styleBoxGlyph s <$> xs

styleBoxes :: [ChartType] -> Rect Double
styleBoxes xss = fold $ styleBox <$> xss

-- | project data to a ViewBox based on style effects
projectOnStyle :: ViewBox Double -> ChartType -> ChartType
projectOnStyle vb d = projectOn vb (styleBox d) d

-- | project data to a ViewBox based on style effects
projectOnStyles :: ViewBox Double -> [ChartType] -> [ChartType]
projectOnStyles vb xss = projectOn vb (styleBoxes xss) <$> xss

treeRect :: Rect Double -> Pair Double -> Tree
treeRect (Rect x z y w) (Pair x' y') =
  RectangleTree $
  rectUpperLeftCorner .~ (Num (x+x'), Num (-(w+y'))) $
  rectWidth .~ Num (fromRational z-fromRational x) $
  rectHeight .~ Num (fromRational w-fromRational y) $
  defaultSvg

treeText :: (P.Text, Pair Double) -> Tree
treeText (t, (Pair x y)) =
  TextTree Nothing (textAt (Num x, Num y) t)

treeShape :: GlyphShape -> Double -> Pair Double -> Tree
treeShape CircleGlyph s (Pair x y) =
  CircleTree $ Circle mempty (Num x, Num y) (Num (s/2))

treeShape SquareGlyph s p = treeRect (s *. one) p

treeShape (RectSharpGlyph x') s p  = treeRect (Pair s (x'*s) *.* one) p

treeShape (RectRoundedGlyph rx ry) s (Pair x' y')  =
  RectangleTree $
  rectUpperLeftCorner .~ (Num (x+x'), Num (-(w+y'))) $
  rectWidth .~ Num (fromRational z-fromRational x) $
  rectHeight .~ Num (fromRational w-fromRational y) $
  rectCornerRadius .~ (Num rx, Num ry) $
  defaultSvg
  where
    (Rect x z y w) = Pair s (x'*s) *.* one

treeShape (EllipseGlyph x') s (Pair x y) =
  EllipseTree $ Ellipse mempty (Num x, Num y) (Num s) (Num $ x'*s)

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
      , EllipticalArc OriginAbsolute [(0.4*s,0.4*s,0.1*s,False,False, V2 (0.65*s) 0)]
      ]
      & drawAttr . fillColor .~ (Last $ Just $ FillNone)
      & drawAttr . strokeColor .~ (Last $ Just $ ColorRef $
                                   PixelRGBA8 255 255 255 255)
      & drawAttr . strokeWidth .~ Last (Just (Num (fromRational s * 0.05)))
      & drawAttr . transform .~ Just [Translate (0.15*s) (0.65*s)]
    ]
  ]
  & drawAttr . transform .~ Just [Translate x y]


treeGlyph :: GlyphStyle -> Pair Double -> Tree
treeGlyph s p =
  treeShape (s ^. field @"shape") (s ^. field @"size") p

tree_ :: ChartType -> Tree
tree_ (RectChart s xs) = groupTrees (daRect s) (treeRect <$> xs <*> pure zero)
tree_ (TextChart s xs) = groupTrees (daText s) (treeText <$> xs)
tree_ (GlyphChart s xs) = groupTrees (daGlyph s) (treeGlyph s <$> xs)

groupTrees :: DrawAttributes -> [Tree] -> Tree
groupTrees da' tree =
  defaultSvg &
  drawAttr %~ (da'<>) &
  groupChildren .~ tree &
  GroupTree

chart :: ViewBox Double -> ChartType -> Chart
chart vb c =
  Chart
  ((:[]) $ tree_ (projectOnStyle vb c))
  vb

chart_ :: ViewBox Double -> ChartType -> Chart
chart_ vb c = chart vb (projectOnPoint vb c)

multi :: ViewBox Double -> [ChartType] -> Chart
multi vb cs =
  Chart
  (tree_ <$> projectOnStyles vb cs)
  vb

multi_ :: ViewBox Double -> [ChartType] -> Chart
multi_ vb xss = multi vb (projectOnPoints vb xss)


-- * chart combinators

padChart :: Double -> Chart -> Chart
padChart p (Chart s (ViewBox vb)) = Chart s (ViewBox (padRect p vb))

frameChart :: RectStyle -> Chart -> Chart
frameChart o (Chart _ (ViewBox vb)) =
  Chart
  ((:[]) . tree_ $ (RectChart o [vb]))
  (ViewBox vb)

wrapTree :: Pair Double -> Chart -> Document
wrapTree (Pair wid hei) (Chart ts (ViewBox vb)) =
  Document
  ((\(Rect x z y w) -> Just (x,-w,z-x,w-y)) vb)
  (Just (Num wid))
  (Just (Num hei))
  ts (Map.empty) "" [] ""

save :: FilePath -> Pair Double -> Chart -> IO ()
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




-- * transformations

-- | rotate a Chart by x degrees.  Multiple rotations will expand the bounding box conservatively.
rotateDA :: Double -> DrawAttributes
rotateDA r = mempty & transform .~ Just [Rotate r Nothing]

translateDA :: Pair Double -> DrawAttributes
translateDA (Pair x y) = mempty & transform .~ Just [Translate x (-y)]

rotateChart :: Double -> Chart -> Chart
rotateChart r (Chart svg (ViewBox vb)) = 
  Chart
  ([groupTrees (rotateDA r) svg])
  (ViewBox $ rotatedRect r vb)

translateChart :: Pair Double -> Chart -> Chart
translateChart p (Chart svg (ViewBox vb)) = 
  Chart
  ([groupTrees (translateDA p) svg])
  (ViewBox $ translateRect p vb)

rotatePoint :: (FromInteger a, TrigField a) => a -> Pair a -> Pair a
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


{-

-- | create a basic chart shape from Options
glyph_ (GlyphOptions s c bc bs (Ellipse a)) = Chart
  ( S.ellipse !
    A.rx (toValue s) !
    A.ry (toValue $ s*a) !
    A.strokeWidth (toValue bs) !
    A.stroke (colorA bc) !
    A.strokeOpacity (toValue $ ucopacity bc) !
    A.fill (colorA c) !
    A.fillOpacity (toValue $ ucopacity c)
  )
  (widen bs $ Pair x (a*x) *.* one)
glyph_ (GlyphOptions s c bc bs Octagaon) = Chart
  (S.g $ mconcat
  [ S.circle !
    A.r (toValue (0.5*s)) !
    A.fill "yellow" !
    A.cx (toValue (0.5*s)) !
    A.cy (toValue (0.5*s))
  , S.ellipse !
    A.rx (toValue (0.05*s)) !
    A.ry (toValue (0.1*s)) !
    A.fill "black" !
    A.cx (toValue (0.35*s)) !
    A.cy (toValue (0.3*s))
  , S.ellipse !
    A.rx (toValue (0.05*s)) !
    A.ry (toValue (0.1*s)) !
    A.fill "black" !
    A.cx (toValue (0.65*s)) !
    A.cy (toValue (0.3*s))
  , S.g
    (S.path ! A.d
     (toValue $
      ("M0,0 A" :: Text)<>show (0.4*s)<>","<>show (0.4*s)<>" "<>
      show (0.1*s)<>" 0,0 "<>show (0.65*s)<>",0") !
     A.fill "none" !
     A.stroke "black" !
     A.strokeWidth (toValue (0.05*s))) !
    A.transform (S.translate (0.15*s) (0.65*s))
  ] !
    A.strokeWidth (toValue bs) !
    A.stroke (colorA bc) !
    A.strokeOpacity (toValue $ ucopacity bc) !
    A.fill (colorA c) !
    A.fillOpacity (toValue $ ucopacity c)
  )
  (widen bs $ s *. one)
glyph_ (GlyphOptions s c bc bs (RectSharp a)) = Chart
  ( S.rect !
    A.height (toValue s) !
    A.width (toValue $ s*a) !
    A.strokeWidth (toValue bs) !
    A.stroke (colorA bc) !
    A.strokeOpacity (toValue $ ucopacity bc) !
    A.fill (colorA c) !
    A.fillOpacity (toValue $ ucopacity c)
  )
  (widen bs $ Pair s (a*s) *.* one)
glyph_ (GlyphOptions s c bc bs (RectRounded a r)) = Chart
  ( S.rect !
    A.height (toValue s) !
    A.width (toValue $ s*a) !
    A.rx (toValue $ r*s) !
    A.ry (toValue $ r*s) !
    A.strokeWidth (toValue bs) !
    A.stroke (colorA bc) !
    A.strokeOpacity (toValue $ ucopacity bc) !
    A.fill (colorA c) !
    A.fillOpacity (toValue $ ucopacity c)
  )
  (widen bs $ Pair x (a*s) *.* one)
glyph_ (GlyphOptions s c bc bs (VLine a)) = line_ a
  [Pair 0 (-x/2)
  , Pair 0 (x/2)]
glyph_ (HLine a) = line_ a
  [ Pair (-x/2) 0
  , Pair (x/2) 0] !
  A.strokeWidth (toValue a)

glyphs opts xs = mconcat $ toList $ (\x -> translate x (glyph_ opts)) <$> xs

glyphChart optss asp r xyss =
  mconcat $ zipWith glyphs optss (projectss r asp xyss)

glyphChart_ optss asp xyss = glyphChart optss asp (range xyss) xyss

pixel_ :: Pixel -> S.Svg
pixel_ (Pixel r c) = 
  rect_ r !
  A.fill (colorA c) !
  A.fillOpacity (toValue $ ucopacity c)

pixels ps = mconcat $ toList $ pixel_ <$> ps

pixelChart asp r pss = mconcat $ pixels . projectPixels r asp . toList <$> pss
  where
    projectPixels r0 r1 ps =
      zipWith Pixel (projectRect r0 r1 . pixelRect <$> ps) (pixelColor <$> ps)

pixelChart_ asp ps = pixelChart asp (fold $ fold . map pixelRect <$> ps) ps

line_ :: LineOptions -> [Pair Double] -> Chart
line_ (LineOptions s c) xs = Chart svg bb
  where
    bb = mconcat (widen s . singleton <$> xs)
    svg = S.polyline !
      A.points (toValue $ intercalate ", " $
                (\(Pair x y) -> show x <> " " <> show (-y)) <$> xs) !
      A.fill "none" !
      A.strokeWidth (toValue s) !
      A.stroke (colorA c) !
      A.strokeOpacity (toValue $ ucopacity c)

lines :: [LineOptions] -> [[Pair Double]] -> Chart
lines opts xss = mconcat (zipWith line_ opts xss)

lineChart :: [LineOptions] -> Rect Double -> Rect Double -> [[Pair Double]] -> Chart
lineChart optss asp r xyss = lines optss (projectss r asp xyss)

lineChart_ optss asp xyss = lineChart optss asp (range xyss) xyss

glines :: [LineOptions] -> [GlyphOptions] -> [[Pair Double]] -> S.Svg
glines opts gopts xs = S.g $ (mconcat $ zipWith glyphs gopts xs) <> lines opts xs

glineChart ls gs asp r xyss =
  glines ls gs (projectss r asp xyss)

glineChart_ ls gs asp xyss = glineChart ls gs asp (range xyss) xyss

lglyphs lopts gopts xs =
  toList $ (\(t, x) -> translate x $ labelled lopts t (glyph_ gopts)) <$> xs

lglyphChart ls gs asp r xyss =
  mconcat $
  mconcat $
  getZipList $
  lglyphs <$> ZipList ls <*> ZipList gs <*>
  ZipList
    (zipWith
       zip
       (map fst . toList <$> xyss)
       (projectss r asp (map snd . toList <$> xyss)))

lglyphChart_ ls gs asp xyss =
  lglyphChart ls gs asp (range (map snd . toList <$> xyss)) xyss


hud_ :: () => HudOptions -> Rect Double -> Rect Double -> (S.Svg, Rect Double)
hud_ (HudOptions p as gs ts ls can) asp@(Ranges ax ay) r@(Ranges rx ry) =
  flip runState asp $ do
    r <- get
    let canvas' = id_ "canvas" (rectAtts can (rect_ r))
    return canvas'

-}

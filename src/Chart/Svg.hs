{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

module Chart.Svg
  ( ViewBox(..)
  , aspect
  , ratio
  , ChartSvg(..)
  , treeRect
  , treeGlyph
  , treeLine
  , treeShape
  , treeText
  , tree
  , projectSpots
  , chartSvg_
  , chartSvg
  , fittedSvg
  , pad
  , frame
  , defaultFrame
  , renderXml
  , renderXmlWith
  , xmlToText
  , write
  , writeWith
  , rotateDA
  , translateDA
  , rotate
  , translate
  , ScratchStyle(..)
  , defaultScratchStyle
  , clearScratchStyle
  , scratchSvgWith
  , scratch
  , scratchWith
  , scratchSvg
  , placedLabel
  ) where
 
import Chart.Core
import Chart.Spot
import Codec.Picture.Types
import Data.Generics.Product (field)
import Data.List (zipWith3)
import Graphics.Svg as Svg hiding (Point, toPoint)
import Graphics.Svg.CssTypes as Svg hiding (Point)
-- import Graphics.Svg.Types as Svg hiding (Point, toPoint)
import Lens.Micro
import Linear.V2
import NumHask.Data.Rect
import NumHask.Prelude as P hiding (Group, rotate, Element)
import Text.XML.Light.Output
import qualified Data.Map as Map
import qualified Data.Text as Text
-- import qualified Data.Map as Map


-- * Svg
-- | An Svg ViewBox
newtype ViewBox a = ViewBox
  { vbArea :: Area a
  } deriving (Show, Eq, Semigroup, Functor, Multiplicative)

-- | convert a ratio of x-plane : y-plane to a ViewBox with a height of one.
aspect :: (FromRatio a, Multiplicative a) => a -> ViewBox a
aspect a = ViewBox $ Area (a * (-0.5)) (a * 0.5) (-0.5) 0.5

-- | convert a ViewBox to a ratio
ratio :: (Divisive a, Subtractive a) => ViewBox a -> a
ratio (ViewBox (Area x z y w)) = (z-x)/(w-y)

-- | Svg of a Chart consists of
-- An Svg `Tree` list and a ViewBox
data ChartSvg a = ChartSvg
  { vbox :: ViewBox a
  , chartTrees :: [Tree]
  } deriving (Eq, Show)

instance (BoundedLattice a) => Semigroup (ChartSvg a) where
  (ChartSvg a b) <> (ChartSvg a' b') = ChartSvg (a<>a') (b<>b')

instance (Chartable a) => Monoid (ChartSvg a) where
  mempty = ChartSvg one mempty

-- * svg primitives
-- | Rectange svg
treeRect :: (ToRatio a) => Area a -> Tree
treeRect a =
  RectangleTree $
  rectUpperLeftCorner .~ (Num x, Num (-w)) $
  rectWidth .~ Num (z-x) $
  rectHeight .~ Num (w-y) $
  defaultSvg
  where
    (Area x z y w) = fromRational <$> a

-- | Text svg
treeText :: (ToRatio a) => P.Text -> Point a -> Tree
treeText t p =
  TextTree Nothing (textAt (Num x, Num (-y)) t)
  where
    (Point x y) = fromRational <$> p

-- | GlyphShape to svg primitive
treeShape :: GlyphShape -> Double -> Point Double -> Tree
treeShape CircleGlyph s p =
  CircleTree $ Circle mempty (Num x, Num (-y)) (Num (fromRational s/2))
  where
    (Point x y) = fromRational <$> p
treeShape SquareGlyph s p = treeRect (translateArea p ((s*) <$> one))
treeShape (RectSharpGlyph x') s p =
  treeRect (translateArea p (scale (Point s (x'*s)) one))
treeShape (RectRoundedGlyph x'' rx ry) s p  =
  RectangleTree $
  rectUpperLeftCorner .~ (Num (x+x'), Num (-(w+y'))) $
  rectWidth .~ Num (fromRational z-fromRational x) $
  rectHeight .~ Num (fromRational w-fromRational y) $
  rectCornerRadius .~ (Num (fromRational rx), Num (fromRational ry)) $
  defaultSvg
  where
    (Area x z y w) = fromRational <$> scale (Point s (x''*s)) one
    (Point x' y') = fromRational <$> p
treeShape (EllipseGlyph x') s (Point x y) =
  EllipseTree $ Ellipse mempty (Num (fromRational x), Num (-(fromRational y)))
  (Num $ fromRational s/two) (Num $ (fromRational x'*fromRational s)/two)
treeShape (VLineGlyph x') s (Point x y) =
  LineTree $ Line (mempty & strokeWidth .~ Last (Just (Num (fromRational x'))))
  (Num (fromRational x), Num (fromRational $ y - s/two))
  (Num (fromRational x), Num (fromRational $ y + s/two))
treeShape (HLineGlyph x') s (Point x y) =
  LineTree $ Line (mempty & strokeWidth .~ Last (Just (Num $ fromRational x')))
  (Num (fromRational $ x - s/two), Num $ fromRational y)
  (Num (fromRational $ x + s/two), Num (fromRational y))
treeShape SmileyGlyph s' p =
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
      & drawAttr . fillColor .~ (Last $ Just FillNone)
      & drawAttr . strokeColor .~ (Last $ Just $ ColorRef $
                                   PixelRGBA8 0 0 0 255)
      & drawAttr . strokeWidth .~ Last (Just (Num (fromRational s * 0.03)))
      & drawAttr . transform .~ Just [Translate (0.18*s) (0.65*s)]
    ]
  ]
  & drawAttr . transform .~ Just [Translate (x - s/2) (y - s/2)]
  where
    s = fromRational s'
    (Point x y) = fromRational <$> p

-- | GlyphStyle to svg primitive
treeGlyph :: GlyphStyle -> Point Double -> Tree
treeGlyph s =
  treeShape (s ^. field @"shape") (s ^. field @"size")

-- | line svg
treeLine :: (Chartable a) => [Point a] -> Tree
treeLine xs =
  PolyLineTree $
  polyLinePoints .~ ((\(Point x y) -> V2 x (-y)) . fmap fromRational <$> xs) $
  defaultSvg

-- | convert a Chart to svg
tree :: (Chartable a) => Chart a -> Tree
tree (Chart (TextA s ts) das xs) =
  groupTrees (das <> daText s) (zipWith treeText ts (toPoint <$> xs))
tree (Chart (GlyphA s) das xs) =
  groupTrees (das <> daGlyph s) (treeGlyph s <$> (toPoint . fmap fromRational <$> xs))
tree (Chart (LineA s) das xs) =
  groupTrees (das <> daLine s) [treeLine (toPoint <$> xs)]
tree (Chart (RectA s) das xs) =
  groupTrees (das <> daRect s) (treeRect <$> (toArea <$> xs))

-- | add drawing attributes as a group svg wrapping a [Tree]
groupTrees :: DrawAttributes -> [Tree] -> Tree
groupTrees da' tree' =
  defaultSvg &
  drawAttr %~ (da'<>) &
  groupChildren .~ tree' &
  GroupTree

-- | create a ChartSvg from a [Chart] and a ViewBox
chartSvg_ :: (Chartable a) =>
  ViewBox a -> [Chart a] -> ChartSvg a
chartSvg_ (ViewBox a) cs = ChartSvg
  (ViewBox a)
  (tree <$> cs)

projectSpots :: (Chartable a) => Area a -> [Chart a] -> [Chart a]
projectSpots a cs = cs'
  where
    xss = projectTo2 a (spots <$> cs)
    ss = annotation <$> cs
    dass = drawatts <$> cs
    cs' = zipWith3 Chart ss dass xss

-- | convert a [Chart] to a ChartSvg, projecting Chart data to the supplied ViewBox, and expanding the ViewBox for chart style if necessary
chartSvg :: (Chartable a) =>
  ViewBox a -> [Chart a] -> ChartSvg a
chartSvg (ViewBox a) cs = chartSvg_ (ViewBox $ styleBoxes cs') cs'
  where
    cs' = projectSpots a cs

-- | convert a [Chart] to a ChartSvg, setting the ViewBox equal to the Chart data area
fittedSvg :: (Chartable a) =>
  [Chart a] -> ChartSvg a
fittedSvg cs =
  chartSvg (ViewBox $ styleBoxes cs) cs

-- | widen a ChartSvg ViewBox by a fraction of the size.
pad :: (Chartable a) => a -> ChartSvg a -> ChartSvg a
pad p (ChartSvg (ViewBox vb) s) = ChartSvg (ViewBox (widenProp p vb)) s

-- | add an enclosing fitted frame to a ChartSvg
frame :: (Chartable a) => RectStyle -> ChartSvg a -> ChartSvg a
frame o (ChartSvg (ViewBox vb) _) =
  ChartSvg
  (ViewBox vb)
  ((:[]) . tree $ Chart (RectA o) mempty [SpotArea vb])

-- | a default frame
defaultFrame :: (Chartable a) => ChartSvg a -> ChartSvg a
defaultFrame ch = frame (border 0.01 blue 1.0) ch <> ch


-- | render a ChartSvg to an xml Document with the supplied size and various bits and pieces
renderXmlWith :: (ToRatio a) => Point a -> Map.Map Text.Text Element -> Text.Text -> [CssRule] -> FilePath -> ChartSvg a -> Document
renderXmlWith (Point wid hei) defs desc css fp (ChartSvg (ViewBox vb) ts) =
  Document
  ((\(Area x z y w) -> Just (x,-w,z-x,w-y)) $ fromRational <$> vb)
  (Just (Num (fromRational wid)))
  (Just (Num (fromRational hei)))
  ts (Map.mapKeys Text.unpack defs) (Text.unpack desc) css fp

-- | render a ChartSvg to an xml Document with the supplied size
renderXml :: (ToRatio a) => Point a -> ChartSvg a -> Document
renderXml p = renderXmlWith p Map.empty "" [] ""

-- | render an xml document to Text
xmlToText :: Document -> P.Text
xmlToText = Text.pack . ppcTopElement prettyConfigPP . xmlOfDocument

-- | write a ChartSvg to a svg file with various Document attributes.
writeWith :: (ToRatio a) => FilePath -> Point a -> Map.Map Text.Text Element -> Text.Text -> [CssRule] -> ChartSvg a -> IO ()
writeWith fp p defs desc css c = saveXmlFile fp (renderXmlWith p defs desc css fp c)

-- | write a ChartSvg to an svg file.
write :: (ToRatio a) => FilePath -> Point a -> ChartSvg a -> IO ()
write fp p = writeWith fp p Map.empty "" []

-- * transformations
-- | A DrawAttributes to rotate by x degrees.
rotateDA :: (ToRatio a) => a -> DrawAttributes
rotateDA r = mempty & transform .~ Just [Rotate (fromRational r) Nothing]

-- | A DrawAttributes to translate by a Point.
translateDA :: (ToRatio a) => Point a -> DrawAttributes
translateDA (Point x y) = mempty & transform .~ Just
  [Translate (fromRational x) (-fromRational y)]

-- | Rotate a ChartSvg expanding the ViewBox as necessary.
-- Multiple rotations will expand the bounding box conservatively.
rotate :: (Chartable a, FromInteger a, TrigField a) => a -> ChartSvg a -> ChartSvg a
rotate r (ChartSvg (ViewBox vb) c) = 
  ChartSvg
  (ViewBox $ rotateArea r vb)
  [groupTrees (rotateDA r) c]

-- | Translate a ChartSvg also moving the ViewBox
translate :: (Additive a, ToRatio a) => Point a -> ChartSvg a -> ChartSvg a
translate p (ChartSvg (ViewBox vb) c) = 
  ChartSvg
  (ViewBox $ translateArea p vb)
  [groupTrees (translateDA p) c]

-- * development helpers

data ScratchStyle = ScratchStyle
  { fileName :: FilePath
  , size :: Double
  , ratioAspect :: Double
  , outerPad :: Double
  , innerPad :: Double
  , frame' :: ChartSvg Double -> ChartSvg Double
  , maybeOrig :: Maybe (Double, PixelRGBA8)
  } deriving (Generic)

defaultScratchStyle :: ScratchStyle
defaultScratchStyle  = ScratchStyle "other/scratchpad.svg" 200 1.5 1.05 1.05 defaultFrame (Just (0.04, red))

clearScratchStyle :: ScratchStyle
clearScratchStyle  = ScratchStyle "other/scratchpad.svg" 400 1 1 1 defaultFrame Nothing

scratchSvgWith :: ScratchStyle -> [ChartSvg Double] -> IO ()
scratchSvgWith s x =
  write
  (s ^. field @"fileName")
  (Point (s ^. field @"ratioAspect" * s ^. field @"size") (s ^. field @"size")) $
  pad (s ^. field @"outerPad") $
  (s ^. field @"frame'") $
  pad (s ^. field @"innerPad") $
  mconcat x

scratchSvg :: [ChartSvg Double] -> IO ()
scratchSvg = scratchSvgWith defaultScratchStyle

scratch :: [Chart Double] -> IO ()
scratch = scratchWith defaultScratchStyle

scratchWith :: ScratchStyle -> [Chart Double] -> IO ()
scratchWith s x =
  write
  (s ^. field @"fileName")
  (Point (s ^. field @"ratioAspect" * s ^. field @"size") (s ^. field @"size")) $
  pad (s ^. field @"outerPad") $
  (s ^. field @"frame'") $
  pad (s ^. field @"innerPad") $
  chartSvg
  (aspect (s ^. field @"ratioAspect"))
  (orig' <> x) where
  orig' = case s^.field @"maybeOrig" of
    Nothing -> mempty
    Just (n,c) -> [showOriginWith n c]

placedLabel :: (Chartable a) => Point a -> a -> Text.Text -> Chart a
placedLabel p d t =
  Chart (TextA defaultTextStyle [t])
  (mempty <> translateDA p <> rotateDA d)
  [zero]

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- |

module Chart.Diff where

import Data.TreeDiff
import Data.Maybe
import qualified Data.TreeDiff.OMap as O
import Chart.Markup
import GHC.Exts

isUnchangedList :: [Edit EditExpr] -> Bool
isUnchangedList xs = all isCpy xs && all isUnchangedExpr (mapMaybe cpy xs)

isCpy :: Edit a -> Bool
isCpy (Cpy _) = True
isCpy _ = False

cpy :: Edit a -> Maybe a
cpy (Cpy a) = Just a
cpy _ = Nothing

isUnchangedEdit :: Edit EditExpr -> Bool
isUnchangedEdit (Cpy e) = isUnchangedExpr e
isUnchangedEdit _ = False

isUnchangedExpr :: EditExpr -> Bool
isUnchangedExpr e = isUnchangedList $ getList e

getList :: EditExpr -> [Edit EditExpr]
getList (EditApp _ xs) = xs
getList (EditRec _ m) = snd <$> O.toList m
getList (EditLst xs) = xs
getList (EditExp _) = []

filterChangedExprs :: EditExpr -> Maybe EditExpr
filterChangedExprs (EditApp n xs) =
  case filter (not . isUnchangedEdit) (filterChangedEdits xs) of
    [] -> Nothing
    xs' -> Just $ EditApp n xs'
filterChangedExprs (EditRec n m) =
  case filterChangedEditMap (O.fromList $ filter (not . isUnchangedEdit . snd) (O.toList m)) of
  Nothing -> Nothing
  Just m' -> Just (EditRec n m')
filterChangedExprs (EditLst xs) =
  case filter (not . isUnchangedEdit) (filterChangedEdits xs) of
    [] -> Nothing
    xs' -> Just (EditLst xs')
filterChangedExprs (EditExp _) = Nothing

filterChangedEdit :: Edit EditExpr -> Maybe (Edit EditExpr)
filterChangedEdit (Cpy a) = Cpy <$> filterChangedExprs a
filterChangedEdit x = Just x

filterChangedEdit' :: (f, Edit EditExpr) -> Maybe (f, Edit EditExpr)
filterChangedEdit' (f, e) = (f,) <$> filterChangedEdit e

filterChangedEdits :: [Edit EditExpr] -> [Edit EditExpr]
filterChangedEdits xs = mapMaybe filterChangedEdit xs

filterChangedEditMap :: O.OMap FieldName (Edit EditExpr) -> Maybe (O.OMap FieldName (Edit EditExpr))
filterChangedEditMap m = case xs' of
  [] -> Nothing
  xs'' -> Just $ O.fromList xs''
  where
    xs = O.toList m
    xs' = mapMaybe filterChangedEdit' xs

patch :: Markup -> Markup -> Maybe (Edit EditExpr)
patch m m' = filterChangedEdit $ ediff m m'

m0 :: Markup
m0 = Markup "top" (Attributes (fromList [(Class, "a"), (Attribute "b","c")])) [MarkupLeaf (Markup "leaf" mempty mempty), Content "text"]

-- Changing class
m1 :: Markup
m1 = Markup "top" (Attributes (fromList [(Class, "b"), (Attribute "b","c")])) [MarkupLeaf (Markup "leaf" mempty mempty), Content "text"]

-- deleting an attribute
m2 :: Markup
m2 = Markup "top" (Attributes (fromList [(Class, "a")])) [MarkupLeaf (Markup "leaf" mempty mempty), Content "text"]

-- inserting an attribute
m3 :: Markup
m3 = Markup "top" (Attributes (fromList [(Class, "a"), (Attribute "b","c"), (Attribute "d", "e")])) [MarkupLeaf (Markup "leaf" mempty mempty), Content "text"]

-- changing a tag
m4 :: Markup
m4 = Markup "newtop" (Attributes (fromList [(Class, "a"), (Attribute "b","c")])) [MarkupLeaf (Markup "leaf" mempty mempty), Content "text"]

-- changing a leaf
m5 :: Markup
m5 = Markup "top" (Attributes (fromList [(Class, "a"), (Attribute "b","c")])) [MarkupLeaf (Markup "newleaf" mempty mempty), Content "text"]

-- deleting a leaf
m6 :: Markup
m6 = Markup "top" (Attributes (fromList [(Class, "a"), (Attribute "b","c")])) [Content "text"]

-- inserting a leaf
m7 :: Markup
m7 = Markup "top" (Attributes (fromList [(Class, "a"), (Attribute "b","c")])) [MarkupLeaf (Markup "newleaf" mempty mempty), MarkupLeaf (Markup "leaf" mempty mempty), Content "text"]

-- inserting Attributes
m8 :: Markup
m8 = Markup "top" (Attributes (fromList [(Class, "a"), (Attribute "b","c")])) [MarkupLeaf (Markup "leaf" (Attributes (fromList [(Class, "a"), (Attribute "b","c")])) mempty), Content "text"]

-- modifying content
m9 :: Markup
m9 = Markup "top" (Attributes (fromList [(Class, "a"), (Attribute "b","c")])) [MarkupLeaf (Markup "leaf" mempty mempty), Content "textual content"]

-- inserting a leaf deeper down
m10 :: Markup
m10 = Markup "top" (Attributes (fromList [(Class, "a"), (Attribute "b","c")])) [MarkupLeaf (Markup "leaf" mempty [MarkupLeaf (Markup "newdeepleaf" mempty [Content "deeper content"])]), Content "textual content"]

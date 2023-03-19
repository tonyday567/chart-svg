{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude
import Test.Tasty                 (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden.Advanced (goldenTest)
import Data.TreeDiff
import Data.Maybe
import qualified Data.TreeDiff.OMap as O
import Chart.Markup
import GHC.Exts
import Chart.Examples
import qualified Data.ByteString              as BS
import FlatParse.Basic
import Chart.Markup.Parser
import Data.Foldable

main :: IO ()
main = defaultMain $ testGroup "tests"
    [ goldenTests
    ]

goldenTests :: TestTree
goldenTests = testGroup "examples" (testExample <$> pathChartOptions)

testExample :: (FilePath, ChartOptions) -> TestTree
testExample (fp, co) = goldenTest fp
        (getMarkupFile fp)
        (pure (markupChartOptions co))
        (\expected actual -> pure (show . ansiWlEditExpr <$> patch expected actual))
        (\_ -> pure ())

getMarkupFile :: FilePath -> IO Markup
getMarkupFile fp = do
        bs <- BS.readFile fp
        case runParser markupP bs of
          OK m _ -> pure m
          Err e -> print e >> fail "parse error"
          Fail -> print ("uncaught parse error" :: BS.ByteString) >> fail "parse error"

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

-- patch testing
printPatchExamples :: IO ()
printPatchExamples = traverse_ (printPatchExample m0) patchExamples

printPatchExample :: Markup -> (String, Markup) -> IO ()
printPatchExample m (s, m') = do
  print s
  case show . ansiWlEditExpr <$> patch m m' of
    Nothing -> putStrLn ("no changes" :: String)
    Just x -> putStrLn x

patchExamples :: [(String, Markup)]
patchExamples =
  [ ("class change", m1)
  , ("delete an attribute", m2)
  , ("insert an attribute", m3)
  , ("change a tag", m4)
  , ("change a markup leaf", m5)
  , ("delete a leaf", m6)
  , ("insert a leaf", m7)
  , ("insert attribute", m8)
  , ("modify content", m9)
  , ("deep leaf insertion", m10)
  ]

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

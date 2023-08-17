{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Chart.Examples
import Chart.Markup
import Data.ByteString qualified as BS
import Data.TreeDiff
import MarkupParse
import MarkupParse.Patch
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden.Advanced (goldenTest)
import Prelude

main :: IO ()
main =
  defaultMain $
    testGroup
      "tests"
      [ goldenTests
      ]

goldenTests :: TestTree
goldenTests = testGroup "examples" (testExample <$> pathChartOptions)

testExample :: (FilePath, ChartOptions) -> TestTree
testExample (fp, co) =
  goldenTest
    fp
    (getMarkupFile fp)
    (pure (normalize $ markupChartOptions co))
    (\expected actual -> pure (show . ansiWlEditExpr <$> patch expected actual))
    (\_ -> pure ())

getMarkupFile :: FilePath -> IO Markup
getMarkupFile fp = do
  bs <- BS.readFile fp
  pure $ normalize $ markup_ Xml bs

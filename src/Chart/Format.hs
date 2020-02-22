{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Chart.Format
  ( FormatN (..),
    defaultFormatN,
    fromFormatN,
    toFormatN,
    fixed,
    comma,
    expt,
    dollar,
    formatN,
    precision,
    formatNs,
  )
where

import Data.Foldable
import Data.List (nub)
import Data.Maybe
import Data.Monoid
import Data.Scientific
import qualified Data.Text as Text
import Data.Text (Text)
import Protolude

data FormatN
  = FormatFixed Int
  | FormatComma Int
  | FormatExpt Int
  | FormatDollar
  | FormatNone
  deriving (Eq, Show, Generic)

defaultFormatN :: FormatN
defaultFormatN = FormatComma 2

fromFormatN :: (IsString s) => FormatN -> s
fromFormatN (FormatFixed _) = "Fixed"
fromFormatN (FormatComma _) = "Comma"
fromFormatN (FormatExpt _) = "Expt"
fromFormatN FormatDollar = "Dollar"
fromFormatN FormatNone = "None"

toFormatN :: (Eq s, IsString s) => s -> Int -> FormatN
toFormatN "Fixed" n = FormatFixed n
toFormatN "Comma" n = FormatComma n
toFormatN "Expt" n = FormatExpt n
toFormatN "Dollar" _ = FormatDollar
toFormatN "None" _ = FormatNone
toFormatN _ _ = FormatNone

fixed :: Int -> Double -> Text
fixed x n = Text.pack $ formatScientific Fixed (Just x) (fromFloatDigits n)

comma :: Int -> Double -> Text
comma n a
  | a < 1000 = fixed n a
  | otherwise = go (floor a) ""
  where
    go :: Int -> Text -> Text
    go x t
      | x < 0 = "-" <> go (- x) ""
      | x < 1000 = Text.pack (show x) <> t
      | otherwise =
        let (d, m) = divMod x 1000
         in go d ("," <> Text.pack (show m))

expt :: Int -> Double -> Text
expt x n = Text.pack $ formatScientific Exponent (Just x) (fromFloatDigits n)

dollar :: Double -> Text
dollar = ("$" <>) . comma 2

formatN :: FormatN -> Double -> Text
formatN (FormatFixed n) x = fixed n x
formatN (FormatComma n) x = comma n x
formatN (FormatExpt n) x = expt n x
formatN FormatDollar x = dollar x
formatN FormatNone x = Text.pack (show x)

-- | Provide formatted text for a list of numbers so that they are just distinguished.  'precision commas 2 ticks' means give the tick labels as much precision as is needed for them to be distinguished, but with at least 2 significant figures, and format Integers with commas.
-- FIXME: busted for round ticks.
precision :: (Int -> Double -> Text) -> Int -> [Double] -> [Text]
precision f n0 xs
  | foldr max 0 xs < 0.01 =
    Text.pack <$> precLoop expt' n0 (fromFloatDigits <$> xs)
  | foldr max 0 xs > 100000 =
    Text.pack <$> precLoop expt' n0 (fromFloatDigits <$> xs)
  | otherwise = precLoop f (fromIntegral n0) xs
  where
    expt' x = formatScientific Exponent (Just x)
    precLoop f' n xs' =
      let s = f' n <$> xs'
       in if s == nub s
            then s
            else precLoop f' (n + 1) xs'

formatNs :: FormatN -> [Double] -> [Text]
formatNs (FormatFixed n) xs = precision fixed n xs
formatNs (FormatComma n) xs = precision comma n xs
formatNs (FormatExpt n) xs = precision expt n xs
formatNs FormatDollar xs = precision (const dollar) 2 xs
formatNs FormatNone xs = Text.pack . show <$> xs

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module Chart.Format
  ( fromFormatN,
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

import Chart.Types
import Data.Foldable
import Data.List (nub)
import Data.Monoid
import Data.Scientific
import qualified Data.Text as Text
import Protolude

fromFormatN :: (IsString s) => FormatN -> s
fromFormatN (FormatFixed _) = "Fixed"
fromFormatN (FormatComma _) = "Comma"
fromFormatN (FormatExpt _) = "Expt"
fromFormatN FormatDollar = "Dollar"
fromFormatN (FormatPercent _) = "Percent"
fromFormatN FormatNone = "None"

toFormatN :: (Eq s, IsString s) => s -> Int -> FormatN
toFormatN "Fixed" n = FormatFixed n
toFormatN "Comma" n = FormatComma n
toFormatN "Expt" n = FormatExpt n
toFormatN "Dollar" _ = FormatDollar
toFormatN "Percent" n = FormatPercent n
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
         in go d ("," <> Text.pack (show' m))
      where
        show' n' = let x' = show n' in (replicate (3 - length x') '0' <> x')

expt :: Int -> Double -> Text
expt x n = Text.pack $ formatScientific Exponent (Just x) (fromFloatDigits n)

dollar :: Double -> Text
dollar = ("$" <>) . comma 2

percent :: Int -> Double -> Text
percent x n = (<> "%") $ fixed x (100 * n)

formatN :: FormatN -> Double -> Text
formatN (FormatFixed n) x = fixed n x
formatN (FormatComma n) x = comma n x
formatN (FormatExpt n) x = expt n x
formatN FormatDollar x = dollar x
formatN (FormatPercent n) x = percent n x
formatN FormatNone x = Text.pack (show x)

-- | Provide formatted text for a list of numbers so that they are just distinguished.  'precision commas 2 ticks' means give the tick labels as much precision as is needed for them to be distinguished, but with at least 2 significant figures, and format Integers with commas.
precision :: (Int -> Double -> Text) -> Int -> [Double] -> [Text]
precision f n0 xs =
  precLoop f (fromIntegral n0) xs
  where
    precLoop f' n xs' =
      let s = f' n <$> xs'
       in if s == nub s || n > 4
            then s
            else precLoop f' (n + 1) xs'

formatNs :: FormatN -> [Double] -> [Text]
formatNs (FormatFixed n) xs = precision fixed n xs
formatNs (FormatComma n) xs = precision comma n xs
formatNs (FormatExpt n) xs = precision expt n xs
formatNs FormatDollar xs = precision (const dollar) 2 xs
formatNs (FormatPercent n) xs = precision percent n xs
formatNs FormatNone xs = Text.pack . show <$> xs

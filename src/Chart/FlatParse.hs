{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

-- | Lower-level flatparse parsers
module Chart.FlatParse
  (
    runParserEither,
    ws,
    int,
    double,
    signed,
    quoted,
    point,
    Spline (..),
    splineP,
    rectP,
    boolP,
    nonEmptyP,
  points, pointPair, pointPairs)
where

import Data.Bool
import Data.ByteString hiding (empty, head, length, map, zip, zipWith)
import Data.Char hiding (isDigit)
import Data.List.NonEmpty
import FlatParse.Basic hiding (cut, lines)
import GHC.Generics
import NumHask.Space
import Prelude hiding (replicate)

-- $setup
-- >>> import DotParse
-- >>> import FlatParse.Basic

-- | run a Parser, erroring on Fail or Err
runParserEither :: Parser ByteString a -> ByteString -> Either ByteString (a, ByteString)
runParserEither p b = case runParser p b of
  OK r leftovers -> Right (r, leftovers)
  Fail -> Left "Fail"
  Err e -> Left e

-- * parsing

-- | consume whitespace
ws :: Parser e ()
ws = $(switch [| case _ of
  " "  -> ws
  "\n" -> ws
  "\t" -> ws
  "\r" -> ws
  _    -> pure () |])

digit :: Parser e Int
digit = (\c -> ord c - ord '0') <$> satisfyASCII isDigit

-- | (unsigned) Int parser
int :: Parser e Int
int = do
  (place, n) <- chainr (\n (!place, !acc) -> (place * 10, acc + place * n)) digit (pure (1, 0))
  case place of
    1 -> empty
    _ -> pure n

digits :: Parser e (Int, Int)
digits = chainr (\n (!place, !acc) -> (place * 10, acc + place * n)) digit (pure (1, 0))

-- |
-- >>> runParser double "1.234x"
-- OK 1.234 "x"
--
-- >>> runParser double "."
-- Fail
--
-- >>> runParser double "123"
-- OK 123.0 ""
--
-- >>> runParser double ".123"
-- OK 0.123 ""
--
-- >>> runParser double "123."
-- OK 123.0 ""
double :: Parser e Double
double = do
  (placel, nl) <- digits
  withOption
    ($(char '.') *> digits)
    ( \(placer, nr) ->
        case (placel, placer) of
          (1, 1) -> empty
          _ -> pure $ fromIntegral nl + fromIntegral nr / fromIntegral placer
    )
    ( case placel of
        1 -> empty
        _ -> pure $ fromIntegral nl
    )

minus :: Parser e ()
minus = $(char '-')

-- |
-- >>> runParser (signed double) "-1.234x"
-- OK (-1.234) "x"
signed :: Num b => Parser e b -> Parser e b
signed p = do
  m <- optional minus
  case m of
    Nothing -> p
    Just () -> negate <$> p

quote :: Parser e ()
quote = $(char '"')

quoted :: Parser e ByteString
quoted = quote *> byteStringOf (some_ (satisfy_ (/= '"'))) <* quote

comma :: Parser e ()
comma = $(char ',')

-- | comma separated Point
point :: Parser e (Point Double)
point = Point <$> double <*> (comma *> double)

-- | dot specification of a cubic spline (and an arrow head which is ignored here)
data Spline = Spline {splineEnd :: Maybe (Point Double), splineStart :: Maybe (Point Double), splineP1 :: Point Double, splineTriples :: [(Point Double, Point Double, Point Double)]} deriving (Eq, Show, Generic)

-- |
-- http://www.graphviz.org/docs/attr-types/splineType/
splineP :: Parser e Spline
splineP =
  Spline
    <$> optional ($(string "e,") *> point)
    <*> optional ($(string "s") *> point)
    <*> point
    <*> some ((,,) <$> point <*> point <*> point)

-- | comma separated rectangle or bounding box
rectP :: Parser e (Rect Double)
rectP = do
  x <- double
  _ <- comma
  y <- double
  _ <- comma
  z <- double
  _ <- comma
  w <- double
  pure $ Rect x z y w

-- | true | false
boolP :: Parser e Bool
boolP =
  (True <$ $(string "true"))
    <|> (False <$ $(string "false"))

-- | NonEmpty version of many
nonEmptyP :: Parser e a -> Parser e () -> Parser e (NonEmpty a)
nonEmptyP p sep = do
  s <- p
  xs <- many (optional sep *> p)
  pure (s :| xs)

points :: Parser e [Point Double]
points = toList <$> nonEmptyP point comma

pointPair :: Parser e (Point Double, Point Double)
pointPair = (,) <$> point <* comma <*> point

pointPairs :: Parser e [(Point Double, Point Double)]
pointPairs = toList <$> nonEmptyP pointPair comma

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
{-# LANGUAGE PatternSynonyms #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

-- | Lower-level flatparse parsers
module Chart.FlatParse
  (
    runParserMaybe,
    runParserEither,
    testParser,
    Expected(..),
    Error(..),
    prettyError,
    cut,
    cut',
    ws,
    ws_,
    wss,
    sep,
    bracketed,
    wrapped,
    digit,
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
import Data.ByteString ( ByteString )
import Data.Char hiding (isDigit)
import Data.List.NonEmpty ( NonEmpty(..) )
import FlatParse.Basic hiding (cut)
import GHC.Generics
import NumHask.Space ( Point(..), Rect(..), pattern Rect)
import Prelude hiding (replicate)
import GHC.Exts
import Data.List (replicate)
-- $setup
-- >>> import Chart.FlatParse
-- >>> import FlatParse.Basic

-- * parser error model taken from flatparse examples

-- | An expected item which is displayed in error messages.
data Expected
  = Msg String  -- ^ An error message.
  | Lit String  -- ^ A literal expected thing.
  deriving (Eq, Show, Ord)

instance IsString Expected where fromString = Lit

-- | A parsing error.
data Error
  = Precise Pos Expected     -- ^ A precisely known error, like leaving out "in" from "let".
  | Imprecise Pos [Expected] -- ^ An imprecise error, when we expect a number of different things,
                             --   but parse something else.
  deriving Show

errorPos :: Error -> Pos
errorPos (Precise p _)   = p
errorPos (Imprecise p _) = p

-- | Merge two errors. Inner errors (which were thrown at points with more consumed inputs)
--   are preferred. If errors are thrown at identical input positions, we prefer precise errors
--   to imprecise ones.
--
--   The point of prioritizing inner and precise errors is to suppress the deluge of "expected"
--   items, and instead try to point to a concrete issue to fix.
merge :: Error -> Error -> Error
merge e e' = case (errorPos e, errorPos e') of
  (p, p') | p < p' -> e'
  (p, p') | p > p' -> e
  (p, _)          -> case (e, e') of
    (Precise{}      , _               ) -> e
    (_              , Precise{}       ) -> e'
    (Imprecise _ es , Imprecise _ es' ) -> Imprecise p (es <> es')
{-# noinline merge #-} -- merge is "cold" code, so we shouldn't inline it.

-- | Pretty print an error. The `B.ByteString` input is the source file. The offending line from the
--   source is displayed in the output.
prettyError :: ByteString -> Error -> String
prettyError b e =

  let pos :: Pos
      pos      = case e of Imprecise pos e -> pos
                           Precise pos e   -> pos
      ls       = linesUtf8 b
      (l, c)   = head $ posLineCols b [pos]
      line     = if l < length ls then ls !! l else ""
      line'     = if length line > 300 then Prelude.drop (c-50) $ Prelude.take (c+50) line else line
      linum    = show l
      lpad     = fmap (const ' ') linum

      expected (Lit s) = show s
      expected (Msg s) = s

      err (Precise _ e)    = expected e
      err (Imprecise _ es) = imprec es

      imprec :: [Expected] -> String
      imprec []     = error "impossible"
      imprec [e]    = expected e
      imprec (e:es) = (expected e <> go es) where
        go []     = ""
        go [e]    = (" or " <> expected e)
        go (e:es) = (", " <> expected e <> go es)

  in (show l <> (":" <> (show c <> (":\n" <> (lpad <> ("|\n" <> (linum <> ("| " <> (line' <> ("\n" <> (lpad <> ("| " <> (replicate c ' ' <> ("^\n" <> ("parse error: expected " <> err e)))))))))))))))

-- | Imprecise cut: we slap a list of items on inner errors.
cut :: Parser Error a -> [Expected] -> Parser Error a
cut p es = do
  pos <- getPos
  cutting p (Imprecise pos es) merge

-- | Precise cut: we propagate at most a single error.
cut' :: Parser Error a -> Expected -> Parser Error a
cut' p e = do
  pos <- getPos
  cutting p (Precise pos e) merge

-- | run a Parser, Nothing on falure
runParserMaybe :: Parser e a -> ByteString -> Maybe a
runParserMaybe p b = case runParser p b of
  OK r _ -> Just r
  Fail -> Nothing
  Err _ -> Nothing

-- | Run parser, Left error on failure.
runParserEither :: Parser Error a -> ByteString -> Either ByteString a
runParserEither p bs = case runParser p bs of
    Err e  -> Left $ strToUtf8 $ prettyError bs e
    OK a _ -> Right a
    Fail   -> Left "uncaught parse error"

-- | Run parser, print pretty error on failure.
testParser :: Show a => Parser Error a -> String -> IO ()
testParser p str = case fromString str of
  b -> case runParser p b of
    Err e  -> putStrLn $ prettyError b e
    OK a _ -> print a
    Fail   -> putStrLn "uncaught parse error"

-- * parsing

isWs :: Char -> Bool
isWs x =
  x == ' ' ||
  x == '\n' ||
  x == '\t' ||
  x == '\r'

-- | single whitespace
ws :: Parser e Char
ws = satisfy isWs

-- | Consume whitespace.
ws_ :: Parser e ()
ws_ = $(switch [| case _ of
  " "  -> ws_
  "\n" -> ws_
  "\t" -> ws_
  "\r" -> ws_
  _    -> pure () |])

-- | multiple whitespace
wss :: Parser e ByteString
wss = byteStringOf $ some ws

-- | some with a separator
sep :: Parser e s -> Parser e a -> Parser e [a]
sep s p = (:) <$> p <*> many (s *> p)

bracketed :: Parser e b -> Parser e b -> Parser e a -> Parser e a
bracketed o c p = o *> p <* c

wrapped :: Parser e () -> Parser e a -> Parser e a
wrapped x p = bracketed x x p

digit :: Parser e Int
digit = (\c -> ord c - ord '0') <$> satisfyAscii isDigit

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
quoted = quote *> byteStringOf (skipSome (skipSatisfy (/= '"'))) <* quote

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
  Rect x z y <$> double

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

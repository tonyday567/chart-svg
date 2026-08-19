{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Simple parser implementation to replace flatparse
module Chart.Parse
  ( -- * Parser type and running
    Parser,
    Result (..),
    runParser,

    -- * Primitives
    satisfy,
    satisfyAscii,
    anyChar,
    eof,

    -- * Combinators
    empty,
    (<|>),
    optional,
    many,
    some,
    skipMany,
    chainr,
    withOption,

    -- * String matching
    char,
    byteString,

    -- * Utilities
    isDigit,
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (MonadPlus (..), ap, void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.Char (isDigit)
import Prelude hiding (replicate)

-- * Types

data Result e a
  = OK a ByteString
  | Fail
  | Err e
  deriving (Show, Functor)

newtype Parser e a = Parser
  { runParser :: ByteString -> Result e a
  }
  deriving (Functor)

instance Applicative (Parser e) where
  pure a = Parser $ \s -> OK a s
  (<*>) = ap

instance Monad (Parser e) where
  Parser p >>= f = Parser $ \s -> case p s of
    OK a s' -> runParser (f a) s'
    Fail -> Fail
    Err e -> Err e

instance Alternative (Parser e) where
  empty = Parser $ const Fail
  Parser p <|> Parser q = Parser $ \s -> case p s of
    Fail -> q s
    ok -> ok

instance MonadPlus (Parser e)

-- * Primitives

satisfy :: (Char -> Bool) -> Parser e Char
satisfy predicate = Parser $ \s ->
  if B.null s
    then Fail
    else
      let c = BC.head s
       in if predicate c
            then OK c (B.tail s)
            else Fail

satisfyAscii :: (Char -> Bool) -> Parser e Char
satisfyAscii = satisfy

anyChar :: Parser e Char
anyChar = Parser $ \s ->
  if B.null s
    then Fail
    else OK (BC.head s) (B.tail s)

eof :: Parser e ()
eof = Parser $ \s ->
  if B.null s
    then OK () s
    else Fail

-- * Combinators

optional :: Parser e a -> Parser e (Maybe a)
optional p = (Just <$> p) <|> pure Nothing

skipMany :: Parser e a -> Parser e ()
skipMany p = go
  where
    go = (p *> go) <|> pure ()

chainr :: (a -> b -> b) -> Parser e a -> Parser e b -> Parser e b
chainr f p z = go
  where
    go = (f <$> p <*> go) <|> z

withOption :: Parser e a -> (a -> Parser e b) -> Parser e b -> Parser e b
withOption p f def = (p >>= f) <|> def

-- * String matching

char :: Char -> Parser e ()
char c = void $ satisfy (== c)

byteString :: ByteString -> Parser e ()
byteString bs = Parser $ \s ->
  if bs `B.isPrefixOf` s
    then OK () (B.drop (B.length bs) s)
    else Fail

-- * Repetition

many :: Parser e a -> Parser e [a]
many p = go
  where
    go = ((:) <$> p <*> go) <|> pure []

some :: Parser e a -> Parser e [a]
some p = (:) <$> p <*> many p

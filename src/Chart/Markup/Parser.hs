{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use unwords" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE TupleSections #-}
{-# HLINT ignore "Use foldMap" #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# HLINT ignore "Use isAsciiLower" #-}
{-# HLINT ignore "Use isAsciiUpper" #-}
{-# LANGUAGE MagicHash #-}
{-# HLINT ignore "Use isDigit" #-}
{-# HLINT ignore "Use void" #-}
{-# HLINT ignore "Use <$" #-}
{-# HLINT ignore "Use infix" #-}

-- https://www.w3.org/TR/SVG2/text.html#TextElement
-- https://www.w3.org/TR/xml/#NT-content
module Chart.Markup.Parser where

import Prelude
import Data.ByteString ( ByteString, readFile)
import GHC.Generics
import FlatParse.Basic hiding (Parser)
import System.Directory
import Chart.Markup
import qualified FlatParse.Basic.Text as T
import Chart.FlatParse


-- $setup
--
-- >>> :set -XOverloadedLabels
-- >>> :set -XOverloadedStrings
-- >>> import Chart
-- >>> import Optics.Core

-- * special chars
-- | opening tag char
lt :: Parser ()
lt = $(char '<')

-- | closing tag char
gt :: Parser ()
gt = $(char '>')

-- | self-closing tag
gtc :: Parser ()
gtc = $(string "/>")

-- | open closer tag
oct = $(string "</")

sp :: Parser ()
sp = $(char ' ')

amp :: Parser ()
amp = $(char '&')

caret :: Parser ()
caret = $(char '^')

slash :: Parser ()
slash = $(char '/')

-- | name separator [6] [8]
sep :: Parser s -> Parser a -> Parser [a]
sep s p = (:) <$> p <*> many (s *> p)


-- | xml production [3]
wss :: Parser String
wss = many ws

-- | TODO: is this more efficient?
wss' :: Parser ByteString
wss' = withSpan (some ws) (\_ s -> unsafeSpanToByteString s)

sq :: ParserT st e ()
sq = $(char '\'')

dq :: ParserT st e ()
dq = $(char '"')

bracketed :: Parser b -> Parser b -> Parser a -> Parser a
bracketed o c p = o *> p <* c

wrapped :: Parser () -> Parser a -> Parser a
wrapped x p = bracketed x x p

wrappedQNoGuard :: Parser a -> Parser a
wrappedQNoGuard p = wrapped dq p <|> wrapped sq p

-- | guard check for closing character
wrappedDq :: Parser ByteString
wrappedDq = wrapped dq (byteStringOf $ many (T.satisfy (/= '"')))

wrappedSq :: Parser ByteString
wrappedSq = wrapped sq (byteStringOf $ many (T.satisfy (/= '\'')))

wrappedQ :: Parser ByteString
wrappedQ =
  wrappedDq <|>
  wrappedSq

-- xml production [25]
eq :: Parser ()
eq = optional wss *> $(char '=') <* optional wss

-- * xml-style name production
-- [2]
xmlChar :: Parser Char
xmlChar = anyChar

-- [4]
nameStartChar :: Parser Char
nameStartChar = fusedSatisfy isLatinLetter isNameStartChar isNameStartChar isNameStartChar

isNameStartChar :: Char -> Bool
isNameStartChar x =
  (x >= 'a' && x <= 'z') ||
  (x >= 'A' && x <= 'Z') ||
  (x == ':') || (x == '_') ||
  (x >= '\xC0' && x <= '\xD6') ||
  (x >= '\xD8' && x <= '\xF6') ||
  (x >= '\xF8' && x <= '\x2FF') ||
  (x >= '\x370' && x <= '\x37D') ||
  (x >= '\x37F' && x <= '\x1FFF') ||
  (x >= '\x200C' && x <= '\x200D') ||
  (x >= '\x2070' && x <= '\x218F') ||
  (x >= '\x2C00' && x <= '\x2FEF') ||
  (x >= '\x3001' && x <= '\xD7FF') ||
  (x >= '\xF900' && x <= '\xFDCF') ||
  (x >= '\xFDF0' && x <= '\xFFFD') ||
  (x >= '\x10000' && x <= '\xEFFFF')

-- [4a]
nameChar :: Parser Char
nameChar = fusedSatisfy isNameCharAscii isNameCharExt isNameCharExt isNameCharExt

isNameCharAscii :: Char -> Bool
isNameCharAscii x =
  (x >= 'a' && x <= 'z') ||
  (x >= 'A' && x <= 'Z') ||
  (x >= '0' && x <= '9') ||
  (x == ':') || (x == '_') ||
  (x == '-') || (x == '.')

isNameCharExt :: Char -> Bool
isNameCharExt x =
  (x >= 'a' && x <= 'z') ||
  (x >= 'A' && x <= 'Z') ||
  (x >= '0' && x <= '9') ||
  (x == ':') || (x == '_') ||
  (x == '-') || (x == '.') ||
  (x == '\xB7') ||
  (x >= '\xC0' && x <= '\xD6') ||
  (x >= '\xD8' && x <= '\xF6') ||
  (x >= '\xF8' && x <= '\x2FF') ||
  (x >= '\x300' && x <= '\x36F') ||
  (x >= '\x370' && x <= '\x37D') ||
  (x >= '\x37F' && x <= '\x1FFF') ||
  (x >= '\x200C' && x <= '\x200D') ||
  (x >= '\x203F' && x <= '\x2040') ||
  (x >= '\x2070' && x <= '\x218F') ||
  (x >= '\x2C00' && x <= '\x2FEF') ||
  (x >= '\x3001' && x <= '\xD7FF') ||
  (x >= '\xF900' && x <= '\xFDCF') ||
  (x >= '\xFDF0' && x <= '\xFFFD') ||
  (x >= '\x10000' && x <= '\xEFFFF')

-- [5]
--
-- >>> runParser xmlName "markup>"
-- OK "markup" ">"
xmlName :: Parser ByteString
xmlName = byteStringOf (nameStartChar >> many nameChar)

-- [6]
xmlNames :: Parser [ByteString]
xmlNames = sep sp xmlName

-- * attributes
xmlAtt :: Parser (ByteString, ByteString)
xmlAtt = (,) <$> (xmlName <* eq) <*> wrappedQ

-- [40]
openTag :: Parser (ByteString, [(ByteString, ByteString)])
openTag =
  lt *> ((,) <$> xmlName <*> many (wss *> xmlAtt) <* optional wss) <* gt

-- [42]
closeTag :: Parser ByteString
closeTag = oct *> xmlName <* optional wss <* gt

-- [44]
emptyElemTag ::  Parser (ByteString, [(ByteString, ByteString)])
emptyElemTag =
  lt *> ((,) <$> xmlName <*> many (wss *> xmlAtt) <* optional wss) <*  gtc

-- * comments
xmlCommentOpen = $(string "<!--")
xmlCommentClose = $(string "-->")

xmlCharNotMinus = byteStringOf $ satisfy (/= '-')

xmlMinusPlusChar = byteStringOf $ $(char '-') *> xmlCharNotMinus

xmlComment = xmlCommentOpen *> byteStringOf (many (xmlCharNotMinus <|> xmlMinusPlusChar)) <* xmlCommentClose

-- * CDATA
-- https://stackoverflow.com/questions/55465703/is-cdata-necessary-when-styling-an-inline-svg
-- [18]
xmlCDSect = xmlCDStart *> xmlCData <* xmlCDEnd

-- [19]
xmlCDStart = $(string "<![CDATA[")

-- [20]
xmlCData = byteStringOf (many xmlChar)

-- [21]
xmlCDEnd = $(string "]]>")

-- * prolog
xmlXml =
  ($(char 'X') <|> $(char 'x')) *>
  ($(char 'M') <|> $(char 'm')) *>
  ($(char 'L') <|> $(char 'l'))

-- [22]
xmlProlog :: Parser ByteString
xmlProlog = byteStringOf $
  optional xmlXMLDecl >>
  many xmlMisc >>
  optional (xmlDoctypedecl >> optional xmlMisc)

-- [23]
xmlXMLDecl = byteStringOf $
  $(string "<?xml") >>
  xmlVersionInfo >>
  optional xmlEncodingDecl >>
  optional wssDDecl >>
  optional wss >>
  $(string "?>")

-- xml production [24]
xmlVersionInfo :: Parser ByteString
xmlVersionInfo = byteStringOf $ wss >> $(string "version") >> eq >> wrappedQNoGuard xmlVersionNum

-- [26]   	VersionNum	   ::=   	'1.' [0-9]+
xmlVersionNum :: Parser ByteString
xmlVersionNum =
  byteStringOf ($(string "1.") >> some (satisfy isDigit))

-- [27]   	Misc	   ::=   	Comment | PI | S
data XmlMiscType = XMiscComment | XMiscPI | XMiscS deriving (Generic, Show)

data XmlMisc = XmlMisc { xmiscType :: XmlMiscType, xmiscContent :: ByteString } deriving (Generic, Show)

xmlMisc :: Parser XmlMisc
xmlMisc =
  (XmlMisc XMiscComment <$> xmlComment) <|>
  -- (XmlMisc XMiscPI <$> xmlPI) <|>
  (XmlMisc XMiscS <$> byteStringOf wss)

-- [28]
xmlDoctypedecl = byteStringOf $
  $(string "<!DOCTYPE") >>
  wss >>
  xmlName >>
  -- optional (wss >> xmlExternalID) >>
  optional wss >>
  optional bracketedSB >> optional wss >>
  $(char '>')

bracketedSB = bracketed $(char '[') $(char ']') (many (satisfy (/= ']')))

-- [32]
wssDDecl :: Parser ByteString
wssDDecl = byteStringOf $
  wss *> $(string "standalone") *> eq *> xmlYesNo

xmlYesNo :: Parser ByteString
xmlYesNo = wrappedQNoGuard (byteStringOf $ $(string "yes") <|> $(string "no"))

-- [33] - [38] removed

-- [77]
xmlTextDecl :: Parser ByteString
xmlTextDecl = byteStringOf $
  $(string "<?xml") >>
  optional xmlVersionInfo >>
  xmlEncodingDecl >>
  optional wss >>
  $(string "?>")

-- | xml production [80]
xmlEncodingDecl :: Parser ByteString
xmlEncodingDecl = wss *> $(string "encoding") *> eq *> wrappedQNoGuard xmlEncName

-- [81]
xmlEncName :: Parser ByteString
xmlEncName = byteStringOf (satisfyAscii isLatinLetter >> many (satisfyAscii (\x -> isLatinLetter x || isDigit x || elem x ("._-" :: [Char]) )))

-- main Parser

-- [1]
data XmlDocument = XmlDocument ByteString Markup [XmlMisc]

xmlDocument :: Parser XmlDocument
xmlDocument = XmlDocument <$> xmlProlog <*> xmlElement <*> many xmlMisc

xmlElement :: Parser Markup
xmlElement =
  ((\(n,as) -> Markup n (mconcat $ inject <$> as) mempty) <$> emptyElemTag) <|>
  -- no close tag = open tag test
  ((\(n,as) c _ -> Markup n (mconcat $ inject <$> as) c) <$> openTag <*> many xmlContent <*> closeTag)

xmlContent :: Parser Content
xmlContent =
  (MarkupLeaf <$> xmlElement) <|>
  (CDSect <$> xmlCDSect ) <|>
  (Comment <$> xmlComment) <|>
  (Content <$> byteStringOf (some (satisfy (/= '<'))))

isoMarkupParse :: ByteString -> Bool
isoMarkupParse x = case runParser xmlElement x of
  OK l "" -> printMarkup l == x
  _ -> False

parseOk :: Parser a -> ByteString -> Bool
parseOk p x = case runParser p x of
  OK _ "" -> True
  OK _ _ -> False
  _ -> False

-- >>> os <- olds'
-- >>> osb <- oldChecks' os
-- >>> osb
-- [True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True]
olds' :: IO [FilePath]
olds' =  fmap (filter (/= ".DS_Store")) (listDirectory "../../git/chart-svg/other")

oldChecks' fs = traverse (fmap (parseOk xmlElement) . Data.ByteString.readFile) $ ("../../git/chart-svg/other/"<>) <$> fs

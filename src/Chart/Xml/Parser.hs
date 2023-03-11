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

module Chart.Xml.Parser where

import Prelude
import Data.ByteString ( ByteString, readFile)
import GHC.Generics
import FlatParse.Basic
import System.Directory
import Data.Bool
import Chart.Xml

-- $setup
--
-- >>> :set -XOverloadedLabels
-- >>> :set -XOverloadedStrings
-- >>> import Chart
-- >>> import Optics.Core

-- * parsing
-- | opening tag char
opener :: Parser e ()
opener = $(char '<')

-- | closing tag char
closer :: Parser e ()
closer = $(char '>')

-- | key separator
blank_ :: Parser e ()
blank_ = $(char ' ')

-- | word
word :: Parser e ByteString
word = byteStringOf (skipSome (skipSatisfy (\x -> x /= ' ' && x /= '>' && x /= '/')))

key :: Parser e ByteString
key = byteStringOf (skipSome (skipSatisfy (\x -> x /= '=' && x /= '>')))

quote :: Parser e ()
quote = $(char '"')

quoted :: Parser e ByteString
quoted = quote *> byteStringOf (skipSome (skipSatisfy (/= '"'))) <* quote

equals :: Parser e ()
equals = $(char '=')

attP :: Parser e (ByteString, ByteString)
attP = (,) <$> (key <* equals) <*> quoted

ws :: Parser e ()
ws = $(switch [| case _ of
  " "  -> ws
  "\n" -> ws
  "\t" -> ws
  "\r" -> ws
  _    -> pure () |])

attsP :: Parser e Attributes
attsP = mconcat . fmap inject <$> many (attP <* ws)

openertag :: Parser e (ByteString, Attributes)
openertag = (,) <$> (opener *> (word <* ws)) <*> (attsP <* closer)

slash :: Parser e ()
slash = $(char '/')

tagP :: Parser e (ByteString, Attributes, TagStatus)
tagP = $(switch [| case _ of
  "</"  -> pure (mempty, mempty, TagClosed)
  "<" -> (,,) <$> (word <* ws) <*> (attsP <* ws) <*> suffixTag
  |])

data TagStatus = TagOpen | TagClosed deriving (Eq, Show, Generic)

suffixTag :: Parser e TagStatus
suffixTag = $(switch [| case _ of
  "/>"  -> pure TagClosed
  ">" -> pure TagOpen
  |])

closertag :: Parser e ()
closertag = opener *> slash *> skipMany (skipSatisfy (/= '>')) <* closer

notLT :: Parser e ByteString
notLT = byteStringOf (skipMany (skipSatisfy (/= '<'))) <|> pure mempty

svgParser :: Parser e Svg
svgParser = do
  (n, as, st) <- tagP
  case st of
    TagOpen -> do
      xs <- many svgParser
      c <- notLT
      _ <- closertag
      pure (Svg n as xs c)
    TagClosed -> pure (Svg n as mempty mempty)


isoSvgParse :: ByteString -> Bool
isoSvgParse x = case runParser svgParser x of
  OK l "" -> svgPrinter l == x
  _ -> False

-- >>> iso x1
-- True
--
-- >>> isoRose x1
-- True
x1 :: ByteString
x1 = "<svg><style>style content</style><g class=\"group\"></g><g class=\"group>\"></g></svg>"

-- >>> x2 <- Data.ByteString.readFile "other/unit.svg"
-- True

-- >>> xOther
-- True
xOther :: IO Bool
xOther = fmap and $ (mapM (fmap isoSvgParse . (Data.ByteString.readFile . ("other/"<>))) . filter (/= ".DS_Store")) =<< listDirectory "other"

-- https://www.w3.org/TR/xml/#NT-content

-- [1]
data XmlDocument = XmlDocument { xdProlog :: ByteString, xdElement :: XmlElement, xdMiscs :: [XmlMisc] } deriving (Show, Generic)

xmlDocument :: Parser e XmlDocument
xmlDocument = XmlDocument <$> xmlProlog <*> xmlElement <*> many xmlMisc

-- [2]
xmlChar :: Parser e Char
xmlChar = anyChar

-- parsing using the XML definition names
-- [3]
xmlWs :: Parser e ()
xmlWs = $(switch [| case _ of
  " "  -> pure ()
  "\n" -> pure ()
  "\t" -> pure ()
  "\r" -> pure ()
 |])

-- forgetting whitespace
xmlS = pure () <* some xmlWs

-- [4]
nameStartChar :: Parser e Char
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
nameChar :: Parser e Char
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
-- >>> runParser xmlName "svg>"
-- OK "svg" ">"
xmlName :: Parser e ByteString
xmlName = byteStringOf (nameStartChar >> many nameChar)

-- [6]
xmlNames :: Parser e [ByteString]
xmlNames = sep xmlName

-- [7]
xmlNmtoken :: Parser e ByteString
xmlNmtoken = byteStringOf (some nameChar)

-- [8]
xmlNmtokens :: Parser e [ByteString]
xmlNmtokens = sep xmlNmtoken

sep :: Parser e a -> Parser e [a]
sep p = (:) <$> p <*> many ($(char ' ') *> p)

sq :: ParserT st e ()
sq = $(char '\'')

dq :: ParserT st e ()
dq = $(char '"')

wrap :: Parser e () -> Parser e a -> Parser e a
wrap x p = x *> p <* x


wrapq' :: Parser e a -> Parser e a
wrapq' p = wrap dq p <|> wrap sq p

wrapDq :: Parser e ByteString
wrapDq = dq *> byteStringOf (many (satisfy (/= '"'))) <* dq

wrapSq :: Parser e ByteString
wrapSq = sq *> byteStringOf (many (satisfy (/= '\''))) <* sq

wrapq :: Parser e ByteString
wrapq =
  wrapDq <|>
  wrapSq

isDelEntityDq x =
  (x == '^') || (x == '%') ||
  (x == '&') || (x == '"')

isDelEntitySq x =
  (x == '^') || (x == '%') ||
  (x == '&') || (x == '"')

-- [9]
xmlEntityValue :: Parser e ByteString
xmlEntityValue =
  wrap dq (byteStringOf $ many (byteStringOf (satisfyAscii isDelEntityDq) <|> byteStringOf xmlPEReference <|> byteStringOf xmlReference)) <|>
  wrap sq (byteStringOf $ many (byteStringOf (satisfyAscii isDelEntitySq) <|> byteStringOf xmlPEReference <|> byteStringOf xmlReference))

-- [10]
isDelAttDq :: Char -> Bool
isDelAttDq x =
  (x == '^') || (x == '<') ||
  (x == '&') || (x == '"')

isDelAttSq :: Char -> Bool
isDelAttSq x =
  (x == '^') || (x == '<') ||
  (x == '&') || (x == '\'')

xmlAttValue :: Parser e ByteString
xmlAttValue =
  wrap dq (byteStringOf $ many (byteStringOf (satisfyAscii isDelAttDq) <|> byteStringOf xmlReference)) <|>
  wrap sq (byteStringOf $ many (byteStringOf (satisfyAscii isDelAttSq) <|> byteStringOf xmlReference))

-- [11]
xmlSystemLiteral :: Parser e ByteString
xmlSystemLiteral =
  wrap dq (byteStringOf $ many ($(char '"') <|> $(char '^'))) <|>
  wrap sq (byteStringOf $ many ($(char '^') <|> $(char '\'')))

-- [12]
xmlPubidLiteral :: Parser e ByteString
xmlPubidLiteral =
  wrap dq (byteStringOf $ many xmlPubidChar) <|>
  wrap sq (byteStringOf $ many xmlPubidCharNotSq)

-- [13]
xmlPubidChar = satisfyAscii isPubidChar

isPubidChar x =
  x == '\x20' ||
  x == '\xD' ||
  x == '\xA' ||
  isLatinLetter x ||
  isDigit x ||
  elem x ("-'()+,./:=?;!*#@$_%" :: [Char])

xmlPubidCharNotSq = satisfyAscii isPubidCharNotSq
isPubidCharNotSq x =
  x == '\x20' ||
  x == '\xD' ||
  x == '\xA' ||
  isLatinLetter x ||
  isDigit x ||
  elem x ("-()+,./:=?;!*#@$_%" :: [Char])

-- [14]
-- CharData	   ::=   	[^<&]* - ([^<&]* ']]>' [^<&]*)

xmlNotCharData :: Parser e ()
xmlNotCharData =
  pure ()
   <* many (satisfyAscii isCharData)
   <* xmlCDEnd
   <* many (satisfyAscii isCharData)

xmlCharData :: Parser e ()
xmlCharData =
  withOption xmlNotCharData (const failed)
  (pure () <* many (satisfyAscii isCharData))

isCharData x =
  (x == '^') || (x == '<') || (x == '&')

-- [15]   	Comment	   ::=   	'<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'

xmlCommentOpen = $(string "<!--")
xmlCommentClose = $(string "-->")

xmlCharNotMinus = byteStringOf $ satisfy (/= '-')

xmlMinusPlusChar = byteStringOf $ $(char '-') *> xmlCharNotMinus

xmlComment = xmlCommentOpen *> byteStringOf (many (xmlCharNotMinus <|> xmlMinusPlusChar)) <* xmlCommentClose

-- [16]
xmlPI = $(string "<?") *> xmlPITarget <* optional xmlS <* $(string "?>")

xmlXml =
  ($(char 'X') <|> $(char 'x')) *>
  ($(char 'M') <|> $(char 'm')) *>
  ($(char 'L') <|> $(char 'l'))

-- [17]
xmlPITarget =
  (failed <* xmlXml) <|>
  xmlName

-- [18]
xmlCDSect = xmlCDStart *> xmlCData <* xmlCDEnd

-- [19]
xmlCDStart = $(string "<![CDATA[")

-- [20]
xmlCData = byteStringOf (many xmlChar)

-- [21]
xmlCDEnd = $(string "]]>")

-- [22]   	prolog	   ::=   	XMLDecl? Misc* (doctypedecl Misc*)?
xmlProlog = byteStringOf $
  optional xmlXMLDecl >>
  many xmlMisc >>
  optional (xmlDoctypedecl >> optional xmlMisc)


-- [23]   	XMLDecl	   ::=   	'<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
xmlXMLDecl = byteStringOf $
  $(string "<?xml") >>
  xmlVersionInfo >>
  optional xmlEncodingDecl >>
  optional xmlSDDecl >>
  optional xmlS >>
  $(string "?>")



-- [24]   	VersionInfo	   ::=   	S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
xmlVersionInfo = byteStringOf $ xmlS >> $(string "version") >> xmlEq >> (wrap dq xmlVersionNum <|> wrap sq xmlVersionNum)


-- [25]   	Eq	   ::=   	S? '=' S?
xmlEq = optional xmlS *> $(char '=') <* optional xmlS


-- [26]   	VersionNum	   ::=   	'1.' [0-9]+
xmlVersionNum :: Parser e ByteString
xmlVersionNum =
  byteStringOf ($(string "1.") >> some (satisfy isDigit))


-- [27]   	Misc	   ::=   	Comment | PI | S

data XmlMiscType = XMiscComment | XMiscPI | XMiscS deriving (Generic, Show)

data XmlMisc = XmlMisc { xmiscType :: XmlMiscType, xmiscContent :: ByteString } deriving (Generic, Show)

xmlMisc :: Parser e XmlMisc
xmlMisc =
  (XmlMisc XMiscComment <$> xmlComment) <|>
  (XmlMisc XMiscPI <$> xmlPI) <|>
  (XmlMisc XMiscS <$> byteStringOf xmlS)

-- [28]
xmlDoctypedecl = byteStringOf $
  $(string "<!DOCTYPE") >>
  xmlS >>
  xmlName >>
  optional (xmlS >> xmlExternalID) >>
  optional xmlS >>
  optional ($(char '[') >> xmlIntSubset >> $(char ']') >> optional xmlS) >>
  $(char '>')

-- [28a]
xmlDeclSep = byteStringOf $ byteStringOf xmlPEReference <|> byteStringOf xmlS

-- [28b]
xmlIntSubset = byteStringOf $ many (xmlMarkupdecl <|> xmlDeclSep)

-- [29]
xmlMarkupdecl = byteStringOf $
  xmlElementdecl <|>
  xmlAttlistDecl <|>
  xmlEntityDecl <|>
  xmlNotationDecl <|>
  xmlPI <|>
  xmlComment

-- [30]
xmlExtSubset = byteStringOf $
  optional xmlTextDecl >>
  xmlExtSubsetDecl

-- [31]
xmlExtSubsetDecl = byteStringOf $
  many $
  xmlMarkupdecl <|>
  xmlConditionalSect <|>
  xmlDeclSep


-- [32]
xmlSDDecl :: Parser e ByteString
xmlSDDecl = byteStringOf $
  xmlS *> $(string "standalone") *> xmlEq *> xmlYesNo

xmlYesNo :: Parser e ByteString
xmlYesNo = wrapq' (byteStringOf $ $(string "yes") <|> $(string "no"))

-- [33] - [38] removed

-- [39]
data XmlElement = XmlElement { xeName :: ByteString, xeAttributes :: [XmlAttribute], xeContents :: [XmlContent] } deriving (Generic, Show)

xmlElement :: Parser e XmlElement
xmlElement =
  xmlEmptyElemTag <|>
  ((\(n, as) c _ -> XmlElement n as c) <$> xmlSTag <*> xmlContent <*> xmlETag)

-- TODO: might be better to not throw info away here, so (Bool, XmlElement)
xmlElementValidateClose :: Parser e (Maybe XmlElement)
xmlElementValidateClose =
  (Just <$> xmlEmptyElemTag) <|>
  ((\(n, as) c ct -> bool Nothing (Just (XmlElement n as c)) (n==ct)) <$> xmlSTag <*> xmlContent <*> xmlETag)

-- [40]
xmlSTag :: Parser e (ByteString, [XmlAttribute])
xmlSTag =
  xmlLT *> ((,) <$> xmlName <*> many (xmlS *> xmlAttribute) <* optional xmlS) <* xmlGT

-- [41]
data XmlAttribute = XmlAttribute { xaName :: ByteString, xaValue :: ByteString } deriving (Generic, Show)

-- TODO: clean this up
-- >>>  runParser xmlAttribute "a = 'b'"
-- OK (XmlAttribute {xaName = "a", xaValue = "b"}) ""
--
-- >>> runParser xmlAttribute "a = \"b\""
-- OK (XmlAttribute {xaName = "a", xaValue = "b"}) ""
xmlAttribute :: Parser e XmlAttribute
xmlAttribute = XmlAttribute <$> xmlName <*> (xmlEq *> wrapq)

-- [42]
xmlETag :: Parser e ByteString
xmlETag = xmlStartCloseTag *> xmlName <* optional xmlS <* xmlGT

xmlStartCloseTag = $(string "</")

-- [43]
data XmlContent =
  XmlCElement XmlElement |
  XmlComment ByteString deriving (Generic, Show)

xmlContent :: Parser e [XmlContent]
xmlContent =
  optional xmlCharData *> many (XmlCElement <$> xmlElement <* optional xmlCharData)

-- [44]
xmlEmptyElemTag :: Parser e XmlElement
xmlEmptyElemTag =
  xmlLT *> (XmlElement <$> xmlName <*> many (xmlS *> xmlAttribute) <* optional xmlS) <* xmlEmptyCloseTag <*> pure mempty

xmlLT :: ParserT st e ()
xmlLT = $(char '<')
xmlGT :: ParserT st e ()
xmlGT = $(char '>')

xmlEmptyCloseTag :: ParserT st e ()
xmlEmptyCloseTag = $(string "/>")

-- [45]
xmlElementdecl = byteStringOf $
  $(string "<!ELEMENT") >>
  xmlS >>
  xmlName >>
  xmlS >>
  xmlContentSpec >>
  optional xmlS

-- [46]
xmlContentSpec = byteStringOf $
  $(string "EMPTY") <|>
  $(string "ANY") <|>
  (pure () <* xmlMixed) <|>
  (pure () <* xmlChildren)

-- [47]
xmlChildren = byteStringOf $
  (xmlChoice <|> xmlSeq) >>
  optional xmlElementContentSuffix

xmlElementContentSuffix = satisfyAscii (`elem` ("?*+" :: [Char]))
-- [48]
xmlCp = byteStringOf $
  (xmlName <|> xmlChoice <|> xmlSeq) >> optional xmlElementContentSuffix

-- [49]
xmlChoice = byteStringOf $
  $(char '(') >>
  optional xmlS >>
  xmlCp >>
  some (optional xmlS >> $(char '|') >> optional xmlS) >>
  optional xmlS >>
  $(char ')')

-- [50]
xmlSeq = byteStringOf $
  $(char '(') >>
  optional xmlS >>
  xmlCp >>
  many (optional xmlS >> $(char '|') >> optional xmlS) >>
  optional xmlS >>
  $(char ')')

-- [51]
xmlMixed = byteStringOf $
  ($(char '(') >>
  optional xmlS >>
  $(string "#PCDATA") >>
  many (optional xmlS >> $(char '|') >> optional xmlS >> xmlName) >>
  optional xmlS >>
  $(string ")*")) <|>
  ($(char '(') >> optional xmlS >>
  $(string "#PCDATA") >> optional xmlS >> $(char ')'))

-- [52]
xmlAttlistDecl = byteStringOf $
  $(string "<!ATTLIST") >>
  xmlS >>
  xmlName >>
  many xmlAttDef >>
  optional xmlS >>
  $(char '>')

-- [53]
xmlAttDef = byteStringOf $
  xmlS >>
  xmlName >>
  xmlS >>
  xmlAttType >>
  xmlS >>
  xmlDefaultDecl

-- [54]
xmlAttType = byteStringOf (xmlStringType <|> xmlTokenizedType <|> xmlEnumeratedType)

-- [55]
xmlStringType = byteStringOf $ $(string "CDATA")

-- [56]
xmlTokenizedType = byteStringOf $
  $(string "ID") <|>
  $(string "IDREF") <|>
  $(string "IDREFS") <|>
  $(string "ENTITY") <|>
  $(string "ENTITIES") <|>
  $(string "NMTOKEN") <|>
  $(string "NMTOKENS")

-- [57]
xmlEnumeratedType = byteStringOf $
  xmlNotationType <|>
  xmlEnumeration

-- [58]
xmlNotationType = byteStringOf $
  $(string "NOTATION") >>
  xmlS >>
  $(char '(') >>
  optional xmlS >>
  xmlName >>
  many (optional xmlS >> $(char '|') >> optional xmlS >> xmlName) >>
  optional xmlS >>
  $(char ')')

-- [59]
xmlEnumeration = byteStringOf $
  $(char '(') >>
  optional xmlS >>
  xmlNmtoken >>
  many (optional xmlS >> $(char '|') >> optional xmlS >> xmlNmtoken) >>
  optional xmlS >>
  $(char ')')

-- [60]
xmlDefaultDecl = byteStringOf $
  $(string "#REQUIRED") <|>
  $(string "#IMPLIED") <|>
  (pure () <* optional ($(string "#FIXED") >> optional xmlS) <* xmlAttValue)

-- [61]
xmlConditionalSect = byteStringOf $
  xmlIncludeSect <|> xmlIgnoreSect

-- [62]
xmlIncludeSect = byteStringOf $
  $(string "<![]") >>
  optional xmlS >>
  $(string "INCLUDE") >>
  optional xmlS >>
  $(char '[') >>
  many xmlExtSubsetDecl >>
  $(string "]]>")

-- [63]
xmlIgnoreSect = byteStringOf $
  $(string "<![") >>
  optional xmlS >>
  $(string "IGNORE") >>
  optional xmlS >>
  $(char '[') >>
  many xmlIgnoreSectContents >>
  $(string "]]>")

-- [64]
xmlIgnoreSectContents = byteStringOf $
  xmlIgnore >>
  many ($(string "<![") >>
  xmlIgnoreSectContents >>
  $(string "]]>") >> xmlIgnore)

-- [65]
xmlIgnore = byteStringOf $
  (failed <* (many xmlChar >> ($(string "<![") <|> $(string "]]>")) >> many xmlChar)) <|>
  many xmlChar

-- [66]
data XmlReferenceType = XmlCharRef | XmlEntityRef | XmlPERef

data XmlReference = XmlReference { xrefType :: XmlReferenceType, xrefName :: ByteString }

xmlCharRef :: Parser e XmlReference
xmlCharRef = (XmlReference XmlCharRef <$> (byteStringOf $
  $(string "&#") >> some (satisfy isDigit))) <* $(char ';')

-- [67]
xmlReference = xmlCharRef <|> xmlEntityRef <|> xmlCharRef

-- [68]
xmlEntityRef :: Parser e XmlReference
xmlEntityRef = XmlReference XmlEntityRef <$> ($(char '&') *> xmlName <* $(char ';'))

-- [69]
xmlPEReference :: Parser e XmlReference
xmlPEReference = XmlReference XmlPERef <$> ($(char '%') *> xmlName <* $(char ';'))

-- [70]
xmlEntityDecl = byteStringOf $
  xmlGEDecl <|> xmlPEDecl

-- [71]
xmlGEDecl = byteStringOf $
  $(string "<!ENTITY") >>
  xmlS >>
  xmlName >>
  xmlS >>
  xmlEntityDef >>
  optional xmlS >>
  $(char '>')


-- [72]
xmlPEDecl = byteStringOf $
  $(string "<!ENTITY") >>
  xmlS >>
  $(char '%') >>
  xmlS >>
  xmlName >>
  xmlS >>
  xmlPEDef >>
  optional xmlS >>
  $(char '>')

-- [73]
xmlEntityDef = byteStringOf $
  xmlEntityValue <|>
  xmlExternalID >> optional xmlNDataDecl

-- [74]
xmlPEDef = byteStringOf $
  xmlEntityValue <|> xmlExternalID

-- [75]
xmlExternalID = byteStringOf $
  ($(string "SYSTEM") >>
  xmlS >>
  xmlSystemLiteral) <|>
  ($(string "PUBLIC") >>
  xmlS >>
  xmlPubidLiteral >>
  xmlS >>
  xmlSystemLiteral)

-- [76]
xmlNDataDecl = byteStringOf $
  xmlS >>
  $(string "NDATA") >>
  xmlS >>
  xmlName

-- [77]
xmlTextDecl = byteStringOf $
  $(string "<?xml") >>
  optional xmlVersionInfo >>
  xmlEncodingDecl >>
  optional xmlS >>
  $(string "?>")

-- [78]
xmlExtParsedEnt = byteStringOf $
  optional xmlTextDecl >>
  xmlContent

-- [79]
-- There is none in the doc

-- [80]
xmlEncodingDecl = xmlS *> $(string "encoding") *> xmlEq *> wrapq' xmlEncName

-- [81]
xmlEncName = byteStringOf (satisfyAscii isLatinLetter >> many (satisfyAscii (\x -> isLatinLetter x || isDigit x || elem x ("._-" :: [Char]) )))

-- [82]
xmlNotationDecl = byteStringOf $
  $(string "<!NOTATION") >>
  xmlS >>
  xmlName >>
  xmlS >>
  (xmlExternalID <|> xmlPublicID) >>
  optional xmlS >>
  $(char '>')

-- [83]
xmlPublicID = byteStringOf $
  $(string "PUBLIC") >>
  xmlS >>
  xmlPubidLiteral

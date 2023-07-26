{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Much of the parsing logic is based on the XML productions found in https://www.w3.org/TR/xml/#NT-content
--
-- As an Xml parser, this is very incomplete and rudimentary, hence not calling it an xml parser.
--
-- My other reference was https://www.w3schools.com/xml/xml_syntax.asp (don't laugh).
module Chart.Markup.Parser
  ( markupP,
    contentP,
    XmlDocument (..),
    xmlDocument,
    xmlProlog,
    xmlXMLDecl,
    xmlDoctypedecl,
    XmlMiscType,
    XmlMisc (..),
    xmlMisc,
    xmlComment,
    lt,
    gt,
    gtc,
    oct,
    sq,
    dq,
    wrappedQ,
    wrappedQNoGuard,
    eq,
    xmlName,
    xmlAtt,
    openTag,
    closeTag,
    emptyElemTag,

    -- * testing
    exampleDocument,
  )
where

import Chart.FlatParse
import Chart.Markup
  ( Content (..),
    Markup (Markup),
    attribute,
  )
import Data.ByteString (ByteString)
import Data.String.Interpolate
import FlatParse.Basic hiding (cut)
import qualified FlatParse.Basic.Text as T
import GHC.Generics
import Prelude

-- $setup
--
-- >>> :set -XOverloadedLabels
-- >>> :set -XOverloadedStrings
-- >>> import Chart
-- >>> import Optics.Core
-- >>> import FlatParse.Basic
-- >>> import Chart.FlatParse

-- * special XML chars

-- | opening tag
--
-- >>> runParserMaybe lt "<"
-- Just ()
lt :: Parser e ()
lt = $(char '<') -- `cut'` Lit "<"

-- | closing tag char
--
-- >>> runParserMaybe gt ">"
-- Just ()
gt :: Parser e ()
gt = $(char '>')

-- | self-closing tag
--
-- >>> runParserMaybe gtc "/>"
-- Just ()
gtc :: Parser e ()
gtc = $(string "/>")

-- | open closer tag
--
-- >>> runParserMaybe oct "</"
-- Just ()
oct :: Parser e ()
oct = $(string "</")

-- | single quote
--
-- >>> runParserMaybe sq "''"
-- Just ()
sq :: ParserT st e ()
sq = $(char '\'')

-- | double quote
--
-- >>> runParserMaybe dq "\""
-- Just ()
dq :: ParserT st e ()
dq = $(char '"')

wrappedDq :: Parser e ByteString
wrappedDq = wrapped dq (byteStringOf $ many (T.satisfy (/= '"')))

-- | guard check for closing quote
wrappedSq :: Parser e ByteString
wrappedSq = wrapped sq (byteStringOf $ many (T.satisfy (/= '\'')))

-- | quote or double quote wrapped
--
-- >>> runParserMaybe wrappedQ "\"quoted\""
-- Just "quoted"
--
-- >>> runParserMaybe wrappedQ "'quoted'"
-- Just "quoted"
wrappedQ :: Parser e ByteString
wrappedQ =
  wrappedDq
    <|> wrappedSq

-- | quote or double quote wrapped
--
-- >>> runParserMaybe (wrappedQNoGuard xmlName) "\"name\""
-- Just "name"
--
-- but will consume quotes if the underlying parser does.
--
-- >>> runParserMaybe (wrappedQNoGuard (many anyChar)) "\"name\""
-- Nothing
wrappedQNoGuard :: Parser e a -> Parser e a
wrappedQNoGuard p = wrapped dq p <|> wrapped sq p

-- | xml production [25]
--
-- >>> runParserMaybe eq " = "
-- Just ()
--
-- >>> runParserMaybe eq "="
-- Just ()
eq :: Parser e ()
eq = optional wss *> $(char '=') <* optional wss

-- [4]
nameStartChar :: Parser e Char
nameStartChar = fusedSatisfy isLatinLetter isNameStartChar isNameStartChar isNameStartChar

isNameStartChar :: Char -> Bool
isNameStartChar x =
  (x >= 'a' && x <= 'z')
    || (x >= 'A' && x <= 'Z')
    || (x == ':')
    || (x == '_')
    || (x >= '\xC0' && x <= '\xD6')
    || (x >= '\xD8' && x <= '\xF6')
    || (x >= '\xF8' && x <= '\x2FF')
    || (x >= '\x370' && x <= '\x37D')
    || (x >= '\x37F' && x <= '\x1FFF')
    || (x >= '\x200C' && x <= '\x200D')
    || (x >= '\x2070' && x <= '\x218F')
    || (x >= '\x2C00' && x <= '\x2FEF')
    || (x >= '\x3001' && x <= '\xD7FF')
    || (x >= '\xF900' && x <= '\xFDCF')
    || (x >= '\xFDF0' && x <= '\xFFFD')
    || (x >= '\x10000' && x <= '\xEFFFF')

-- [4a]
nameChar :: Parser e Char
nameChar = fusedSatisfy isNameCharAscii isNameCharExt isNameCharExt isNameCharExt

isNameCharAscii :: Char -> Bool
isNameCharAscii x =
  (x >= 'a' && x <= 'z')
    || (x >= 'A' && x <= 'Z')
    || (x >= '0' && x <= '9')
    || (x == ':')
    || (x == '_')
    || (x == '-')
    || (x == '.')

isNameCharExt :: Char -> Bool
isNameCharExt x =
  (x >= 'a' && x <= 'z')
    || (x >= 'A' && x <= 'Z')
    || (x >= '0' && x <= '9')
    || (x == ':')
    || (x == '_')
    || (x == '-')
    || (x == '.')
    || (x == '\xB7')
    || (x >= '\xC0' && x <= '\xD6')
    || (x >= '\xD8' && x <= '\xF6')
    || (x >= '\xF8' && x <= '\x2FF')
    || (x >= '\x300' && x <= '\x36F')
    || (x >= '\x370' && x <= '\x37D')
    || (x >= '\x37F' && x <= '\x1FFF')
    || (x >= '\x200C' && x <= '\x200D')
    || (x >= '\x203F' && x <= '\x2040')
    || (x >= '\x2070' && x <= '\x218F')
    || (x >= '\x2C00' && x <= '\x2FEF')
    || (x >= '\x3001' && x <= '\xD7FF')
    || (x >= '\xF900' && x <= '\xFDCF')
    || (x >= '\xFDF0' && x <= '\xFFFD')
    || (x >= '\x10000' && x <= '\xEFFFF')

-- | name string according to xml production rule [5]
--
-- >>> runParserMaybe xmlName "name"
-- Just "name"
xmlName :: Parser e ByteString
xmlName = byteStringOf (nameStartChar >> many nameChar)

-- | attribute pair
--
-- >>> runParserMaybe xmlAtt "style = 'fancy'"
-- Just ("style","fancy")
xmlAtt :: Parser e (ByteString, ByteString)
xmlAtt = (,) <$> (xmlName <* eq) <*> wrappedQ

-- | open xml tag as per xml production rule [40]
--
-- >>> runParserMaybe openTag "<g style='fancy'>"
-- Just ("g",[("style","fancy")])
openTag :: Parser Error (ByteString, [(ByteString, ByteString)])
openTag =
  lt *> ((,) <$> xmlName <*> many (wss *> xmlAtt) <* optional wss) <* gt `cut'` Msg "open tag expected"

-- | closing tag as per [42]
--
-- >>> runParserMaybe closeTag "</g>"
-- Just "g"
closeTag :: Parser Error ByteString
closeTag = oct *> xmlName <* optional wss <* gt `cut'` Msg "close tag expected"

-- | empty element tag as per [44]
--
-- >>> runParserMaybe emptyElemTag "<br/>"
-- Just ("br",[])
emptyElemTag :: Parser Error (ByteString, [(ByteString, ByteString)])
emptyElemTag =
  lt *> ((,) <$> xmlName <*> many (wss *> xmlAtt) <* optional wss) <* gtc

-- * comments

xmlCommentOpen :: Parser e ()
xmlCommentOpen = $(string "<!--")

xmlCommentClose :: Parser e ()
xmlCommentClose = $(string "-->")

xmlCharNotMinus :: Parser e ByteString
xmlCharNotMinus = byteStringOf $ satisfy (/= '-')

xmlMinusPlusChar :: Parser e ByteString
xmlMinusPlusChar = byteStringOf $ $(char '-') *> xmlCharNotMinus

-- | xml comment
--
--
-- >>> runParserMaybe xmlComment "<!-- comment -->"
-- Just " comment "
xmlComment :: Parser e ByteString
xmlComment = xmlCommentOpen *> byteStringOf (many (xmlCharNotMinus <|> xmlMinusPlusChar)) <* xmlCommentClose

-- * prolog

-- | xml production rule [22]
--
-- The library doesn't do any analysis of the prolog string nor produces it, hence it is just parsed as a ByteString
--
-- >>> runParser (ws_ *> xmlProlog) exampleDocument
-- OK "<?xml version=\"1.0\" standalone=\"yes\" ?>\n\n<!--open the DOCTYPE declaration -\n  the open square bracket indicates an internal DTD-->\n<!DOCTYPE foo [\n\n<!--define the internal DTD-->\n  <!ELEMENT foo (#PCDATA)>\n\n<!--close the DOCTYPE declaration-->\n]>\n" "<foo>Hello World.</foo>\n"
xmlProlog :: Parser e ByteString
xmlProlog =
  byteStringOf $
    xmlXMLDecl
      >> many xmlMisc
      >> optional (xmlDoctypedecl >> optional xmlMisc)

-- | XML declaration as per production rule [23]
--
-- >>> runParserMaybe xmlXMLDecl "<?xml version=\"1.0\" standalone=\"yes\" ?>"
-- Just "<?xml version=\"1.0\" standalone=\"yes\" ?>"
xmlXMLDecl :: Parser e ByteString
xmlXMLDecl =
  byteStringOf $
    $(string "<?xml")
      >> xmlVersionInfo
      >> optional xmlEncodingDecl
      >> optional wssDDecl
      >> optional wss
      >> $(string "?>")

-- xml production [24]
xmlVersionInfo :: Parser e ByteString
xmlVersionInfo = byteStringOf $ wss >> $(string "version") >> eq >> wrappedQNoGuard xmlVersionNum

-- | xml production [26]
xmlVersionNum :: Parser e ByteString
xmlVersionNum =
  byteStringOf ($(string "1.") >> some (satisfy isDigit))

-- | Whether an 'XmlMisc' is comment or whitespace
data XmlMiscType = XMiscComment | XMiscS deriving (Generic, Show, Eq)

-- | A comment or whitespace outside of the main document [27]
--
--   not as per [27] (missing PI)
data XmlMisc = XmlMisc {xmiscType :: XmlMiscType, xmiscContent :: ByteString} deriving (Generic, Show, Eq)

-- | Parser for miscellaneous guff
xmlMisc :: Parser e XmlMisc
xmlMisc =
  (XmlMisc XMiscComment <$> xmlComment)
    <|> (XmlMisc XMiscS <$> wss)

-- | Typical xml header text
exampleDocument :: ByteString
exampleDocument =
  [i|
<?xml version="1.0" standalone="yes" ?>

<!--open the DOCTYPE declaration -
  the open square bracket indicates an internal DTD-->
<!DOCTYPE foo [

<!--define the internal DTD-->
  <!ELEMENT foo (\#PCDATA)>

<!--close the DOCTYPE declaration-->
]>
<foo>Hello World.</foo>
|]

-- | Doctype declaration as per production rule [28]
--
-- >>> runParserMaybe xmlDoctypedecl "<!DOCTYPE foo [ declarations ]>"
-- Just "<!DOCTYPE foo [ declarations ]>"
xmlDoctypedecl :: Parser e ByteString
xmlDoctypedecl =
  byteStringOf $
    $(string "<!DOCTYPE")
      >> wss
      >> xmlName
      >>
      -- optional (wss >> xmlExternalID) >>
      optional wss
      >> optional bracketedSB
      >> optional wss
      >> $(char '>')

bracketedSB :: Parser e [Char]
bracketedSB = bracketed $(char '[') $(char ']') (many (satisfy (/= ']')))

-- [32]
wssDDecl :: Parser e ByteString
wssDDecl =
  byteStringOf $
    wss *> $(string "standalone") *> eq *> xmlYesNo

xmlYesNo :: Parser e ByteString
xmlYesNo = wrappedQNoGuard (byteStringOf $ $(string "yes") <|> $(string "no"))

-- | xml production [80]
xmlEncodingDecl :: Parser e ByteString
xmlEncodingDecl = wss *> $(string "encoding") *> eq *> wrappedQNoGuard xmlEncName

-- [81]
xmlEncName :: Parser e ByteString
xmlEncName = byteStringOf (satisfyAscii isLatinLetter >> many (satisfyAscii (\x -> isLatinLetter x || isDigit x || elem x ("._-" :: [Char]))))

-- main Parser

-- | An XML document as pre production rule [1]
data XmlDocument = XmlDocument ByteString Markup [XmlMisc] deriving (Show, Eq)

-- | Note that the library builds a Markup as per the SVG standards and not a Document.
--
-- >>> runParser (ws_ *> xmlDocument) exampleDocument
-- OK (XmlDocument "<?xml version=\"1.0\" standalone=\"yes\" ?>\n\n<!--open the DOCTYPE declaration -\n  the open square bracket indicates an internal DTD-->\n<!DOCTYPE foo [\n\n<!--define the internal DTD-->\n  <!ELEMENT foo (#PCDATA)>\n\n<!--close the DOCTYPE declaration-->\n]>\n" (Markup {tag = "foo", atts = Attributes {attMap = fromList []}, contents = [Content "Hello World."]}) [XmlMisc {xmiscType = XMiscS, xmiscContent = "\n"}]) ""
xmlDocument :: Parser Error XmlDocument
xmlDocument = XmlDocument <$> (ws_ *> xmlProlog) <*> markupP <*> many xmlMisc

-- | Main parser for a single Markup (xml-like) element
--
-- >>> runParser markupP "<foo>Hello World.</foo>"
-- OK (Markup {tag = "foo", atts = Attributes {attMap = fromList []}, contents = [Content "Hello World."]}) ""
markupP :: Parser Error Markup
markupP =
  ((\(n, as) -> Markup n (mconcat $ attribute <$> as) mempty) <$> emptyElemTag)
    <|>
    -- no close tag = open tag test
    ((\(n, as) c _ -> Markup n (mconcat $ attribute <$> as) c) <$> openTag <*> many contentP <*> closeTag `cut` ["open tag", "content", "close tag"])

-- | Inner contents of an element.
--
-- >>> runParser (some contentP) "<foo>Hello World.</foo>content<!-- comment -->"
-- OK [MarkupLeaf (Markup {tag = "foo", atts = Attributes {attMap = fromList []}, contents = [Content "Hello World."]}),Content "content",Comment " comment "] ""
contentP :: Parser Error Content
contentP =
  (MarkupLeaf <$> markupP)
    <|> (Comment <$> xmlComment)
    <|> (Content <$> byteStringOf (some (satisfy (/= '<'))))

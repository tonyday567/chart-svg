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
{-# HLINT ignore "Use <>" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

-- https://www.w3.org/TR/SVG2/text.html#TextElement
-- https://www.w3.org/TR/xml/#NT-content
module Chart.Markup.Parser where

import Prelude
import Data.ByteString ( ByteString, readFile)
import GHC.Generics
import FlatParse.Basic hiding (cut)
import System.Directory
import Chart.Markup
    ( Content(..), Markup(Markup), inject, printMarkup )
import qualified FlatParse.Basic.Text as T
import Chart.FlatParse
import qualified Data.ByteString as BS
import GHC.Exts

-- $setup
--
-- >>> :set -XOverloadedLabels
-- >>> :set -XOverloadedStrings
-- >>> import Chart
-- >>> import Optics.Core

-- * special chars
-- | opening tag char
lt :: Parser Error ()
lt = $(char '<') -- `cut'` Lit "<"

-- | closing tag char
gt :: Parser e ()
gt = $(char '>')

-- | self-closing tag
gtc :: Parser e ()
gtc = $(string "/>")

-- | open closer tag
oct = $(string "</")

sp :: Parser e ()
sp = $(char ' ')

amp :: Parser e ()
amp = $(char '&')

caret :: Parser e ()
caret = $(char '^')

slash :: Parser e ()
slash = $(char '/')

-- | name separator [6] [8]
sep :: Parser e s -> Parser e a -> Parser e [a]
sep s p = (:) <$> p <*> many (s *> p)

-- | xml production [3]
wss :: Parser e String
wss = many ws

-- | TODO: is this more efficient?
wss' :: Parser e ByteString
wss' = withSpan (some ws) (\_ s -> unsafeSpanToByteString s)

sq :: ParserT st e ()
sq = $(char '\'')

dq :: ParserT st e ()
dq = $(char '"')

bracketed :: Parser e b -> Parser e b -> Parser e a -> Parser e a
bracketed o c p = o *> p <* c

wrapped :: Parser e () -> Parser e a -> Parser e a
wrapped x p = bracketed x x p

wrappedQNoGuard :: Parser e a -> Parser e a
wrappedQNoGuard p = wrapped dq p <|> wrapped sq p

-- | guard check for closing character
wrappedDq :: Parser e ByteString
wrappedDq = wrapped dq (byteStringOf $ many (T.satisfy (/= '"')))

wrappedSq :: Parser e ByteString
wrappedSq = wrapped sq (byteStringOf $ many (T.satisfy (/= '\'')))

wrappedQ :: Parser e ByteString
wrappedQ =
  wrappedDq <|>
  wrappedSq

-- xml production [25]
eq :: Parser e ()
eq = optional wss *> $(char '=') <* optional wss

-- * xml-style name production
-- [2]
xmlChar :: Parser e Char
xmlChar = anyChar

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
-- >>> runParser xmlName "markup>"
-- OK "markup" ">"
xmlName :: Parser e ByteString
xmlName = byteStringOf (nameStartChar >> many nameChar)

-- [6]
xmlNames :: Parser e [ByteString]
xmlNames = sep sp xmlName

-- * attributes
xmlAtt :: Parser e (ByteString, ByteString)
xmlAtt = (,) <$> (xmlName <* eq) <*> wrappedQ

-- [40]
openTag :: Parser Error (ByteString, [(ByteString, ByteString)])
openTag =
  lt *> ((,) <$> xmlName <*> many (wss *> xmlAtt) <* optional wss) <* gt `cut'` Msg "open tag expected"

-- [42]
closeTag :: Parser Error ByteString
closeTag = oct *> xmlName <* optional wss <* gt `cut'` Msg "close tag expected"

-- [44]
emptyElemTag ::  Parser Error (ByteString, [(ByteString, ByteString)])
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
xmlProlog :: Parser e ByteString
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
xmlVersionInfo :: Parser e ByteString
xmlVersionInfo = byteStringOf $ wss >> $(string "version") >> eq >> wrappedQNoGuard xmlVersionNum

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
wssDDecl :: Parser e ByteString
wssDDecl = byteStringOf $
  wss *> $(string "standalone") *> eq *> xmlYesNo

xmlYesNo :: Parser e ByteString
xmlYesNo = wrappedQNoGuard (byteStringOf $ $(string "yes") <|> $(string "no"))

-- [33] - [38] removed

-- [77]
xmlTextDecl :: Parser e ByteString
xmlTextDecl = byteStringOf $
  $(string "<?xml") >>
  optional xmlVersionInfo >>
  xmlEncodingDecl >>
  optional wss >>
  $(string "?>")

-- | xml production [80]
xmlEncodingDecl :: Parser e ByteString
xmlEncodingDecl = wss *> $(string "encoding") *> eq *> wrappedQNoGuard xmlEncName

-- [81]
xmlEncName :: Parser e ByteString
xmlEncName = byteStringOf (satisfyAscii isLatinLetter >> many (satisfyAscii (\x -> isLatinLetter x || isDigit x || elem x ("._-" :: [Char]) )))

-- main Parser

-- [1]
data XmlDocument = XmlDocument ByteString Markup [XmlMisc]

xmlDocument :: Parser Error XmlDocument
xmlDocument = XmlDocument <$> xmlProlog <*> xmlElement <*> many xmlMisc

xmlElement :: Parser Error Markup
xmlElement =
  ((\(n,as) -> Markup n (mconcat $ inject <$> as) mempty) <$> emptyElemTag) <|>
  -- no close tag = open tag test
  ((\(n,as) c _ -> Markup n (mconcat $ inject <$> as) c) <$> openTag <*> many xmlContent <*> closeTag `cut` ["open tag", "content", "close tag"])

xmlContent :: Parser Error Content
xmlContent =
  (MarkupLeaf <$> xmlElement) <|>
  (CDSect <$> xmlCDSect ) <|>
  (Comment <$> xmlComment) <|>
  (Content <$> byteStringOf (some (satisfy (/= '<'))))

isoMarkupParse :: ByteString -> Bool
isoMarkupParse x = case runParser xmlElement x of
  OK l "" -> printMarkup l == x
  _ -> False

parseOk :: Parser e a -> ByteString -> Bool
parseOk p x = case runParser p x of
  OK _ "" -> True
  OK _ _ -> False
  _ -> False

-- >>> os <- olds'
-- >>> osb <- oldChecks' os
-- >>> osb
-- [True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True]
fileList :: FilePath -> IO [FilePath]
fileList fp =  fmap (filter (/= ".DS_Store")) (listDirectory fp)

isoFile :: FilePath -> IO Bool
isoFile fp = do
  bs <- BS.readFile fp
  pure $ isoMarkupParse bs

eqFiles :: FilePath -> FilePath -> IO Bool
eqFiles fp  fp' = do
  bs <- BS.readFile fp
  bs' <- BS.readFile fp
  pure $ bs == bs'



oldChecks' fs = traverse (fmap (parseOk xmlElement) . Data.ByteString.readFile) $ ("../../git/chart-svg/other/"<>) <$> fs

fileCheck fp = do
  bs <- BS.readFile ("other/" <> fp)
  case runParserMaybe xmlElement bs of
    Nothing -> pure False
    Just m -> pure $ bs == printMarkup m

-- parse error model
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
    (Imprecise _ es , Imprecise _ es' ) -> Imprecise p (es ++ es')
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
      imprec (e:es) = expected e ++ go es where
        go []     = ""
        go [e]    = " or " ++ expected e
        go (e:es) = ", " ++ expected e ++ go es

  in show l ++ ":" ++ show c ++ ":\n" ++
     lpad   ++ "|\n" ++
     linum  ++ "| " ++ line' ++ "\n" ++
     lpad   ++ "| " ++ replicate c ' ' ++ "^\n" ++
     "parse error: expected " ++
     err e

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

-- | Run parser, print pretty error on failure.
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

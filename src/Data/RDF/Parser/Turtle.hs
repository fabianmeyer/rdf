{-,
Module      : Data.RDF.Parser.Turtle
Description : Representation and Incremental Processing of RDF Data
Copyright   : Fabian Meyer 2017
License     : MIT
Maintainer  : meyerf@gmail.com
Stability   : Provisional
Portability : Portable

A parser for <https://www.w3.org/TR/turtle/>.
-}

{-# LANGUAGE OverloadedStrings #-}

module Data.RDF.Parser.Turtle where

import Data.Attoparsec.Text.Lazy as AL

import Data.Text.Lazy as TL
import Data.Monoid
import Control.Applicative
import Data.Maybe

-- [1]	turtleDoc	::=	statement*
-- [2]	statement	::=	directive | triples '.'
-- [3]	directive	::=	prefixID | base | sparqlPrefix | sparqlBase
-- [4]	prefixID	::=	'@prefix' PNAME_NS IRIREF '.'
-- [5]	base	::=	'@base' IRIREF '.'
-- [5s]	sparqlBase	::=	"BASE" IRIREF
-- [6s]	sparqlPrefix	::=	"PREFIX" PNAME_NS IRIREF
-- [6]	triples	::=	subject predicateObjectList | blankNodePropertyList predicateObjectList?
-- [7]	predicateObjectList	::=	verb objectList (';' (verb objectList)?)*
-- [8]	objectList	::=	object (',' object)*
-- [9]	verb	::=	predicate | 'a'
-- [10]	subject	::=	iri | BlankNode | collection
-- [11]	predicate	::=	iri
-- [12]	object	::=	iri | BlankNode | collection | blankNodePropertyList | literal
-- [13]	literal	::=	RDFLiteral | NumericLiteral | BooleanLiteral
-- [14]	blankNodePropertyList	::=	'[' predicateObjectList ']'
-- [15]	collection	::=	'(' object* ')'
-- [16]	NumericLiteral	::=	INTEGER | DECIMAL | DOUBLE
-- [128s]	RDFLiteral	::=	String (LANGTAG | '^^' iri)?
-- [133s]	BooleanLiteral	::=	'true' | 'false'
-- [17]	String	::=	STRING_LITERAL_QUOTE | STRING_LITERAL_SINGLE_QUOTE | -- STRING_LITERAL_LONG_SINGLE_QUOTE | STRING_LITERAL_LONG_QUOTE
-- [135s]	iri	::=	IRIREF | PrefixedName
-- [136s]	PrefixedName	::=	PNAME_LN | PNAME_NS
-- [137s]	BlankNode	::=	BLANK_NODE_LABEL | ANON


-- [18]	IRIREF	::=	'<' ([^#x00-#x20<>"{}|^`\] | UCHAR)* '>' /* #x00=NULL #01-#x1F=control codes #x20=space */
-- iriRef :: Parser Text
-- iriRef = char '<' *> ([^#x00-#x20<>"{}|^`\] | UCHAR)* <* '>'

-- [139s]	PNAME_NS	::=	PN_PREFIX? ':'
pnameNs :: Parser Text
pnameNs = liftA2 (<>) (fromMaybe "" <$> optional pnPrefix) (singleton <$> char ':')

-- [140s]	PNAME_LN	::=	PNAME_NS PN_LOCAL
pnameLn :: Parser Text
pnameLn = liftA2 (<>) pnameNs pnLocal

-- [141s]	BLANK_NODE_LABEL	::=	'_:' (PN_CHARS_U | [0-9])
blankNodeLabel :: Parser Text
blankNodeLabel = TL.fromStrict <$> string "_:" *> liftA2 (<>) (singleton <$> (pnCharsU <|> choice (char <$> ['0' .. '9']))) (fromMaybe "" <$> optional (liftA2 (<>) (pack <$> many (pnChars <|> char '.')) (singleton <$> pnChars)))

-- [144s]	LANGTAG	::=	'@' [a-zA-Z]+ ('-' [a-zA-Z0-9]+)*
langtag :: Parser Text
langtag = do
  _ <- char '@'
  prefix <- pack <$> some (choice $ char <$> ['a' .. 'z'] <> ['A' .. 'Z'])
  suffix <- TL.concat <$> many (liftA2 (<>) (pack <$> some (choice $ char <$> ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'])) (singleton <$> char '-'))
  return $ prefix <> suffix

-- [19]	INTEGER	::=	[+-]? [0-9]+
integer :: Parser Integer
integer = AL.signed AL.decimal

-- [20]	DECIMAL	::=	[+-]? [0-9]* '.' [0-9]+
decimal :: Parser Rational
decimal = AL.signed AL.rational

-- [21]	DOUBLE	::=	[+-]? ([0-9]+ '.' [0-9]* EXPONENT | '.' [0-9]+ EXPONENT | [0-9]+ EXPONENT)
double :: Parser Double
double = AL.signed AL.double
-- [154s]	EXPONENT	::=	[eE] [+-]? [0-9]+

-- [22]	STRING_LITERAL_QUOTE	::=	'"' ([^#x22#x5C#xA#xD] | ECHAR | UCHAR)* '"'
-- #x22=" #x5C=\ #xA=new line #xD=carriage return
stringLiteralQuote :: Parser Text
stringLiteralQuote = char '\"' *> (pack <$> many (satisfy sat <|> eChar <|> uChar)) <* char '\"'
  where sat '\x22' = False
        sat '\x5C' = False
        sat '\xA' = False
        sat '\xD' = False
        sat _ = True

-- [23]	STRING_LITERAL_SINGLE_QUOTE	::=	"'" ([^#x27#x5C#xA#xD] | ECHAR | UCHAR)* "'" /* #x27=' #x5C=\ #xA=new line #xD=carriage return */
stringLiteralSingleQuote :: Parser Text
stringLiteralSingleQuote = char '\'' *> (pack <$> many (satisfy sat <|> eChar <|> uChar)) <* char '\''
  where sat '\x27' = False
        sat '\x5C' = False
        sat '\xA' = False
        sat '\xD' = False
        sat _ = True

-- [24]	STRING_LITERAL_LONG_SINGLE_QUOTE	::=	"'''" (("'" | "''")? ([^'\] | ECHAR | UCHAR))* "'''"
stringLiteralLongSingleQuote :: Parser Text
stringLiteralLongSingleQuote = string "\'\'\'" *> (TL.concat <$> many (liftA2 (<>) (fromMaybe "" <$> optional (singleton <$> char '\'' <|> TL.fromStrict <$> string "\'\'")) (singleton <$> (satisfy sat <|> eChar <|> uChar)))) <* string "\'\'\'"
  where sat '\'' = False
        sat '\\' = False
        sat _ = True

-- [25]	STRING_LITERAL_LONG_QUOTE	::=	'"""' (('"' | '""')? ([^"\] | ECHAR | UCHAR))* '"""'
stringLiteralLongQuote :: Parser Text
stringLiteralLongQuote = string "\"\"\"" *> (TL.concat <$> many (liftA2 (<>) (fromMaybe "" <$> optional (singleton <$> char '"' <|> TL.fromStrict <$> string "\"\"")) (singleton <$> (satisfy sat <|> eChar <|> uChar))))
    <* string "\"\"\""
  where sat '\"' = False
        sat '\\' = False
        sat _ = True

-- [26]	UCHAR	::=	'\u' HEX HEX HEX HEX | '\U' HEX HEX HEX HEX HEX HEX HEX HEX
uChar :: Parser Char
uChar = string "\\u" *> (TL.pack <$> AL.count 4 hex) *> pure 'a'
    <|> string "\\U" *> (TL.pack <$> AL.count 8 hex) *> pure 'b'

-- [159s]	ECHAR	::=	'\' [tbnrf"'\]
eChar :: Parser Char
eChar = char '\\' *> choice
  [ char 't' *> pure '\t'
  , char 'b' *> pure '\b'
  , char 'n' *> pure '\n'
  , char 'r' *> pure '\r'
  , char 'f' *> pure '\f'
  , char '"' *> pure '\"'
  , char '\'' *> pure '\''
  , char '\\'  *> pure '\\' ]

-- [161s]	WS	::=	#x20 | #x9 | #xD | #xA /* #x20=space #x9=character tabulation #xD=carriage return #xA=new line */
ws :: Parser Char
ws = choice $ char <$> ['\x20', '\x9', '\xD', '\xA']

-- [162s]	ANON	::=	'[' WS* ']'
anon :: Parser ()
anon = char '[' *> many ws *> char ']' *> pure ()


-- [163s]	PN_CHARS_BASE	::=	[A-Z] | [a-z] | [#x00C0-#x00D6] | [#x00D8-#x00F6] | [#x00F8-#x02FF] | [#x0370-#x037D] | [#x037F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
pnCharsBase :: Parser Char
pnCharsBase = AL.choice $ AL.char <$> pnCharsBase'

pnCharsBase' :: String
pnCharsBase' = ['A' .. 'Z'] <> ['a' .. 'z']
  <> ['\x00C0' .. '\x00D6'] <> ['\x00D8' .. '\x00F6'] <> ['\x00F8' .. '\x02FF']
  <> ['\x0370' .. '\x037D'] <> ['\x037F' .. '\x1FFF'] <> ['\x200C' ..'\x200D']
  <> ['\x2070' .. '\x218F'] <> ['\x2C00' .. '\x2FEF'] <> ['\x3001' .. '\xD7FF']
  <> ['\xF900' .. '\xFDCF'] <> ['\xFDF0' .. '\xFFFD'] <> ['\x10000' .. '\xEFFFF']


-- [164s]	PN_CHARS_U	::=	PN_CHARS_BASE | '_'
pnCharsU :: Parser Char
pnCharsU = pnCharsU <|> char '_'

-- [166s]	PN_CHARS	::=	PN_CHARS_U | '-' | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040]
pnChars :: Parser Char
pnChars = pnCharsU <|> choice (char <$> pnChars')

pnChars' :: String
pnChars' = '-' : ['0' .. '9'] <> ['\x00B7'] <> ['\x0300' .. '\x036F'] <> ['\x203F' .. '\x2040']

-- [167s]	PN_PREFIX	::=	PN_CHARS_BASE ((PN_CHARS | '.')* PN_CHARS)?
pnPrefix :: Parser Text
pnPrefix = do
  prefix <- singleton <$> pnCharsBase
  suffix <- fromMaybe "" <$> optional (do
    a <- pack <$> many (pnChars <|> char '.')
    b <- singleton <$> pnChars
    return $ a <> b)
  return $ prefix <> suffix

-- [168s]	PN_LOCAL	::=	(PN_CHARS_U | ':' | [0-9] | PLX) ((PN_CHARS | '.' | ':' | PLX)* (PN_CHARS | ':' | PLX))?
pnLocal :: Parser Text
pnLocal = do
  prefix <- singleton <$> pnCharsU <|> singleton <$> choice (char <$> ':' : ['0' .. '9']) <|> plx
  suffix <- fromMaybe "" <$> optional (do
    a <- TL.concat <$> many (singleton <$> pnChars <|> singleton <$> char '.' <|> singleton <$> char ':' <|> plx)
    b <- singleton <$> pnChars <|> singleton <$> char ':' <|> plx
    return $ a <> b)
  return $ prefix <> suffix

-- [169s]	PLX	::=	PERCENT | PN_LOCAL_ESC
plx :: Parser Text
plx = percent <|> pnLocalEsc

-- [170s]	PERCENT	::=	'%' HEX HEX
percent :: Parser Text
percent = pack <$> sequence [AL.char '%', hex, hex]

-- [171s]	HEX	::=	[0-9] | [A-F] | [a-f]
hex :: Parser Char
hex = AL.choice $ AL.char <$> ['0' .. '9'] <> ['A' .. 'F'] <> ['a' .. 'f']

-- [172s]	PN_LOCAL_ESC	::=	'\' ('_' | '~' | '.' | '-' | '!' | '$' | '&' | "'" | '(' | ')' | '*' | '+' | ',' | ';' | '=' | '/' | '?' | '#' | '@' | '%')
pnLocalEsc :: Parser Text
pnLocalEsc =  pack <$> sequence [AL.char '\\', AL.choice (AL.char <$> ['_', '~', '.', '-', '!', '$', '&', '\'', '(' , ')', '*', '+', ',', ';', '=', '/', '?', '#', '@', '%'])]

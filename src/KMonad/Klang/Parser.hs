module KMonad.Klang.Parser

where

import KMonad.Prelude hiding (try)

import KMonad.Klang.Types

import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Text.Megaparsec.Char.Lexer as L
import qualified RIO.Text as T



--------------------------------------------------------------------------------
-- $typ
--
-- These are the basic parsing types describing what tokens we read and how we
-- report errors.

type Parser  = Parsec Void Text
type PErrors = ParseErrorBundle Text Void

-- | Consume whitespace
sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment  ";;")
  (L.skipBlockComment "#|" "|#")

-- | Consume whitespace after the provided parser
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Consume 1 symbol
symbol :: Text -> Parser ()
symbol = void . L.symbol sc

-- | Valid characters in symbols and keywords
valid :: Parser Char
valid = alphaNumChar  <|> (oneOf special <?> "symbol")
  where special = "-+/*?!" :: String

-- | Parse a KExpr from Text
parseKExpr :: Text -> Either Text KExpr
parseKExpr t = case runParser (sc *> exprP) "" t of
  Left e  -> Left . T.pack $ errorBundlePretty e
  Right k -> Right k

--------------------------------------------------------------------------------
-- $kexpr

-- | Parse a 'KEXpr'
exprP :: Parser KExpr
exprP = lexeme . choice . map try $
  [ intP, boolP, listP, vectorP, textP, nilP, keywordP, quoteP , quasiquoteP
  , unquoteP , spliceUnquoteP , symP]

-- | Read an integer number
intP :: Parser KExpr
intP = lexeme (KInt <$> L.signed (fail "whitespace") L.decimal <?> "integer")

-- | Parse a symbol
symP :: Parser KExpr
symP = lexeme $ KSym . T.pack <$> some valid

-- | Parse a list of KEXpr
listP :: Parser KExpr
listP = lexeme $ do
  _ <- lexeme $ char '('
  v <- lexeme $ (many exprP <?> "list")
  _ <- lexeme $ char ')'
  pure $ KList v

-- | Parse a vector of KExpr
vectorP :: Parser KExpr
vectorP = lexeme $ do
  _ <- lexeme $ char '['
  v <- lexeme $ (many exprP <?> "vector")
  _ <- lexeme $ char ']'
  pure $ KList v
 
-- | Parse a boolean
boolP :: Parser KExpr
boolP = lexeme (symbol "true"  *> pure (KBool True)
            <|> symbol "false" *> pure (KBool False)) <?> "bool"

-- | Parse the `nil` value
nilP :: Parser KExpr
nilP = lexeme (symbol "nil" *> pure KNil <?> "nil")

-- | Parse a colon-prefixed keyword
keywordP :: Parser KExpr
keywordP = lexeme (char ':' >> KKeyword . T.pack <$> some valid <?> "keyword")

-- | Parse text with escaped characters between "s
textP :: Parser KExpr
textP = lexeme $ do
  _ <- char '\"'
  s <- manyTill L.charLiteral (char '\"') <?> "text"
  pure . KText . T.pack $ s

readerMacro :: Text -> (KExpr -> KExpr) -> Parser KExpr
readerMacro t f = lexeme $ string t >> f <$> exprP

quoteP :: Parser KExpr
quoteP = readerMacro "'" $ \v -> KList [KSym "quote", v]

quasiquoteP :: Parser KExpr
quasiquoteP = readerMacro "`" $ \v -> KList [KSym "quasiquote", v]

unquoteP :: Parser KExpr
unquoteP = readerMacro "~" $ \v -> KList [KSym "unquote", v]

spliceUnquoteP :: Parser KExpr
spliceUnquoteP = readerMacro "~@" $ \v -> KList [KSym "splice-unquote", v]

{-# LANGUAGE OverloadedStrings #-}

module HW3.Parser
  (
    parse
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Text.Megaparsec as TM
import qualified Text.Megaparsec.Char.Lexer as L

import HW3.Base

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Char
import Data.Functor (void)
import Data.List (intercalate)
import Data.Void (Void)
import Data.Word (Word8)
import Text.Megaparsec (choice, eof, many, manyTill, notFollowedBy, optional, runParser, satisfy,
                        sepBy, sepBy1, sepEndBy, try, (<|>))
import Text.Megaparsec.Char (char, space1, string)
import Text.Megaparsec.Char.Lexer (scientific)
import Text.Megaparsec.Error (ParseErrorBundle)

type Parser = TM.Parsec Void String

-- | Consumes spaces.
space :: Parser ()
space = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

-- | Consumes all spaces after the given parser.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

-- | Parses given string.
symbol :: String -> Parser String
symbol = L.symbol space

-- | Parses rational values.
parseRational :: Parser Rational
parseRational = do
  sign <- optional (try $ symbol "-")
  number <- toRational <$> (lexeme scientific)
  case sign of
    Nothing  -> return number
    (Just _) -> return (-1 * number)

-- | Parses @HiValueNumber.
parseNumber :: Parser HiExpr
parseNumber = (HiExprValue . HiValueNumber) <$> parseRational

-- | Parses @HiValueBool.
parseBool :: Parser HiExpr
parseBool = (HiExprValue . HiValueBool) <$>
  (choice
    [ True <$ symbol "true"
    , False <$ symbol "false"
    ])

-- | Parses @HiValueNull.
parseNull :: Parser HiExpr
parseNull = HiExprValue <$> (HiValueNull <$ (symbol "null"))

-- | Parses @HiActionCwd.
parseCwd :: Parser HiExpr
parseCwd = (HiExprValue . HiValueAction) <$> (HiActionCwd <$ (symbol "cwd"))

-- | Parses @HiActionNow.
parseNow :: Parser HiExpr
parseNow = (HiExprValue . HiValueAction) <$> (HiActionNow <$ (symbol "now"))

-- | Parses @HiValueString.
parseString :: Parser HiExpr
parseString = (HiExprValue . HiValueString . T.pack) <$>
  lexeme ((char '\"') >> (manyTill L.charLiteral (char '\"')))

-- | Parses two-digit hexadecimal numbers.
hex :: Parser Word8
hex = do
  ch1 <- satisfy isHexDigit
  ch2 <- satisfy isHexDigit
  return (fromIntegral ((toInt ch1) * 16 + (toInt ch2))) where
    toInt :: Char -> Int
    toInt ch =
      if isDigit ch
      then (ord ch - ord '0')
      else (ord ch - ord 'a' + 10)

-- | Parses @HiValueBytes.
parseByteArray :: Parser HiExpr
parseByteArray = (HiExprValue . HiValueBytes . BS.pack) <$>
  ((symbol "[#") *> (sepEndBy hex space1) <* (symbol "#]"))

-- | Parses sequence of arguments, separated by ',',
-- starting and ending with given strings.
parseArgs :: String -> String -> Parser a -> Parser [a]
parseArgs start end item =
  (symbol start) *> (sepBy (lexeme item) (symbol ",")) <* (symbol end)

-- | Parses pair of @HiExpr.
parsePair :: Parser (HiExpr, HiExpr)
parsePair = do
  key <- lexeme expr
  void (symbol ":")
  value <- lexeme expr
  return (key, value)

-- | Parses function name.
parseFunctionName :: Parser HiExpr
parseFunctionName = toHiExpr <$>
  (choice
    [ HiFunDiv <$ symbol "div"
    , HiFunMul <$ symbol "mul"
    , HiFunAdd <$ symbol "add"
    , HiFunSub <$ symbol "sub"
    , HiFunAnd <$ symbol "and"
    , HiFunOr <$ symbol "or"
    , HiFunLessThan <$ symbol "less-than"
    , HiFunGreaterThan <$ symbol "greater-than"
    , HiFunEquals <$ symbol "equals"
    , HiFunNotLessThan <$ symbol "not-less-than"
    , HiFunNotGreaterThan <$ symbol "not-greater-than"
    , HiFunNotEquals <$ symbol "not-equals"
    , HiFunIf <$ symbol "if"
    , HiFunLength <$ symbol "length"
    , HiFunToUpper <$ symbol "to-upper"
    , HiFunToLower <$ symbol "to-lower"
    , HiFunReverse <$ symbol "reverse"
    , HiFunTrim <$ symbol "trim"
    , HiFunList <$ symbol "list"
    , HiFunRange <$ symbol "range"
    , HiFunFold <$ symbol "fold"
    , HiFunNot <$ symbol "not"
    , HiFunPackBytes <$ symbol "pack-bytes"
    , HiFunUnpackBytes <$ symbol "unpack-bytes"
    , HiFunEncodeUtf8 <$ symbol "encode-utf8"
    , HiFunDecodeUtf8 <$ symbol "decode-utf8"
    , HiFunZip <$ symbol "zip"
    , HiFunUnzip <$ symbol "unzip"
    , HiFunSerialise <$ symbol "serialise"
    , HiFunDeserialise <$ symbol "deserialise"
    , HiFunRead <$ symbol "read"
    , HiFunWrite <$ symbol "write"
    , HiFunMkDir <$ symbol "mkdir"
    , HiFunChDir <$ symbol "cd"
    , HiFunParseTime <$ symbol "parse-time"
    , HiFunRand <$ symbol "rand"
    , HiFunEcho <$ symbol "echo"
    , HiFunCount <$ symbol "count"
    , HiFunKeys <$ symbol "keys"
    , HiFunValues <$ symbol "values"
    , HiFunInvert <$ symbol "invert"
    ])

-- | Converts @HiFun to @HiExpr.
toHiExpr :: HiFun -> HiExpr
toHiExpr = HiExprValue . HiValueFunction

-- | Parses arguments of functions.
parseFunctionArgs :: Parser [HiExpr]
parseFunctionArgs = parseArgs "(" ")" expr

-- | Parses string argument after dot.
parseStringArg :: Parser String
parseStringArg =
  (intercalate "-")
  <$>
    ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum))
    `sepBy1`
    (char '-')

-- | Parses dot access.
parseDotApply :: Parser [HiExpr]
parseDotApply = do
  void (char '.')
  ((: []) . HiExprValue . HiValueString . T.pack) <$> (lexeme parseStringArg)

-- | Parses term.
parseTerm :: Parser HiExpr
parseTerm = do
  function <- lexeme (choice
    [ try parseNull
    , try parseNow
    , try parseCwd
    , try parseString
    , try parseBool
    , try parseNumber
    , try parseByteArray
    , try parseList
    , try parseDict
    , try parseFunctionName
    , (symbol "(") *> expr <* (symbol ")")
    ])
  ops <- many $ do
    maybeArgs <- optional (parseFunctionArgs <|> parseDotApply)
    case maybeArgs of
      Nothing -> do
        void (symbol "!")
        return HiExprRun
      (Just args) -> return (flip HiExprApply args)
  return (foldl (flip ($)) function ops)

-- | Parses list.
parseList :: Parser HiExpr
parseList = (HiExprApply (toHiExpr HiFunList)) <$> (parseArgs "[" "]" expr)

-- | Parses dict.
parseDict :: Parser HiExpr
parseDict = HiExprDict <$> (parseArgs "{" "}" parsePair)

-- | Parses expression.
expr :: Parser HiExpr
expr = makeExprParser parseTerm table

-- | Infix binary functions table.
table :: [[Operator Parser HiExpr]]
table = [ [ InfixL (binary "*" HiFunMul)
          , InfixL (binary "/" HiFunDiv) ],
          [ InfixL (binary "+" HiFunAdd)
          , InfixL (binary "-" HiFunSub) ],
          [ InfixN (binary "<=" HiFunNotGreaterThan)
          , InfixN (binary "<" HiFunLessThan)
          , InfixN (binary ">=" HiFunNotLessThan)
          , InfixN (binary ">" HiFunGreaterThan)
          , InfixN (binary "==" HiFunEquals)
          , InfixN (binary "/=" HiFunNotEquals) ],
          [ InfixR (binary "&&" HiFunAnd) ],
          [ InfixR (binary "||" HiFunOr) ]
          ]

-- | Parses infix binary function application.
binary :: String -> HiFun -> Parser (HiExpr -> HiExpr -> HiExpr)
binary name hiFun = (f <$ parseOp name) where
  f :: HiExpr -> HiExpr -> HiExpr
  f left right = HiExprApply (toHiExpr hiFun) [left, right]

  parseOp :: String -> Parser String
  parseOp x = case x of
    "/" -> (lexeme . try) (string x <* notFollowedBy (string "="))
    _   -> symbol x

-- | Parses the given string. Returns either a parsed @HiExpr, or error.
parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (space *> expr <* eof) ""

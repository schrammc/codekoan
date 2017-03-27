-- |
-- Copyright: Christof Schramm 2016
-- License: All rights reserved
--
-- This module provides an implementation of the 'Langauge' datatype
-- for the haskell programming language. This implementation includes
-- a type 'Haskell', which is a type without content to be used as a
-- phantom type, a haskell tokenizer, and other things necessary to
-- analyze haskell code.
--
-- This module is intended to parse standard Haskell2010 Code and
-- accepts a limited set of benign syntax extensions. The tokenizer
-- will generally consume most code files, however obscure language
-- extensions may lead to incorrect results.

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Thesis.CodeAnalysis.Language.Haskell where

import Control.Applicative ((<|>))
import Data.Char
import Thesis.CodeAnalysis.Language
import Thesis.CodeAnalysis.Language.Haskell.Internal.HsToken
import Thesis.CodeAnalysis.Language.CommonTokenParsers
import Thesis.CodeAnalysis.Language.Internal
import Thesis.CodeAnalysis.Language.Internal.StandardTokenBlockAnalysis

import Data.Attoparsec.Text as AP
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Monoid ((<>))

-- | A value-less type to tag haskell-stuff at the type-level
data Haskell

haskell :: Language HsToken Haskell
haskell = Language { languageFileExtension = ".hs"
                   , languageName = "haskell"
                   , tokenize = tokenizeHs
                   , isTokenIdentifier = (isHsIdentifier)
                   , removeComments = LanguageText
                   , languageGenBlockData = \_ _ -> flatBlockData
                   }

tokenizeHs :: LanguageText Haskell -> Maybe (TokenVector HsToken Haskell)
tokenizeHs LanguageText{..} = buildTokenVector <$> parseResult
  where
    parseResult = case AP.parseOnly parseHs langText of
      Right x -> Just x
      Left _  -> Nothing
    parseHs = many1 partParser

partParser :: Parser (Int, Maybe HsToken)
partParser = do
  (txt, token) <- AP.match $ (const Nothing <$> hsCommentOrSpace) <|>
                             (Just <$> hsTokenP)
  return (Text.length txt, token)

hsTokenP :: Parser HsToken
hsTokenP =     "("     *> pure HsLParen
           <|> ")"     *> pure HsRParen
           <|> "["     *> pure HsLBrack
           <|> "]"     *> pure HsRBrack
           <|> ","     *> pure HsComma
           <|> hsPragmaP
           <|> hsStringLitP
           <|> "{"     *> pure HsLBrace
           <|> "}"     *> pure HsRBrace
           <|> hsOperatorP
           <|> numberP
           <|> hsSafeIdentifierP

-- | Parser for pragmas
hsPragmaP :: Parser HsToken
hsPragmaP = do
  string "{-#"
  manyTill anyChar (string "#-}")
  return HsPragma

hsSafeIdentifierP :: Parser HsToken
hsSafeIdentifierP = do
  (token, text) <- hsIdentifierP
  case token of
    HsIdentifier TypeOrConstructor -> return token
    HsIdentifier TypeVariableOrId  ->
      case text of
        "if"       -> return HsIf
        "then"     -> return HsThen
        "else"     -> return HsElse
        "let"      -> return HsLet
        "in"       -> return HsIn
        "do"       -> return HsDo
        "where"    -> return HsWhere
        "class"    -> return HsClass
        "data"     -> return HsData
        "default"  -> return HsDefault
        "module"   -> return HsModule
        "import"   -> return HsImport
        "infix"    -> return HsInfix
        "infixl"   -> return HsInfixL
        "infixr"   -> return HsInfixR
        "type"     -> return HsType
        "newtype"  -> return HsNewtype
        "instance" -> return HsInstance
        "case"     -> return HsCase
        "of"       -> return HsOf
        "_"        -> return HsUnderscore
        _ -> return token
    _ -> error "unexpected token in hsSafeIdentifierP"

hsOperatorP :: Parser HsToken
hsOperatorP = do
  txt <- hsOperatorTextP
  case txt of
    "->" -> return HsRightarrow
    "<-" -> return HsLeftarrow
    "@"  -> return HsAt
    "|"  -> return HsPipe
    "*"  -> return HsTimes
    "/"  -> return HsDiv
    "+"  -> return HsAdd
    "-"  -> return HsMinus
    "::" -> return HsDoubleColon
    "=>" -> return HsContextArrow
    "==" -> return HsEq
    "\\" -> return HsBackslash
    "="  -> return HsAssign
    "/=" -> return HsNotEq
    "."  -> return HsOpCompose
    "<$>" -> return HsOpFmap
    "<*>" -> return HsOpAp
    ">>=" -> return HsOpBind
    ">>"  -> return HsOpThen
    "*>"  -> return HsOpThen
    ":"   -> return HsOpCons
    xs | Text.head xs == ':' -> return HsOpInfixConstructor
    _    -> return HsOperator
    

numberP :: Parser HsToken
numberP = (((signed decimal :: Parser Integer) *> pure ()) <|>
           ((signed hexadecimal :: Parser Integer) *> pure ()) <|>
           (scientific *> pure ()) ) *> pure HsNumber

hsOperatorTextP :: Parser Text
hsOperatorTextP = do
  takeWhile1 allowed
  where
    allowed :: Char -> Bool
    allowed c = c `elem` [ ':', '!', '#', '$', '%', '&', '*', '+', '.', '/', '<'
                         , '=', '>', '?', '@', '\\', '^', '|', '-', '~']

-- | An identifier consists of a letter followed by zero or more letters,
-- digits, underscores, and single quotes.
--
-- This also accepts compound identifiers like Data.Text.splitOn
--
-- https://www.haskell.org/onlinereport/lexemes.html
--
hsIdentifierP :: Parser (HsToken, Text)
hsIdentifierP = do
  xs <- takeWhile1 (\c -> isAlpha c || c == '_')
  rest <- AP.takeWhile allowed
  let t = xs <> rest
  if isUpper $ Text.head xs
    then ("." *> hsIdentifierP) <|> (pure $ (HsIdentifier TypeOrConstructor, t))
    else return $ (HsIdentifier TypeVariableOrId ,t)
  where
    allowed c = isAlphaNum c || (c == '\'') || c == '_'

--------------------------------------------------------------------------------
--
-- Comments and whitespace

hsCommentOrSpace :: Parser ()
hsCommentOrSpace = haskellLineComment <|>
                   haskellBlockComment <|>
                   (AP.takeWhile1 isHorizontalSpace *> pure ()) <|>
                   (AP.string "\n" *> pure ())

-- | A haskell line comment
haskellLineComment :: Parser ()
haskellLineComment = lineComment "--"

-- | A haskell comment block
haskellBlockComment :: Parser ()
haskellBlockComment = do
  string "{-"
  next <- peekChar
  if next == Just '#'
    then fail "Comment block is actually a pragma block"
    else do
     let content = (string "-}" *> return ()) <|>
                   (char '-' *> content) <|>
                   (do
                       xs <- AP.takeWhile (/= '-')
                       if xs == "" then return () else content)
     content
  return ()

hsStringLitP = do
  _ <- hsStringP
  return HsStringLiteral

hsStringP = do
  char '"'
  result <- go False
  return $ '"':result
  where
    go escaped = do
      c <- anyChar
      if | c == '"' && not escaped -> return [c]
         | c == '\\' && escaped -> (c:) <$> go False
         | c == '\\' -> (c:) <$> go True
         | otherwise -> (c:) <$> go escaped

{-# LANGUAGE OverloadedStrings #-}
module Thesis.CodeAnalysis.Language.CommonTokenParsers where

import Control.Applicative ((<|>))
import Data.Monoid ((<>))

import Data.Text
import Data.Attoparsec.Text as AP

-- | A parser for a literal delimited by @\'@. Note, that while the parser is
-- named @characterLiteral@ it does not actually enforce, that the literal is a
-- single character. Empty literals or literals containing more than one
-- character are accepted as well.
characterLiteral :: Parser ()
characterLiteral = do
  char '\''
  literalContent
  where
    literalContent = do
      AP.takeWhile (\c -> not $ c `elem` ['\\','\''])
      (char '\'' *> pure ())
        <|> (string "\\\'" *> literalContent)
        <|> (anyChar *> literalContent)

-- | A paraser for a string literal as it is recognized by java.
stringLiteral :: Parser Text
stringLiteral = do
  char '\"'
  literalContent ""
  where
    literalContent xs = do
      ys <- AP.takeWhile (\c -> not $ c `elem` ['\\','\"'])
      let xs' = xs <> ys
      (char '\"' *> pure (cons '\"' xs'))
        <|> (string "\\\"" *> literalContent ("\\\"" <> xs))
        <|> (anyChar >>= \c -> literalContent (cons c xs))

-- | Java / C style comment (block or line)
javaStyleComment :: Parser ()
javaStyleComment = javaLineComment <|> javaBlockComment

-- | Java / C style comment (line)
javaLineComment :: Parser ()
javaLineComment = do
  string "//"
  let content = endOfLine <|>
                endOfInput <|>
                (do
                    xs <- AP.takeWhile (not . isEndOfLine)
                    if xs == "" then return () else content )
  content
  return ()

-- | Java / C style comment (block)
javaBlockComment :: Parser ()
javaBlockComment = do
  string "/*"
  let content = (string "*/" *> return ()) <|>
                (char '*' *> content) <|>
                (do
                    xs <- AP.takeWhile (/= '*')
                    if xs == "" then return () else content)
  content
  return ()

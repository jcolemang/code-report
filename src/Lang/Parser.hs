
module Lang.Parser
  ( runParseLang
  )
where

import Lang.Lang

import Data.Void
import Control.Monad
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Error as E
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Stream as S

langWhitespace :: M.Parsec Void String ()
langWhitespace =
  let spaceChars = void C.space1
      lineComment = ";;"
      blockCommentStart = ";#"
      blockCommentEnd = "#;"
  in
    L.space spaceChars
            (L.skipLineComment lineComment)
            (L.skipBlockComment blockCommentStart blockCommentEnd)

langLexeme :: M.Parsec Void String a -> M.Parsec Void String a
langLexeme =
  L.lexeme langWhitespace

langSymbol :: String -> M.Parsec Void String ()
langSymbol s = L.symbol langWhitespace s >> return ()

parens :: M.Parsec Void String a -> M.Parsec Void String a
parens = M.between (langSymbol "(") (langSymbol ")")


-- runParseLang :: String -> Either (E.ParseError String Void) Lang
runParseLang input =
  M.parseMaybe (langWhitespace >> parseLang) input


parseLang :: M.Parsec Void String Lang
parseLang =
  parseIdentifier
  
parseIdentifier :: M.Parsec Void String Lang
parseIdentifier = do
  iden <- langLexeme (M.some C.letterChar)
  return (Identifier iden)


  

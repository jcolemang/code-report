
module Lang.Parser
  ( runParseLang
  )
where

import Lang.Lang

import Data.Void
import Control.Monad
import Control.Applicative
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
  M.parse (langWhitespace >> parseLang <* M.eof) "" input

parseLang :: M.Parsec Void String Lang
parseLang =
  parseExpr

parseExpr :: M.Parsec Void String Lang
parseExpr =
  parseVariable
  <|> M.try parseLambda
  <|> M.try parseApplication
  <|> parseLet
  
parseIdentifier :: M.Parsec Void String String
parseIdentifier =
  langLexeme (M.some C.letterChar)

parseVariable :: M.Parsec Void String Lang
parseVariable = Identifier <$> parseIdentifier

parseFormals :: M.Parsec Void String Formals
parseFormals = do
  formals <- M.some parseIdentifier
  return $ Formals formals

parseLambda :: M.Parsec Void String Lang
parseLambda = parens $ do
  _ <- langLexeme (C.string "lambda")
  formals <- parens parseFormals
  body <- parseExpr
  return $ Lambda formals body

parseApplication :: M.Parsec Void String Lang
parseApplication = do
  func <- parens parseExpr
  return $ App func

parseLetPair :: M.Parsec Void String LetPair
parseLetPair = parens $ do
  iden <- parseIdentifier
  val <- parseExpr
  return $ LetPair iden val

parseLet :: M.Parsec Void String Lang
parseLet = parens $ do
  _ <- langLexeme (C.string "let")
  pairs <- parens $ some parseLetPair
  body <- parseExpr
  return $ Let pairs body

  
  


    
  


  

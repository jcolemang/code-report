
module Lang.Parser
  ( runParseProgram
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
runParseProgram input =
  M.parse (langWhitespace >> parseProgram <* M.eof) "" input

parseProgram :: M.Parsec Void String Program
parseProgram =
  Program <$> M.some parseStmt

parseExpr :: M.Parsec Void String Expr
parseExpr =
  parseVariable
  <|> (Primitive <$> parsePrimitive)
  <|> parens (parseLambda
               <|> parseLet
               <|> parseApplication
             )

  
parseIdentifier :: M.Parsec Void String String
parseIdentifier =
  langLexeme (M.some C.letterChar)

parseVariable :: M.Parsec Void String Expr
parseVariable = Identifier <$> parseIdentifier

parseFormals :: M.Parsec Void String Formals
parseFormals = do
  formals <- M.some parseIdentifier
  return $ Formals formals

parseLambda :: M.Parsec Void String Expr
parseLambda = do
  _ <- langLexeme (C.string "lambda")
  formals <- parens parseFormals
  body <- parseExpr
  return $ Lambda formals body

parseApplication :: M.Parsec Void String Expr
parseApplication = do
  func <- parseExpr
  args <- M.many parseExpr
  return $ App func args

parseLetPair :: M.Parsec Void String LetPair
parseLetPair = parens $ do
  iden <- parseIdentifier
  val <- parseExpr
  return $ LetPair iden val

parseLet :: M.Parsec Void String Expr
parseLet = do
  _ <- langLexeme (C.string "let")
  pairs <- parens $ M.some parseLetPair
  body <- parseExpr
  return $ Let pairs body

parseDefinition :: M.Parsec Void String Stmt
parseDefinition = parens $ do
  _ <- langLexeme (C.string "define")
  name <- parseIdentifier
  body <- parseExpr
  return $ Definition name body

parseStmt :: M.Parsec Void String Stmt
parseStmt =
  parseDefinition

parseInteger :: M.Parsec Void String Primitive
parseInteger =
  LangNum <$> langLexeme L.decimal

parsePrimitive :: M.Parsec Void String Primitive
parsePrimitive =
  parseInteger



  
  


    
  


  

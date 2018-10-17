
module Lang.Lang
where

import Text.Megaparsec

data SourceAnnotation
  = S
  { srcStart :: SourcePos
  , srcEnd :: SourcePos
  } deriving ( Show
             , Eq
             )
  

data Annotated a =
  A a SourceAnnotation
  deriving ( Show
           , Eq
           )

data Primitive
  = LangNum Int
  deriving ( Show
           , Eq
           )

data Formals
  = Formals [String]
  deriving ( Show
           , Eq
           )

data LetPair
  = LetPair String (Annotated Expr)
  deriving ( Show
           , Eq
           )

data Expr
  = Identifier String
  | Lambda Formals (Annotated Expr)
  | App (Annotated Expr) [Annotated Expr]
  | Let [LetPair] (Annotated Expr)
  | Primitive Primitive
  deriving ( Show
           , Eq
           )

data Stmt
  = Definition String (Annotated Expr)
  deriving ( Show
           , Eq
           )

data Program
  = Program [Annotated Stmt]
  deriving ( Show
           , Eq
           )

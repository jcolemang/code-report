
module Lang.Lang
where

data Primitive
  = LangNum Int
  deriving ( Show )

data Formals
  = Formals [String]
  deriving ( Show )

data LetPair
  = LetPair String Expr
  deriving ( Show )

data Expr
  = Identifier String
  | Lambda Formals Expr
  | App Expr [Expr]
  | Let [LetPair] Expr
  | Primitive Primitive
  deriving ( Show )

data Stmt
  = Definition String Expr
  deriving ( Show )

data Program
  = Program [Stmt]
  deriving ( Show )

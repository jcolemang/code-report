
module Lang.Lang
where

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
  = LetPair String Expr
  deriving ( Show
           , Eq
           )

data Expr
  = Identifier String
  | Lambda Formals Expr
  | App Expr [Expr]
  | Let [LetPair] Expr
  | Primitive Primitive
  deriving ( Show
           , Eq
           )

data Stmt
  = Definition String Expr
  deriving ( Show
           , Eq
           )

data Program
  = Program [Stmt]
  deriving ( Show
           , Eq
           )

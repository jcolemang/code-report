
module Lang.Lang
where

data Formals
  = Formals [String]
  deriving ( Show )

data LetPair
  = LetPair String Lang
  deriving ( Show )

data Lang
  = Identifier String
  | Lambda Formals Lang
  | App Lang
  | Let [LetPair] Lang
  deriving ( Show )

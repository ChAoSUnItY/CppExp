module CppExp.Token (Token (..), identT, openParenthesisT, closeParenthesisT, commaT) where

data Token
  = Ident String
  | Comma
  | Ellipsis
  | OpenParethesis
  | CloseParenthesis
  | DirectiveStart
  | Backslash
  | Newline
  deriving (Show)

identT :: Token -> Bool
identT (Ident _) = True
identT _ = False

openParenthesisT :: Token -> Bool
openParenthesisT OpenParethesis = True
openParenthesisT _ = False

closeParenthesisT :: Token -> Bool
closeParenthesisT CloseParenthesis = True
closeParenthesisT _ = False

commaT :: Token -> Bool
commaT Comma = True
commaT _ = False

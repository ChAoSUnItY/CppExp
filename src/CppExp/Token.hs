module CppExp.Token (module CppExp.Token) where

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

tokenToString :: Token -> String
tokenToString tk = case tk of
    Ident ident      -> ident
    Comma            -> ","
    Ellipsis         -> "..."
    OpenParethesis   -> "("
    CloseParenthesis -> ")"
    DirectiveStart   -> "#"
    Backslash        -> "\\"
    Newline          -> "\n"

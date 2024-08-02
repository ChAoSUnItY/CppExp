module CppExp.Token (module CppExp.Token) where

data Token
    = Ident String
    | Comma
    | Ellipsis
    | OpenParethesis
    | CloseParenthesis
    | OpenBracket
    | CloseBracket
    | OpenSquare
    | CloseSquare
    | DirectiveStart
    | Backslash
    | Newline
    | Plus
    | Minus
    | Mult
    | Divide
    deriving (Show)

tokenToString :: Token -> String
tokenToString tk = case tk of
    Ident ident      -> ident
    Comma            -> ","
    Ellipsis         -> "..."
    OpenParethesis   -> "("
    CloseParenthesis -> ")"
    OpenBracket      -> "<"
    CloseBracket     -> ">"
    OpenSquare       -> "["
    CloseSquare      -> "]"
    DirectiveStart   -> "#"
    Backslash        -> "\\"
    Newline          -> "\n"
    Plus             -> "+"
    Minus            -> "-"
    Mult             -> "*"
    Divide           -> "/"

-- | Safely converts list of token to string by inserting space between tokens
tokensToString :: [Token] -> String
tokensToString = unwords . map tokenToString

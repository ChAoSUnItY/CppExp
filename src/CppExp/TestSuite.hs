module CppExp.TestSuite (macroDefOnly1) where

import CppExp.Token (Token (..))

macroDefOnly1 :: [Token]
macroDefOnly1 =
  [ Ident "a",
    OpenParethesis,
    Ident "gg",
    Comma,
    Ident "kek",
    CloseParenthesis,
    Ident "gg",
    Backslash,
    Newline,
    Ident "kek",
    Comma,
    Newline
  ]

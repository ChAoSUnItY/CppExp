module CppExp.Parser (module CppExp.Parser) where

import Prelude hiding (fail, (*>), (<*), (<*>))
import CppExp.Data (PState, MacroParam (..), Def (Alias, Macro), Var, mapVar, defineSym)
import Data.Char (isAlphaNum, isAlpha)
import CppExp.Token (Token (..))
import qualified Data.Bifunctor as Bifunctor
import CppExp.Utils (uncurry3r)

type Parser a s b = ([a], s) -> [(([a], s), b)]

-- Symbol parser
symbol :: (Eq a) => a -> Parser a s a
symbol x = satisfy (x ==)

-- Token parser
token :: (Eq a) => [a] -> Parser a s [a]
token k (xs, s)
  | k == take n xs = [((drop n xs, s), xs)]
  | otherwise = []
  where
    n = length k

-- symbol predicate parser
satisfy :: (a -> Bool) -> Parser a s a
satisfy _ ([], _) = []
satisfy p (x : xs, s) = [((xs, s), x) | p x]

-- "id" parser
epsilon :: Parser a s ()
epsilon = succeed ()

-- "const" parser
succeed :: r -> Parser a s r
succeed v xs = [(xs, v)]

-- "fail" parser
fail :: Parser a s r
fail _ = []

infixr 6 <&> , <& , &>

infixr 4 <|>

-- Sequential parser
(<&>) :: Parser a s x -> Parser a s y -> Parser a s (x, y)
(p1 <&> p2) xs =
  [ (xs2, (v1, v2))
    | (xs1, v1) <- p1 xs,
      (xs2, v2) <- p2 xs1
  ]

-- First value selector
(<&) :: Parser a s r -> Parser a s v -> Parser a s r
p <& q = p <&> q <@ fst

-- Second value selector
(&>) :: Parser a s r -> Parser a s v -> Parser a s v
p &> q = p <&> q <@ snd

-- Maybe use Alternative?
-- Alternative parser
(<|>) :: Parser a s r -> Parser a s r -> Parser a s r
(p1 <|> p2) xs = p1 xs ++ p2 xs

-- Space parser
sp :: Parser Char s r -> Parser Char s r
sp p = p . Bifunctor.first (dropWhile (== ' '))

-- Complete parser
just :: Parser a s r -> Parser a s r
just p = filter (null . fst . fst) . p

infixr 5 <@, <@-, <@=

-- Result transformer
(<@) :: Parser a s r -> (r -> v) -> Parser a s v
(p <@ f) xs = [(ys, f v) | (ys, v) <- p xs]

-- State transformer
(<@-) :: Parser a s r -> (s -> s) -> Parser a s r
(p <@- f) xs = [((a, f s), v) | ((a, s), v) <- p xs]

-- State/Result transformer
(<@=) :: Parser a s r -> ((s, r) -> (s, v)) -> Parser a s v
(p <@= f) xs = [ ((a, s'), v) 
                 | ((a, s), r) <- p xs,
                   (s', v) <- [f (s, r)]
               ]

-- Parser for zero or more
many :: Parser a s r -> Parser a s [r]
many p = p <&> many p <@ uncurry (:)
         <|> succeed []

-- Parser for one or more
many1 :: Parser a s r -> Parser a s [r]
many1 p = p <&> many p <@ uncurry (:)

-- Greedy parser
first :: Parser a s r -> Parser a s r
first p xs | null r = []
           | otherwise = [head r]
           where r = p xs

-- Actual Parser

newline :: Parser Char s ()
newline = symbol '\n' &> epsilon

backslash :: Parser Char s ()
backslash = symbol '\\' &> epsilon

comma :: Parser Char s ()
comma = symbol ',' &> epsilon

lparen :: Parser Char s ()
lparen = symbol '(' &> epsilon

rparen :: Parser Char s ()
rparen = symbol ')' &> epsilon

directiveStart :: Parser Char s ()
directiveStart = symbol '#' &> epsilon

ident :: Parser Char s String
ident = first $ (satisfy isAlpha <|> symbol '_') <&> many (satisfy isAlphaNum) <@ uncurry (:)

ppToken :: Parser Char s Token
ppToken = comma &> succeed Comma
        <|> ident <@ Ident

anyToken :: Parser Char s Token
anyToken = newline &> succeed Newline
         <|> backslash &> succeed Backslash
         <|> comma &> succeed Comma
         <|> lparen &> succeed OpenParethesis
         <|> rparen &> succeed CloseParenthesis
         <|> ident <@ Ident

macroParams :: Parser Char PState MacroParam
macroParams = sp (token "...") <@ const VarArgs
            <|> sp ident <&> sp comma &> macroParams <@ uncurry Param
            <|> sp ident <@ flip Param EndOfParams

aliasTkList :: Parser Char PState [Token]
aliasTkList = sp newline  <@ const []
            <|> sp ppToken <&> sp aliasTkList <@ uncurry (:)

macroTkList :: Parser Char PState [Either Token Var]
macroTkList pstate@(_, state)
        = sp newline <@ const []
        <|> sp backslash &> sp newline &> macroTkList
        <|> sp ppToken <&> macroTkList <@ uncurry (:) . Bifunctor.first (`mapVar` state)
        $ pstate

macroDef :: Parser Char PState Def
macroDef = sp ident <&> aliasTkList <@ uncurry Alias
         <|> sp ident <&> lparen &> macroParams <&> sp rparen &> macroTkList <@ uncurry3r Macro

directive :: Parser Char PState ()
directive = sp (token "define") &> macroDef <@= (\(s, def) -> (defineSym def s, ()))

cpp :: Parser Char PState [Token]
cpp = sp directiveStart &> directive &> cpp
    <|> sp anyToken <&> cpp <@ uncurry (:)
    <|> succeed []

module CppExp.Parser (module CppExp.Parser) where

import Prelude hiding (fail)
import CppExp.Data (PState, MacroParam (..), Def (Alias, Macro), Var, mapVar, defineSym, newState, defineMacroParams)
import Data.Char (isAlphaNum, isAlpha)
import CppExp.Token (Token (..), tokenToString)
import qualified Data.Bifunctor as Bifunctor
import CppExp.Utils (uncurry3r)
import CppExp.Replace (tryReplace)
import Data.Maybe (fromMaybe)

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

infixr 5 <@, <@?, <@-, <@~, <@=, <@?=

-- Result transformer
(<@) :: Parser a s r -> (r -> v) -> Parser a s v
(p <@ f) xs = [(ys, f r) | (ys, r) <- p xs]

(<@?) :: Parser a s r -> (r -> Maybe v) -> Parser a s v
(p <@? f) xs = [ (ys, v)
                 | (ys, r) <- p xs
                 , Just v <- [f r]
               ]

-- State transformer
(<@-) :: Parser a s r -> (s -> s) -> Parser a s r
(p <@- f) xs = [((a, f s), r) | ((a, s), r) <- p xs]

-- Result by state transformer
(<@~) :: Parser a s r -> ((s, r) -> v) -> Parser a s v
(p <@~ f) xs = [(ys, f (s, r)) | (ys@(_, s), r) <- p xs]

-- State/Result transformer
(<@=) :: Parser a s r -> ((s, r) -> (s, v)) -> Parser a s v
(p <@= f) xs = [ ((a, s'), v)
                 | ((a, s), r) <- p xs
                 , (s', v) <- [f (s, r)]
               ]

(<@?=) :: Parser a s r -> ((s, r) -> Maybe (s, v)) -> Parser a s v
(p <@?= f) xs = [ ((a, s'), v)
                  | ((a, s), r) <- p xs
                  , Just (s', v) <- [f (s, r)]
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

type CppParser r = Parser Char PState r

newline :: CppParser ()
newline = symbol '\n' &> epsilon

backslash :: CppParser ()
backslash = symbol '\\' &> epsilon

ellipsis :: CppParser ()
ellipsis = token "..." &> epsilon

comma :: CppParser ()
comma = symbol ',' &> epsilon

lparen :: CppParser ()
lparen = symbol '(' &> epsilon

rparen :: CppParser ()
rparen = symbol ')' &> epsilon

directiveStart :: CppParser ()
directiveStart = symbol '#' &> epsilon

identStart :: CppParser Char
identStart = satisfy isAlpha
           <|> symbol '_'

identRemain :: CppParser Char
identRemain = satisfy isAlphaNum
            <|> symbol '_'

ident :: CppParser String
ident = first $ identStart <&> many identRemain <@ uncurry (:)

ppToken :: CppParser Token
ppToken = comma &> succeed Comma
        <|> ident <@ Ident

identReplacable :: CppParser [Token]
identReplacable state@(_, pstate)
        = first
        ( sp ident <&> sp lparen &> sp macroArgs <& sp rparen <@? (\(macroName, args) -> tryReplace macroName (Just args) pstate)
        <|> sp ident <@? (\aliasName -> tryReplace aliasName Nothing pstate)
        ) state

macroArgs :: CppParser [[Token]]
macroArgs = sp macroArgAnyToken <&> sp comma &> macroArgs <@ uncurry (:)
          <|> sp macroArgAnyToken <@ (: [])

macroArgAnyToken :: CppParser [Token]
macroArgAnyToken
        = lparen &> macroArgAnyToken <&> rparen &> macroArgAnyToken <@ (\(inner, outer) -> OpenParethesis : inner ++ [CloseParenthesis] ++ outer)
        <|> ident <&> macroArgAnyToken <@ uncurry (:) . Bifunctor.first Ident
        <|> succeed []

-- | Accepts any legal tokens with few exceptions:
--   @Ident@ 
anyToken :: CppParser Token
anyToken = newline &> succeed Newline
         <|> backslash &> succeed Backslash
         <|> directiveStart &> succeed DirectiveStart
         <|> ellipsis &> succeed Ellipsis
         <|> comma &> succeed Comma
         <|> lparen &> succeed OpenParethesis
         <|> rparen &> succeed CloseParenthesis

macroParams :: CppParser MacroParam
macroParams = macroParams' <@= (\(pst, params) -> (defineMacroParams params pst, params))

macroParams' :: CppParser MacroParam
macroParams' = sp ellipsis &> succeed VarArgs
            <|> sp ident <&> sp comma &> macroParams <@ uncurry Param
            <|> sp ident <@ flip Param EndOfParams

aliasTkList :: CppParser [Token]
aliasTkList = sp newline  &> succeed []
            <|> sp ppToken <&> sp aliasTkList <@ uncurry (:)

macroTkList :: CppParser [Either Token Var]
macroTkList state@(_, pstate)
        = sp newline <@ const []
        <|> sp backslash &> sp newline &> macroTkList
        <|> sp ppToken <&> macroTkList <@ uncurry (:) . Bifunctor.first (`mapVar` pstate)
        $ state

macroDef :: CppParser Def
macroDef = first
         $ sp ident <&> aliasTkList <@ uncurry Alias
         <|> sp ident <&> lparen &> macroParams <&> sp rparen &> macroTkList <@ uncurry3r Macro

directive :: CppParser ()
directive = sp (token "define") &> macroDef <@= (\(s, def) -> (defineSym def s, ()))

cpp :: CppParser [Token]
cpp = sp directiveStart &> directive &> cpp
    <|> identReplacable <&> cpp <@ uncurry (++)
    <|> sp anyToken <&> cpp <@ uncurry (:)
    <|> succeed []

prettyCpp :: String -> String
prettyCpp input = unwords $ map tokenToString output
    where
      output = snd $ head $ just cpp (input, newState)

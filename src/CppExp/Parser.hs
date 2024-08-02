module CppExp.Parser (module CppExp.Parser) where

import Prelude hiding (fail)
import CppExp.Data (
    PState (PState),
    MacroParam (..),
    Def (..),
    Var,
    mapVar,
    defineSym,
    newState,
    defineMacroParams,
    removeAlias,
    removeMacro, applicable
    )
import Data.Char (isAlphaNum, isAlpha)
import CppExp.Token (Token (..), tokenToString, tokensToString)
import qualified Data.Bifunctor as Bifunctor
import CppExp.Utils (uncurry3r, (!?))
import Data.List (elemIndex, intercalate)

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

infixr 5 @>, <@, <@?, <@-, <@~, <@=, <@?=

(@>) :: Parser a s r -> v -> Parser a s v
(p @> f) xs = [(ys, f) | (ys, _) <- p xs]

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

-- Token Parsers

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

-- Pure token parsers
-- (Parsers that always produces tokens without triggering additional actions)
pureAnyToken :: CppParser Token
pureAnyToken = symbol '<' @> OpenBracket
             <|> symbol '>' @> CloseBracket
             <|> symbol '[' @> OpenSquare
             <|> symbol ']' @> CloseSquare
             <|> symbol '+' @> Plus
             <|> symbol '-' @> Minus
             <|> symbol '*' @> Mult
             <|> symbol '/' @> Divide

-- | Accepts any legal tokens with few exceptions:
--   @Ident@ 
anyToken :: CppParser Token
anyToken = newline @> Newline
         <|> backslash @> Backslash
         <|> directiveStart @> DirectiveStart
         <|> ellipsis @> Ellipsis
         <|> comma @> Comma
         <|> lparen @> OpenParethesis
         <|> rparen @> CloseParenthesis
         <|> pureAnyToken
-- | Accpets any token in
ppToken :: CppParser Token
ppToken = ident <@ Ident
        <|> lparen @> OpenParethesis
        <|> rparen @> CloseParenthesis
        <|> pureAnyToken

-- Expandable parsers

-- | Tries to expand a potential alias / macro. Returns @Just@ if expansion succeed, 
--   otherwise @Nothing@.
tryExpand :: String -> Maybe [[Token]] -> PState -> Maybe [Token]
tryExpand defName args pst
    = case args of
        Just args' -> let macro = removeMacro defName pst in
            case macro of
                Just (Macro _ params tks, pst') ->
                    if applicable args' params then
                        let result = expandMacro tks args' (defineMacroParams params pst') in
                            case result of
                                Just tks' ->
                                    Just $ snd $ head $ just cpp (tokensToString tks', pst')
                                Nothing   -> Nothing
                    else
                        error $ "Fatal: Argument count is not applicable to macro `" ++ defName ++ "`"
                _ -> Nothing
        Nothing    -> let alias = removeAlias defName pst in
            case alias of
                Just (Alias _ tks, pst') ->
                    Just $ snd $ head $ just cpp (tokensToString tks, pst')
                _ -> Nothing

expandMacro :: [Either Token Var] -> [[Token]] -> PState -> Maybe [Token]
expandMacro [] _ _ = Just []
expandMacro (Right var : tks) args pst@(PState _ vs)
    = case arg of
        Just rp -> (rp ++) <$> expandMacro tks args pst
        Nothing -> Nothing
    where
        idx = elemIndex var vs
        arg = if var == "__VAR_ARGS__" then
            intercalate [Comma] . flip drop args <$> idx
        else
            idx >>= (args !?)
expandMacro (Left tk : tks) args pst
    = (tk :) <$> expandMacro tks args pst

identExpandable :: CppParser [Token]
identExpandable state@(_, pstate)
    = first
    ( sp ident <&> sp lparen &> sp macroArgs <& sp rparen <@? (\(macroName, args) -> tryExpand macroName (Just args) pstate)
    <|> sp ident <@? (\aliasName -> tryExpand aliasName Nothing pstate)
    <|> sp ident <@ pure . Ident
    ) state

macroArgs :: CppParser [[Token]]
macroArgs = sp macroArgAnyToken <&> sp comma &> macroArgs <@ uncurry (:)
          <|> sp macroArgAnyToken <@ (: [])

macroArgAnyToken :: CppParser [Token]
macroArgAnyToken
    = macroArgParenStrictMatch <&> macroArgAnyToken <@ uncurry (++)
    <|> succeed []

macroArgParenStrictMatch :: CppParser [Token]
macroArgParenStrictMatch
    = sp lparen &> macroArgAnyToken <& sp rparen <@
        (\inner -> OpenParethesis : inner ++ [CloseParenthesis])
    <|> macroArgAnyToken' <@ pure

macroArgAnyToken' :: CppParser Token
macroArgAnyToken'
    = sp ident <@ Ident
     <|> sp pureAnyToken

macroParams :: CppParser MacroParam
macroParams = macroParams' <@= (\(pst, params) -> (defineMacroParams params pst, params))

macroParams' :: CppParser MacroParam
macroParams' = sp ellipsis @> VarArgs
            <|> sp ident <&> sp comma &> macroParams <@ uncurry Param
            <|> sp ident <@ flip Param EndOfParams

aliasTkList :: CppParser [Token]
aliasTkList = sp newline @> []
            <|> sp backslash &> sp newline &> aliasTkList
            <|> sp ppToken <&> sp aliasTkList <@ uncurry (:)

macroTkList :: CppParser [Either Token Var]
macroTkList state@(_, pstate)
    = sp newline @> []
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
    <|> identExpandable <&> cpp <@ uncurry (++)
    <|> sp anyToken <&> cpp <@ uncurry (:)
    <|> succeed []

prettyCpp :: String -> String
prettyCpp input = unwords $ map tokenToString output
    where
      output = snd $ head $ just cpp (input, newState)

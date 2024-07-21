module Lib (module Lib, module Token) where

import Data.List (find)
import Control.Monad.State (get, put, State)
import Token (Token (Ident, Ellipsis, Newline, OpenParethesis, Backslash), commaT, closeParenthesisT)

type Var = String
type DefName = String

data MacroParam = Param Var MacroParam
                | Term
                | VarArgs
                deriving (Show)

collectVars :: MacroParam -> [Var]
collectVars (Param var remain) = var : collectVars remain
collectVars _ = []

data Def = Alias DefName [Token]
         | Macro DefName MacroParam [Either Token Var]
         deriving (Show)

type SymTable = [Def]

emptySymTable :: SymTable
emptySymTable = []

defineSym :: Def -> SymTable -> SymTable
defineSym = (:)

getDefName :: Def -> DefName
getDefName (Alias name _) = name
getDefName (Macro name _ _) = name

findDef :: Token -> SymTable -> Maybe Def
findDef (Ident ident) st = find (\d -> getDefName d == ident) st
findDef _ _ = Nothing

type Parser a = State ([Token], SymTable) a

acceptToken :: (Token -> Bool) -> Parser Bool
acceptToken tkPred = do
    (tks, st) <- get
    case tks of
        (tk : tks') ->
            if tkPred tk then do
                put (tks', st)
                return True
            else do
                put (tks, st)
                return False
        _ -> return False

identToken :: Parser (Maybe String)
identToken = do
    (tks, st) <- get
    case tks of
        (Ident ident : tks') -> do
            put (tks', st)
            return $ Just ident
        _ -> return Nothing

parseMacroParam :: Parser (Maybe MacroParam)
parseMacroParam = do
    (tks, st) <- get
    case tks of
        (Ellipsis : tks') -> do
            put (tks', st)
            return (Just VarArgs)
        (Ident ident : tks') -> do
            put (tks', st)
            hasNextParam <- acceptToken commaT

            if hasNextParam then do
                param <- parseMacroParam
                return $ Param ident <$> param
            else do
                return $ Just $ Param ident Term
        _ ->
            return Nothing

parseAliasRep :: Parser [Token]
parseAliasRep = do
    (tks, st) <- get
    case tks of
        (Newline : tks') -> do
            put (tks', st)
            return []
        (tk : tks') -> do
            put (tks', st)
            remain <- parseAliasRep
            return (tk : remain)
        [] -> do
            return []

parseMacroRep :: [Var] -> Parser (Maybe [Either Token Var])
parseMacroRep vars = do
    (tks, st) <- get
    case tks of
        (Newline : tks') -> do
            put (tks', st)
            return $ Just []
        (Backslash : Newline : tks') -> do
            put (tks', st)
            remain <- parseMacroRep vars
            return $ liftA2 (:) (Just $ Left Newline) remain
        (ident'@(Ident ident) : tks') -> do
            put (tks', st)
            remain <- parseMacroRep vars
            case find (== ident) vars of
                (Just var) ->
                    return $ liftA2 (:) (Just $ Right var) remain
                Nothing ->
                    return $ liftA2 (:) (Just $ Left ident') remain
        (tk : tks') -> do
            put (tks', st)
            remain <- parseMacroRep vars
            return $ liftA2 (:) (Just $ Left tk) remain
        _ -> do
            return Nothing

parseMacroDefinition :: Parser (Maybe Def)
parseMacroDefinition = do
    defName <- identToken
    (tks, st) <- get
    case (defName, tks) of
        (Just defName', OpenParethesis : tks') -> do
            put (tks', st)
            macroParams <- parseMacroParam
            result <- acceptToken closeParenthesisT
            let vars = maybe [] collectVars macroParams
            if result then do
                replacement <- parseMacroRep vars
                return $ Macro defName' <$> macroParams <*> replacement
            else
                return Nothing
        (Just defName', _) -> do
            Just . Alias defName' <$> parseAliasRep
        _ -> return Nothing


-- exec :: SymTable -> [Token] -> [Token]

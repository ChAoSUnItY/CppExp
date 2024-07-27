module CppExp (module CppExp) where

import Control.Monad.State (State, evalState, execState, get, modify, put, runState)
import CppExp.Token (Token (..), closeParenthesisT, commaT)
import Data.Bifunctor (Bifunctor (second))
import Data.List (find)
import CppExp.Parser (Parser)

type Var = String

type DefName = String

data MacroParam
  = Param Var MacroParam
  | Term
  | VarArgs
  deriving (Show)

collectVars :: MacroParam -> [Var]
collectVars (Param var remain) = var : collectVars remain
collectVars _ = []

data Def
  = Alias DefName [Token]
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

type Parser' a = State ([Token], SymTable) a

acceptToken :: (Token -> Bool) -> Parser' Bool
acceptToken tkPred = do
  (tks, st) <- get
  case tks of
    (tk : tks') ->
      if tkPred tk
        then do
          put (tks', st)
          return True
        else do
          put (tks, st)
          return False
    _ -> return False

identToken :: Parser' (Maybe String)
identToken = do
  (tks, st) <- get
  case tks of
    (Ident ident : tks') -> do
      put (tks', st)
      return $ Just ident
    _ -> return Nothing

parseMacroParam :: Parser' (Maybe MacroParam)
parseMacroParam = do
  (tks, st) <- get
  case tks of
    (Ellipsis : tks') -> do
      put (tks', st)
      return (Just VarArgs)
    (Ident ident : tks') -> do
      put (tks', st)
      hasNextParam <- acceptToken commaT

      if hasNextParam
        then do
          param <- parseMacroParam
          return $ Param ident <$> param
        else do
          return $ Just $ Param ident Term
    _ ->
      return Nothing

parseAliasRep :: Parser' [Token]
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

parseMacroRep :: [Var] -> Parser' (Maybe [Either Token Var])
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

parseMacroDefinition :: Parser' (Maybe Def)
parseMacroDefinition = do
  defName <- identToken
  (tks, st) <- get
  case (defName, tks) of
    (Just defName', OpenParethesis : tks') -> do
      put (tks', st)
      macroParams <- parseMacroParam
      result <- acceptToken closeParenthesisT
      let vars = maybe [] collectVars macroParams
      if result
        then do
          replacement <- parseMacroRep vars
          return $ Macro defName' <$> macroParams <*> replacement
        else
          return Nothing
    (Just defName', _) -> do
      Just . Alias defName' <$> parseAliasRep
    _ -> return Nothing

parseDirective :: Parser' (Maybe ())
parseDirective = do
  (tks, st) <- get
  case tks of
    (Ident "define" : tks') -> do
      put (tks', st)
      def <- parseMacroDefinition
      case def of
        (Just def') -> do
          modify (second (defineSym def'))
          return $ Just ()
        Nothing ->
          return Nothing
    _ ->
      return Nothing

exec' :: Parser' (Maybe [Token])
exec' = do
  (tks, st) <- get
  case tks of
    (DirectiveStart : tks') -> do
      put (tks', st)
      result <- parseDirective
      case result of
        (Just ()) ->
          exec'
        Nothing ->
          return Nothing
    -- (Ident ident : tks') ->
    -- TODO: Execute replacement
    (tk : tks') -> do
      put (tks', st)
      liftA2 (:) (Just tk) <$> exec'
    [] ->
      return $ Just []

runCpp :: [Token] -> (Maybe [Token], ([Token], SymTable))
runCpp tks = runState exec' (tks, emptySymTable)

evalCpp :: [Token] -> Maybe [Token]
evalCpp tks = evalState exec' (tks, emptySymTable)

execCpp :: [Token] -> ([Token], SymTable)
execCpp tks = execState exec' (tks, emptySymTable)

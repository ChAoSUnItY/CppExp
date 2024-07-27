module CppExp.Data (module CppExp.Data) where

import CppExp.Token (Token (..))
import Data.List (find)

type Var = String

type DefName = String

data MacroParam
  = Param Var MacroParam
  | EndOfParams
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

type VarScope = [Var]

getDefName :: Def -> DefName
getDefName (Alias name _) = name
getDefName (Macro name _ _) = name

findDef :: Token -> SymTable -> Maybe Def
findDef (Ident ident) st = find (\d -> getDefName d == ident) st
findDef _ _ = Nothing

data PState = PState SymTable VarScope
  deriving (Show)

newState :: PState
newState = PState [] []

mapVar :: Token -> PState -> Either Token Var
mapVar ident'@(Ident ident) (PState _ vs) = case p of
  Just var -> Right var
  Nothing -> Left ident'
  where
    p = find (== ident) vs
mapVar t _ = Left t

defineSym :: Def -> PState -> PState
defineSym def (PState st vs) = PState (def : st) vs

module CppExp.Data (module CppExp.Data) where

import CppExp.Token (Token (..))
import Data.List (find)
import CppExp.Utils (removeFirst)
import Control.Monad (liftM2)
import Data.Bifunctor (Bifunctor(second))

type Var = String

type DefName = Var

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

isAlias :: Def -> Bool
isAlias Alias {} = True
isAlias _        = False

isMacro :: Def -> Bool
isMacro Macro {} = True
isMacro _        = False

getDefName :: Def -> DefName
getDefName (Alias name _) = name
getDefName (Macro name _ _) = name

type SymTable = [Def]

findDef' :: DefName -> SymTable -> Maybe Def
findDef' defName = find ((== defName) . getDefName)

removeDef' :: (Def -> Bool) -> DefName -> SymTable -> Maybe (Def, SymTable)
removeDef' p defName st
    = case def of
        Just def' -> Just (def', st')
        Nothing  -> Nothing
    where
        def = findDef' defName st
        st' = removeFirst (liftM2 (&&) p ((== defName) . getDefName)) st

type VarScope = [Var]

data PState = PState SymTable VarScope
    deriving (Show)

newState :: PState
newState = PState [] []

findDef :: String -> PState -> Maybe Def
findDef ident (PState st _) = findDef' ident st

removeDef :: (Def -> Bool) -> DefName -> PState -> Maybe (Def, PState)
removeDef p defName (PState st vs)
    = removeDef' p defName st >>= Just . second (`PState` vs)

removeAlias :: DefName -> PState -> Maybe (Def, PState)
removeAlias = removeDef isAlias

removeMacro :: DefName -> PState -> Maybe (Def, PState)
removeMacro = removeDef isMacro

mapVar :: Token -> PState -> Either Token Var
mapVar ident'@(Ident ident) (PState _ vs)
    = case p of
        Just var -> Right var
        Nothing  -> Left ident'
    where
        p = find (== ident) vs
mapVar t _ = Left t

defineSym :: Def -> PState -> PState
defineSym def (PState st vs) = PState (def : st) vs

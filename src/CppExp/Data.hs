module CppExp.Data (
    PState (PState), 
    MacroParam (..), 
    Def (..), 
    Var,
    applicable,
    mapVar, 
    defineSym, 
    newState, 
    defineMacroParams, 
    removeAlias, 
    removeMacro
) where

import CppExp.Token (Token (..))
import Data.List (find)
import CppExp.Utils (removeFirst)
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
collectVars VarArgs = ["__VAR_ARGS__"]
collectVars EndOfParams = []

hasVarArgs :: MacroParam -> Bool
hasVarArgs (Param _ remain) = hasVarArgs remain
hasVarArgs VarArgs = True
hasVarArgs EndOfParams = False

paramsLength :: MacroParam -> Int
paramsLength (Param _ remain) = 1 + paramsLength remain
paramsLength VarArgs = 1
paramsLength EndOfParams = 0

-- | Checks if argument list has either same length with @MacroParam@, 
--   or has same or larger length than @MacroParam@ (if @MacroParam@ ends with VarArgs)
applicable :: [[Token]] -> MacroParam -> Bool
applicable tks macroParams
    | hasVarArgs macroParams && tksLen >= paramsLen = True
    | tksLen == paramsLen = True
    | otherwise = False
    where
        tksLen = length tks
        paramsLen = paramsLength macroParams

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

type SymTable = [(String, Def)]

findDef' :: DefName -> SymTable -> Maybe Def
findDef' = lookup

findDefByPred' :: (Def -> Bool) -> DefName -> SymTable -> Maybe Def
findDefByPred' p defName st =
    case def of
        Just def' ->
            if p def' then
                Just def'
            else
                Nothing
        Nothing -> Nothing
    where
        def = findDef' defName st

removeDefByPred' :: (Def -> Bool) -> DefName -> SymTable -> Maybe (Def, SymTable)
removeDefByPred' p defName st
    = case def of
        Just def' -> Just (def', st')
        Nothing  -> Nothing
    where
        def = findDefByPred' p defName st
        st' = removeFirst ((== defName) . fst) st

type VarScope = [Var]

data PState = PState SymTable VarScope
    deriving (Show)

newState :: PState
newState = PState [] []

removeDefByPred :: (Def -> Bool) -> DefName -> PState -> Maybe (Def, PState)
removeDefByPred p defName (PState st vs)
    = removeDefByPred' p defName st >>= Just . second (`PState` vs)

removeAlias :: DefName -> PState -> Maybe (Def, PState)
removeAlias = removeDefByPred isAlias

removeMacro :: DefName -> PState -> Maybe (Def, PState)
removeMacro = removeDefByPred isMacro

mapVar :: Token -> PState -> Either Token Var
mapVar ident'@(Ident ident) (PState _ vs)
    = case p of
        Just var -> Right var
        Nothing  -> Left ident'
    where
        p = find (== ident) vs
mapVar t _ = Left t

defineVars :: [Var] -> PState -> PState
defineVars vars (PState st _) = PState st vars

defineMacroParams :: MacroParam -> PState -> PState
defineMacroParams macroParams = defineVars (collectVars macroParams)

defineSym :: Def -> PState -> PState
defineSym def (PState st vs) = PState ((defName, def) : st') vs
    where
        defName = getDefName def
        st' = removeFirst ((== defName) . fst) st

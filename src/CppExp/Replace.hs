module CppExp.Replace(module CppExp.Replace) where
import CppExp.Data (Def (Alias, Macro), PState (PState), removeAlias, Var, removeDef, defineMacroParams, removeMacro)
import CppExp.Token (Token (..))
import Data.List (elemIndex)
import CppExp.Utils ((!?))

tryReplace :: String -> Maybe [[Token]] -> PState -> Maybe [Token]
tryReplace defName args pst
    = case args of
        Just args' -> let result = removeMacro defName pst in
            case result of
                Just (Macro _ params tks, pst') -> replaceMacro tks args' (defineMacroParams params pst')
                _                               -> Nothing
        Nothing    -> let result = removeAlias defName pst in
            case result of
                Just (Alias _ tks, _) -> Just tks
                _                     -> Nothing

-- Replaces alias with sequence of tokens, the target definition is guaranteed to be exist
-- replaceAlias :: [Token] -> PState -> [Token]
-- replaceAlias [] _ = []
-- replaceAlias (ident'@(Ident ident) : tks) pst
--     = case result of
--         Just (Alias _ rpTks, pst') -> replaceAlias rpTks pst' ++ replaceAlias tks pst
--         _                          -> ident' : replaceAlias tks pst
--     where
--         result = removeAlias ident pst
-- replaceAlias (tk : tks) pst = tk : replaceAlias tks pst

-- Replace macro with sequence of tokens, the target definition is guaranteed to be exist
replaceMacro :: [Either Token Var] -> [[Token]] -> PState -> Maybe [Token]
replaceMacro [] _ _ = Just []
replaceMacro (Right var : tks) args pst@(PState _ vs)
    = case arg of
        Just rp -> (rp ++) <$> replaceMacro tks args pst
        Nothing -> Nothing
    where
        idx = elemIndex var vs
        arg = idx >>= (args !?)
replaceMacro (Left tk : tks) args pst
    = (tk :) <$> replaceMacro tks args pst 

-- #define F(X,Y) X+Y
-- 
-- z = F(2*z, 3*w) * 2 

-- [Var "X", Plus, Var "Y"] -> [("X", [Num 2, Mul, Ident z]), ...] -> PState { st = {...}, vs = ["X", "Y"] }

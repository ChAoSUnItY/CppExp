module CppExp.Replace(module CppExp.Replace) where
import CppExp.Data (Def (Alias), PState, removeAlias)
import CppExp.Token (Token (..))

tryReplaceAlias :: Token -> PState -> [Token]
tryReplaceAlias ident@(Ident {}) pst = replaceAlias [ident] pst
tryReplaceAlias tk _ = [tk]

-- Replaces alias with sequence of tokens, the target definition is guranteed to be exist
replaceAlias :: [Token] -> PState -> [Token]
replaceAlias [] _ = []
replaceAlias (ident'@(Ident ident) : tks) pst
    = case result of
        Just (Alias _ rpTks, pst') -> replaceAlias rpTks pst' ++ replaceAlias tks pst
        _                          -> ident' : replaceAlias tks pst
    where
        result = removeAlias ident pst
replaceAlias (tk : tks) pst = tk : replaceAlias tks pst


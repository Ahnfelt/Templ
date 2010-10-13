module Interpreter where
import qualified Data.Map as Map
import Expression

interpret :: [Definition] -> Environment Value -> Exp -> Value
interpret ds m (EAt _ e) = interpret ds m e
interpret _ m (EVariable x) = m Map.! x
interpret _ _ (EString s) = VString s
interpret ds m (EChoice e1 e2) =
    case interpret ds m e1 of
        VException -> interpret ds m e2
        v -> v
interpret ds m (EList es _) =
    let vs = map (interpret ds m) es in
    if any (\v -> v == VException) vs 
        then VException 
        else VList vs
interpret ds m (ERecord fs) =
    let fs' = Map.map (interpret ds m) fs in
    if VException `elem` Map.elems fs' then VException else VRecord fs'
interpret ds m (ELookup e l) =
    case interpret ds m e of
        VException -> VException
        VRecord fs -> case Map.lookup l fs of
            Nothing -> VException
            Just v -> v
interpret ds m (EConcat e1 e2) =
    case (interpret ds m e1, interpret ds m e2) of
        (VException, _) -> VException
        (_, VException) -> VException
        (VString s1, VString s2) -> VString (s1 ++ s2)
interpret ds m (EIterate x e1 e2) =
    case interpret ds m e1 of
        VException -> VException
        VList vs -> 
            let vs' = map (\v -> interpret ds (Map.insert x v m) e2) vs in
            foldl concatenate (VString "") vs'
    where
        concatenate VException _ = VException
        concatenate _ VException = VException
        concatenate (VString s1) (VString s2) = VString (s1 ++ s2)
interpret ds@(Definition f' x' _ _ e':ds') m (ECall f e) =
    if f /= f' then interpret ds' m (ECall f e) else
    case interpret ds m e of
        VException -> VException
        v -> interpret ds' (Map.insert x' v m) e'


module Checker (checkAll, check, subtype) where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad
import Expression

instance Monad (Either a) where
    return = Right
    Left l >>= f = Left l
    Right r >>= f = f r

problem :: Position -> String -> Either String a
problem (0, 0) s = Left s
problem (l, c) s = Left (show l ++ ":" ++ show c ++ ": " ++ s)

subtype :: Position -> Type -> Type -> Either String ()
subtype p t1 t2 | t1 == t2 = return ()
subtype p (TList t1) (TList t2) = subtype p t1 t2
subtype p (TRecord m1) (TRecord m2) = do
    Map.foldWithKey field (Right ()) m2
    where
        field _ _ (Left s) = Left s
        field l (t2, q2) (Right ()) = case Map.lookup l m1 of
            Nothing | not q2 -> problem p ("Missing field: " ++ l)
            Nothing | q2 -> return ()
            Just (t1, q1) -> do
                if q1 && not q2 
                    then problem p ("Optional field cannot be required: " ++ l) 
                    else subtype p t1 t2
subtype p t1 t2 = problem p (show t1 ++ " is not compatible with " ++ show t2)

general :: Position -> Type -> Type -> Either String Type
general p TString TString = return (TString)
general p (TList t1) (TList t2) = do
    t <- general p t1 t2
    return (TList t)
general p (TRecord fs1) (TRecord fs2) = do
    let fs = Map.intersectionWith combine fs1 fs2
    let rs = [Left s | Left s <- Map.elems fs]
    if null rs
        then return (TRecord (Map.map (\(Right v) -> v) fs))
        else let Left s = head rs in problem p s
    where
        combine (t1, q1) (t2, q2) = do
            t <- general p t1 t2
            return (t, q1 || q2)
general p t1 t2 = problem p ("No common supertype for: " ++ show t1 ++ " and " ++ show t2)

check :: Position -> [Definition] -> Environment Type -> Exp -> Either String (Type, Bool)
check p ds m (EString _) = return (TString, False)
check p ds m (EConcat e1 e2) = do
    (t1, k1) <- check p ds m e1
    subtype p t1 TString
    (t2, k2) <- check p ds m e2
    subtype p t2 TString
    return (TString, k1 || k2)
check p ds m (EList (e:es) t) = do 
    (t1, k1) <- check p ds m e
    subtype p t1 t
    (_, k2) <- check p ds m (EList es t)
    return (TList t, k1 || k2)
check p ds m (EList [] t) = do
    return (TList t, False)
check p ds m (EIterate x e1 e2) = do
    r <- check p ds m e1
    case r of
        (TList t1, k1) -> do
            (t2, k2) <- check p ds (Map.insert x t1 m) e2
            subtype p t2 TString
            return (TString, k1 || k2)
        (t1, _) -> problem p ("Must be a list: " ++ show t1)
check p ds m (EVariable x) = do
    case Map.lookup x m of
        Just t -> return (t, False)
        Nothing -> problem p ("Undefined variable: " ++ x)
check p [] _ (ECall f _) = problem p ("Undefined function: " ++ f)
check p (Definition f' _ _ _ _:ds') m (ECall f e) | f /= f' = do
    check p ds' m (ECall f e)
check p ds@(Definition f' _ t1' t2' _:_) m (ECall f e) | f == f' = do
    (t, k) <- check p ds m e
    subtype p t t1'
    return (t2', k)
check p ds m (ERecord fs) = do
    let fs' = Map.map (check p ds m) fs
    case filter (\v -> case v of Right _ -> True; _ -> False) (Map.elems fs') of
        (r:_) -> r
        [] -> do
            let fs'' = Map.map (\(Right v) -> v) fs'
            return (TRecord fs'', any (\(_, b) -> b) (Map.elems fs''))
check p ds m (ELookup e l) = do
    r <- check p ds m e
    case r of
        (TRecord fs, k) ->
            if Map.member l fs 
                then let (t, q') = fs Map.! l in return (t, k || q')
                else problem p ("Field not present: " ++ l)
        (t, _) -> problem p ("Not a record: " ++ show t)
check p ds m (EChoice e1 e2) = do
    (t1, _) <- check p ds m e1
    (t2, k) <- check p ds m e2
    t <- general p t1 t2
    return (t, k)
check _ ds m (EAt p e) = check p ds m e

checkAll [] = return ()
checkAll (Definition f x t1 t2 e:ds) = do
    (t, k) <- check (0, 0) ds (Map.singleton x t1) e
    if k 
        then problem (0, 0) ("Functions must not throw exceptions: " ++ f)
        else subtype (0, 0) t t2
    checkAll ds


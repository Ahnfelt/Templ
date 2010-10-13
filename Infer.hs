module Infer (runInfer, infer, withVariable) where

import Type
import Expression

import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State

data InferState = InferState {
    fields :: [(FieldConstraint, Maybe Position)],
    environment :: Map Variable TypeScheme,
    substitution :: Map TypeVariable Type,
    optional :: Bool,
    position :: Maybe Position,
    index :: Int
    } deriving Show

type Infer a = State InferState a

infer :: Expression -> Infer Type
infer (EAt position e) = withPosition (Just position) (infer e)
infer (EVariable x) = useVariable x
infer (EApply e1 e2) = do
    t1 <- infer e1
    t2 <- infer e2
    t3 <- newTypeVariable "apply"
    unify t1 (TFunction t2 t3)
    return t3
infer (ELambda x e) = do
    t1 <- newTypeVariable "lambda"
    t2 <- withVariable x (Map.empty, t1) (infer e)
    return (TFunction t1 t2)
infer (ELet x e1 e2) = do
    t1 <- infer e1
    scheme <- generalize t1
    withVariable x scheme (infer e2)
infer (EText _) = return TText
infer (ECons e1 e2) = do
    t1 <- infer e1
    t2 <- infer e2
    unify (TList t1) t2
infer (ENil) = liftM TList (newTypeVariable "nil")
infer (EFor x e1 e2) = do
    t <- newTypeVariable "for"
    t1 <- infer e1
    unify t1 (TList t)
    t2 <- withVariable x (Map.empty, t) (infer e2)
    unify t2 TText
infer (EType e1 scheme e2) = do
    -- TODO: Check that that the scheme implies (modulo renaming) the inferred scheme (after inferring e2)
    t <- instantiate scheme
    t' <- infer e1
    unify t t'
    infer e2
infer (EAccess e l) = do
    t1 <- infer e
    t2 <- newTypeVariable "access"
    optionalBranch <- liftM optional get
    generateFieldConstraint t1 l t2 (not optionalBranch)
    return t2
infer (ERecord fields) = do
    fieldTypes <- liftM Map.fromList (mapM inferField (Map.toList fields))
    return (TRecord fieldTypes)
    where
        inferField (l, (e, required')) = do
            t <- if required' then infer e else withOptional (infer e)
            return (l, (t, required'))
infer (EConcat e1 e2) = do
    t1 <- infer e1
    t2 <- infer e2
    unify t1 TText
    unify t2 TText
infer (EChoice e1 e2) = do
    t1 <- withOptional (infer e1)
    t2 <- infer e2
    unify t1 t2


unify :: Type -> Type -> Infer Type
unify t1 t2 = do
    s <- liftM substitution get
    unify' (substitute s t1) (substitute s t2)
    where
        unify' (TVariable a1) (TVariable a2) | a1 == a2 = return (TVariable a1)
        unify' (TVariable a1) t2 = replace a1 t2
        unify' t1 (TVariable a2) = replace a2 t1
        unify' (TFunction t1 t2) (TFunction t1' t2') = liftM2 TFunction (unify t1 t1') (unify t2 t2')
        unify' (TText) (TText) = return TText
        unify' (TList t1) (TList t2) = liftM TList (unify t1 t2)
        unify' (TRecord fields1) (TRecord fields2) = do
            let fields1' = Map.filter snd (fields1 `Map.difference` fields2)
            let fields2' = Map.filter snd (fields2 `Map.difference` fields1)
            when (not (Map.null fields1')) $ report ("Inferred " ++ show t2 ++ " but expected " ++ show t1)
            when (not (Map.null fields2')) $ report ("Inferred " ++ show t1 ++ " but expected " ++ show t2)
            liftM TRecord (unionWithM unifyField fields1 fields2)
        unify' t1 t2 = report ("Inferred " ++ show t1 ++ " but expected " ++ show t2)
        unifyField (t1, required1') (t2, required2') = do
            t <- unify t1 t2
            return (t, required1' && required2')


runInfer :: Infer Type -> TypeScheme
runInfer monad = evalState (monad >>= generalize) newInferState


generalize :: Type -> Infer TypeScheme
generalize t = do
    fields' <- liftM fields get
    fields'' <- substitutionFixPoint (foldM checkFieldConstraint Map.empty fields')
    s <- liftM substitution get
    let t' = substitute s t
    environment' <- liftM environment get
    let nonFree' = Set.unions (map (free . snd) (Map.elems environment'))
    let free' = freeFixPoint fields'' (free t) `Set.difference` nonFree'
    let free'' = (Map.fromList (map (\k -> (k, Map.empty)) (Set.toList free')))
    let fields''' = Map.union (Map.intersection fields'' free'') free'' 
    return (fields''', t')


instantiate :: TypeScheme -> Infer Type
instantiate (records', t) | Map.null records' = return t
instantiate (records', t) = do
    let free' = Set.toList (Map.keysSet records')
    ts <- mapM newTypeVariable free'
    let s = Map.fromList (zip free' ts)
    state <- get
    let records'' = concatMap (\(a, fields) -> [
            ((substitute s (TVariable a), l, substitute s t, required), position state) | 
            (l, (t, required)) <- Map.assocs fields]) (Map.assocs records')
    put (state { fields = records'' ++ fields state })
    return (substitute s t)
    

freeFixPoint :: Map TypeVariable (Map Label (Type, Bool)) -> Set TypeVariable -> Set TypeVariable
freeFixPoint records as = 
    let as' = Map.fold (\fields as -> Set.unions (as : map (free . fst) (Map.elems fields))) as records in
    if Set.size as' /= Set.size as then freeFixPoint records as' else as'

substitutionFixPoint :: Infer a -> Infer a
substitutionFixPoint monad = do
    size <- liftM (Map.size . substitution) get
    result <- monad
    size' <- liftM (Map.size . substitution) get
    if size /= size' then substitutionFixPoint monad else return result

checkFieldConstraint :: (Map TypeVariable (Map Label (Type, Bool))) -> (FieldConstraint, Maybe Position) -> Infer (Map TypeVariable (Map Label (Type, Bool)))
checkFieldConstraint records ((t1, l, t2, required), position) = withPosition position $ do
    s <- liftM substitution get
    let t1' = substitute s t1
    let t2' = substitute s t2
    case (t1', t2') of
        (TVariable x, t2) -> case Map.lookup x records of
            Just fields -> case Map.lookup l fields of
                Just (t2', required') -> do
                    t <- unify t2 t2'
                    return (Map.insert x (Map.insert l (t, required || required') fields) records) 
                Nothing -> return (Map.insert x (Map.insert l (t2, required) fields) records)
            Nothing -> return (Map.insert x (Map.singleton l (t2, required)) records)
        (TRecord fields, t2) -> case Map.lookup l fields of
            Just (_, False) | required -> report (show t1' ++ " may not have field " ++ show l)
            Just (t1, required') -> do
                unify t1 t2
                return records
            Nothing | required -> report (show t1' ++ " does not have field " ++ show l)
            Nothing -> return records
        (t, _) -> report (show t ++ " is not a record type and thus cannot have a field " ++ show l)

newInferState :: InferState
newInferState = InferState {
    fields = [],
    environment = Map.empty,
    substitution = Map.empty,
    optional = False,
    position = Nothing,
    index = 0
    }

report :: String -> Infer a
report message = do
    position' <- liftM position get
    error (message ++ case position' of 
        Just (name, line, column) -> " at line " ++ show line ++ ", column " ++ show column ++
            (if name /= "" then " in " ++ show name else "")
        Nothing -> "")

unionWithM :: (Ord k, Monad m) => (v -> v -> m v) -> Map k v -> Map k v -> m (Map k v)
unionWithM function map1 map2 = do
    let keys1 = Map.keysSet map1
    let keys2 = Map.keysSet map2
    let unique1 = Set.toList (Set.difference keys1 keys2)
    let unique2 = Set.toList (Set.difference keys2 keys1)
    let common = Set.toList (Set.intersection keys1 keys2)
    let unique1' = zip unique1 (map (map1 Map.!) unique1)
    let unique2' = zip unique2 (map (map2 Map.!) unique2)
    let common1' = map (map1 Map.!) common
    let common2' = map (map2 Map.!) common
    common' <- mapM (uncurry function) (zip common1' common2')
    return (Map.fromList (zip common common' ++ unique1' ++ unique2'))

withVariable :: Variable -> TypeScheme -> Infer a -> Infer a
withVariable x scheme monad = do
    state <- get
    let scheme' = Map.lookup x (environment state)
    put (state { environment = Map.insert x scheme (environment state) })
    result <- monad
    state' <- get
    case scheme' of
        Just scheme' -> put (state' { environment = Map.insert x scheme' (environment state') })
        Nothing -> put (state' { environment = Map.delete x (environment state') })
    return result

useVariable :: Variable -> Infer Type
useVariable x = do
    state <- get
    case Map.lookup x (environment state) of
        Just scheme -> instantiate scheme
        Nothing -> report ("Variable " ++ show x ++ " is not in scope")

newTypeVariable :: String -> Infer Type
newTypeVariable name = do
    state <- get
    put (state { index = index state + 1 })
    return (TVariable (name ++ "#" ++ show (index state)))

replace :: TypeVariable -> Type -> Infer Type
replace a t | Set.member a (free t) = report ("Cannot construct the infinite type " ++ a ++ " = " ++ show t)
replace a t = do
    state <- get
    put (state { substitution = Map.insert a t (substitution state) })
    return t

withPosition :: Maybe Position -> Infer a -> Infer a
withPosition position' monad = do
    position'' <- liftM position get
    state <- get
    put (state { position = position' })
    result <- monad
    state' <- get
    put (state' { position = position'' })
    return result

withOptional :: Infer a -> Infer a
withOptional monad = do
    state <- get
    put (state { optional = True })
    result <- monad
    state' <- get
    put (state' { optional = False })
    return result

generateFieldConstraint :: Type -> Label -> Type -> Bool -> Infer ()
generateFieldConstraint t1 l t2 required = do
    state <- get
    put (state { fields = ((t1, l, t2, required), position state) : fields state })


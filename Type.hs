module Type where

import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Label = String

type TypeVariable = String

data Type
    = TVariable TypeVariable
    | TFunction Type Type
    | TText
    | TList Type
    | TRecord (Map Label (Type, Bool))
    deriving Eq

instance Show Type where
    show t = prettyType (Map.empty, t)

type FieldConstraint = (Type, Label, Type, Bool)

type TypeScheme = (Map TypeVariable (Map Label (Type, Bool)), Type)

count :: Type -> Map TypeVariable Int
count (TVariable a) = Map.singleton a 1
count (TFunction t1 t2) = Map.unionWith (+) (count t1) (count t2)
count (TText) = Map.empty
count (TList t) = count t
count (TRecord fields) = Map.unionsWith (+) (map count (map fst (Map.elems fields)))

free :: Type -> Set TypeVariable
free t = Map.keysSet (count t)

substitute :: Map TypeVariable Type -> Type -> Type
substitute s t@(TVariable a) = maybe t (substitute s) (Map.lookup a s)
substitute s t@(TText) = t
substitute s (TFunction t1 t2) = TFunction (substitute s t1) (substitute s t2)
substitute s (TList t) = TList (substitute s t)
substitute s (TRecord fields) = TRecord (Map.map (\(t, required) -> (substitute s t, required)) fields)

-- TODO: This won't display right if the same field constrained variable occurs twice (perhaps use count?)
prettyType :: TypeScheme -> String
prettyType (records, t) = pretty t
    where 
        pretty t = case t of
            TVariable a -> case Map.lookup a records of
                Just fields | not (Map.null fields) -> "(" ++ intercalate ", " fields' ++ ")"
                    where 
                        fields' = map field (Map.toList fields)
                        field (l, (t, required)) = l ++ optional required ++ ": " ++ pretty t
                        optional required = if required then "" else "?"
                _ -> a
            TFunction t1 t2 -> pretty t1 ++ " -> " ++ pretty t2
            TText -> "String"
            TList t -> "[" ++ pretty t ++ "]"
            TRecord fields -> "(" ++ intercalate ", " fields' ++ ")"
                where
                    fields' = map field (Map.toList fields)
                    field (l, (t, required)) = l ++ (if required then "" else "?") ++ ": " ++ pretty t

foo = undefined
    where
        unify :: Type -> Type -> Either String (Map TypeVariable Type)
        unify (TVariable a1) (TVariable a2) | a1 == a2 = return Map.empty
        unify (TVariable a1) t2 = return (Map.singleton a1 t2)
        unify t1 (TVariable a2) = return (Map.singleton a2 t1)
        unify (TFunction t1 t2) (TFunction t1' t2') = liftM2 Map.union (unify t1 t1') (unify t2 t2')
        unify (TText) (TText) = TText
        unify (TList t1) (TList t2) = unify t1 t2
        unify (TRecord fields1) (TRecord fields2) = do
            let fields1' = Map.filter snd (fields1 `Map.difference` fields2)
            let fields2' = Map.filter snd (fields2 `Map.difference` fields1)
            if not (Map.null fields1') 
                then Left ("Inferred " ++ show t2 ++ " but expected " ++ show t1) 
                else if not (Map.null fields2') 
                    then Right ("Inferred " ++ show t1 ++ " but expected " ++ show t2)
                    else unionWithM unify fields1 fields2
        unify t1 t2 = Left ("Inferred " ++ show t1 ++ " but expected " ++ show t2)


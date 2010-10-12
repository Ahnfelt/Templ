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

instance Show Type where
    show t = prettyType (Map.empty, Set.empty, t)

type FieldConstraint = (Type, Label, Type)

type RequiredConstraint = (Type, Label)

type TypeScheme = (Map TypeVariable (Map Label Type), Set (TypeVariable, Label), Type)

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
prettyType (records, required, t) = pretty t
    where 
        pretty t = case t of
            TVariable a -> case Map.lookup a records of
                Just fields | not (Map.null fields) -> "(" ++ intercalate ", " fields' ++ ")"
                    where 
                        fields' = map field (Map.toList fields)
                        field (l, t) = l ++ optional a l ++ ": " ++ pretty t
                        optional a l = if Set.member (a, l) required then "" else "?"
                _ -> a
            TFunction t1 t2 -> pretty t1 ++ " -> " ++ pretty t2
            TText -> "String"
            TList t -> "[" ++ pretty t ++ "]"
            TRecord fields -> "(" ++ intercalate ", " fields' ++ ")"
                where
                    fields' = map field (Map.toList fields)
                    field (l, (t, required)) = l ++ (if required then "" else "?") ++ ": " ++ pretty t


module Expression where
import qualified Data.Map as Map
import qualified Data.List as List

type Position = (Int, Int)

type Variable = String
type Label = String
type Function = String
type Optional = Bool
type Fields t = Map.Map Label t
type Environment t = Map.Map Variable t

data Definition
    = Definition Function Variable Type Type Exp

data Type
    = TString
    | TList Type
    | TRecord (Fields (Type, Optional))
    deriving Eq

data Exp
    = EVariable Variable
    | EString String
    | EConcat Exp Exp
    | EList [Exp] Type
    | EChoice Exp Exp
    | ERecord (Fields Exp)
    | ELookup Exp Label
    | EIterate Variable Exp Exp
    | ECall Function Exp
    | EAt Position Exp
    deriving (Show)

data Value 
    = VString String
    | VList [Value]
    | VRecord (Fields Value)
    | VException
    deriving (Show, Eq)

instance Show Definition where
    show (Definition f x t1 t2 e) = 
        f ++ " ($" ++ x ++ ": " ++ show t1 ++ "): " ++ show t2 ++ " {...}"

instance Show Type where
    show TString = "*"
    show (TList t) = "[" ++ show t ++ "]"
    show (TRecord fs) = "(" ++ List.intercalate ", " (map field (Map.toList fs)) ++ ")"
        where
            field (l, (t, q)) = l ++ ": " ++ show t ++ if q then "?" else ""


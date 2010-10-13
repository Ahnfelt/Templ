module Expression (Position, Variable, Text, Expression (..), Value (..), Label) where

import Type

import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Position = (String, Int, Int)

type Variable = String

type Text = String

type ModuleName = [String]

type Module = Either Expression (Map Variable Expression)

data Expression
    = EAt Position Expression
    | EVariable Variable
    | EApply Expression Expression
    | ELambda Variable Expression
    | ELet Variable Expression Expression
    | EText Text
    | ECons Expression Expression
    | ENil
    | EFor Variable Expression Expression
    | EAccess Expression Label
    | ERecord (Map Label (Expression, Bool))
    | EConcat Expression Expression
    | EChoice Expression Expression
    | EImport ModuleName String Expression
    deriving Show

data Value
    = VLambda (Map Variable Value) Variable Expression
    | VText Text
    | VList [Value]
    | VRecord (Map Label Value)
    deriving Show

errorWithPosition :: String -> Maybe Position -> Sting
errorWithPosition message position = message ++ case position of 
    Just (name, line, column) -> " at line " ++ show line ++ ", column " ++ show column ++
        (if name /= "" then " in " ++ show name else "")
    Nothing -> "")


module Interpret where

import Expression

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.Error

data InterpretState = InterpretState {
    environment :: Map Variable Value,
    position :: Maybe Position
    }

data InterpretError = InterpretError (Maybe Position) Label deriving Show

instance Error InterpretError where
    noMsg = undefined
    strMsg message = undefined

type Interpret a = ReaderT InterpretState (Either InterpretError) a

interpret :: Expression -> Interpret Value
interpret (EAt position' e) = withPosition (Just position') (interpret e)
interpret (EVariable x) = useVariable x
interpret (EApply e1 e2) = do
    v1 <- interpret e1
    v2 <- interpret e2
    case v1 of
        VLambda closure x e -> withVariables (Map.insert x v2 closure) (interpret e)
        _ -> report (show v1 ++ " is not a function and thus cannot be applied")
interpret (ELambda x e) = liftM3 VLambda (liftM environment ask) (return x) (return e)
interpret (ELet x e1 e2) = do
    v1 <- interpret e1
    withVariables (Map.singleton x v1) (interpret e2)
interpret (EText text) = return (VText text)
interpret (ECons e1 e2) = do
    v1 <- interpret e1
    v2 <- interpret e2
    case v2 of
        VList vs -> return (VList (v1:vs))
        _ -> report (show v2 ++ " is not a list and thus cannot be on the right hand side of a cons")
interpret (ENil) = return (VList [])
interpret (EFor x e1 e2) = do
    v1 <- interpret e1
    case v1 of
        VList vs -> do
            texts <- mapM iteration vs
            return (VText (concat texts))
        _ -> report (show v1 ++ " is not a list and thus cannot be iterated over")
    where
        iteration v = do
            v' <- withVariables (Map.singleton x v) (interpret e2)
            case v' of
                VText text -> return text
                _ -> report (show v' ++ " is not a string but is the result of an iteration")
interpret (EAccess e l) = do
    v <- interpret e
    case v of
        VRecord fields -> case Map.lookup l fields of
            Just v' -> return v'
            Nothing -> do
                error' <- liftM2 InterpretError (liftM position ask) (return l)
                lift (Left error')
        _ -> report (show v ++ " is not a record and thus cannot have a field " ++ show l)
interpret (ERecord fields) = liftM (VRecord . Map.fromList . catMaybes) (mapM interpretField (Map.toList fields))
    where
        interpretField (l, (e, required)) = (do
            v <- interpret e
            return (Just (l, v))) `catchError` (\(InterpretError position' l') -> if required
                then withPosition position' (report ("This record does not have the field " ++ show l'))
                else return Nothing)
interpret (EConcat e1 e2) = do
    v1 <- interpret e1
    v2 <- interpret e2
    case (v1, v2) of
        (VText text1, VText text2) -> return (VText (text1 ++ text2))
        (_, VText _) -> report (show v1 ++ " is not a string and thus cannot be concatenated")
        (VText _, _) -> report (show v2 ++ " is not a string and thus cannot be concatenated")
        (_, _) -> report (show v1 ++ " and " ++ show v2 ++ " are not strings and thus cannot be concatenated")
interpret (EChoice e1 e2) = do
    interpret e1 `catchError` const (interpret e2)

runInterpret :: Interpret a -> Map Variable Value -> Either InterpretError a
runInterpret monad environment' = runReaderT monad (InterpretState { environment = environment', position = Nothing })

withPosition :: Maybe Position -> Interpret a -> Interpret a
withPosition position' monad = local (\state -> state { position = position' }) monad

withVariables :: Map Variable Value -> Interpret a -> Interpret a
withVariables environment' monad = local (\state -> state { environment = environment' `Map.union` environment state }) monad

useVariable :: Variable -> Interpret Value
useVariable x = do
    environment' <- liftM environment ask
    case Map.lookup x environment' of
        Just v -> return v
        Nothing -> report ("Variable " ++ show x ++ " is not in scope here")

report :: String -> Interpret a
report message = do
    position' <- liftM position ask
    error (message ++ case position' of 
        Just (name, line, column) -> " at line " ++ show line ++ ", column " ++ show column ++
            (if name /= "" then " in " ++ show name else "")
        Nothing -> "")


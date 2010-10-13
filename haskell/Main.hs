module Main where

import Expression
import Type
import Infer
import Interpret
import Parser

import qualified Data.Map as Map
import System

main = do
    [filename, input] <- getArgs
    file <- readFile filename
    let input' = case parseExpression "" input of
            Left error' -> error (show error')
            Right input' -> input'
    let file' = case parseTextExpression filename file of
            Left error' -> error (show error')
            Right file' -> file'
    print input'
    print file'
    let file'' = ELambda "data" file'
    let input'' = runInfer (infer input')
    let file''' = runInfer (infer file'')
    let scheme = runInfer (
            withVariable "#template" file''' $ 
            withVariable "#input" input'' $ 
            infer (EApply (EVariable "#template") (EVariable "#input")))
    putStrLn (prettyType file''')
    putStrLn (prettyType input'')
    putStrLn (prettyType scheme)
    case runInterpret (interpret (EApply file'' input')) Map.empty of
        Right (VText text) -> putStrLn ("VText:\n" ++ text)
        Right v -> putStrLn (show v)
        Left error' -> putStrLn (show error')


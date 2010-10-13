module Main where

import Expression
import Type
import Infer
import Interpret
import Parser

import qualified Data.Map as Map
import System

main = do
    arguments <- getArgs
    case arguments of
        ["run", filename, input] -> do
            file <- readFile filename
            let input' = case parseExpression "" input of
                    Left error' -> error (show error')
                    Right input' -> input'
            let file' = case parseTextExpression filename file of
                    Left error' -> error (show error')
                    Right file' -> file'
            let file'' = ELambda "data" file'
            case runInterpret (interpret (EApply file'' input')) Map.empty of
                Right (VText text) -> putStr text
                Right v -> putStrLn ("Not a string value: " ++ show v)
                Left error' -> putStrLn (show error')
        ["check", filename, input] -> do
            file <- readFile filename
            let input' = case parseTypeScheme "" input of
                    Left error' -> error (show error')
                    Right input' -> input'
            let file' = case parseTextExpression filename file of
                    Left error' -> error (show error')
                    Right file' -> file'
            let file'' = ELambda "data" file'
            let file''' = runInfer (infer file'')
            case runInfer (
                withVariable "#template" file''' $ 
                withVariable "#input" input' $ 
                infer (EApply (EVariable "#template") (EVariable "#input"))) of
                scheme@(_, TText) -> return ()
                scheme@(_, t) -> error ("The result type should be string but was " ++ prettyType scheme)
        ["info", filename] -> do
            file <- readFile filename
            let file' = case parseTextExpression filename file of
                    Left error' -> error (show error')
                    Right file' -> file'
            let file'' = ELambda "data" file'
            let file''' = runInfer (infer file'')
            putStrLn (show file')
            putStrLn (prettyType file''')


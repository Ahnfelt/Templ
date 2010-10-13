module Main where
import qualified Data.Map as Map
import System.IO (stderr, hPutStrLn)
import System.Environment (getArgs)
import Checker
import Expression
import Parser
import Interpreter

main = do
    [a, f, s] <- getArgs
    p <- readFile f
    case a of
        "check" -> checkFile f p s
        "run" -> runFile f p s

checkFile n p t = do
    case parseProgram n p of
        Left s -> error (show s)
        Right ds@(Definition f x t1 t2 e:_) -> do
            case checkAll ds of
                Left s -> error s
                Right () -> case parseType t t of
                    Left s -> error (show s)
                    Right t' -> case subtype (0, 0) t' t1 of 
                        Left s -> error s
                        Right _ -> return ()

runFile n p e = do
    case parseProgram n p of
        Left s -> error (show s)
        Right ds@(Definition f _ _ _ _:_) -> case parseExpression e e of
            Left s -> error (show s)
            Right e' -> do
                let VString s = interpret ds Map.empty (ECall f e')
                putStr s


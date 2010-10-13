module Parser (parseTextExpression, parseExpression) where

import Expression
import Type

import Text.ParserCombinators.Parsec
import Control.Monad
import Data.Either
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Parse a = GenParser Char () a

withPosition :: Parse Expression -> Parse Expression
withPosition monad = do
    position <- getPosition
    e <- monad
    return (EAt (sourceName position, sourceLine position, sourceColumn position) e)

whitespace :: Parse ()
whitespace = skipMany (space <|> oneOf "\r\n")

upperName :: Parse String
upperName = liftM2 (:) upper (many alphaNum)

lowerName :: Parse String
lowerName = liftM2 (:) lower (many alphaNum)

callName :: Parse String
callName = try $ do
    char '@'
    upperName

keyword :: String -> Parse ()
keyword name = try $ do
    char '@'
    name' <- lowerName
    when (name /= name') (unexpected ("unknown keyword @" ++ name'))

variableName = try $ do
    char '$'
    lowerName

call = do
    x <- callName
    whitespace
    e <- expression
    return (EApply (EVariable x) e)

variable = liftM EVariable variableName

pattern = variableName <|> do
    char '('
    whitespace
    p <- pattern
    whitespace
    char ')'
    return p

atomicTypeTerm = typeParenthesis <|> typeVariable <|> atomicType <|> listType <|> recordType
    where
        typeParenthesis = try $ do
            char '('
            whitespace
            t <- typeTerm
            whitespace
            char ')'
            return t
        typeVariable = liftM TVariable lowerName
        atomicType = do
            name <- upperName
            when (name /= "String") (unexpected ("unknown type " ++ show name))
            return TText
        listType = do
            char '['
            whitespace
            t <- typeTerm
            whitespace
            char ']'
            return (TList t)
        recordType = try $ do
            char '('
            fields <- field `sepBy` (do whitespace; char ',')
            whitespace
            char ')'
            return (TRecord (Map.fromList fields))
            where
                field = do
                    whitespace
                    l <- lowerName
                    whitespace
                    required <- (do char '?'; whitespace; return False) <|> return True
                    char ':'
                    whitespace
                    t <- typeTerm
                    return (l, (t, required))

typeTerm = functionType <|> atomicTypeTerm
    where
        functionType = try $ do
            t1 <- atomicTypeTerm
            whitespace
            string "->"
            whitespace
            t2 <- typeTerm
            return (TFunction t1 t2)

typeScheme = try (do t <- atomicTypeTerm; return (Map.empty, t)) <|> do
    char '(' 
    whitespace
    constraints' <- constraints
    t <- atomicTypeTerm
    whitespace
    char ')'
    let fieldConstraints' = Map.unionsWith unionOne 
            (map (\(a, l, t, required) -> Map.singleton a (Map.singleton l (t, required))) constraints')
    return (fieldConstraints', t)
    where
        unionOne map1 map2 = 
            let map = map1 `Map.intersection` map2 in
            if not (Map.null map) -- TODO: Make the following "unexpected" rather than an error
                then error ("duplicate label " ++ show (fst (Map.findMin map)))
                else Map.union map1 map2
        constraints = try $ do
            cs <- constraint `sepBy1` (try $ do whitespace; char ','; whitespace)
            whitespace
            string "=>"
            whitespace
            return cs
        constraint = try $ do
            a <- lowerName
            whitespace
            char '.'
            whitespace
            l <- lowerName
            whitespace
            required <- option True (do char '?'; return False)
            whitespace
            char ':'
            whitespace
            t <- typeTerm
            return (a, l, t, required)

builtin after =
    [ do
        keyword "function"
        whitespace
        name <- upperName
        whitespace
        x <- pattern
        whitespace
        e1 <- expression
        e2 <- after
        return (ELet name (ELambda x e1) e2)
    , do
        keyword "local"
        whitespace
        x <- variableName
        whitespace
        e1 <- expression
        e2 <- after
        return (ELet x e1 e2)
    , do
        keyword "let"
        whitespace
        x <- variableName
        whitespace
        e1 <- expression
        whitespace
        e2 <- expression
        return (ELet x e1 e2)
    , do
        keyword "lambda"
        whitespace
        x <- pattern
        whitespace
        e1 <- expression
        return (ELambda x e1)
    , do
        keyword "for"
        whitespace
        x <- variableName
        whitespace
        e1 <- expression
        whitespace
        e2 <- expression
        return (EFor x e1 e2)
    , do
        keyword "type"
        whitespace
        e1 <- expression
        whitespace
        scheme <- typeScheme
        e2 <- after
        return (EType e1 scheme e2)
    , keyword2 "apply" EApply
    , keyword2 "concat" EConcat
    , keyword2 "choice" EChoice
    ]
    where 
        keyword1 name constructor = do
            keyword name
            whitespace
            e1 <- expression
            return constructor e1
        keyword2 name constructor = do
            keyword name
            whitespace
            e1 <- expression
            whitespace
            e2 <- expression
            return (constructor e1 e2)

list = do
    char '['
    es <- (do whitespace; expression) `sepBy` (do whitespace; char ',')
    whitespace
    char ']'
    return (foldr ECons ENil es)

parenthesis = try $ do
    char '('
    whitespace
    e <- expression
    whitespace
    char ')'
    return e

record = do
    char '('
    fields <- field `sepBy` (do whitespace; char ',')
    whitespace
    char ')'
    return (ERecord (Map.fromList fields))
    where
        field = do
            whitespace
            l <- lowerName
            whitespace
            required <- (do char '?'; whitespace; return False) <|> return True
            char ':'
            whitespace
            e <- expression
            return (l, (e, required))

access = do
    e <- atomicExpression
    ls <- many $ try $ do
        char '.'
        lowerName
    return (foldl EAccess e ls)

common after = textMode <|> call <|> choice (builtin after)

atomicExpression = withPosition $ parenthesis <|> variable <|> list <|> record <|> common (do whitespace; expression)

expression = access

textMode = do
    char '{'
    e <- textExpression
    char '}'
    return e

textInsert = do
    x <- variableName
    ls <- many $ try $ do
        char '.'
        lowerName
    return (foldl EAccess (EVariable x) ls)

text = do
    cs <- many1 (noneOf "$@{}|")
    return (EText cs)

textEscape = try $ do
    char '$'
    c <- oneOf "$@{}|"
    return (EText [c])

textConcat = do
    es <- many1 atomicTextExpression
    return (foldl1 EConcat es)

textChoice = do
    es <- sepBy1 (option (EText "") textConcat) (char '|')
    return (foldl1 EChoice es)

atomicTextExpression = withPosition $ textInsert <|> text <|> textEscape <|> common textExpression

textExpression = textChoice

parseTextExpression :: String -> String -> Either ParseError Expression
parseTextExpression name input = parse (do e <- textExpression; eof; return e) name input

parseExpression :: String -> String -> Either ParseError Expression
parseExpression name input = parse (do e <- expression; eof; return e) name input


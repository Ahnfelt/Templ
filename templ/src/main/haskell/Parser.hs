module Parser (parseProgram, parseType, parseExpression) where
import Prelude hiding (lookup)
import Text.ParserCombinators.Parsec hiding (label)
import Text.ParserCombinators.Parsec.Pos
import qualified Data.Map as Map
import Expression

parseProgram :: SourceName -> String -> Either ParseError [Definition]
parseProgram = parse program

parseType :: SourceName -> String -> Either ParseError Type
parseType = parse typeExpression

parseExpression :: SourceName -> String -> Either ParseError Exp
parseExpression = parse expression

at e = do
    p <- getPosition
    e' <- e
    return (EAt (sourceLine p, sourceColumn p) e')

whitespace = do
    skipMany (oneOf " \t\r\n") <?> ""

identifier l = do
    c <- oneOf l
    cs <- many (letter <|> digit)
    return (c:cs)

function = identifier ['A'..'Z'] <?> "function name"
label = identifier ['a'..'z'] <?> "label"
variable = identifier ['a'..'z'] <?> "variable"

program = do
    ds <- many1 definition
    whitespace
    eof
    return (reverse ds)
    where
        definition = do
            f <- try $ do whitespace; function
            whitespace
            char '('
            whitespace
            char '$'
            x <- variable
            whitespace
            char ':'
            whitespace
            t1 <- typeExpression
            whitespace
            char ')'
            whitespace
            char ':'
            whitespace
            t2 <- typeExpression
            whitespace
            e <- expression
            return (Definition f x t1 t2 e)

expression = at $ do
    e1 <- lookup
    o <- optionMaybe $ try $ do
        whitespace
        char '|'
        whitespace
        expression
    return $ case o of
        Just e2 -> EChoice e1 e2
        Nothing -> e1

lookup = at $ do
    e <- expressionOther
    dots e
    where
        dots e = option e $ try $do
            whitespace
            char '.'
            whitespace
            l <- label
            dots (ELookup e l)

expressionOther = at $ do
    term <|> record <|> list <|> insert <|> parenthesis
    where
        record = do
            char '('
            fs <- field `sepBy` do {whitespace; char ','}
            whitespace
            char ')'
            return (ERecord (Map.fromList fs))
        field = do
            whitespace
            l <- label
            whitespace
            char '='
            whitespace
            e <- expression
            return (l, e)
        list = do
            char '['
            whitespace
            es <- option [] items
            char ':'
            whitespace
            t <- typeExpression
            whitespace
            char ']'
            return (EList es t)
        items = try $ do
            e <- expression
            whitespace
            es <- option [] $ do
                string ","
                whitespace
                items
            return (e:es)
        insert = do
            char '$'
            x <- variable
            return (EVariable x)
        parenthesis = do
            char '('
            whitespace
            e <- expression
            whitespace
            char ')'
            return e

term = at $ do
    iterate <|> call <|> curly
    where
        iterate = do
            try $ do
                string "@for"
                whitespace
                char '$'
            x <- variable
            whitespace
            char ':'
            whitespace
            e1 <- expression
            whitespace
            e2 <- expression
            return (EIterate x e1 e2)
        call = do
            char '@'
            f <- function
            whitespace
            e <- expression
            return (ECall f e)
        curly = do
            char '{'
            e <- option (EString "") text
            char '}'
            return e

text = at $ do
    e1 <- textOther
    o <- optionMaybe $ do
        char '|'
        text
    return $ case o of
        Just e2 -> EChoice e1 e2
        Nothing -> e1

textOther = at $ do
    e1 <- string <|> term <|> escape <|> lookup
    o <- optionMaybe textOther
    return $ case o of
        Just e2 -> EConcat e1 e2
        Nothing -> e1
    where
        lookup = do
            char '$'
            x <- variable
            dots (EVariable x)
        dots e = option e $ do
            char '.'
            l <- label
            dots (ELookup e l)
        string = do
            s <- many1 (noneOf special)
            return (EString s)
        escape = try $ do
            char '$'
            c <- oneOf special
            return (EString [c])
        special = "@$|{}"

typeExpression = do
    record <|> list <|> string
    where
        record = do
            whitespace
            char '('
            fs <- field `sepBy` (do whitespace; char ','; whitespace)
            whitespace
            char ')'
            return (TRecord (Map.fromList fs))
        field = do
            l <- label
            whitespace
            char ':'
            whitespace
            t <- typeExpression
            o <- optionMaybe (do whitespace; char '?')
            whitespace
            return (l, (t, o /= Nothing))
        list = do
            char '['
            whitespace
            t <- typeExpression
            whitespace
            char ']'
            return (TList t)
        string = do
            char '*'
            return TString


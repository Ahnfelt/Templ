module Templ where

import Expression
import qualified Parser
import qualified Checker
import Text.ParserCombinators.Parsec

type ErrorMessage = String

check :: Exp -> Type -> Maybe ErrorMessage
check = Nothing -- TODO

parseAndCheck :: SourceName -> String -> String -> Maybe ErrorMessage
parseAndCheck filename input typeString = 
    case Parser.parseProgram filename input of
      Left error -> return $ Just error
      Right exp -> 
          let (o, e') = check e
              o' = simplify o
          in case principal e' of
                Left error -> do Just (show err)
                Right t -> Right (e', o', t) -- TODO


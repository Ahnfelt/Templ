package templ

import util.parsing.combinator.RegexParsers
import templ.Expression._

object Parser extends RegexParsers {
  def withPosition(parser: Parser[Expression]) = parser
  def whitespace = "[ \t\r\n]*"r
  def upperName = "[A-Z][a-zA-Z0-9]*"r
  def lowerName = "[a-z][a-zA-Z0-9]*"r
  def callName = "@" ~> upperName
  def keyword(name: String) = "@" ~> name
  def variableName = "$" ~> lowerName
  def call = callName ~ whitespace ~ expression ^^ {
    case x ~ _ ~ e =>
      EApply(EVariable(x), e)
  }
  def variable = variableName ^^ (x => EVariable(x))
  def pattern: Parser[String] = variableName |
          "(" ~> whitespace ~> pattern <~ whitespace <~ ")"
  def construct(after: Parser[Expression]) =
    keyword("function") ~> whitespace ~>
            upperName ~ whitespace ~ pattern ~ whitespace ~ expression ~ after ^^ {
      case name ~ _ ~ x ~ _ ~ e1 ~ e2 =>
        ELet(name, ELambda(x, e1), e2)
    } |
    keyword("local") ~> whitespace ~>
            variableName ~ whitespace ~ expression ~ after ^^ {
      case x ~ _ ~ e1 ~ e2 =>
        ELet(x, e1, e2)
    } |
    keyword("let") ~> whitespace ~>
            variableName ~ whitespace ~ expression ~ whitespace ~ expression ^^ {
      case x ~ _ ~ e1 ~ _ ~ e2 =>
        ELet(x, e1, e2)
    } |
    keyword("lambda") ~> whitespace ~>
            pattern ~ whitespace ~ expression ^^ {
      case x ~ _ ~ e =>
        ELambda(x, e)
    } |
    keyword("for") ~> whitespace ~>
            variableName ~ whitespace ~ expression ~ whitespace ~ expression ^^ {
      case x ~ _ ~ e1 ~ _ ~ e2 =>
        EFor(x, e1, e2)
    }
  def list = "[" ~> repsep(whitespace ~> expression, whitespace ~ ",") <~ "]" ^^ {
    case es =>
      es.foldRight[Expression](ENil())(ECons(_, _))
  }
  def parenthesis = "(" ~> whitespace ~> expression <~ whitespace <~ ")"
  def record = "(" ~> repsep(recordField, whitespace ~ ",") <~ whitespace <~ ")" ^^ {
    case fields =>
      ERecord(Map(fields: _*))
  }
  def recordField = whitespace ~> lowerName ~ whitespace ~
          opt("?" <~ whitespace) ~ ":" ~ whitespace ~ expression ^^ {
    case l ~ _ ~ optional ~ _ ~ _ ~ e =>
      (l, (e, optional.isEmpty))
  }
  def lookup = atomicExpression ~ rep("." ~> lowerName) ^^ {
    case e ~ ls =>
      ls.foldLeft[Expression](e)(ELookup(_, _))
  }
  def common(after: Parser[Expression]) = textMode | call | construct(after)
  def atomicExpression = withPosition(
    parenthesis | variable | list | record | common(whitespace ~> expression))
  def expression: Parser[Expression] = lookup
  def textMode = "{" ~> textExpression <~ "}"
  def textInsert = "$" ~> lowerName ~ rep("." ~> lowerName) ^^ {
    case x ~ ls =>
      ls.foldLeft[Expression](EVariable(x))(ELookup(_, _))
  }
  def text = "[^$@{}|]+".r ^^ {EText(_)}
  def textEscape = "[$][$@{}|]".r ^^ {text => EText(text.drop(1))}
  def textConcat = rep1(atomicTextExpression) ^^ {es => es.reduceLeft(EConcat(_, _))}
  def textChoice = rep1sep(opt(textConcat) ^^ {_.getOrElse(EText(""))}, "|") ^^ {
    case es =>
      es.reduceLeft(EChoice(_, _))
  }
  def atomicTextExpression = withPosition(textInsert | text | textEscape | common(textExpression))
  def textExpression: Parser[Expression] = textChoice

  def parseFile(fileName: String) : Expression = {
    val content = io.Source.fromFile(fileName).mkString
    parseAll(textExpression, content).get
  }
}

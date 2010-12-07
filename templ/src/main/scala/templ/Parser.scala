package templ

import util.parsing.combinator.RegexParsers
import templ.Expression._
import templ.Text._
import util.parsing.input.Positional

object Parser extends RegexParsers {
  override val skipWhitespace = false
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
  def angleBrackets[T](inside: Parser[T]): Parser[T] =
          "<" ~> whitespace ~> inside <~ whitespace <~ ">" | inside
  def construct(after: Parser[Expression]) =
    keyword("import") ~> whitespace ~> angleBrackets(
            rep1sep(upperName, ".") ~ opt((".*" ^^^ List()) |
            "." ~> whitespace ~> "(" ~> whitespace ~>
            rep1sep((upperName ~ whitespace ~ "=" ~ whitespace ~ upperName ^^ {
              case variable ~ _ ~ _ ~ _ ~ symbol => variable -> symbol
            }) | (upperName ^^ {symbol => symbol -> symbol}), whitespace ~ "," ~ whitespace)
            <~ whitespace <~ ")")) ~ after ^^ {
      case module ~ None ~ e =>
        EImport(Map(module.last -> module.last), module.dropRight(1), e)
      case module ~ Some(pairs) ~ e =>
        EImport(Map(pairs: _*), module, e)
    } |
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
    } |
    keyword("try") ~> whitespace ~> expression ^^ unreliable |
    keyword("first") ~> whitespace ~> expression ^^ { EFirst(_) } |
    keyword("last") ~> whitespace ~> expression ^^ { ELast(_) } |
    keyword("front") ~> whitespace ~> expression ^^ { EFront(_) } |
    keyword("back") ~> whitespace ~> expression ^^ { EBack(_) } |
    keyword("html") ~> whitespace ~> expression ^^ { EEscape(_, Text.escapeHtml) } |
    keyword("javascript") ~> whitespace ~> expression ^^ { EEscape(_, Text.escapeBackslashString) } |
    keyword("raw") ~> whitespace ~> expression ^^ { EEscape(_, identity) }
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
          opt("?" <~ whitespace) ~ "=" ~ whitespace ~ expression ^^ {
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
  def textMode = "{" ~> textExpression <~ "}" ^^ { EReliable(_) }
  def textInsert = "$" ~> lowerName ~ rep("." ~> lowerName) ^^ {
    case x ~ ls =>
      ls.foldLeft[Expression](EVariable(x))(ELookup(_, _))
  }
  def text = "[^$@{}|]+".r ^^ { string => EText(SEscape(SString(string), identity)) }
  def textEscape = "[$][$@{}|]".r ^^ {text => EText(SString(text.drop(1)))}
  def textAppend = rep1(atomicTextExpression) ^^ {es => es.reduceLeft(EAppend(_, _))}
  def textChoice = rep1sep(opt(textAppend) ^^ {_.getOrElse(EText(SString("")))}, "|") ^^ {
    case es =>
      es.reduceLeft(EChoice(_, _))
  }
  def atomicTextExpression = withPosition(textInsert | text | textEscape | common(textExpression))
  def textExpression: Parser[Expression] = textChoice

  def withPosition(parser: Parser[Expression]): Parser[Expression] = {
    class ColumnLine extends Positional
    positioned(success(new ColumnLine)) ~ parser ^^ {
      case columnLine ~ e => EAt(("", columnLine.pos), e)
    }
  }

  def parseFile(fileName: String) : Expression = {
    val content = io.Source.fromFile(fileName).mkString
    parseAll(textExpression, content).get
  }

  /** Removes an outermost EReliable requirement, if there is one. */
  private def unreliable(e: Expression): Expression = e match {
    case EAt(position, e) => EAt(position, unreliable(e))
    case EReliable(e) => e
    case e => e
  }
}

package templ

sealed trait Expression

object Expression {
  type Variable = String
  type Position = (String, Int, Int)
  type Text = String
  type Label = String
  type Required = Boolean

  case class EAt (position: Position, expression: Expression) extends Expression
  case class EVariable (variable: Variable) extends Expression
  case class EApply (function: Expression, argument: Expression) extends Expression
  case class ELambda (variable: Variable, body: Expression) extends Expression
  case class ELet (variable: Variable, value: Expression, body: Expression) extends Expression
  case class EText (text: Text) extends Expression
  case class ECons (head: Expression, tail: Expression) extends Expression
  case class ENil extends Expression
  case class EFor (variable: Variable, list: Expression, body: Expression) extends Expression
  case class ELookup (record: Expression, label: Label) extends Expression
  case class ERecord (fields: Map[Label, (Expression, Required)]) extends Expression
  case class EConcat (left: Expression, right: Expression) extends Expression
  case class EChoice (primary: Expression, secondary: Expression) extends Expression

  def errorWithPosition(message: String, position: Position) = {
    position match {
      case (filename, line, column) =>
        message + " at line " + line + ", column " + column +
                (if(!filename.isEmpty) " in '" + filename + "'" else "")
    }
  }
}

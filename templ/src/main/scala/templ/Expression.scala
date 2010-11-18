package templ

sealed trait Expression

object Expression {
  type Variable = String
  type Position = (String, scala.util.parsing.input.Position)
  type Label = String
  type Required = Boolean

  case class EAt(position: Position, expression: Expression) extends Expression 
  case class EVariable(variable: Variable) extends Expression
  case class EApply(function: Expression, argument: Expression) extends Expression
  case class ELambda(variable: Variable, body: Expression) extends Expression
  case class ELet(variable: Variable, value: Expression, body: Expression) extends Expression
  case class EReliable(body: Expression) extends Expression
  case class EEscape(body: Expression, mechanism: String => String) extends Expression
  case class EText(text: Text) extends Expression
  case class ECons(head: Expression, tail: Expression) extends Expression
  case class ENil extends Expression
  case class EFirst(list: Expression) extends Expression
  case class ELast(list: Expression) extends Expression
  case class EFront(list: Expression) extends Expression
  case class EBack(list: Expression) extends Expression
  case class EFor(variable: Variable, list: Expression, body: Expression) extends Expression
  case class ELookup(record: Expression, label: Label) extends Expression
  case class ERecord(fields: Map[Label, (Expression, Required)]) extends Expression
  case class EAppend(left: Expression, right: Expression) extends Expression
  case class EChoice(primary: Expression, secondary: Expression) extends Expression

  def errorWithPosition(message: String, position: Position) = {
    position match {
      case (filename, lineColumn) =>
        message + " at line " + lineColumn.line + ", column " + lineColumn.column +
                (if(!filename.isEmpty) " in '" + filename + "':\n" else ":\n") + lineColumn.longString
    }
  }
}

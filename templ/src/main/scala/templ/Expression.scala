package templ

sealed trait Expression

object Expression {
  type Variable = String
  type Position = (String, scala.util.parsing.input.Position)
  type Label = String
  type Required = Boolean
  type Module = List[String]
  type Symbol = String

  case class EAt(position: Position, expression: Expression) extends Expression
  case class EImport(symbols: Map[Variable, Symbol], module: Module, body: Expression) extends Expression
  case class EVariable(variable: Variable) extends Expression
  case class EApply(function: Expression, argument: Expression) extends Expression
  case class ELambda(variable: Variable, body: Expression) extends Expression
  case class ELet(variable: Variable, value: Expression, body: Expression) extends Expression
  case class ETry(body: Expression) extends Expression
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

  def imports(expression: Expression): Set[Either[Module, Module]] = expression match {
    case EAt(_, e) => imports(e)
    case EImport(symbols, module, e) => (if(symbols.isEmpty) Set(Left(module)) else
      Set((for(symbol <- symbols.values) yield Right(module :+ symbol)).toSeq: _* )) ++
      imports(e)
    case EVariable(_) => Set()
    case EApply(e1, e2) => imports(e1) ++ imports(e2)
    case ELambda(_, e) => imports(e)
    case ELet(_, e1, e2) => imports(e1) ++ imports(e2)
    case ETry(e) => imports(e)
    case EReliable(e) => imports(e)
    case EEscape(e, _) => imports(e)
    case EText(_) => Set()
    case ECons(e1, e2) => imports(e1) ++ imports(e2)
    case ENil() => Set()
    case EFirst(e) => imports(e)
    case ELast(e) => imports(e)
    case EFront(e) => imports(e)
    case EBack(e) => imports(e)
    case EFor(_, e1, e2) => imports(e1) ++ imports(e2)
    case ELookup(e, _) => imports(e)
    case ERecord(fields) => (fields.values.map { case (e, _) => imports(e) }).foldLeft[Set[Either[Module, Module]]](Set())(_ ++ _)
    case EAppend(e1, e2) => imports(e1) ++ imports(e2)
    case EChoice(e1, e2) => imports(e1) ++ imports(e2)
  }

  def errorWithPosition(message: String, position: Position) = {
    position match {
      case (filename, lineColumn) =>
        message + " at line " + lineColumn.line + ", column " + lineColumn.column +
                (if(!filename.isEmpty) " in '" + filename + "':\n" else ":\n") + lineColumn.longString
    }
  }
}

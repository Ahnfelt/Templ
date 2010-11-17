package templ

import templ.Expression._

sealed trait Value

object Value {
  case class VLambda (context: Map[Variable, Value], variable: Variable, body: Expression) extends Value
  case class VText (text: Text) extends Value
  case class VList (list: List[Value]) extends Value
  case class VRecord (fields: Map[Label, Value]) extends Value
}

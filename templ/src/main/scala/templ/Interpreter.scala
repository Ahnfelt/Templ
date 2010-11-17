package templ

import collection.immutable._
import templ.Expression._
import templ.Value._

object Interpreter extends Application {
  case class LookupException(position: Position) extends RuntimeException

  def interpretFile (fileName: String, javaValue: AnyRef) : String = {
    val value = JavaValues.fromObject(javaValue)
    val expression = Parser.parseFile(fileName)
    val result = interpret(expression, Map(("data", value)), (fileName, 0, 0))
    result match {
      case VText(text) => text
      case _ => report("Template does not return a string", (fileName, 0, 0))
    }
  }

  def interpret (expression: Expression, environment: Map[Variable, Value], position: Position) : Value =
    expression match {
      case EAt (position, expression) => interpret(expression, environment, position)
      case EVariable (variable) =>
        if (environment contains variable) {
          environment(variable)
        } else {
           report("Variable "+variable+" is not in scope here", position)
        }
      case EApply (function, argument) =>
        val v1 = interpret(function, environment, position)
        val v2 = interpret(argument, environment, position)
        v1 match {
          case VLambda(context, variable, body) =>
            val environment2 = (environment ++ context) + ((variable, v2))
            interpret(body, environment2, position)
          case _ => report (v1 + " is not a function and thus cannot be applied", position)
        }
      case ELambda (variable, body) => VLambda(environment, variable, body)
      case ELet (variable, value, body) =>
        val v1 = interpret(value, environment, position)
        val environment2 = environment + ((variable, v1))
        interpret(body, environment2, position)
      case EText (text) => VText(text)
      case ECons (head, tail) =>
        val v1 = interpret(head, environment, position)
        val v2 = interpret(tail, environment, position)
        (v2) match {
          case VList (list) => VList (v1 :: list)
          case _ => report (v2 + " is not a list and thus cannot be on the right hand side of a cons", position)
        }
      case ENil () => VList (List())
      case EFor (variable, list, body) =>
        val v1 = interpret(list, environment, position)
        v1 match {
          case VList (list) =>
            val vs = for(val v <- list; val environment2 = environment + ((variable, v))) yield interpret(body, environment2, position)
            val text = vs map {
              case VText (text) => text
              case v => report (v + " is not a string but is the result of an iteration", position)
            }
            VText(text.foldLeft("")(_ + _))
          case v => report (v + " is not a list and thus cannot be iterated over", position)
        }
      case ELookup (record, label) =>
        val v = interpret(record, environment, position)
        v match {
          case VRecord (fields) =>
            if (fields.contains(label)) {
              fields(label)
            } else {
              throw LookupException(position)
            }
          case _ => report (v + " is not a record and cannot have field " + label, position)
        }
      case ERecord (fields) =>
        val fields2 = fields.mapValues({ case (e, _) => interpret(e, environment, position) })
        VRecord(fields2)
      case EConcat (left, right) =>
        val text1 = interpret(left, environment, position) match {
          case VText (text) => text
          case v => report (v + " is not a string on the left side of this concatenation", position)
        }
        val text2 = interpret(right, environment, position) match {
          case VText (text) => text
          case v => report (v + " is not a string on the right side of this concatenation", position)
        }
        VText (text1 + text2)
      case EChoice (primary, secondary) =>
        try {
          interpret(primary, environment, position)
        } catch {
          case _: LookupException => interpret(secondary, environment, position)
        }
    }

  def report [T] (message: String, position: Position) : T = {
    throw new RuntimeException(errorWithPosition(message, position))
  }

}

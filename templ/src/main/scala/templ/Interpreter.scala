package templ

import scala.util.parsing.input.NoPosition
import collection.immutable._
import templ.Expression._
import templ.Value._
import templ.Text._

object Interpreter extends Application {
  class InterpreterException(message: String) extends RuntimeException(message)
  abstract class AlternativeException(val position: Position) extends RuntimeException
  class LookupException(position: Position) extends AlternativeException(position)
  class ListException(position: Position) extends AlternativeException(position)

  def interpretFile(fileName: String, javaValue: AnyRef) : String = {
    val value = JavaValues.lazyValue(javaValue)
    val expression = Parser.parseFile(fileName)
    val result = interpret(expression)(Map(("data", value)), VRecord(Map()), (fileName, NoPosition))
    result match {
      case VText(text) => SEscape(text, Text.escapeHtml).toString
      case _ => report("Template does not return a string", (fileName, NoPosition))
    }
  }

  def interpret(expression: Expression)(implicit environment: Map[Variable, Value], modules: Value, position: Position) : Value =
    expression match {
      case EAt(position, expression) =>
        interpret(expression)(environment, modules, position)
      case EImport(symbols, moduleNames, body) =>
        var module = modules
        def lookup(name: String) = {
          module match {
            case VRecord(fields) =>
              if(fields.contains(name)) fields(name)
              else report("Cannot find " + name + " in this import", position)
            case _ => report("This module does not have any subodules", position)
          }
        }
        for(moduleName <- moduleNames) module = lookup(moduleName)
        val bindings = if(symbols.isEmpty) {
          module match {
            case VRecord(fields) => fields
            case _ => report("Cannot find any submodules to import", position)
          }
        } else {
          symbols.mapValues(lookup _)
        }
        interpret(body)(environment ++ bindings, modules, position)
      case EVariable(variable) =>
        if(environment.contains(variable)) {
          environment(variable)
        } else {
           report("Variable " + variable + " is not in scope here", position)
        }
      case EApply(function, argument) =>
        val v1 = interpret(function)
        val v2 = interpret(argument)
        v1 match {
          case VLambda(context, variable, body) =>
            interpret(body)((environment ++ context) + ((variable, v2)), modules, position)
          case _ => report("The following is not a function and thus cannot be applied", positionOf(function, position))
        }
      case ELambda(variable, body) => VLambda(environment, variable, body)
      case ELet(variable, value, body) =>
        val v1 = interpret(value)
        interpret(body)(environment + ((variable, v1)), modules, position)
      case EEscape(body, mechanism) =>
        interpret(body) match {
          case VText(text) => VText(SEscape(text, mechanism))
          case v => report("The following is not a string and thus cannot be escaped", positionOf(body, position))
        }
      case EText(text) => VText(text)
      case ETry(body) => interpret(body)
      case EReliable(body) => interpret(body)
      case ECons(head, tail) =>
        val v1 = interpret(head)
        val v2 = interpret(tail)
        (v2) match {
          case VList(list) => VList (v1 :: list)
          case _ => report("The following is not a list and thus cannot be on the right hand side of a cons", positionOf(tail, position))
        }
      case ENil() => VList (List())
      case EFirst(list: Expression) =>
        val v1 = interpret(list)
        v1 match {
          case VList(List()) => throw new ListException(position);
          case VList(list) => list.first
          case v => report("The following is not a list and thus has no first element", positionOf(list, position))
        }
      case ELast(list: Expression) =>
        val v1 = interpret(list)
        v1 match {
          case VList(List()) => throw new ListException(position);
          case VList(list) => list.last
          case v => report("The following is not a list and thus has no last element", positionOf(list, position))
        }
      case EFront(list: Expression) =>
        val v1 = interpret(list)
        v1 match {
          case VList(List()) => throw new ListException(position);
          case VList(list) => VList(list.dropRight(1));
          case v => report("The following is not a list and thus has no front elements", positionOf(list, position))
        }
      case EBack(list: Expression) =>
        val v1 = interpret(list)
        v1 match {
          case VList(List()) => throw new ListException(position);
          case VList(list) => VList(list.drop(1));
          case v => report("The following is not a list and thus has no back elements", positionOf(list, position))
        }
      case EFor(variable, list, body) =>
        val v1 = interpret(list)
        v1 match {
          case VList(list) =>
            val vs = for(val v <- list)
              yield interpret(body)(environment + ((variable, v)), modules, position)
            val text = vs map {
              case VText(text) => text
              case v => report("The following is not a string but is the result of an iteration", positionOf(body, position))
            }
            VText(text.foldLeft[Text](SString(""))(_ + _))
          case v => report("The following is not a list and thus cannot be iterated over", positionOf(list, position))
        }
      case ELookup(record, label) =>
        val v = interpret(record)
        v match {
          case VRecord(fields) =>
            if (fields.contains(label)) {
              fields(label)
            } else {
              throw new LookupException(position)
            }
          case VAbstract(fields) =>
            fields.field(label) match {
              case Some(v) => v
              case None => throw new LookupException(position)
            }
          case _ => report("The following is not a record and cannot have field " + label, positionOf(record, position))
        }
      case ERecord(fields) =>
        val fields2 = fields.mapValues({ case (e, _) => interpret(e) })
        VRecord(fields2)
      case EAppend(left, right) =>
        val text1 = interpret(left) match {
          case VText(text) => text
          case v => report("The following is not a string on the left side of this concatenation", positionOf(left, position))
        }
        val text2 = interpret(right) match {
          case VText(text) => text
          case v => report ("The following is not a string on the right side of this concatenation", positionOf(right, position))
        }
        VText(text1 + text2)
      case EChoice(primary, secondary) =>
        try {
          interpret(primary)
        } catch {
          case _: AlternativeException => interpret(secondary)
        }
    }

  def positionOf(e: Expression, position: Position): Position = e match {
    case EAt(position, e) => positionOf(e, position)
    case e => position
  }

  def report[T](message: String, position: Position) : T = {
    throw new InterpreterException(errorWithPosition(message, position))
  }

}

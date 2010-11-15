package templ

import templ.Expression._
import templ.Value._

object Test extends Application {

  val e = EChoice(ELookup(ERecord(Map(("x", (EText("first"), true)))), "x"), EText("second"))
  val v = Interpreter.interpret(e, Map(), ("", 0, 0))
  println(v)

  val p = "@for $x [{a}, {b}, {c}] {$x}"
  val result = Parser.parseAll(Parser.textExpression, p)
  println(result.get)
  println(Interpreter.interpret(result.get, Map(), ("", 0, 0)))

  //print("Hello World!1" + " Another string")
  
}

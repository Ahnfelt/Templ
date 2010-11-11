package templ

import templ.Expression._
import templ.Value._

object Test extends Application {

  val c = EChoice(ELookup(ERecord(Map(("x", (EText("first"), true)))), "x"), EText("second"))
  val v = Interpreter.interpret(c, Map(), ("", 0, 0))
  print(v)

  //print("Hello World!1" + " Another string")
  
}

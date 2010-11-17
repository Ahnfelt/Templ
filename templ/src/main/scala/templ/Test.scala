package templ

import templ.Expression._
import templ.Value._
import reflect.BeanProperty
import java.util.{Collections, Arrays}

object Test extends Application {
  class Person {
    def getName = "Peter"
    def getEmail = "peter@gmail.com"
  }

  val e = EChoice(ELookup(ERecord(Map(("x", (EText("first"), true)))), "x"), EText("second"))
  val v = Interpreter.interpret(e, Map(), ("", 0, 0))
  println(v)

  val program = "@for $x [{a}, {b}, {c}] {$x}"
  val result = Parser.parseAll(Parser.textExpression, program)
  println(result.get)
  println(Interpreter.interpret(result.get, Map(), ("", 0, 0)))

  println(JavaValues.fromObject("foo"))
  println(JavaValues.fromObject(Collections.singletonList("bar")))
  println(JavaValues.fromObject(new Person))
  //print("Hello World!1" + " Another string")
}

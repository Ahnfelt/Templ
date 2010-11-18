package templ

import _root_.templ.Value.VText
import templ.Expression._
import templ.Text._
import java.util.Collections

object Test extends Application {
  class Person {
    def getName = "Peter"
    def getEmail = "peter@gmail.com"
  }

  {
    val e = EChoice(ELookup(ERecord(Map(("x", (EText(SString("first")), true)))), "x"), EText(SString("second")))
    val v = Interpreter.interpret(e, Map(), ("", 0, 0))
    println(v)
  }

  {
    println()
    def run(program: String, map: Map[Variable, Value] = Map()) {
      val result = Parser.parseAll(Parser.textExpression, program)
      println(program)
      println(result.get)
      println(Interpreter.interpret(result.get, map, ("", 0, 0)))
      println()
    }
    run("@for $x [{a}, {b}, {c}] {$x}")
    run("@first [{a}, {b}, {c}]")
    run("@last [{a}, {b}, {c}]")
    run("@front [{a}, {b}, {c}]")
    run("@back [{a}, {b}, {c}]")
    run("@first [] | No elements")
    run("@html {@let $foo {<b>Foo</b>} {$foo $bar}}", Map(("bar", VText(SString("<i>bar</i> & baz")))))
  }

  {
    println(JavaValues.fromObject("foo"))
    println(JavaValues.fromObject(Collections.singletonList("bar")))
    println(JavaValues.fromObject(new Person))
  }
}

package templ

import scala.util.parsing.input.NoPosition
import templ.Value._
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
    val v = Interpreter.interpret(e, Map(), ("", NoPosition))
    println(v)
  }

  {
    println()
    def run(program: String, map: Map[Variable, Value] = Map()) {
      val result = Parser.parseAll(Parser.textExpression, program)
      println(program)
      println(result.get)
      println(Interpreter.interpret(result.get, map, ("", NoPosition)))
      println()
    }
    run("@for $x [{a}, {b}, {c}] {$x}")
    run("@first [{a}, {b}, {c}]")
    run("@last [{a}, {b}, {c}]")
    run("@front [{a}, {b}, {c}]")
    run("@back [{a}, {b}, {c}]")
    run("@first [] | No elements")
    run("@html {@let $foo {<b>Foo</b>} {$foo $bar}}", Map(("bar", VText(SString("<i>bar</i> & baz")))))
    run("""This is a long string with
          @for $x {foo} {}
          a for loop over a non-list""")
  }

  {
    println(JavaValues.fromObject("foo"))
    println(JavaValues.fromObject(Collections.singletonList("bar")))
    println(JavaValues.fromObject(new Person))
  }
}

package templ

import templ.Interpreter.InterpreterException
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
      try {
        println(Interpreter.interpret(result.get, map, ("", NoPosition)))
      } catch {
        case exception: InterpreterException =>
          println(exception.getMessage.replace("\n\n", "\n"))
      }
      println()
    }
    run("@for $x [{a}, {b}, {c}] {$x}")
    run("@first [{a}, {b}, {c}]")
    run("@last [{a}, {b}, {c}]")
    run("@front [{a}, {b}, {c}]")
    run("@back [{a}, {b}, {c}]")
    run("@first [] | No elements")
    run("@html {@let $foo {<b>Foo</b>} {$foo $bar}}", Map("bar" -> VText(SString("<i>bar</i> & baz"))))
    run("""This is a long string with
          @for $x {foo} {}
          a for loop over a non-list""")
    class TestData2 {
      def getBaz = "Works!"
    }
    class TestData {
      def getBar = new TestData2
    }
    run("$data.bar.baz", Map("data" -> JavaValues.lazyValue(new TestData)))
  }

  {
    println(JavaValues.strictValue("foo"))
    println(JavaValues.strictValue(Collections.singletonList("bar")))
    println(JavaValues.strictValue(new Person))
  }
}

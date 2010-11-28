package templ

import util.parsing.combinator.RegexParsers;

trait Translator {
  /**
   * Must find the translation for an input string, and return
   * it with any placeholders replaced by the given values.
   * It will throw a PlaceholdersException if the number of
   * values don't match the number of placeholders.
   */
  def translate(input: String, values: String*): String
}

object Translator {
  class PlaceholdersException(actual: Int, required: Int) extends IllegalArgumentException

  class MultiTranslator(languages: Map[String, Map[String, List[Either[String, Int]]]]) {
    /**
     * Returns a new translator with the same language data but a different
     * language selected.
     */
    def forLanguage(language: String) = new SingleTranslator(language, languages)
  }

  class SingleTranslator(language: String, languages: Map[String, Map[String, List[Either[String, Int]]]]) extends Translator {
    // Note: languages map a language name, like "english" to another map from input strings
    // "I have %s friends" to output containing placeholders
    private val translations = languages(language)

    def translate(input: String, values: String*): String = {
      val builder = new StringBuilder
      var max = -1
      for(translation <- translations(input)) translation match {
        case Left(value) =>
        case Right(index) => max = Math.max(index, max)
      }
      if(values.size != max + 1) throw new PlaceholdersException(values.size, max)
      for(translation <- translations(input)) translation match {
        case Left(value) => builder.append(value)
        case Right(index) => builder.append(values(index))
      }
      builder.toString
    }
  }

  object Parser extends RegexParsers {
    override val skipWhitespace = false
    def whitespace = "[ \t\r\n]*|[#][^\r\n]*"r
    def language = whitespace ~> "[A-Za-z0-9-_]+".r <~ whitespace
    def variable = "$" ~> "[a-z][a-zA-Z0-9]*".r ^^ ((name: String) => Right(name))
    def text = "([^${}]|[$][$]|[$][{]|[$][}])+".r ^^ ((value: String) => Left(value)) |
            "{" ~> variable <~ "}" | variable
    def string = whitespace ~> "{" ~> rep(text) <~ "}" <~ whitespace
    def pair = language ~ string ^^ { case language ~ string => (language, string) }
    def definitions = rep1(pair)
  }

  def parse(input: String): MultiTranslator = {
    val definitions = Parser.parseAll(Parser.definitions, input).get
    val (primaryLanguage, firstString) = definitions.head
    val map = scala.collection.mutable.Map[String, Map[String, List[Either[String, Int]]]]()
    var (key, names) = placeholders(firstString)
    for((language, string) <- definitions) {
      if(language == primaryLanguage) {
        val (newKey, newNames) = placeholders(string)
        key = newKey
        names = newNames
      }
      if(!map.contains(language)) map(language) = Map()
      map(language) = map(language) ++ Map(key -> string.map {
        case Left(value) => Left(value)
        case Right(name) => Right(names(name))
      })
    }
    new MultiTranslator(Map(map.toList: _*))
  }

  def placeholders(string: List[Either[String, String]]): (String, Map[String, Int]) = {
    val key = new StringBuilder
    string.foreach {
      case Left(value) => key.append(value.replace("%", "%%"))
      case Right(name) => key.append("%s")
    }
    var count = 0
    val names = string.flatMap {
      case Left(value) => None
      case Right(name) =>
        count += 1
        Some(name, count - 1)
    }
    (key.toString, Map(names: _*))
  }
}

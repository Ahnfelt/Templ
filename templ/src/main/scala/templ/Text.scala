package templ

import templ.Text._

/**
 * Defines a string type whose append method (the + operator) takes constant time,
 * and which supports custom escaping of all atomic Strings.
 */
sealed trait Text {
  def +(text: Text) = SAppend(this, text)
  def appendTo(builder: StringBuilder, mechanism: String => String = identity): Unit = this match {
    case SString(value) => 
      builder.append(mechanism(value))
    case SAppend(left, right) =>
      left.appendTo(builder, mechanism)
      right.appendTo(builder, mechanism)
    case SEscape(text, mechanism) =>
      text.appendTo(builder, mechanism)
  }
  override def toString = {
    val builder = new StringBuilder
    appendTo(builder)
    builder.toString
  }
}

object Text {
  case class SString(value: String) extends Text
  case class SAppend(left: Text, right: Text) extends Text
  case class SEscape(text: Text, mechanism: String => String) extends Text

  def escapeHtml(string: String) = string.
          replace("&", "&amp;").
          replace("\"", "&quot;").
          replace("'", "&apos;").
          replace("<", "&lt;").
          replace(">", "&gt;")

  def escapeBackslashString(string: String) = string.
          replace("\\", "\\\\").
          replace("\"", "\\\"").
          replace("'", "\\'")
}

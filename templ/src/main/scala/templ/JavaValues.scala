package templ

import templ.Value._
import templ.Text._
import scala.collection.immutable.Map
import scala.collection.JavaConversions._
import java.lang.reflect.Method
import java.lang.Iterable

object JavaValues {
  def lazyValue(value: AnyRef): Value = value match {
    case value: String => VText(SString(value))
    case value: Iterable[AnyRef] => VList(value.map(lazyValue _).toList)
    case value => VAbstract(new Abstract {
      def instanceOf(name: String) = {
        false // TODO
      }
      def field(label: String) = getter(value, label) match {
        case Some(result) => Some(lazyValue(result))
        case None => None
      }
    })
  }

  def strictValue(value: AnyRef): Value = value match {
    case value: String => VText(SString(value))
    case value: Iterable[AnyRef] => VList(value.map(strictValue _).toList)
    case value =>
      val methods = value.getClass.getMethods
      val getters = methods.filter(isGetter _)
      val fields = getters.map(method => (label(method), call(value, method)))
      VRecord(Map((for((l, Some(v)) <- fields) yield (l, v)): _*))
  }

  def isGetter(method: Method): Boolean = {
    !method.getDeclaringClass.isAssignableFrom(classOf[Object]) &&
    method.getName().startsWith("get")
  }

  def call(record: Object, method: Method): Option[Value] = {
    method.invoke(record) match {
      case null => None
      case value => Some(strictValue(value))
    }

  }

  def label(method: Method): String = {
    method.getName().substring(3, 4).toLowerCase + method.getName().drop(4)
  }

  def getter(value: AnyRef, label: String): Option[AnyRef] = {
    val name = "get" + label.substring(0, 1).toUpperCase + label.drop(1)
    try {
      val method = value.getClass().getMethod(name)
      return Some(method.invoke(value))
    } catch {
      case _: NoSuchMethodException => println(name); None
    }
  }
}

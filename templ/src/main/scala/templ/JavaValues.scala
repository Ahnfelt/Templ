package templ

import templ.Value._
import scala.collection.immutable.Map
import scala.collection.JavaConversions._
import java.lang.reflect.Method
import java.lang.Iterable

object JavaValues {
  def fromObject(value: AnyRef): Value = {
    value match {
      case value: String => VText(value)
      case value: Iterable[AnyRef] => VList(value.map(fromObject _).toList)
      case value =>
        val methods = value.getClass.getMethods
        val getters = methods.filter(isGetter _)
        val fields = getters.map(method => (label(method), call(value, method)))
        VRecord(Map(fields: _*))
    }
  }

  def isGetter(method: Method): Boolean = {
    !method.getDeclaringClass.isAssignableFrom(classOf[Object]) &&
    method.getName().startsWith("get")
  }

  def call(record: Object, method: Method): Value = {
    fromObject(method.invoke(record))
  }

  def label(method: Method): String = {
    method.getName().substring(3, 4).toLowerCase + method.getName().drop(4)
  }
}

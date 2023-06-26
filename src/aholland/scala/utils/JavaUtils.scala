package aholland.scala.utils

import java.util.Collections
import scala.jdk.CollectionConverters._

object JavaUtils:
 def unmodifiableList[E](item: E*): java.util.List[E] = Collections.unmodifiableList(item.toList.asJava)

 def unmodifiableList[A](list: List[A]): java.util.List[A] = Collections.unmodifiableList(list.asJava)

 implicit class JavaUnmodifiable[A](list: List[A]):
  def toJavaUnmodifiable: java.util.List[A] = unmodifiableList(list)

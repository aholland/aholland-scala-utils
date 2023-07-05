package io.github.aholland.scala.utils

case class NumberedItem[+T](offset: Int, item: T):
 def number: Int = offset + 1
 def isFirst: Boolean = offset == 0
 def isEven: Boolean = number % 2 == 0
 def isOdd: Boolean = !isEven
 def transform[NT](t: T => NT): NumberedItem[NT] = NumberedItem[NT](offset, t(item))

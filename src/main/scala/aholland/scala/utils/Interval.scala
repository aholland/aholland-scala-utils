package aholland.scala.utils

case class Interval[T](lower: T, upper: T, inclusiveLower: Boolean = true, inclusiveUpper: Boolean = true)(implicit n: Fractional[T]) {
 require(n.lteq(lower, upper), s"Lower bound cannot be larger than upper bound. lower=$lower, upper=$upper")
 private val lowerCheck = if inclusiveLower then n.lteq(lower, _) else n.lt(lower, _)
 private val upperCheck = if inclusiveUpper then n.lteq(_, upper) else n.lt(_, upper)

 def contains(num: T): Boolean = lowerCheck(num) && upperCheck(num)
}
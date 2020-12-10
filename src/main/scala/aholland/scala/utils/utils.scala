package aholland.scala

import java.time.{LocalDate, YearMonth}
import java.util.Calendar
import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}

package object utils {

  //TODO propose this
  implicit class SingleOption[A](option: Option[A]) {
    def process[B](sideEffect: A => B): Option[A] = {
      option.map(sideEffect)
      option
    }

    def mapIn[B](f: A => B): Option[B] = option.map(f)

    //TODO use instead of map, everywhere
    def mapOut[B](f: A => B, default: => B): B = if (option.isDefined) f(option.get) else default

    def flatMapIn[B, C](f: A => Option[B], sideEffect: => C = ()): Option[B] = { //sideEffect is really for logging
      val result = option.flatMap(f)
      if (option.isEmpty) sideEffect
      result
    }

    def flatMapOut[B](f: A => Option[B], default: B): B = option.flatMap(f).getOrElse(default)

    //TODO test default
    def forDefined[U, V](sideEffect: A => U, default: => V = ()): Unit = if (option.isEmpty) default else sideEffect(option.get)

    def isSo(predicate: A => Boolean): Boolean = option.exists(predicate)

    def is(value: A*): Boolean = value.foldLeft(false)((b, v) => b || option.contains(v))

    def notSo(predicate: A => Boolean): Boolean = !option.exists(predicate)

    def not(value: A): Boolean = !option.contains(value)

    def isEmptyOrIsSo(predicate: A => Boolean): Boolean = option.isEmpty || option.isSo(predicate)

    def isEmptyOrIs(value: A): Boolean = option.isEmpty || option.is(value)
  }

  implicit class SingleFuture[A](future: Future[A]) {
    def onResponse[U](sideEffect: A => U)(implicit executor: ExecutionContext): Unit = future.foreach(sideEffect)(executor)
  }

  implicit class BooleanOption(boolean: Boolean) {
    def mapIn[T](code: => T): Option[T] = if (boolean) Some(code) else None

    def flatMapIn[T](code: => Option[T]): Option[T] = if (boolean) code else None

    def mapOut[T](code: => T, default: => T): T = if (boolean) code else default

    def flatMapOut[T](code: => Option[T], default: T): T = if (boolean) code.getOrElse(default) else default
  }

  implicit class PositionZipper[+A](seq: Seq[A]) {
    //TODO performance
    // https://gitter.im/scala/scala Rob Norris @tpolecat Aug 26 23:46 "When I'm counting cookies I start at one."
    def numbered: Seq[NumberedItem[A]] = seq.zipWithIndex.map(z => NumberedItem(z._2, z._1))
  }

  implicit class WhereSplitter[+A](list: List[A]) {
    def splitAtIndexWhere(p: A => Boolean): Option[(List[A], List[A])] = {
      val index = list.indexWhere(p)
      if (index < 0) None else Some(list.splitAt(index))
    }
    def splitAfterLastIndexWhere(p: A => Boolean): Option[(List[A], List[A])] = {
      val index = list.lastIndexWhere(p)
      if (index < 0) None else Some(list.splitAt(index+1))
    }
  }

  implicit class SimpleListenerList[R](listeners: ListBuffer[() => R]) {
    def fire(): Unit = listeners.toList.foreach(_ ()) //TODO use wherever possible, extend
  }

  implicit class DataListenerList[T, R](listeners: ListBuffer[T => R]) {
    def fire(t: T): Unit = listeners.toList.foreach(_ (t)) //TODO use wherever possible, extend
  }

  val bdZero: BigDecimal = BigDecimal(0)

  case class WithOpPrev[A, B](opPrev: Option[B], item: A)

  //TODO commonality with scanLeftOpPrev?
  def withOpPrevs[A](list: List[A], startIndex: Int = 0): List[WithOpPrev[A, A]] = {
    val items = list.drop(startIndex)
    val opPrevs = list.take(startIndex).lastOption +: items.dropRight(1).map(Some(_))
    opPrevs.zip(items).map(z => WithOpPrev(z._1, z._2))
  }

  //TODO ? don't want to overdo the implicits. This needn't be implicit. But use everywhere for opPrev.
  def scanLeftOpPrev[A, B](list: Seq[A], op: (Option[B], A) => B): Seq[B] = {
    def someB(opB: Option[B], a: A): Option[B] = Some(op(opB, a))

    list.scanLeft(Option.empty[B])(someB).flatten
  }

  class SeqHolder[T](val sequence: Seq[T]) {
    def positionOf(elem: T): Int = sequence.indexOf(elem) + 1

    def asOption: Option[Seq[T]] = if (sequence.isEmpty) None else Some(sequence)
  }

  implicit class SeqHolderForSeq[T](s: Seq[T]) extends SeqHolder[T](s)

  implicit class SeqHolderForItem[T](item: T) extends SeqHolder[T](Seq(item))

  //TODO can definitely be improved with better understanding of lists and builders.
  implicit class SeqHolderForOption[T](opItem: Option[T]) extends SeqHolder[T](opItem.toList)

  def sequenceOf[T](p: SeqHolder[T]*): Seq[T] = p.foldLeft(Seq.empty[T])((seq, holder) => seq ++ holder.sequence)

  implicit class StringGrammar(s: String) {
    def in(candidates: String*): Boolean = candidates.contains(s)

    def toOpInt: Option[Int] = try Some(s.toInt) catch {
      case _: NumberFormatException => None
    }
  }

  implicit class Thrush[T](obj: T) {
    def |>[U](map: T => U): U = map(obj)
  }

  implicit class StringSeq(strings: Seq[String]) {
    def concat(separator: String): String = if (strings.isEmpty) "" else strings.reduce(StringUtils.concatenator(separator))
  }

  implicit class DateInMonth(date: LocalDate) {
    def yearMonth: YearMonth = YearMonth.from(date)
    def isEndOfMonth: Boolean = yearMonth.atEndOfMonth() == date
  }
}

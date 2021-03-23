package aholland.scala

import java.time.{LocalDate, YearMonth}
import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

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
  def forDefined[U, V](sideEffect: A => U, default: => V = ()): Unit = if (option.isDefined) sideEffect(option.get) else default

  def isSo(predicate: A => Boolean): Boolean = option.exists(predicate)

  def is(value: A*): Boolean = value.foldLeft(false)((b, v) => b || option.contains(v))

  def notSo(predicate: A => Boolean): Boolean = !option.exists(predicate)

  def not(value: A): Boolean = !option.contains(value)

  def isEmptyOrIsSo(predicate: A => Boolean): Boolean = option.isEmpty || option.isSo(predicate)

  def isEmptyOrIs(value: A): Boolean = option.isEmpty || option.is(value)

  def ifElse(condition: Option[A] => Boolean, alternativeOption: => Option[A]): Option[A] = ifElse(condition(option), alternativeOption)

  def ifElse(condition: Boolean, alternativeOption: => Option[A]): Option[A] = if (condition) option else alternativeOption
 }

 implicit class SingleTry[T](t: Try[T]) {
  def mapIn[U](f: T => U, opExceptor: Option[Throwable => Throwable] = None): Try[U] = {
   if (t.isSuccess || opExceptor.isEmpty)
    t.map(f)
   else {
    Failure(opExceptor.get(t.failed.get))
   }
  }

  def mapOut[U](f: T => U, recoverer: Throwable => U): U = {
   t match {
    case Success(value) => f(value)
    case Failure(exception) => recoverer(exception)
   }
  }

  def flatMapIn[U](f: T => Try[U]): Try[U] = {
   t.flatMap(f)
  }

  def flatMapOut[U](f: T => Try[U], recoverer: Throwable => U): U = {
   t.flatMap(f).getOrElse(recoverer(t.failed.get))
  }

  def flatMapOptionIn[U](f: T => Option[U], msgIfNone: String = "f mapped to None"): Try[U] = {
   val tryOpU: Try[Option[U]] = mapIn(f)
   tryOpU.transform(_.mapOut(Success(_), Failure[U](new Throwable(msgIfNone))), Failure[U])
  }

  def forSuccess[U, V](sideEffect: T => U, recoverer: Throwable => Unit = _ => ()): Unit = {
   t match {
    case Success(value) => sideEffect(value)
    case Failure(exception) => recoverer(exception)
   }
  }

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
  def numbered(): Seq[NumberedItem[A]] = seq.zipWithIndex.map(z => NumberedItem(z._2, z._1))
  def numbered(start: Int): Seq[NumberedItem[A]] = seq.zipWithIndex.map(z => NumberedItem(start - 1 + z._2, z._1))
 }

 /**
  * For method flatten. So that a sequence of NumberedItem of type Option[B] can become sequence of NumberedItem of type B, with the None entries elided.
  * Note: the numbers will still be in order but numbers will be missing where the items were None.
  */
 implicit def ni2to[B](ni: NumberedItem[Option[B]]): IterableOnce[NumberedItem[B]] = ni.item.mapOut(b => Some(ni.transform(_ => b)), None)

 implicit class WhereSplitter[+A](list: List[A]) {
  def splitAtIndexWhere(p: A => Boolean): Option[(List[A], List[A])] = {
   val index = list.indexWhere(p)
   if (index < 0) None else Some(list.splitAt(index))
  }

  def splitAfterLastIndexWhere(p: A => Boolean): Option[(List[A], List[A])] = {
   val index = list.lastIndexWhere(p)
   if (index < 0) None else Some(list.splitAt(index + 1))
  }
 }

 implicit class SimpleListenerList[R](listeners: ListBuffer[() => R]) {
  def fire(): Unit = listeners.toList.foreach(_ ()) //TODO use wherever possible, extend
 }

 implicit class DataListenerList[T, R](listeners: ListBuffer[T => R]) {
  def fire(t: T): Unit = listeners.toList.foreach(_ (t)) //TODO use wherever possible, extend
 }

 val bdZero: BigDecimal = BigDecimal(0)
 type BigDecimalInterval = Interval[BigDecimal]

 implicit class IntervalAwareBigDecimal(bd: BigDecimal) {
  def in(interval: BigDecimalInterval): Boolean = interval.contains(bd)
 }

 case class WithOpPrev[A, B](opPrev: Option[B], item: A)

 //TODO commonality with scanLeftOpPrev?
 def withOpPrevs[A](seq: Seq[A], startIndex: Int = 0): Seq[WithOpPrev[A, A]] = {
  val items = seq.drop(startIndex)
  val opPrevs = seq.take(startIndex).lastOption +: items.dropRight(1).map(Some(_))
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

  def filterOut(chars: String): String = StringUtils.filterOut(s, chars)
 }

 implicit class Thrush[T](obj: T) {
  def |>[U](map: T => U): U = map(obj)
 }

 implicit class DateInMonth(date: LocalDate) {
  def yearMonth: YearMonth = YearMonth.from(date)

  def isEndOfMonth: Boolean = yearMonth.atEndOfMonth() == date
 }

 implicit class ListRecursionHelper[A](list: List[A]) {
  def ~++(condition: Boolean, otherList: => List[A]): List[A] = if (condition) list ++ otherList else list

  def ~++(condition: List[A] => Boolean, otherList: => List[A]): List[A] = ~++(condition(list), otherList)

  def appendIf(condition: Boolean, otherList: => List[A]): List[A] = ~++(condition, otherList)

  def appendIf(condition: List[A] => Boolean, otherList: => List[A]): List[A] = ~++(condition, otherList)
 }
}

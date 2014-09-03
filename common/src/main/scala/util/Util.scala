package org.buttercoin.common

import java.util.Date
import scala.reflect.ClassTag
import scala.util.control.NonFatal
import java.security.MessageDigest

import org.buttercoin.common.models.core._

package object util {
  // Taken from http://stackoverflow.com/questions/6909053/enforce-type-difference
  sealed class =!=[A,B]
  trait LowerPriorityImplicits {
    /** Dummy method for type-checking support */
    implicit def equal[A]: =!=[A, A] = sys.error("should not be called")
  }
  object =!= extends LowerPriorityImplicits {
    implicit def nequal[A,B]: =!=[A,B] = new =!=[A,B]
  }
  // end

  type =?>[A, B] = PartialFunction[A, B]

  case class DateWithNanos(date: Date, nanos: Long)
  object DateWithNanos {
    def now = DateWithNanos(new Date(), System.nanoTime())
  }
  implicit val dateWithNanosOrdering: Ordering[DateWithNanos] = Ordering.by(d => (d.date, d.nanos))

  def asOpt[T](x: Any)(implicit ct: ClassTag[T]): Option[T] = x match {
    case (y: T) => Some(y)
    case _ => None
  }
}

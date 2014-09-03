package org.buttercoin.common.util

import java.util.Date

import scalaz._
import Scalaz._

trait Validator[T] {
  def missingMessage: String
  def validate: T => Validation[String, T]
}

object validations {
  type ValidatorF[T] = T => Validation[String, T]
  /* used like:
   *  someValue :> valid */
  def valid[T : Validator]: ValidatorF[T] = implicitly[Validator[T]].validate

  /* Check if a date is in the past according to the clock */
  val isInThePast: ValidatorF[Date] =
    requireThat[Date](_ <= DateWithNanos.now.date) orMsg (_ + " must not be in the future")

  // Fail if the provided string is empty
  def nonEmpty[T <% String]: ValidatorF[T] =
    requireThat[T](_.toString.trim.length > 0) orMsg (_ + s" required")

  // Fail if the provided string doesn't match the regular expression
  def matching[T <: String](pattern: String): ValidatorF[T] =
    requireThat[T](_ matches pattern) orMsg (_ + s" must match $pattern")

  def startsWith[T <: String](prefix: String): ValidatorF[T] =
    requireThat[T](_ startsWith prefix) orMsg (_ + s" must start with $prefix")

  // Fail if the provided value is not one of the alternatives
  def oneOf[T](alternatives: T*): ValidatorF[T] = 
    requireThat[T](alternatives.contains(_)) orMsg (_ + s" must be one of: $alternatives")

  // Fail unless input value length is less than or equal to limit
  def maxLength[T <: String](length: Int): ValidatorF[T] =
    requireThat[T](_.length <= length) orMsg (_ + s" is too long")

  // Fail unless input value as string has exactly matching length
  def mustHaveLength[T <: String](length: Int): ValidatorF[T] =
    requireThat[T](_.length == length) orMsg (_ + s" must have length $length")

  // Fail if the predicate fails and provide failureMsg(value)
  def requireThat[T](pred: T => Boolean): ValidatorF[T] = { value =>
    if(pred(value)) { value.success }
    else { s"$value is invalid".failure }
  }

  // Enforce the predicate if req statement is true, or allow success pass-through
  def requireIf[T](req: T => Boolean)(pred: ValidatorF[T]): ValidatorF[T] = { value =>
    if(req(value)) { pred(value) }
    else { alwaysValid[T](value) }
  }

  // Perform an inner validation and return the original object
  def check[T]: (T => Validation[String, Any]) => ValidatorF[T] = f => value =>
    f(value) map (_ => value)

  // Never fail
  def alwaysValid[T]: ValidatorF[T] = _.success

  // shortcut for creating simple validators from a ValidatorF[T]
  def validator[T]: ValidatorF[T] => Validator[T] = body =>
    new Validator[T] {
      val missingMessage = "value required"
      def validate = body
    }

  // use 'and' to combine validation clauses in the DSL:
  //  (x.foo :> valid) and
  //  (x.bar :> someConstraint) and
  //  x.success
  implicit class ValidationOps[T](val x: Validation[String, T]) {
    def and[U](next: => Validation[String, U]): Validation[String, U] = x.flatMap(_ => next)
  }

  // use -&- to and together constraints in the DSL:
  //  x.foo :>
  //    valid -&- somethingElse and
  //  x.success
  implicit class ValidationFOps[T](val xf: ValidatorF[T]) {
    def -&-(next: ValidatorF[T]): ValidatorF[T] = t =>
      (xf(t) |@| next(t))((_,_) => t)

    // Fail with a message parameterized by the value
    def orMsg(msgF: T => String): ValidatorF[T] = { value =>
      xf(value) ||| msgF(value).failure
    }

    // Fail with a given message
    def orMsg(msg: String): ValidatorF[T] = orMsg(_ => msg)
  }

  // use :> to apply constraints to a value
  // this is effectively just f.apply(_) swapped
  implicit class LiftValidationOps[T](val from: T) extends AnyVal {
    def :>(f: ValidatorF[T]) = f(from)
  }
}

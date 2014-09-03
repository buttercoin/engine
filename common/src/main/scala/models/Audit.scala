package org.buttercoin.common.models

import java.util.Date

import scalaz._
import Scalaz._

package object audit {
  case class AuditEvent[T](evt: T, timestamp: Long = new Date().getTime)

  trait Audit[TSubject, TEvt] {
    type This
    val subject: TSubject
    val events: List[AuditEvent[TEvt]]
    protected def log(e: AuditEvent[TEvt]): This
  }
}

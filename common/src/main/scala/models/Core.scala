package org.buttercoin.common.models

import java.util.UUID
import org.buttercoin.common.util.Validator
import org.buttercoin.common.util.validations._
import scala.language.implicitConversions

import scalaz._
import Scalaz._

package object core {
  trait AccountIDTag
  type AccountID = UUID @@ AccountIDTag
  def AccountID: UUID => AccountID = Tag[UUID, AccountIDTag]
}

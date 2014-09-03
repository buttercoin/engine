package org.buttercoin.common

import models.money._
import messages.CreditTrade
import com.typesafe.config._

import scalaz._
import Scalaz._

package object fees {
  trait FeeStrategyIDTag
  type FeeStrategyID = String @@ FeeStrategyIDTag
  def FeeStrategyID(uuid: String): FeeStrategyID = Tag[String, FeeStrategyIDTag](uuid)

  @SerialVersionUID(1L)
  trait FeeStrategy extends Serializable {
    type FeeInfo[T] = (T, T, BigDecimal)

    val parent: Option[FeeStrategy] = None
    def feeBreakdown[T <: Currency](msg: CreditTrade[T]): FeeInfo[T]
  }

  @SerialVersionUID(1L)
  trait StrategyFactory extends Serializable {
    protected val config: Config
    def apply(): FeeStrategy
  }
}

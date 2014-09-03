package org.buttercoin.common.models

import com.typesafe.config.Config
import org.buttercoin.common.messages._
import org.buttercoin.common.models.currency._
import org.buttercoin.common.models.money._

import scalaz._
import Scalaz._

object limits {
  // Simple limits to track fix min and max amounts we allow in an order or transaction
  //  These numbers can be set per currency, so USD can be handled separately from BTC, etc
  trait SystemLimit {
    val min: BigDecimal
    val max: BigDecimal

    private def isPositive(amount: BigDecimal) = { amount > zero }
    def isValid(amount: BigDecimal): Boolean = { isPositive(amount) && min <= amount && amount <= max }
  }

  case class OrderLimit(min: BigDecimal, max: BigDecimal) extends SystemLimit
}

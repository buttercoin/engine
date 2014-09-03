package org.buttercoin.common.models

import org.buttercoin.common.messages.Result
import org.buttercoin.common.util.validations._
import core.AccountID
import money._
import currency._
import java.util.UUID

import scalaz._
import Scalaz._

object order {
  type OrderID = UUID

  sealed trait Parity
  case object Bid extends Parity
  case object Ask extends Parity

  sealed trait TradeCompletionType
  case object LiquidityMaker extends TradeCompletionType
  case object LiquidityTaker extends TradeCompletionType

  sealed trait Type
  final case class Limit(price: Currency, quantity: Currency) extends Type
  final case class Market(offered: Currency, received: String) extends Type

  sealed trait Status
  sealed trait Funded { self: Status =>
    val spent: Currency
    val earned: Currency
    val liquidityType: TradeCompletionType
  }

  case object Pending extends Status
  final case class Opened(quantity: Currency) extends Status
  final case class Canceled(refund: Currency) extends Status
  final case class Filled(spent: Currency, earned: Currency, liquidityType: TradeCompletionType) extends Status with Funded
  final case class Reopened(spent: Currency, earned: Currency, liquidityType: TradeCompletionType) extends Status with Funded

  final case class Event(orderId: OrderID, accountId: AccountID, status: Status) extends Result

  implicit val validateCurrency = validator {
    requireThat[Currency](_.amount > 0) orMsg ("Currency must be positive: " + _)
  }

  implicit val validateOrderType = validator[Type] {
    check { _ match {
      case Limit(p, q) if p.code != q.code =>
        p :> valid[Currency].orMsg("Invalid limit price: " + _.format()) and
        p :> requireThat[Currency]{_.validOrderAmount()}.orMsg("Invalid order price: " + _.format()) and
        q :> valid[Currency].orMsg("Invalid limit quantity: " + _.format()) and
        q :> requireThat[Currency]{_.validOrderAmount()}.orMsg("Invalid order quantity: " + _.format())
      case Market(o, r) if o.code != r =>
        o :> valid[Currency].orMsg("Invalid market offer: " + _.format()) and
        o :> requireThat[Currency]{_.validOrderAmount()}.orMsg("Invalid order amount: " + _.format())
      case _ => "Must specify two different currencies when creating an order".failure
    } }
  }
}

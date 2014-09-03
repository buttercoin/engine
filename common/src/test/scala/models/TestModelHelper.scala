package org.buttercoin.common.testhelper

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Gen => ScalaGen}

import java.util.{Date, UUID}
import org.buttercoin.common.models.core._
import org.buttercoin.common.models.currency._
import org.buttercoin.common.models.money._
import org.buttercoin.common.models.order._
import org.buttercoin.common.query._
import org.buttercoin.common.testhelper.Gen._

object ModelGen {
  // Order history and order event models
  implicit val arbOrderParity: Arbitrary[Parity] = Arbitrary(ScalaGen.oneOf(Bid, Ask))

  implicit val arbLimit: Arbitrary[Limit] = Arbitrary(for {
    price <- arbitrary[USD]
    quantity <- arbitrary[BTC]
  } yield Limit(price, quantity))

  implicit val arbMarket: Arbitrary[Market] = Arbitrary(for {
    offered <- arbitrary[USD]
  } yield Market(offered, "BTC"))

  implicit val arbOrderType: Arbitrary[Type] =
    Arbitrary(ScalaGen.oneOf(arbitrary[Limit], arbitrary[Market]))

  implicit val arbOpenedOrderStatus: Arbitrary[Opened] =
    Arbitrary(for { quantity <- arbitrary[USD] } yield Opened(quantity))

  implicit val arbCanceledOrderStatus: Arbitrary[Canceled] =
    Arbitrary(for { refund <- arbitrary[USD] } yield Canceled(refund))

  implicit val arbFilledOrderStatus: Arbitrary[Filled] = Arbitrary(for {
    spent <- arbitrary[USD]
    earned <- arbitrary[BTC]
    lType <- arbitrary[TradeCompletionType]
  } yield Filled(spent, earned, lType))

  implicit val arbReopenedOrderStatus: Arbitrary[Reopened] = Arbitrary(for {
    spent <- arbitrary[USD]
    earned <- arbitrary[BTC]
    lType <- arbitrary[TradeCompletionType]
  } yield Reopened(spent, earned, lType))

  implicit val arbOrderStatus: Arbitrary[Status] =
    Arbitrary(ScalaGen.oneOf(ScalaGen.const(Pending), arbitrary[Opened], arbitrary[Canceled], arbitrary[Filled], arbitrary[Reopened]))

  implicit val arbTradeCompletionType: Arbitrary[TradeCompletionType] =
    Arbitrary(ScalaGen.oneOf(LiquidityMaker, LiquidityTaker))

  implicit val arbOrderEvent: Arbitrary[Event] = Arbitrary(for {
    orderId <- arbitrary[UUID]
    acctId <- arbitrary[AccountID]
    status <- arbitrary[Status]
  } yield Event(orderId, acctId, status))
}


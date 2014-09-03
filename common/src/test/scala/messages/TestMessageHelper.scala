package org.buttercoin.common.testhelper

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Gen => ScalaGen}

import java.util.{Date, UUID}
import org.buttercoin.common.messages._
import org.buttercoin.common.models.core._
import org.buttercoin.common.models.currency._
import org.buttercoin.common.models.order._
import org.buttercoin.common.testhelper.Gen._
import org.buttercoin.common.testhelper.ModelGen._
import org.buttercoin.common.query._

object MessageGen {
  // Exchange error messages
  implicit val arbOperationRequestError: Arbitrary[OperationRequestError] =
    Arbitrary(for { message <- nonEmptyString } yield OperationRequestError(message))

  implicit val arbBackendOperationRequestError: Arbitrary[BackendOperationRequestError] =
    Arbitrary(for { message <- nonEmptyString } yield BackendOperationRequestError(message))

  implicit val arbFieldOperationRequestError: Arbitrary[FieldOperationRequestError] = Arbitrary(for {
    fieldName <- nonEmptyString
    error1 <- nonEmptyString
    error2 <- nonEmptyString
  } yield FieldOperationRequestError(fieldName, List(error1, error2)))

  // Core exchange messages
  implicit val arbGetTicker: Arbitrary[GetTicker] = Arbitrary(for {
    left <- nonEmptyString
    right <- nonEmptyString
  } yield GetTicker(left, right))

  implicit val arbTicker: Arbitrary[Ticker] = Arbitrary(for {
    bid <- arbitrary[USD]
    ask <- arbitrary[USD]
    last <- arbitrary[USD]
  } yield Ticker(bid, ask, last))

  implicit val arbGetBalances: Arbitrary[GetBalances] =
    Arbitrary(for { acctId <- arbitrary[AccountID] } yield GetBalances(acctId))

  implicit val arbBalances: Arbitrary[Balances] = Arbitrary(for {
    usdBalance <- arbitrary[USD]
    btcBalance <- arbitrary[BTC]
  } yield Balances(List(usdBalance, btcBalance)))

  implicit val arbBalanceChanged: Arbitrary[BalanceChanged] = Arbitrary(for {
    acctId <- arbitrary[AccountID]
    balance <- arbitrary[USD]
  } yield BalanceChanged(acctId, balance))

  // Trades, fees, market depth and current exchange state messages and results
  implicit val arbCreditTradeUSD: Arbitrary[CreditTrade[USD]] =
    Arbitrary(for {
      acctId <- arbitrary[AccountID]
      orderId <- arbitrary[UUID]
      amount <- arbitrary[USD]
      tType <- arbitrary[TradeCompletionType]
    } yield CreditTrade(acctId, orderId, amount, tType))

  implicit val arbCreditTradeBTC: Arbitrary[CreditTrade[BTC]] =
    Arbitrary(for {
      acctId <- arbitrary[AccountID]
      orderId <- arbitrary[UUID]
      amount <- arbitrary[BTC]
      tType <- arbitrary[TradeCompletionType]
    } yield CreditTrade(acctId, orderId, amount, tType))

  implicit val arbGetFeeHistory: Arbitrary[GetFeeHistory] =
    Arbitrary(for { acctId <- arbitrary[AccountID] } yield GetFeeHistory(acctId))

  implicit val arbFeeAssessed: Arbitrary[FeeAssessed] =
    Arbitrary(for {
      acctId <- arbitrary[AccountID]
      orderId <- arbitrary[UUID]
      amount <- arbitrary[USD]
      rate <- ratioGen
      tType <- arbitrary[TradeCompletionType]
    } yield FeeAssessed(acctId, orderId, amount, rate, amount * rate, tType))

  implicit val arbDepthChange: Arbitrary[DepthChange] =
    Arbitrary(for {
      parity <- arbitrary[Parity]
      price <- arbitrary[USD]
      quantity <- arbitrary[BTC]
    } yield DepthChange(parity, price, quantity))

  implicit val arbMarketDepth: Arbitrary[MarketDepth] =
    Arbitrary(for {
      parity <- arbitrary[Parity]
      price <- arbitrary[USD]
      quantity <- arbitrary[BTC]
    } yield MarketDepth(parity, price, quantity))

  implicit val arbGetMarketDepth: Arbitrary[GetMarketDepth] =
    Arbitrary(for {
      price <- nonEmptyString
      quantity <- nonEmptyString
    } yield GetMarketDepth(price, quantity))

  // Store query messages for orders
  implicit val arbTotalOpenOrders: Arbitrary[TotalOpenOrders] =
    Arbitrary(for {
      cur1 <- arbitrary[USD]
      cur2 <- arbitrary[BTC]
    } yield TotalOpenOrders(List(cur1, cur2)))

  implicit val arbGetTotalLedgerBalances: Arbitrary[GetTotalLedgerBalances] =
    Arbitrary(for {
      acct1 <- arbitrary[AccountID]
      acct2 <- arbitrary[AccountID]
    } yield GetTotalLedgerBalances(List(acct1, acct2)))

  implicit val arbTotalLedgerBalances: Arbitrary[TotalLedgerBalances] =
    Arbitrary(for {
      cur1 <- arbitrary[USD]
      cur2 <- arbitrary[BTC]
    } yield TotalLedgerBalances(List(cur1, cur2)))

  implicit val arbOrderQueryType: Arbitrary[OrderQueryType] =
    Arbitrary(ScalaGen.oneOf(OpenOrderQuery, ClosedOrderQuery, AllOrderQuery))

  implicit val arbCancelOrder: Arbitrary[CancelOrder] =
    Arbitrary(for {
      acctId <- arbitrary[AccountID]
      orderId <- arbitrary[UUID]
    } yield CancelOrder(acctId, orderId))

  implicit val arbGetOpenOrders: Arbitrary[GetOpenOrders] =
    Arbitrary(for {
      acctId <- arbitrary[AccountID]
      skip <- ScalaGen.option[Int](positiveInt)
      limit <- ScalaGen.option[Int](positiveInt)
    } yield GetOpenOrders(acctId, skip, limit))

  implicit val arbGetOrderHistory: Arbitrary[GetOrderHistory] =
    Arbitrary(for {
      acctId <- arbitrary[AccountID]
      skip <- ScalaGen.option[Int](positiveInt)
      limit <- ScalaGen.option[Int](positiveInt)
    } yield GetOrderHistory(acctId, skip, limit))

  implicit val arbInternalOrderQuery: Arbitrary[InternalOrderQuery] =
    Arbitrary(for {
      acctId <- ScalaGen.option[AccountID](arbitrary[AccountID])
      skip <- ScalaGen.option[Int](positiveInt)
      limit <- ScalaGen.option[Int](positiveInt)
      hasMin <- arbitrary[Boolean]
      hasMax <- arbitrary[Boolean]
      status <- arbitrary[Status]
      orderId <- ScalaGen.option[UUID](arbitrary[UUID])
      parity <- ScalaGen.option[Parity](arbitrary[Parity])
      oType <- ScalaGen.option[Type](arbitrary[Type])
      qType <- arbitrary[OrderQueryType]
    } yield {
      val min = hasMin match {
        case false => None
        case true => Some(new Date().getTime - 1000 * 60 * 60 * 24)
      }
      val max = hasMax match {
        case false => None
        case true => Some(new Date().getTime - 1000 * 60 * 60 * 12)
      }
      InternalOrderQuery(acctId, skip, limit, min, max, orderId, List(status), parity, oType, qType)
    })

  // General arbitrary generators for different object types
  implicit val arbRequest: Arbitrary[Request] =
    Arbitrary(ScalaGen.oneOf(arbitrary[GetTicker], arbitrary[GetBalances]))

  implicit val arbResult: Arbitrary[Result] =
    Arbitrary(ScalaGen.oneOf(arbitrary[Ticker], arbitrary[Balances]))

  // Inter-process messages
  implicit val arbMessageRequest: Arbitrary[MessageRequest] = Arbitrary(for {
    message <- arbitrary[Request]
    id <- ScalaGen.option(arbitrary[UUID])
  } yield MessageRequest(message, id))

  implicit val arbMessageSuccess: Arbitrary[MessageSuccess] = Arbitrary(for {
    id <- arbitrary[UUID]
    res <- arbitrary[Result]
  } yield MessageSuccess(res, id))

  implicit val arbMessageFailure: Arbitrary[MessageFailure] = Arbitrary(for {
    id <- arbitrary[UUID]
    error1 <- nonEmptyString
    error2 <- nonEmptyString
  } yield MessageFailure(List(error1, error2), id))

  implicit val arbResultSeq: Arbitrary[ResultSeq] = Arbitrary(for {
    res1 <- arbitrary[Result]
    res2 <- arbitrary[Result]
  } yield ResultSeq(List(res1, res2)))

  implicit val arbBroadcastMessage: Arbitrary[BroadcastMessage] = Arbitrary(for {
    message <- arbitrary[Result]
  } yield BroadcastMessage(message))

  implicit val arbUserMessage: Arbitrary[UserMessage[Result]] = Arbitrary(for {
    message <- arbitrary[Result]
    acctId <- arbitrary[AccountID]
  } yield UserMessage(message, acctId))
}


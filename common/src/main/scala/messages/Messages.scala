package org.buttercoin.common

import akka.actor.ActorRef
import java.util.Date
import java.util.UUID
import org.buttercoin.common.models.core._
import org.buttercoin.common.models.currency._
import org.buttercoin.common.models.money._
import org.buttercoin.common.models.order._
import org.buttercoin.common.query._
import org.buttercoin.common.util._
import scala.language.implicitConversions

import scalaz._
import Scalaz._

package object messages {
  case class RegisterForBroadcast(actor: ActorRef)
  case class UnregisterForBroadcast(actor: ActorRef)
  case class RegisterForChannel(actor: ActorRef, channelName: String)
  case class UnregisterForChannel(actor: ActorRef, channelName: String)

  type MessageID = UUID

  // Core message types used for internal communication between actors
  final case class MessageRequest(message: Request, id: Option[MessageID])
    extends RoutedMessage { type MessageType = Request }

  sealed trait MessageResponse { val id: MessageID }
  final case class MessageSuccess(result: Result, id: MessageID) extends MessageResponse
  final case class MessageFailure(errors: List[String], id: MessageID) extends MessageResponse

  case class ResultSeq(seq: Seq[Result]) extends Result

  trait RoutedMessage {
    type MessageType
    val message: MessageType
  }

  trait RoutedResult extends RoutedMessage { type MessageType <: Result }
  case class BroadcastMessage(message: Result) extends RoutedResult { type MessageType = Result }
  case class UserMessage[T <: Result](message: T, acctId: AccountID) extends RoutedResult { type MessageType = T }

  trait Message
  trait Request extends Message
  trait Result extends Message
  trait QueryResult extends Result
  trait UpdateEvent extends Result

  final case class ExchangeOperationSuccessResult() extends Result

  // Message for authentication via websocket in appserver
  final case class AuthenticationSuccess() extends Result

  // Trait for message notifying client of object deleted
  trait ResourceDeleted[T <: Result] extends Result { val resource: T }

  trait RequestError extends Result
  final case class OperationRequestError(message: String) extends RequestError
  final case class BackendOperationRequestError(message: String) extends RequestError
  final case class FieldOperationRequestError(fieldName: String, messages: List[String]) extends RequestError

  // Message types for client queries related to their own account
  trait Query extends Request
  trait AssociatedAccount { def accountId: AccountID }
  trait AccountQuery extends Query with AssociatedAccount

  trait OperationResult extends Result
  case class OperationFailure() extends Throwable with OperationResult


  trait Operation extends Request
  trait AccountOperation extends Operation with AssociatedAccount
  trait MarketQuery extends Query {
    val left: String
    val right: String
  }
  trait SimpleQuery extends Query


  // Ticker and balance messages to fetch exchange information for users
  final case class GetTicker(left: String, right: String) extends MarketQuery
  final case class Ticker(bid: Currency, ask: Currency, last: Currency) extends QueryResult

  final case class GetBalances(accountId: AccountID) extends AccountQuery
  final case class Balances(balances: Seq[Currency]) extends QueryResult
  final case class BalanceChanged(accountId: AccountID, newBalance: Currency) extends UpdateEvent with OperationResult

  // //Live Notifications messages
  //final case class GetLiveNotifications(accountId: AccountID) extends AccountQuery
  //final case class LiveNotificationDeleted(notificationId: BSONObjectID) extends Result

  // Messages to fetch order information as well as create and cancel new orders
  sealed trait InternalQuery extends Request {
    val skip: Option[Int]
    val limit: Option[Int]
  }
  final case class GetOpenOrders(
    accountId: AccountID,
    skip: Option[Int] = None,
    limit: Option[Int] = Some(100)) extends AccountQuery with InternalQuery

  final case class InternalOrderQuery(
    accountId: Option[AccountID] = None,
    skip: Option[Int] = None,
    limit: Option[Int] = Some(100),
    dateMin: Option[Long] = None,
    dateMax: Option[Long] = None,
    orderId: Option[OrderID] = None,
    status: List[Status] = List.empty,
    parity: Option[Parity] = None,
    orderType: Option[Type] = None,
    queryType: OrderQueryType = AllOrderQuery) extends InternalQuery

  final case class GetOrderHistory(
    accountId: AccountID,
    skip: Option[Int] = None,
    limit: Option[Int] = Some(100)) extends AccountQuery with InternalQuery

  final case class CancelOrder(accountId: AccountID, orderId: OrderID) extends AccountOperation

  // Messages to fetch the sum of ledger balances
  final case class GetTotalLedgerBalances(accountsBlacklist: List[AccountID]) extends Request
  final case class TotalLedgerBalances(balances: Seq[Currency]) extends Result

  // Messages to fetch the sum of open orders
  case object GetTotalOpenOrders extends Request
  final case class TotalOpenOrders(balances: Seq[Currency]) extends Result


  // Messages to get information about trades, fees, and market depth
  case class CreditTrade[T <: Currency](accountId: AccountID, orderId: OrderID, amount: T, completedAs: TradeCompletionType) extends AccountOperation

  final case class GetFeeHistory(accountId: AccountID) extends AccountQuery
  final case class FeeAssessed(accountId: AccountID,
                               orderId: OrderID,
                               fundedAmount: Currency,
                               feeRate: BigDecimal,
                               feeAmount: Currency,
                               tradeType: TradeCompletionType) extends Result {
    def noFeeAssessed(): Boolean = feeAmount.amount === BigDecimal("0")
  }

  final case class DepthChange(parity: Parity, price: Currency, quantity: Currency)
  final case class MarketDepth(parity: Parity, price: Currency, quantity: Currency) extends Result
  final case class GetMarketDepth(priceCode: String, quantityCode: String) extends Query
}

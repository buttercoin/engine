package org.buttercoin.jersey

import akka.actor._
import com.lmax.disruptor.{ util => _, _ }
import com.lmax.disruptor.dsl.Disruptor
import org.buttercoin.common.messages._
import org.buttercoin.common.util.validations._
import org.buttercoin.common.models.currency._
import org.buttercoin.common.models.money._
import org.buttercoin.common.models.order
import org.buttercoin.common.models.orderInfo._
import org.buttercoin.jersey.models.snapshot._
import org.buttercoin.jersey.messages._
import org.eligosource.eventsourced.core.Message
import scala.concurrent.stm._

import scalaz._
import Scalaz._

import shapeless._
import shapeless.syntax.singleton._
import shapeless.record._

import com.typesafe.scalalogging.slf4j.StrictLogging

package object engine {
  sealed trait Op
  case object Nop extends Op
  case class CO(order: CreateOrder) extends Op
  case class XO(order: CancelOrder) extends Op
  case class STSNAP(snap: JerseySnapshotRequest) extends Op
  case class LDSNAP(snap: Seq[MarketSnapshot]) extends Op
}

case class EngineEvent(
  var op: engine.Op = engine.Nop,
  var updates: ValidationNel[String, List[RoutedMessage]] = Nil.success,
  var seqNum: Long = 0
) {
  def sanitize() = {
    op = engine.Nop
    updates = Nil.success
    seqNum = 0
  }
}

case class EngineComplete(
  var nextActions: List[Request],
  var results: ValidationNel[String, List[RoutedMessage]],
  seqNum: Long)

object EngineEvent {
  val EVENT_FACTORY = new EventFactory[EngineEvent] {
    def newInstance(): EngineEvent = new EngineEvent
  }
}

class TradeEngine(disruptor: Disruptor[EngineEvent], executor: ActorRef) extends EventHandler[EngineEvent] with StrictLogging {
  disruptor.handleEventsWith(this)

  type OperationResult = (List[Request], ValidationNel[String, List[RoutedMessage]])

  val usdbtcStack = market.Stack.apply[USD, BTC]
  private var usdbtcMarket : market.Market2[USD, BTC] = usdbtcStack.Market()

  def markets() = Seq[market.Market2[_,_]](
    usdbtcMarket
  )

  def selectMarket(op: CreateOrder, f: MktAction[_, _]): OperationResult = op.pair match {
    case (p: USD, q: BTC) => mktDo(f, usdbtcMarket, p, q)
    case (q: BTC, p: USD) => mktDo(f, usdbtcMarket, p, q)
    case (p, q) => LedgerDeposit(op.accountId, op.info.offered).point[List] -> "No market for: $p $q".failureNel
  }

  def route: PartialFunction[Result, RoutedMessage] = {
    case i: order.Event => ChannelMessage(Message(i), "org.buttercoin.order.event")
    case t: Ticker => BroadcastMessage(t)
  }

  type MktAction[PC <: Currency, QC <: Currency] = (market.Market2[PC, QC], PC, QC) => market.Output
  def mktDo[PC <: Currency, QC <: Currency]
    (f: MktAction[_, _], mkt: market.Market2[PC, QC], p: PC, q: QC): OperationResult =
  {
    val out = f.asInstanceOf[MktAction[PC, QC]](mkt, p, q)
    out._2.toList -> out._1.map(route).toList.successNel[String]
  }

  def doCreateOrder(co: CreateOrder): OperationResult = {
    def run[PC <: Currency, QC <: Currency]: MktAction[PC, QC] = { (mkt, price, qty) =>
      (co.info.parity, co.info.orderType) match {
        case (parity, _: order.Limit) => mkt.runLimitOrder(parity, co.info.accountId, price, qty, co.info.orderId)
        case (order.Bid, _: order.Market) => mkt.runMarketBid(co.info.accountId, price, co.info.orderId)
        case (order.Ask, _: order.Market) => mkt.runMarketAsk(co.info.accountId, qty, co.info.orderId)
      }
    }

    try {
      (co :> valid) map { _ =>
        selectMarket(co, run).map(_.map(OperationSuccess(OrderAccepted(co.info.orderId)) :: _))
      } valueOr { err =>
        LedgerDeposit(co.info.accountId, co.info.offered).point[List] ->
          s"Error executing order: $err".failureNel
      }
    } catch {
      case err: Throwable => // NonFatal here??
        logger.error(s"Error executing order: ${err.getMessage}", err)

        LedgerDeposit(co.info.accountId, co.info.offered).point[List] ->
          s"Error executing order: ${err.getMessage}".failureNel
    }
  }

  def doCancelOrder(co: CancelOrder): OperationResult = {
    // Try to cancel the order in each market one-by-one
    markets
      .map(_.cancelById(co.orderId, co.accountId))
      .collectFirst { case Some(x) => x }
      .map { x =>
        x._2.toList -> (
          OperationSuccess(x._1(0)) :: x._1.map(route(_)).toList
        ).success
      } getOrElse {
        Nil -> s"Error cancelling order ${co.orderId}: Order not found".failureNel
      }
  }

  def createSnapshot(snap: JerseySnapshotRequest): OperationResult = atomic { implicit txn =>
    snap.markets() = List(MarketSnapshot("USD", "BTC", usdbtcMarket))
    Nil -> Nil.success
  }

  def loadSnapshot(snaps: Seq[MarketSnapshot]): OperationResult = {
    snaps collect {
      case MarketSnapshot("USD", "BTC", mkt) =>
        usdbtcMarket = mkt.asInstanceOf[market.Market2[USD, BTC]]
      case MarketSnapshot(p, q, _) =>
        logger.error(s"Invalid market in snapshot: $p - $q")
    }
    Nil -> Nil.success
  }

  def onEvent(event: EngineEvent, seqNum: Long, endOfBatch: Boolean) = {
    val (actions, results) = event.op match {
      case engine.Nop => Nil -> Nil.success
      case engine.CO(clo) => doCreateOrder(clo)
      case engine.XO(co) => doCancelOrder(co)
      case engine.STSNAP(x) => createSnapshot(x)
      case engine.LDSNAP(x) => loadSnapshot(x)
    }

    val xs = event.updates |+| results
    executor ! EngineComplete(
      actions,
      event.updates |+| results,
      event.seqNum)
  }

  def tickers: Seq[QueryResult] = {
    Seq(usdbtcMarket.ticker)
  }

  def depthInfo = {
    markets().map(_.depthInfo).foldLeft(Iterator[DepthChange]()) { _ ++ _ }
  }

  def openOrderIds = {
    markets().map(_.openOrderIds).foldLeft(Seq[order.OrderID]()) { _ ++ _ }
  }
}

package org.buttercoin.engine.actors

import org.buttercoin.common.messages._
import org.buttercoin.common.actor.Upstream
import org.buttercoin.common.models.core._
import org.buttercoin.common.models.currency._
import org.buttercoin.common.models.money._
import org.buttercoin.common.models.order
import org.buttercoin.common.models.order.OrderID
import org.buttercoin.common.models.orderInfo._
import org.buttercoin.common.util._
import org.buttercoin.engine.messages.ChannelMessage
import org.buttercoin.engine.messages.{ LedgerDeposit, ChannelMessage }
import org.buttercoin.engine.models.snapshot._

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.stm._
import scala.util.Success
import collection.immutable.{ Map, Set }
import java.util.UUID


trait OrderStore extends Upstream {
  import org.buttercoin.common.util.MultiMapOps._
  var ordersById = Map[OrderID, OrderInfoHistory]()
  var ordersByAcct = Map[AccountID, Set[OrderID]]()

  override def onSetUpstream(newRef: ActorRef, oldRef: ActorRef) = {
    oldRef ! UnregisterForChannel(self, "org.buttercoin.order.event")
    newRef ! RegisterForChannel(self, "org.buttercoin.order.event")
  }

  def finalizeOrder(hist: OrderInfoHistory) = {
    hist.refundAmount map { refund =>
      implicit val timeout = Timeout(5.seconds)
      val req = MessageRequest(
        LedgerDeposit(hist.subject.accountId, refund), Some(UUID.randomUUID))
      (upstream ? req) onComplete {
        case Success(MessageSuccess(_, _)) =>
        case _ =>
          log.error("unable to finalize order history: " + hist)
      }
    }

    ordersByAcct = ordersByAcct.removeBinding(hist.subject.accountId, hist.subject.orderId)
    ordersById = ordersById - hist.subject.orderId
    upstream ! ChannelMessage(hist, "org.buttercoin.archive")
  }

  register {
    case msg: OrderInfo =>
      val history = OrderInfoHistory(msg)
      ordersById = ordersById + (msg.orderId -> history)
      ordersByAcct = ordersByAcct.addBinding(msg.accountId, msg.orderId)
      upstream ! UserMessage(history, msg.accountId)

    case msg: order.Event =>
      for {
        x <- ordersById.get(msg.orderId)
        hist <- x.send(msg.status)
      } yield {
        updateMarketDepth(hist)
        if(hist.isFinal) {
          finalizeOrder(hist)
        } else {
          ordersById = ordersById + (hist.subject.orderId -> hist)
        }

        upstream ! UserMessage(msg, msg.accountId)
      }

    case msg: InternalOrderQuery => {

      val targetOrdersByAccount: List[OrderInfoHistory] = msg.accountId match {
        case None => ordersById.toList.map{ _._2 }
        case Some(acctId) => ordersByAcct.get(acctId).getOrElse(List()).toList.map{ id => ordersById.get(id) }.flatten
      }
      val targetOrdersById = msg.orderId match {
        case None => targetOrdersByAccount
        case Some(orderId) => targetOrdersByAccount filter { _.subject.orderId == orderId }
      }
      val targetOrdersByStatus = msg.status.isEmpty match {
        case true => targetOrdersById
        case false => {
          val queryStatusClasses = msg.status.map { _.getClass }
          targetOrdersById filter { o => queryStatusClasses.contains(o.state.getClass) }
        }
      }
      val targetOrdersByAction = msg.parity match {
        case None => targetOrdersByStatus
        case Some(parity) => targetOrdersByStatus filter { _.subject.parity == parity }
      }
      val targetOrdersByType = msg.orderType match {
        case None => targetOrdersByAction
        case Some(oType) => targetOrdersByAction filter { _.subject.orderType.getClass == oType.getClass }
      }
      val targetOrdersByMinDate = msg.dateMin match {
        case None => targetOrdersByType
        case Some(minTime) => targetOrdersByType filter { _.subject.createdAt.date.getTime >= minTime }
      }
      val targetOrdersByMaxDate = msg.dateMax match {
        case None => targetOrdersByMinDate
        case Some(maxTime) => targetOrdersByMinDate filter { _.subject.createdAt.date.getTime <= maxTime }
      }

      val skip = msg.skip.getOrElse(0)
      val limit = msg.limit.getOrElse(targetOrdersByMaxDate.length)

      sender ! ResultSeq(
        targetOrdersByMaxDate.sortBy(_.subject.createdAt)(Ordering[DateWithNanos].reverse).slice(skip, skip + limit)
      )

    }

    case NotifyOrderFailed(accountId, orderId) =>
      ordersByAcct = ordersByAcct.removeBinding(accountId, orderId)
      ordersById = ordersById - orderId

    case 'GetFullDepth =>
      sender ! calculateFullDepth

    case GetTotalOpenOrders =>
      sender ! TotalOpenOrders(totalCommittedFunds)

    case x: EngineSnapshotRequest => atomic { implicit txn =>
      x.orderStore() = Some(OrderStoreSnapshot(ordersById.toList, ordersByAcct.toList))
    }

    case EngineSnapshotOffer(snap) => {
      ordersById = snap.orderStore.ordersById.toMap
      ordersByAcct = snap.orderStore.ordersByAcct.toMap
      sender ! 'OK
    }
  }

  def updateMarketDepth(hist: OrderInfoHistory) = {
    hist.calculateDepthChange map { delta =>
      upstream ! ChannelMessage(delta, "org.buttercoin.market.depth")
    }
  }

  def calculateFullDepth: Seq[MarketDepth] = {
    ordersById.values
      .map(_.calculateMarketDepth)
      .collect { case Some(x) => x }
      .groupBy { x => (x.price, x.parity) }
      .map { x =>
        val (price, parity) = x._1
        val factory = x._2.head.quantity.factory
        val depth = factory(x._2.map(_.quantity.amount).sum)
        MarketDepth(parity, price, depth)
      }
      .toSeq
  }

  def totalCommittedFunds: Seq[Currency] = {
    val xs = calculateFullDepth.foldLeft((usd(0), btc(0))) { (acc, x) => x match {
      case MarketDepth(order.Bid, price, depth) =>
        (acc._1 + (price.asInstanceOf[USD] * depth.amount), acc._2)
      case MarketDepth(order.Ask, price, depth) =>
        (acc._1, acc._2 + depth.asInstanceOf[BTC])
    } }

    Seq(xs._1, xs._2)
  }
}

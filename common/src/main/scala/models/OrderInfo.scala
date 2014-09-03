package org.buttercoin.common.models

import java.util.UUID
import org.buttercoin.common.models.audit._
import org.buttercoin.common.models.core._
import org.buttercoin.common.models.currency._
import org.buttercoin.common.models.money._
import org.buttercoin.common.models.order._
import org.buttercoin.common.messages._
import org.buttercoin.common.util._
import org.buttercoin.common.util.validations._
import scala.Some

import scalaz._
import Scalaz._


object orderInfo {
  case class OrderInfo(
    accountId: AccountID,
    orderId: OrderID,
    parity: Parity,
    orderType: Type,
    createdAt: DateWithNanos = DateWithNanos.now
  ) extends Result {
    def offered: Currency = (parity, orderType) match {
      case (_, Market(o, _)) => o
      case (Bid, Limit(p, q)) => factoryFor(p).make(p.amount * q.amount)
      case (Ask, Limit(_, q)) => q
    }
  }

  case class OrderAccepted(orderId: OrderID) extends Result

  @SerialVersionUID(1L)
  case class OrderInfoHistory(subject: OrderInfo, events: List[AuditEvent[Status]] = List(AuditEvent(Pending)))
    extends Audit[OrderInfo, Status] with Stateful with Result
  {
    type State = Status
    type Message = Status
    type This = OrderInfoHistory

    def state = events(0).evt
    protected def log(x: AuditEvent[Status]) = copy(events = x :: events)
    protected def transition: (State, State, Long) => Option[This] = { (from, to, timestamp) =>
      Some(log(AuditEvent(to, timestamp)))
    }

    private def setStatus(x: Status) = x -> transition

    def isFinal = state.isInstanceOf[Canceled] || state.isInstanceOf[Filled]

    def isImmediatelyCanceled = events.size == 3 &&
      events(0).evt.isInstanceOf[order.Canceled] &&
      events(1).evt.isInstanceOf[order.Opened] &&
      events(2).evt == order.Pending

    def displayOrderType = subject.orderType match {
      case order.Limit(_, _) => "limit"
      case order.Market(_, _) => "market"
    }

    def remaining: Currency = {
      if (state.isInstanceOf[Opened]) {
        subject.offered
      } else if (state.isInstanceOf[Reopened]) {
        var amount = subject.offered.amount
        events foreach { _.evt match {
          case Reopened(spent, _, _) => {
            amount = amount - spent.amount
          }
          case _ =>
        } }
        factoryFor(subject.offered)(amount)
      } else {
        factoryFor(subject.offered)(0)
      }
    }


    lazy val tally = events
        .collect { case x: Funded => (x.spent.amount, x.earned.amount) }
        .suml

    state(Pending) { on =>
      on { case x => setStatus(x) }
    }

    state[Opened] { on =>
      on {
        case x: Reopened => setStatus(x)
        case x: Canceled => setStatus(x)
        case x: Filled => setStatus(x)
      }
    }

    state[Reopened] { on =>
      on {
        case x: Reopened => setStatus(x)
        case x: Canceled => setStatus(x)
        case x: Filled => setStatus(x)
      }
    }

    def refundAmount: Option[Currency] = {
      val parity = subject.parity
      if(!isFinal) { return None }
      val OC = subject.offered.factory
      val refund = events.map(_.evt).foldLeft(subject.offered) { (acc, x) =>
        x match {
          case Canceled(x) => OC(acc.amount - x.amount)
          case x: Funded => OC(acc.amount - x.spent.amount)
          case _ => acc
        }
      }

      if(refund.amount > 0) { Some(refund) }
      else { None }
    }

    def calculateMarketDepth: Option[MarketDepth] = {
      val parity = subject.parity
      if(isFinal || state == order.Pending) return None
      asOpt[order.Limit](subject.orderType) map { limit =>
        val qty = limit.quantity
        MarketDepth(parity, limit.price,
          events.foldLeft(limit.quantity) { (acc, x) =>
            asOpt[order.Funded](x.evt) map { funded =>
              if(parity == order.Bid) {
                qty.factory(acc.amount - funded.earned.amount)
              } else {
                qty.factory(acc.amount - funded.spent.amount)
              }
            } getOrElse(acc)
          }
        )
      }
    }

    def calculateDepthChange: Option[DepthChange] = {
      val parity = subject.parity
      asOpt[order.Limit](subject.orderType) flatMap { limit =>
        state match {
          case x: order.Funded if x.liquidityType == order.LiquidityMaker =>
            val qty = if(parity == order.Bid) {
              x.earned.factory(-x.earned.amount)
            } else {
              x.spent.factory(-x.spent.amount)
            }
            Some(DepthChange(parity, limit.price, qty))
          case order.Reopened(_, _, order.LiquidityTaker) =>
            calculateMarketDepth.map( md => DepthChange(parity, limit.price, md.quantity) )
          case x: order.Opened =>
            Some(DepthChange(parity, limit.price, limit.quantity))
          case x: order.Canceled =>
            val (earned, spent) = tally
            val qty = if(parity == order.Ask) {
              x.refund
            } else {
              limit.quantity.factory {
                limit.quantity.amount - spent
              }
            }

            Some(DepthChange(parity, limit.price, qty.factory(-qty.amount)))
          case _ => None
        }
      }
    }
  }

  final case class CreateOrder(info: OrderInfo) extends AccountOperation {
    val accountId = info.accountId
    val pair = info.orderType match {
      case order.Limit(p, q) => (p, q)
      case order.Market(o, r) => (o, factoryFor(r).get.zero)
    }
  }

  def createLimitBidOrder(accountId: AccountID, price: Currency, quantity: Currency) = CreateOrder {
    OrderInfo(accountId, UUID.randomUUID, order.Bid, order.Limit(price, quantity))
  }

  def createLimitAskOrder(accountId: AccountID, price: Currency, quantity: Currency) = CreateOrder {
    OrderInfo(accountId, UUID.randomUUID, order.Ask, order.Limit(price, quantity))
  }

  def createMarketBidOrder(accountId: AccountID, offered: Currency)
    (rF: CurrencyFactory[_ <: Currency]) = CreateOrder {
      OrderInfo(accountId, UUID.randomUUID, order.Bid, order.Market(offered, rF.code))
    }

  def createMarketAskOrder(accountId: AccountID, offered: Currency)
    (rF: CurrencyFactory[_ <: Currency]) = CreateOrder {
      OrderInfo(accountId, UUID.randomUUID, order.Ask, order.Market(offered, rF.code))
    }

  implicit val validateOrderInfo = validator[OrderInfo] {
    check(_.orderType :> valid)
  }

  implicit val validateCreateOrder = validator[CreateOrder] {
    check(_.info :> valid)
  }
}

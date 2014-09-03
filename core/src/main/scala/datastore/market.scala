package org.buttercoin.jersey

import org.buttercoin.common.util._
import org.buttercoin.common.models.core._
import org.buttercoin.common.models.money._
import org.buttercoin.common.models.order
import order.OrderID
import org.buttercoin.common.models.orderInfo._
import org.buttercoin.common.messages._
import java.util.UUID

import scalaz._
import Scalaz._

package object market {
  type Output = (Vector[Result], Vector[Request])

  object Stack {
    def apply[PC <: Currency : math.Ordering : CurrencyFactory, QC <: Currency : CurrencyFactory]
      (implicit ne1: PC =!= QC) =
        new Stack[PC, QC] with Serializable {
          var priceOrdering = implicitly[math.Ordering[PC]]
          var priceFactory = implicitly[CurrencyFactory[PC]]
          var qtyFactory = implicitly[CurrencyFactory[QC]]
        }
  }

  trait Market2[PC <: Currency, QC <: Currency] extends Serializable {
    def cancelById(id: UUID, accountId: AccountID): Option[Output]
    def runLimitOrder(parity: order.Parity, acctId: AccountID, price: PC, qty: QC, orderId: OrderID): Output
    def runMarketBid(acctId: AccountID, offered: PC, orderId: OrderID): Output
    def runMarketAsk(acctId: AccountID, offered: QC, orderId: OrderID): Output
    def ticker: Ticker
    def depthInfo: Iterator[DepthChange]
    def openOrderIds: Seq[OrderID]

    def openOrders: Seq[Any]
  }

  trait Stack[PC <: Currency, QC <: Currency] extends book.Stack[PC, QC] {
    case class Market() extends Market2[PC, QC] {
      val bidBook = new BidBook
      val askBook = new AskBook

      private def updateLastPrice(amt: PC) = {
        lastPrice = Some(amt)
        (ticker.point[Vector] -> mzero[Vector[Request]])
      }

      def runLimitBid(order: BidLimitOrder) = {
        val ((miss, last), out) = askBook.fill(order)
        val res = out |+| miss.map(bidBook.open(_)).getOrElse(mzero[Output])
        last.map { l => res |+| updateLastPrice(l) } getOrElse(res)
      }

      def runLimitAsk(order: AskLimitOrder) = {
        val ((miss, last), out) = bidBook.fill(order)
        val res = out |+| miss.map(askBook.open(_)).getOrElse(mzero[Output])
        last.map { l => res |+| updateLastPrice(l) } getOrElse(res)
      }

      def runMarketBid(acctId: AccountID, offered: PC, orderId: OrderID) = {
        val ((_, last), out) = askBook.fill(BidMarketOrder(acctId, offered, orderId))
        last.map { l => out |+| updateLastPrice(l) } getOrElse(out)
      }

      def runMarketAsk(acctId: AccountID, offered: QC, orderId: OrderID) = {
        val ((_, last), out) = bidBook.fill(AskMarketOrder(acctId, offered, orderId))
        last.map { l => out |+| updateLastPrice(l) } getOrElse(out)
      }

      def runLimitOrder(parity: order.Parity, acctId: AccountID, price: PC, qty: QC, orderId: OrderID) = {
        if (parity == order.Bid) {
          runLimitBid { BidLimitOrder(acctId, price, qty, orderId) }
        } else {
          runLimitAsk { AskLimitOrder(acctId, price, qty, orderId) }
        }
      }

      def cancelById(id: UUID, accountId: AccountID) = {
        bidBook.cancel(id, accountId) orElse askBook.cancel(id, accountId)
      }

      protected var lastPrice: Option[PC] = None

      def ticker = Ticker(
        bidBook.top.map(_.price).getOrElse(priceFactory.zero),
        askBook.top.map(_.price).getOrElse(priceFactory.zero),
        lastPrice.getOrElse(priceFactory.zero))

      def depthInfo = {
        bidBook.allOrders.map { o =>
          DepthChange(order.Bid, o.price, o.quantity)
        } ++ askBook.allOrders.map { o =>
          DepthChange(order.Ask, o.price, o.quantity)
        }
      }

      def openOrderIds: Seq[OrderID] =
        bidBook.allOrders.map(_.orderId).toSeq ++ askBook.allOrders.map(_.orderId).toSeq

      def openOrders: Seq[Any] = bidBook.allOrders.toSeq ++ askBook.allOrders.toSeq
    }
  }
}


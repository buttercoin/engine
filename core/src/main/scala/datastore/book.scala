package org.buttercoin.engine

import java.util.UUID
import org.buttercoin.common.util._
import org.buttercoin.common.models.core.AccountID
import org.buttercoin.common.models.money._
import org.buttercoin.common.models.order
import order.{ TradeCompletionType, LiquidityMaker, LiquidityTaker }
import org.buttercoin.common.models.currency._
import org.buttercoin.engine.messages.LedgerDeposit
import org.buttercoin.common.messages.{ Request, CreditTrade }

import scalaz.{ Order => zOrder, Ordering => zOrdering, _ }
import Scalaz._

import shapeless.{ HNil, :: }
import shapeless.syntax.singleton._
import shapeless.record._

package object book {
  // kept for tests
  object Stack {
    def apply[PC <: Currency : math.Ordering : CurrencyFactory, QC <: Currency : CurrencyFactory]
      (implicit ne1: PC =!= QC) =
        new Stack[PC, QC] {
          var priceOrdering = implicitly[math.Ordering[PC]]
          var priceFactory = implicitly[CurrencyFactory[PC]]
          var qtyFactory = implicitly[CurrencyFactory[QC]]
        }
  }

  trait Stack[PC <: Currency, QC <: Currency] extends orders.Stack[PC, QC] {
    import market.Output

    trait Store {
      self: Book =>

      def insert(order: OrderT): Unit
      def remove(order: OrderT): OrderT
      def removeById(order: UUID): Option[OrderT]
      def removeAll(orders: Set[OrderT]): Unit
      def size: Int
      def top: Option[OrderT]
      def allOrders: Iterator[OrderT]
    }

    trait OrderMatching {
      self: Book with Store with OutputGenerator =>
      trait FillResult2
      case object NoMatch2 extends FillResult2

      implicit val rFactory: CurrencyFactory[RC]
      implicit val oFactory: CurrencyFactory[OC]

      def matchOrders[T <: FillOrder](fillWith: T)
      = {
        var curFill: Option[T] = Some(fillWith)
        var spent = rFactory(0)
        var earned = oFactory(0)
        var partial = false
        var reopen: Option[(OrderT, (OrderT, OrderT))] = None
        var lastPrice: Option[PC] = None

        def tally(order: OrderT): Unit = {
          spent = spent + order.received
          earned = earned + order.offered
        }

        val completed = allOrders.takeWhile { curOrder =>
          curFill map { x =>
            x.compareMatch(curOrder) match {
              case MatchAll =>
                // The current order fully matches the fill order
                curFill = None
                lastPrice = Some(curOrder.price)
                val q = x.quantityAtPrice(curOrder.price)
                if (q === curOrder.quantity) {
                  // We're exactly equal, nothing to reopen
                  tally(curOrder)
                } else {
                  // The current order is bigger, split it and reopen the rest
                  val (fill, remaining) = curOrder.split(q)
                  reopen = Some(curOrder, fill -> remaining)
                  tally(fill)
                }
                true
              case MatchSome => 
                val (_, open) = x.split(curOrder)
                tally(curOrder)
                curFill = Some(open.asInstanceOf[T]) //.asInstanceOf[FillOrder[T]])
                lastPrice = Some(curOrder.price)
                true
              case MatchNone => false
            }
          } getOrElse(false)
        }

        ("closed" ->> completed.toSet) ::
        ("earned" ->> earned) ::
        ("spent" ->> spent) ::
        ("residual" ->> curFill) ::
        ("reopened" ->> reopen) ::
        ("lastPrice" ->> lastPrice) :: HNil
      }

      def fill[T <: FillOrder](fillWith: T): ((Option[T], Option[PC]), Output) = {
        val res = matchOrders(fillWith)
        removeAll(res("closed"))

        val filled = res("reopened").map { x =>
          val (original, (_, r)) = x
          insert(r)
          res("closed") - original
        } getOrElse(res("closed"))

        val earned = if(res("spent").amount > 0 && res("earned").amount > 0 ) {
          if(res("residual").isEmpty) {
            genFilled(fillWith, res("spent"), res("earned"))
          } else {
            genReopened(fillWith, res("spent"), res("earned"))
          }
        } else {
          mzero[Output]
        }

        val outputs = filled foldMap (genFilled(_))
        val reOut = res("reopened") map { x =>
          val (_, (f, _)) = x
          genReopened(f)
        } getOrElse(mzero[Output])
        val missOut = res("residual") map(genMiss(_)) getOrElse(mzero[Output])

        (res("residual"), res("lastPrice")) -> (outputs |+| reOut |+| earned |+| missOut)
      }
    }

    trait Book extends Store with OrderParity with OrderMatching with OutputGenerator with Serializable {
      import scala.collection.immutable.{ SortedSet, HashMap }
      type OrderT <: orders.Meta with Limit
                    with OrderParity.Of[this.type]
                    with BookOrdered[OrderT]
                    with LimitSplittable[OrderT]
      type FillOrder = orders.Meta with Matching.Aux[OrderT] with Splittable[_] with Offered

      var set = SortedSet[OrderT]()
      var idMap = Map[UUID, OrderT]()

      def open(order: OrderT) = {
        insert(order)
        genOpened(order)
      }

      def cancel(orderId: UUID, accountId: AccountID) = {
        idMap.get(orderId) flatMap { order =>
          if(order.accountId == accountId) {
            Some(genCanceled(remove(order)))
          } else {
            None
          }
        }
      }

      def insert(order: OrderT) = {
        set = set + order
        idMap = idMap + (order.orderId -> order)
      }

      def remove(order: OrderT) = {
        set = set - order
        idMap = idMap - order.orderId
        order
      }

      // NOTE - This doesnt seem to be called by anyone externally,
      //  but it should likely include accountId to ensure the order
      //  being removed by the caller belongs to account (i.e. cancel)
      def removeById(id: UUID) = idMap.get(id).map(remove)

      def removeAll(orders: Set[OrderT]) = {
        set = set &~ orders
        idMap = idMap -- orders.map(_.orderId)
      }

      def size = set.size
      def top = set.headOption
      def allOrders = set.toIterator
    }

    class BidBook extends Book with Bid {
      type OrderT = BidLimitOrder
      val rFactory = implicitly[CurrencyFactory[RC]]
      val oFactory = implicitly[CurrencyFactory[OC]]
    }

    class AskBook extends Book with Ask {
      type OrderT = AskLimitOrder
      val rFactory = implicitly[CurrencyFactory[RC]]
      val oFactory = implicitly[CurrencyFactory[OC]]
    }

    trait OutputGenerator {
      self: Book =>
      import shapeless._
      import org.buttercoin.common.models.orderInfo.OrderInfo

      def genMiss(miss: FillOrder): Output = miss match {
        case _: Market =>
          order.Event(
            miss.orderId,
            miss.accountId,
            order.Canceled(miss.offered)
          ).point[Vector] ->
            LedgerDeposit(miss.accountId, miss.offered).point[Vector]
        case _ => mzero[Output]
      }

      def genOpened(opened: OrderT): Output =
        order.Event(
          opened.orderId,
          opened.accountId,
          order.Opened(opened.quantity)
        ).point[Vector] -> mzero[Vector[Request]]

      def genFilled(order: OrderT): Output = {
        genFilled(order, order.offered, order.received, LiquidityMaker)
      }

      def genFilled(filled: orders.Meta, spent: Currency, earned: Currency, liqType: TradeCompletionType=LiquidityTaker): Output = {
        order.Event(
          filled.orderId,
          filled.accountId,
          order.Filled(spent, earned, liqType)
        ).point[Vector] ->
          CreditTrade(filled.accountId, filled.orderId, earned, liqType).point[Vector]
      }

      def genReopened(filled: OrderT): Output = {
        genReopened(filled, filled.offered, filled.received, LiquidityMaker)
      }

      def genReopened(filled: orders.Meta, spent: Currency, earned: Currency, liqType: TradeCompletionType=LiquidityTaker): Output = {
        order.Event(
          filled.orderId,
          filled.accountId,
          order.Reopened(spent, earned, liqType)
        ).point[Vector] ->
          CreditTrade(filled.accountId, filled.orderId, earned, liqType).point[Vector]
      }

      def genCanceled: OrderT => Output = { canceled =>
        order.Event(
          canceled.orderId,
          canceled.accountId,
          order.Canceled(canceled.offered)
        ).point[Vector] ->
          LedgerDeposit(canceled.accountId, canceled.offered).point[Vector]
      }
    }
  }
}


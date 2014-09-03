package org.buttercoin.jersey

import java.util.UUID
import org.buttercoin.common.util._
import org.buttercoin.common.models.core.AccountID
import org.buttercoin.common.models.currency._
import org.buttercoin.common.models.money._
import org.buttercoin.common.models.money.CurrencyImplicits._

import scalaz.{ Order => zOrder, _ }
import Scalaz._

package object orders {
  @SerialVersionUID(1L)
  trait Meta extends Serializable {
    val accountId: AccountID
    val orderId: UUID
    val createdAt: DateWithNanos
  }

  case class Metadata(accountId: AccountID, orderId: UUID, createdAt: DateWithNanos) extends Meta

  // kept for tests
  object Stack {
    def apply[PC <: Currency : CurrencyFactory : math.Ordering,
              QC <: Currency : CurrencyFactory]
      (implicit ne1: PC =!= QC) =
        new Stack[PC, QC] {
          var priceOrdering = implicitly[math.Ordering[PC]]
          var priceFactory = implicitly[CurrencyFactory[PC]]
          var qtyFactory = implicitly[CurrencyFactory[QC]]
        }
  }

  trait Stack[PC <: Currency, QC <: Currency] {
    implicit var priceOrdering: math.Ordering[PC]
    implicit var priceFactory: CurrencyFactory[PC]
    implicit var qtyFactory: CurrencyFactory[QC]

    trait Priced { def price: PC }
    trait PriceCheck { def isMatchingPrice(p: PC): Boolean }

    trait OrderParity { type OC <: Currency; type RC <: Currency }
    object OrderParity {
      type Aux[A <: Currency, B <: Currency] = OrderParity { type OC = A; type RC = B }
      type Inv[T <: OrderParity] = OrderParity { type OC = T#RC; type RC = T#OC }
      type Of[T <: OrderParity] = OrderParity { type OC = T#OC; type RC = T#RC }
    }

    trait Bid extends OrderParity { type OC = PC; type RC = QC }
    trait Ask extends OrderParity { type OC = QC; type RC = PC }

    sealed trait MatchResult
    case object MatchNone extends MatchResult
    case object MatchAll extends MatchResult
    case object MatchSome extends MatchResult

    trait Matching {
      type MatchT
      def compareMatch[T <: MatchT](other: T): MatchResult
    }

    object Matching {
      type Aux[T] = Matching { type MatchT = T }
    }

    trait Offered {
      type OC <: Currency
      def offered: OC
    }
    object Offered { type Aux[C <: Currency] = Offered { type OC = C } }

    trait Received {
      type RC <: Currency
      def received: RC
    }
    object Received { type Aux[C <: Currency] = Received { type RC = C } }

    trait Quantity {
      //type QC <: Currency
      def quantity: QC
    }

    trait Limit extends Quantity with Priced with PriceCheck with Offered with Received /*with OrderParity*/ with Matching { 
      type MatchT <: Priced with Offered.Aux[RC] with Quantity
      def compareMatch[T <: MatchT](other: T): MatchResult = {
        if(isMatchingPrice(other.price)) {
          if(other.offered >= received && other.quantity >= quantity) { MatchAll }
          else { MatchSome }
        }
        else { MatchNone }
      }
    }

    trait Market extends Offered /*with OrderParity*/ with Matching {
      type MatchT <: Priced with Received.Aux[OC] //with OrderParity.Inv[this.type]
      def compareMatch[T <: MatchT](other: T): MatchResult = {
        if(other.received >= offered) { MatchAll }
        else { MatchSome }
      }
    }

    trait BookOrdered[T <: Meta with Priced] extends math.Ordered[T] {
      self: Meta with Priced =>
        protected val bookPriceOrdering: math.Ordering[PC]
        def compare(other: T): Int = {
          val priceComp = bookPriceOrdering.compare(price, other.price)
          if(priceComp == 0) {
            createdAt.nanos compare other.createdAt.nanos
          } else {
            priceComp
          }
        } 

        def isMatchingPrice(p: PC) = bookPriceOrdering.compare(p, price) >= 0
    }

    // Yay F-bounded polymorphism to the rescue!
    trait Splittable[T <: Splittable[T]] extends Matching {
      def split(other: MatchT): (T, T)
      def quantityAtPrice(p: PC): QC
    }

    trait LimitSplittable[T <: LimitSplittable[T]] extends Limit with Splittable[T] {
      def factory: QC => T
      def split(other: MatchT) = {
        assert(other.quantity < quantity)
        (factory(other.quantity), factory(quantity - other.quantity))
      }

      def split(amt: QC): (T, T) = {
        assert(amt < quantity)
        (factory(amt), factory(quantity - amt))
      }

      def quantityAtPrice(p: PC) = {
        if(isMatchingPrice(p)) { quantity }
        else { qtyFactory(0) }
      }
    }

    sealed trait BidLimitOrder extends Meta with Limit with Bid with BookOrdered[BidLimitOrder]
      with LimitSplittable[BidLimitOrder]
    {
      val bookPriceOrdering = inverseOrdering(priceOrdering)
      type MatchT = AskLimitOrder
      val factory = BidLimitOrder(accountId, price, _: QC, orderId, createdAt)
    }

    sealed trait AskLimitOrder extends Meta with Limit with Ask with BookOrdered[AskLimitOrder]
      with LimitSplittable[AskLimitOrder]
    {
      val bookPriceOrdering = priceOrdering
      type MatchT = BidLimitOrder
      val factory = AskLimitOrder(accountId, price, _: QC, orderId, createdAt)
    }

    sealed trait BidMarketOrder extends Meta with Market with Bid
      with Splittable[BidMarketOrder]
    {
      type MatchT = AskLimitOrder
      val factory = BidMarketOrder(accountId, _: PC, orderId, createdAt)
      def split(other: MatchT) = {
        assert(other.received < offered)
        (factory(other.received), factory(offered - other.received))
      }

      def quantityAtPrice(p: PC) = qtyFactory(offered.amount / p.amount)
    }

    sealed trait AskMarketOrder extends Meta with Market with Ask
      with Splittable[AskMarketOrder]
    {
      type MatchT = BidLimitOrder
      val factory = AskMarketOrder(accountId, _: QC, orderId, createdAt)
      def split(other: MatchT) = {
        assert(other.received < offered)
        (factory(other.received), factory(offered - other.received))
      }

      def quantityAtPrice(p: PC) = offered
    }

    def BidLimitOrder(acctId: AccountID, p: PC, qty: QC, orderId: UUID,
      time: DateWithNanos=DateWithNanos.now): BidLimitOrder with Meta =
    {
      assert(qty.amount > 0, s"order quantity is <= 0 [${qty}]")
      assert(p.amount > 0, s"order price is <= 0 [${p}]")
      new Metadata(acctId, orderId, time) with BidLimitOrder {
        val price = p
        val quantity = qty
        val offered = p * qty.amount
        val received = qty
        override def toString() = s"BidLimit(price: $p, qty: $qty, off: $offered)"
      }
    }

    def AskLimitOrder(acctId: AccountID, p: PC, qty: QC, orderId: UUID,
      time: DateWithNanos=DateWithNanos.now): AskLimitOrder with Meta =
    {
      assert(qty.amount > 0, s"order quantity is <= 0 [${qty}]")
      assert(p.amount > 0, s"order price is <= 0 [${p}]")
      new Metadata(acctId, orderId, time) with AskLimitOrder {
        val price = p
        val quantity = qty
        val offered = qty
        val received = p * qty.amount
        override def toString() = s"AskLimit(price: $p, qty: $qty, rec: $received)"
      }
    }

    def BidMarketOrder(acctId: AccountID, qty: PC, orderId: UUID,
      time: DateWithNanos=DateWithNanos.now): BidMarketOrder with Meta =
    {
      assert(qty.amount > 0, s"order quantity is <= 0 [${qty}]")
      new Metadata(acctId, orderId, time) with BidMarketOrder {
        val offered = qty
      }
    }

    def AskMarketOrder(acctId: AccountID, qty: QC, orderId: UUID,
      time: DateWithNanos=DateWithNanos.now): AskMarketOrder with Meta =
    {
      assert(qty.amount > 0, s"order quantity is <= 0 [${qty}]")
      new Metadata(acctId, orderId, time) with AskMarketOrder {
        val offered = qty
      }
    }
  }

  def inverseOrdering[T](orig: math.Ordering[T]): math.Ordering[T] = new math.Ordering[T] {
    def compare(x: T, y: T): Int = -(orig.compare(x, y))
  }
}


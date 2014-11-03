package test.datastore

import org.scalatest.{ FunSpec, BeforeAndAfter, Matchers }
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import org.buttercoin.common.models.core.AccountID
import org.buttercoin.common.util._
import org.buttercoin.engine._
import messages.LedgerDeposit
import org.buttercoin.common.messages._
import org.buttercoin.common.models.currency._
import org.buttercoin.common.models.money._
import org.buttercoin.common.models.money.CurrencyImplicits._
import org.buttercoin.common.models.order
import org.buttercoin.common.testhelper.Gen._
import shapeless.test.illTyped
import java.util.UUID

import shapeless.{ HNil, :: }
import shapeless.syntax.singleton._
import shapeless.record._

class BookSpec extends FunSpec
               with GeneratorDrivenPropertyChecks
               with Matchers
               with BeforeAndAfter
{
  val acctId = AccountID(UUID.randomUUID)
  val mCent = usdFactory.penny.amount

  describe("Stack") {
    describe("initialization") {
      it("should require different, concrete currency types") {
        illTyped("orders.Stack[USD, USD]")
        illTyped("orders.Stack[USD, Currency]")
        illTyped("orders.Stack[Currency, USD]")
      }
    }

    val stack = book.Stack[USD, BTC]

    describe("Store") {
      import stack._

      it("should only insert appropriate orders") {
        val book = new BidBook
        val bid = BidLimitOrder(acctId, usd(1), btc(1), UUID.randomUUID)
        val bidMkt = BidMarketOrder(acctId, usd(1), UUID.randomUUID)
        val ask = AskLimitOrder(acctId, usd(1), btc(1), UUID.randomUUID)

        book.insert(bid)
        book.size should be ( 1 )
        book.top should be ( Some(bid) )
        illTyped("book.insert(bidMkt)")
        illTyped("book.insert(ask)")
      }

      it("should sort bids from high to low") {
        val d = usd(1)
        val b = btc(1)
        val book = new BidBook
        val bid = BidLimitOrder(acctId, d, b, UUID.randomUUID)
        val bid2 = BidLimitOrder(acctId, d, b, UUID.randomUUID)
        val lowBid = BidLimitOrder(acctId, d / 2, b, UUID.randomUUID)
        val highBid = BidLimitOrder(acctId, d * 2, b, UUID.randomUUID)

        book.insert(bid2)
        book.insert(bid)
        book.insert(lowBid)
        book.insert(highBid)
        book.set.toList should be ( List(highBid, bid, bid2, lowBid) )
        book.idMap.keys should be ( Set(highBid.orderId, bid.orderId, bid2.orderId, lowBid.orderId) )
      }

      it("should sort asks from low to high") {
        val d = usd(1)
        val b = btc(1)
        val book = new AskBook
        val ask = AskLimitOrder(acctId, d, b, UUID.randomUUID)
        val ask2 = AskLimitOrder(acctId, d, b, UUID.randomUUID)
        val lowAsk = AskLimitOrder(acctId, d / 2, b, UUID.randomUUID)
        val highAsk = AskLimitOrder(acctId, d * 2, b, UUID.randomUUID)

        book.insert(ask2)
        book.insert(ask)
        book.insert(lowAsk)
        book.insert(highAsk)
        book.set.toList should be ( List(lowAsk, ask, ask2, highAsk) )
        book.idMap.keys should be ( Set(lowAsk.orderId, ask.orderId, ask2.orderId, highAsk.orderId) )
      }

      it("should be able to remove an order by id") {
        val book = new BidBook
        val o = BidLimitOrder(acctId, usd(1), btc(1), UUID.randomUUID)
        book.insert(o)
        book.removeById(o.orderId)
        book.size should be ( 0 )
      }

      it("should be able to bulk remove orders") {
        val book = new BidBook
        val o1 = BidLimitOrder(acctId, usd(1), btc(1), UUID.randomUUID)
        val o2 = BidLimitOrder(acctId, usd(1), btc(1), UUID.randomUUID)

        book.insert(o1)
        book.insert(o2)

        book.removeAll(Set(o1, o2))
        book.size should be ( 0 )
      }
    }

    describe("OrderMatching") {
      import stack._

      it("should be able to only fill the quantity of a misspriced limit") {
        forAll(pos[USD], pos[BTC], ratioGen) { (d0, b0, r) =>
          val book = new BidBook
          val b = b0 + btc(BigDecimal(2*r))
          val d = d0 + usd(BigDecimal(r))
          val bid = BidLimitOrder(acctId, d, b, UUID.randomUUID)
          val ask = AskLimitOrder(acctId, d / 2, b, UUID.randomUUID)

          book.insert(bid)

          val res = book.matchOrders(ask)
          res("spent") should be ( ask.offered )
          res("earned").amount should be ( bid.offered.amount +- mCent )
          res("closed") should be ( Set(bid) )
          res("residual") should be ( None )
          res("reopened") should be ( None )
        }
      }

      it("should be able to fill a matching limit order") {
        forAll(pos[USD], pos[BTC]) { (d, b) =>
          val book = new BidBook
          val bid = BidLimitOrder(acctId, d, b, UUID.randomUUID)
          val ask = AskLimitOrder(acctId, d, b, UUID.randomUUID)
          book.insert(bid)
          val res = book.matchOrders(ask)
          res should be {
            Set(bid) :: ask.received :: ask.offered :: None :: None :: Some(d) :: HNil
          }
        }
      }

      it("it should be able to partially fill a matching market order") {
        forAll(pos[USD], pos[BTC], ratioGen) { (d, b, r) =>
          val book = new BidBook
          val bid = BidLimitOrder(acctId, d, b, UUID.randomUUID)
          val ask = AskMarketOrder(acctId, b * (1 + r), UUID.randomUUID)

          book.insert(bid)

          val res = book.matchOrders(ask)
          res("spent") should be ( bid.received )
          res("earned") should be ( bid.offered )
          res("closed") should be ( Set(bid) )
          res("reopened") should be ( None )

          val Some(residual) = res("residual")
          residual.orderId should be ( ask.orderId )
          residual.offered should be ( b * (1 + r) - b )
        }
      }

      describe("fill Output") {
        it("it should be able to fill a matching market order") {
          forAll(pos[USD], pos[BTC]) { (d, b) =>
            val book = new BidBook
            val bid = BidLimitOrder(acctId, d, b, UUID.randomUUID)
            val ask = AskMarketOrder(acctId, b, UUID.randomUUID)

            book.insert(bid)

            val ((None, Some(d_)), (res, req)) = book.fill(ask)
            d_ should be ( d )
            res should have length ( 2 )
            req should have length ( 2 )
          }
        }

        it("it should be able to fill a small matching market order") {
          forAll(pos[USD], pos[BTC]) { (d, b) =>
            val book = new BidBook
            val bid = BidLimitOrder(acctId, d, b * 3, UUID.randomUUID)
            val ask = AskMarketOrder(acctId, b, UUID.randomUUID)

            book.insert(bid)

            val ((None, Some(d_)), (res, req)) = book.fill(ask)
            d_ should be ( d )
            res should have length ( 2 )
            res(0) should be ( order.Event(bid.orderId, acctId, order.Reopened(bid.offered / 3, ask.offered, order.LiquidityMaker)) )
            res(1) should be ( order.Event(ask.orderId, acctId, order.Filled(ask.offered, bid.offered / 3, order.LiquidityTaker)) )

            req should have length ( 2 )
          }
        }

        it("it should be able to partially fill a matching market order") {
          forAll(pos[USD], pos[BTC]) { (d, b) =>
            val book = new BidBook
            val bid = BidLimitOrder(acctId, d, b, UUID.randomUUID)
            val ask = AskMarketOrder(acctId, b * 2, UUID.randomUUID)

            book.insert(bid)

            val ((Some(extra), Some(d_)), (res, req)) = book.fill(ask)
            d_ should be ( d )
            res should have length ( 3 )
            res(1) should be ( order.Event(ask.orderId, acctId, order.Reopened(b, bid.offered, order.LiquidityTaker)) )
            res(2) should be ( order.Event(ask.orderId, acctId, order.Canceled(bid.received)) )

            req should have length ( 3 )
            req(1) should be ( CreditTrade(ask.accountId, ask.orderId, bid.offered, order.LiquidityTaker) )
            req(2) should be ( LedgerDeposit(ask.accountId, extra.offered) )
          }
        }

        it("should be able to partially fill a large limit order in the book") {
          forAll(pos[USD], pos[BTC], ratioGen) { (d0, b0, r) =>
            val book = new BidBook
            val b = b0 + btc(BigDecimal(2*r))
            val d = d0 + usd(BigDecimal(r))
            val bid = BidLimitOrder(acctId, d, b, UUID.randomUUID)
            val ask = AskLimitOrder(acctId, d, b * r, UUID.randomUUID)

            book.insert(bid)

            val ((None, Some(d_)), (res, req)) = book.fill(ask)
            d_ should be ( d )
            res should have length ( 2 )
            res(0) should be ( order.Event( bid.orderId, acctId, order.Reopened(ask.received, ask.offered, order.LiquidityMaker)) )
            res(1) should be ( order.Event( ask.orderId, acctId, order.Filled(ask.offered, ask.received, order.LiquidityTaker)) )

            req should have length ( 2 )
          }
        }

        it("should be able to only fill the quantity of a misspriced limit") {
          forAll(pos[USD], pos[BTC], ratioGen) { (d0, b0, r) =>
            val book = new BidBook
            val b = b0 + btc(BigDecimal(2*r))
            val d = d0 + usd(BigDecimal(r))
            val bid = BidLimitOrder(acctId, d, b, UUID.randomUUID)
            val ask = AskLimitOrder(acctId, d / 2, b, UUID.randomUUID)

            book.insert(bid)

            val ((None, Some(d_)), (res, req)) = book.fill(ask)
            d_ should be ( d )
            res should have length ( 2 )
            res(0) should be ( order.Event( bid.orderId, acctId, order.Filled(bid.offered, bid.received, order.LiquidityMaker)) )
            res(1) should be ( order.Event( ask.orderId, acctId, order.Filled(bid.received, bid.offered, order.LiquidityTaker)) )

            req should have length ( 2 )
          }
        }
      }

      describe("Cancellation") {
        it("should be possible to cancel an open order with the correct AccountID") {
          val book = new BidBook
          val bid = BidLimitOrder(acctId, usd("1"), btc("1"), UUID.randomUUID)
          book.insert(bid)

          val result = book.cancel(bid.orderId, acctId)
          result should be ( Some(
            Vector(order.Event( bid.orderId, acctId, order.Canceled(usd("1")))),
            Vector(LedgerDeposit(acctId, usd("1")))) )
        }

        it("should not be possible to cancel an order for a different Account") {
          val book = new BidBook
          val bid = BidLimitOrder(acctId, usd("1"), btc("1"), UUID.randomUUID)
          book.insert(bid)

          val result = book.cancel(bid.orderId, AccountID(UUID.randomUUID))
          result should be ( None )
        }
      }
    }
  }
}

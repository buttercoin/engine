package test.models

import java.util.UUID
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.{ FunSpec, Matchers }
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.buttercoin.common.messages.DepthChange
import org.buttercoin.common.models.core._
import org.buttercoin.common.models.money._
import org.buttercoin.common.models.money.CurrencyImplicits._
import org.buttercoin.common.models.currency._
import org.buttercoin.common.models.order
import org.buttercoin.common.models.orderInfo._
import org.buttercoin.common.util.Validator
import org.buttercoin.common.util.validations._
import org.buttercoin.common.testhelper.Gen._

import scalaz._
import Scalaz._

class OrderInfoSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  val d = usd("10000")
  val b = btc("20")
  val genInfo = for {
    acctId <- arbitrary[UUID]
    orderId <- arbitrary[UUID]
    parity <- Gen.oneOf(order.Bid, order.Ask)
    orderType <- Gen.oneOf(
      if(parity == order.Bid) { order.Market(d, "BTC") } else { order.Market(b, "USD") },
      order.Limit(d, b))
  } yield {
    OrderInfo(AccountID(acctId), orderId, parity, orderType)
  }

  def validate[T : Validator](x: T) = x :> valid

  describe("OrderInfo") {
    describe("Validation") {
      it("should sucessfully validate a correct order info") {
        forAll(genInfo) { info =>
          validate(info) should be ( Success(info) )
        }
      }

      it("should fail when an order is for the same currency") {
        val msg = "Must specify two different currencies when creating an order"
        var info = OrderInfo(AccountID(UUID.randomUUID), UUID.randomUUID, order.Bid,
          order.Market(btc("1"), "BTC"))
        validate(info) should be ( Failure(msg) )
        info = OrderInfo(AccountID(UUID.randomUUID), UUID.randomUUID, order.Bid,
          order.Limit(btc("1"), btc("1")))
        validate(info) should be ( Failure(msg) )
      }

      it("should fail when a currency is zero or negative") {
        var info = OrderInfo(AccountID(UUID.randomUUID), UUID.randomUUID, order.Bid,
          order.Limit(usd("-1"), btc("1")))
        var msg = "Invalid limit price: $-1.00"
        validate(info) should be ( Failure(msg) )

        info = OrderInfo(AccountID(UUID.randomUUID), UUID.randomUUID, order.Bid,
          order.Limit(usd("1"), btc("0")))
        msg = "Invalid limit quantity: 0 Bitcoin"
        validate(info) should be ( Failure(msg) )

        info = OrderInfo(AccountID(UUID.randomUUID), UUID.randomUUID, order.Bid,
          order.Market(usd("0"), "BTC"))
        msg = "Invalid market offer: $0.00"
        validate(info) should be ( Failure(msg) )
      }
    }
  }

  describe("CreateOrder") {
    describe("Validation") {
      it("should successfully validate a correct request") {
        forAll(genInfo) { info =>
          validate(CreateOrder(info)) should be ( Success(CreateOrder(info)) )
        }
      }

      it("should fail when an order is for the same currency") {
        val msg = "Must specify two different currencies when creating an order"
        var info = createMarketBidOrder(
          AccountID(UUID.randomUUID), btc("1"))(factoryFor(btc(0)))
        validate(info) should be ( Failure(msg) )
        info = createLimitBidOrder( AccountID(UUID.randomUUID), btc("1"), btc("1"))
        validate(info) should be ( Failure(msg) )
      }

      it("should fail when a currency is zero or negative") {
        var info = createLimitBidOrder(AccountID(UUID.randomUUID), usd("-1"), btc("1"))
        var msg = "Invalid limit price: $-1.00"
        validate(info) should be ( Failure(msg) )

        info = createLimitBidOrder(AccountID(UUID.randomUUID), usd("1"), btc("0"))
        msg = "Invalid limit quantity: 0 Bitcoin"
        validate(info) should be ( Failure(msg) )

        info = createMarketBidOrder(AccountID(UUID.randomUUID), usd("0"))(factoryFor(btc("0")))
        msg = "Invalid market offer: $0.00"
        validate(info) should be ( Failure(msg) )
      }
    }
  }

  describe("OrderInfoHistory") {
    it("should start in Pending") {
      forAll(genInfo) { info =>
        val hist = OrderInfoHistory(info)
        hist.state should be ( order.Pending )
        hist.events(0).evt should be ( order.Pending )
      }
    }

    it("should be cancellable from Pending") {
      forAll(genInfo) { info =>
        val original = OrderInfoHistory(info)
        val cancel = order.Canceled(original.subject.offered)

        val Some(hist) = original.send(cancel)
        hist.state should be ( cancel )
        hist.events(0).evt should be ( cancel )
        hist.events.tail should be ( original.events )
      }
    }

    describe("calculateDepthChange") {
      it("should report an appropriate depth change when it starts opened") {
        val info = OrderInfo(AccountID(UUID.randomUUID), UUID.randomUUID, order.Bid,
          order.Limit(usd("100"), btc("1")))
        val original = OrderInfoHistory(info)
        val opened = order.Opened(usd("100"))

        val Some(hist) = original.send(opened)
        hist.state should be ( opened )
        hist.calculateDepthChange.get should be ( DepthChange(info.parity, usd("100"), btc("1.0")) )
      }

      it("should report an appropriate depth change when it starts reopened") {
        val info = OrderInfo(AccountID(UUID.randomUUID), UUID.randomUUID, order.Bid,
          order.Limit(usd("100"), btc("1")))
        val original = OrderInfoHistory(info)
        val reopen = order.Reopened(usd("50"), btc("0.5"), order.LiquidityTaker)

        val Some(hist) = original.send(reopen)
        hist.state should be ( reopen )
        hist.calculateDepthChange.get should be ( DepthChange(info.parity, usd("100"), btc("0.5")) )
      }

      it("should report an appropriate depth change when a bid is paritally funded") {
        val info = OrderInfo(AccountID(UUID.randomUUID), UUID.randomUUID, order.Bid,
          order.Limit(usd("100"), btc("1")))
        val original = OrderInfoHistory(info)
        val opened = order.Opened(usd("100"))
        val reopen = order.Reopened(usd("50"), btc("0.5"), order.LiquidityMaker)

        val Some(hist) = original.send(opened).get.send(reopen)
        hist.state should be ( reopen )
        hist.calculateDepthChange.get should be ( DepthChange(info.parity, usd("100"), btc("-0.5")) )
      }

      it("should report an appropriate depth change when an ask is paritally funded") {
        val info = createLimitAskOrder(AccountID(UUID.randomUUID), usd("100"), btc("1")).info
        val original = OrderInfoHistory(info)
        val opened = order.Opened(usd("100"))
        val reopen = order.Reopened(usd("50"), btc("0.5"), order.LiquidityMaker)

        val Some(hist) = original.send(opened).get.send(reopen)
        hist.state should be ( reopen )
        hist.calculateDepthChange.get should be ( DepthChange(info.parity, usd("100"), usd("-50")) )
      }

      it("should report an appropriate depth change when a bid is canceled") {
        pending
        //val info = OrderInfo(AccountID(UUID.randomUUID), UUID.randomUUID, order.Bid,
          //order.Limit(usd("100"), btc("1")))
        //val original = OrderInfoHistory(info)
        //val opened = order.Opened(usd("100"))
        //val reopen = order.Reopened(usd("50"), btc("0.5"), order.LiquidityMaker)
        //val cancel = order.Canceled(usd("50"))

        //val Some(hist) = original.send(opened).get.send(reopen).get.send(cancel)
        //hist.state should be ( cancel )
        //hist.calculateDepthChange.get should be ( DepthChange(info.parity, usd("100"), btc("-0.5")) )
      }

      it("should report an appropriate depth change when a ask is canceled") {
        val info = OrderInfo(AccountID(UUID.randomUUID), UUID.randomUUID, order.Ask,
          order.Limit(usd("100"), btc("1")))
        val original = OrderInfoHistory(info)
        val opened = order.Opened(usd("100"))
        val reopen = order.Reopened(usd("50"), btc("0.5"), order.LiquidityMaker)
        val cancel = order.Canceled(usd("50"))

        val Some(hist) = original.send(opened).get.send(reopen).get.send(cancel)
        hist.state should be ( cancel )
        hist.calculateDepthChange.get should be ( DepthChange(info.parity, usd("100"), usd("-50")) )
      }

      it("should report no depth change for a pending order") {
        val info = OrderInfo(AccountID(UUID.randomUUID), UUID.randomUUID, order.Ask,
          order.Limit(usd("100"), btc("1")))
        val hist = OrderInfoHistory(info)

        hist.state should be ( order.Pending )
        hist.calculateDepthChange should be ( None )
      }
    }

    it("should be able to calculate a refund amount for a finalized order") {
      forAll(genInfo) { info =>
        val original = OrderInfoHistory(info)
        val spent = original.subject.offered.factory(original.subject.offered.amount / 2)
        val cancel = order.Canceled(original.subject.offered)
        val fill = order.Filled(spent, spent, order.LiquidityMaker)

        original.refundAmount should be ( None )
        original.send(fill).get.refundAmount should be ( Some(spent) )
        original.send(cancel).get.refundAmount should be ( None )
      }
    }
  }

  val usdFail = OrderInfo(AccountID(UUID.randomUUID), UUID.randomUUID, order.Bid, order.Limit(usd("0.001"), btc("500")))
  val usdFailLarge = OrderInfo(AccountID(UUID.randomUUID), UUID.randomUUID, order.Ask, order.Limit(usd("1000001"), btc("1000")))
  describe("OrderInfo") {
    describe("Validation") {
      it("should reject an order based on order limits") {
        validate(usdFail) should be ( Failure("Invalid order price: $0.00") )
        validate(usdFailLarge) should be ( Failure("Invalid order price: $1,000,001.00") )
      }
    }
  }
}

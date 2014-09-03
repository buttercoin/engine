package test.jersey

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalatest.{ FunSpec, Matchers }
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import com.typesafe.config._
import java.util.UUID
import org.buttercoin.common.fees._
import org.buttercoin.common.messages.CreditTrade
import org.buttercoin.common.models.core.AccountID
import org.buttercoin.common.models.currency._
import org.buttercoin.common.models.money._
import org.buttercoin.common.models.order._
import org.buttercoin.common.testhelper.Gen._


class FeesSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  describe("FeeStrategyID") {
    it("should be created from a string") {
      forAll { (x: String) =>
        FeeStrategyID(x).toString() should be (x)
      }
    }
  }


  val genCreditTrade = for {
    acctId <- arbitrary[UUID]
    orderId <- arbitrary[UUID]
    amount <- arbitrary[USD]
    completedAs <- Gen.oneOf(LiquidityMaker, LiquidityTaker)
  } yield {
    CreditTrade[USD](AccountID(acctId), orderId, amount, completedAs)
  }


  class TestFeeStrategy() extends FeeStrategy {
    val feeRate: BigDecimal = BigDecimal(0.1)

    override def feeBreakdown[T <: Currency](msg: CreditTrade[T]): FeeInfo[T] = {
      implicit val factory = factoryFor(msg.amount).asInstanceOf[CurrencyFactory[T]]
      val fee = msg.amount * feeRate
      val credit = msg.amount - fee
      (credit, fee, feeRate)
    }
  }

  describe("FeeStrategy") {
    it("should compute fee breakdown correctly") {
      forAll(genCreditTrade) { creditTrade =>
        val feeStrategy: TestFeeStrategy = new TestFeeStrategy()
        val (credit, fee, feeRate) = feeStrategy.feeBreakdown(creditTrade)
        feeRate should be (feeStrategy.feeRate)
        fee should be (creditTrade.amount * feeStrategy.feeRate)
        credit should be (creditTrade.amount * (1 - feeStrategy.feeRate))
      }
    }
  }

}

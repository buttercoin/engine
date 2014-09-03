package test.models

import org.scalatest.{ FunSpec, Matchers }
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.buttercoin.common.models.money._
import org.buttercoin.common.models.money.CurrencyImplicits._
import org.buttercoin.common.models.currency._
import org.buttercoin.common.util._
import org.buttercoin.common.util.validations._
import org.buttercoin.common.testhelper.Gen._

import scalaz._
import Scalaz._

class CurrencySpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  def validate[T : Validator](x: T) = x :> valid

  describe("Currency") {
    it("should be able to be constructed from a string") {
      usd("5") should be (usd(BigDecimal(5)))
    }

    it("should be a monoid") {
      forAll { (a: USD, b: USD) =>
        (a |+| b).amount should be ( a.amount + b.amount )
      }
    }

    it("should be invertable") {
      forAll { (x: USD) =>
        -x should be ( usd(-(x.amount)) )
      }
    }

    it("should be able to add and subract another of the same type of currency") {
      forAll { (a: USD, b: USD) =>
        (a - b) should be ( usd(a.amount - b.amount) )
        (a + b) should be ( usd(a.amount + b.amount) )
      }
      // USD("5") - BTC("1") - should not compile
    }

    it("should be able to be multiplied and divided by a scalar") {
      forAll { (a: USD, b: Double) =>
        (a * b) should be ( usd(a.amount * b) )
        whenever(b != 0) {
          (a / b) should be ( usd(a.amount / b) )
        }
      }
    }

    it("should preserve precision when performing math operations") {
      val x: USD = usd("2") / 3
      x should be (usd("0.66666666666667"))
    }

    it("should provide a validator that ensures amounts are positive") {
      var x: USD = usd("1")
      validate( x ) should be ( Success(x) )

      x = usd("0")
      validate( x ) should be ( Failure(s"Currency amounts must be positive") )

      x = usd("-1")
      validate( x ) should be ( Failure(s"Currency amounts must be positive") )
    }

    it("should record whether a currency type is crypto") {
      val x: USD = usd("1")
      x.factory.isCrypto should be (false)
    }
  }

  it("should enforce specific incoming format before parsing") {
    "123.12" matches usdFactory.pattern should be (true)
    "0.12" matches usdFactory.pattern should be (true)
    ".12" matches usdFactory.pattern should be (false)
    "1234." matches usdFactory.pattern should be (false)
    "1E12345" matches usdFactory.pattern should be (false)
    "1,123.12" matches usdFactory.pattern should be (false)
    "123.123" matches usdFactory.pattern should be (false)
    "123abc" matches usdFactory.pattern should be (false)
    "1000000000" matches usdFactory.pattern should be (false)

    "123.12" matches btcFactory.pattern should be (true)
    "0.12" matches btcFactory.pattern should be (true)
    "0.00000001" matches btcFactory.pattern should be (true)
    ".12" matches btcFactory.pattern should be (false)
    "1E12345" matches btcFactory.pattern should be (false)
    "1,123.123123" matches btcFactory.pattern should be (false)
    "213bac" matches btcFactory.pattern should be (false)
  }

  it ("should format usd with thousand separators, two decimals and round half even") {
    usd("78613423486").format() should be("$78,613,423,486.00")
    usd("1").format() should be("$1.00")
    usd("1000").format() should be("$1,000.00")
    usd("0.123").format() should be("$0.12")
    usd("0.345").format() should be("$0.34")
    usd("0.34500001").format() should be("$0.35")
    usd("31415926535897.9323846").format() should be("$31,415,926,535,897.93")
  }

  it ("should format BTC currency with the word \"Bitcoin\"") {
    btc("0.12").format() should be("0.12 Bitcoin")
    btc("-1").format() should be("-1 Bitcoin")
    btc("1").format() should be("1 Bitcoin")
    btc("0").format() should be("0 Bitcoin")
    btc("-2").format() should be("-2 Bitcoin")
  }

  it ("should format BTC currency with the word \"Bitcoins\" when the amount is greater than 1") {
    btc("2").format() should be("2 Bitcoins")
    btc("1.00000001").format() should be("1.00000001 Bitcoins")
    btc("1.5").format() should be("1.5 Bitcoins")
  }

  describe("classTagFor") {
    import scala.reflect.ClassTag
    val usdTag = implicitly[ClassTag[USD]]
    val btcTag = implicitly[ClassTag[BTC]]

    it("should be able to get a ClassTag from a string") {
      classTagFor("USD") should be ( usdTag )
      classTagFor("BTC") should be ( btcTag )
      a [MatchError] should be thrownBy classTagFor("FAKE")
    }

    it("should be able to get a ClassTag from an instance") {
      final case class FAKE(amount: BigDecimal) extends Currency {
        def format() = "FAKE"
      }

      classTagFor(usd("0")) should be ( usdTag )
      classTagFor(btc("0")) should be ( btcTag )
      a [MatchError] should be thrownBy classTagFor(FAKE("0"))
    }
  }

  describe("codeFor") {
    it("should be able to get a currency code from an instance") {
      codeFor(usd("0")) should be ( "USD" )
      codeFor(btc("0")) should be ( "BTC" )
    }
  }

  describe("factoryFor") {
    it("should produce None for invalid values") {
      factoryFor("FAKE") should be ( None )
      factoryFor(TypeSymbol("FAKE", Seq())) should be ( None )
    }
  }
}

package test.models

import org.scalatest.{ FunSpec, Matchers }

import org.buttercoin.common.models.money._
import org.buttercoin.common.models.money.CurrencyImplicits._

class MoneySpec extends FunSpec with Matchers {

  describe("CurrencyMonoid") {
    it("should satisfy monoid properties") {
      import org.buttercoin.common.models.currency._

      usdMonoid.zero should be( usd(BigDecimal(0)) )
      usdMonoid.append(usd(BigDecimal(1)), usd(BigDecimal(2))) should be( usd(BigDecimal(1 + 2)) )
    }
  }

  describe("CurrencyImplicits") {
    it("should handle implicit conversions to currency") {
      import org.buttercoin.common.models.currency.{usd, usdFactory}

      StringAsCurrency.toCurrency("1") should be (usd(BigDecimal(1)))
    }
  }

  describe("CurrencyFactory") {
    it("should provide currency-specific configuration info") {
      import org.buttercoin.common.models.currency._

      usdFactory.code should be ("USD")
      usdFactory.zero should be (usd(BigDecimal(0)))
      usdFactory.isFiat should be (true)
      usdFactory.isCrypto should be (false)

      usdFactory.precision should not be (2)
      btcFactory.precision should be (14)

      usdFactory.penny should be (usd(BigDecimal(1e-14)))

      usdFactory.systemOrderLimit.min should be (BigDecimal("0.01"))
      btcFactory.systemOrderLimit.min should be (BigDecimal("0.00001"))

      usdFactory.systemOrderLimit.max should be (BigDecimal("1000000"))
      btcFactory.systemOrderLimit.max should be (BigDecimal("1000000"))
    }
  }

}

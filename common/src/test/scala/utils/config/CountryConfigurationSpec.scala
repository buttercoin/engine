package test.utils.config

import org.scalatest.{ Matchers, FunSpec }
import org.buttercoin.common.util.countryConfiguration._

class CountryConfigurationSpec extends FunSpec with Matchers {

  describe("CountryConfiguration") {
    it("should only allow whitelisted currencies") {
      val config: CountryConfiguration = CountryConfiguration(Seq("USD", "JPY"))
      config.currencyAllowed("USD") should be (true)
      config.currencyAllowed("EUR") should be (false)
      config.currencyAllowed("") should be (false)
    }

    it("should only lookup whitelisted countries") {
      stringToCountryConfiguration.isDefinedAt("US") should be (true)
      stringToCountryConfiguration.isDefinedAt("Ummmm") should be (true)

      implicitly[CountryConfiguration]("US") should be (US)
      implicitly[CountryConfiguration]("Ummmm") should be (InvalidCountryConfiguration)
    }
  }

}

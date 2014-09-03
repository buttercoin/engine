package org.buttercoin.common.util

import com.typesafe.config.ConfigFactory
import org.buttercoin.common.util.validations._
import org.buttercoin.common.util.config.fallback._

import scalaz._
import Scalaz._

package object countryConfiguration {
  case class CountryConfiguration(currencyCodes: Seq[String] = List()) {
    def currencyAllowed(code: String): Boolean = currencyCodes.contains(code)
  }

  private def getCountryCurrencies(code: String): Seq[String] =
    ConfigFactory.load.getConfig("countries." + code).getStringListWithFallback("currencies")

  val InvalidCountryConfiguration = CountryConfiguration()
  val US = CountryConfiguration( getCountryCurrencies("US") )

  implicit val stringToCountryConfiguration: PartialFunction[String, CountryConfiguration] = {
    case "US" => US
    case _ => InvalidCountryConfiguration
  }

  // Fail if the provided string doesn't match the regular expression
  def matchingCountry[T <: String](regexMap: Map[CountryConfiguration, String])
                                  (implicit country: CountryConfiguration): ValidatorF[T] =
    requireThat[T](_ matches regexMap(country)) orMsg "Invalid format"
}

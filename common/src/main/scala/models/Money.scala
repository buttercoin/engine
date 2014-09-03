package org.buttercoin.common.models

import com.typesafe.config.ConfigFactory
import org.buttercoin.common.models.limits._
import org.buttercoin.common.util._
import org.buttercoin.common.util.validations._
import scala.language.implicitConversions

import scalaz._
import Scalaz._

package object money {
  trait Currency {
    val amount: BigDecimal
    def format(): String
  }

  @SerialVersionUID(1L)
  trait CurrencySettings[T] extends Serializable {
    val code: String
    val maker: BigDecimal => T
    val pattern: String
  }

  implicit def CurrencyValidator[T <: Currency] = validator {
    requireThat[T](_.amount > 0) orMsg "Currency amounts must be positive"
  }

  @SerialVersionUID(1L)
  trait CurrencyFactory[T <: Currency] extends Serializable {
    def apply(amount: BigDecimal): T = make(amount)
    val make: BigDecimal => T
    val precision: Int
    val code: String
    val pattern: String
    val roundingMode = BigDecimal.RoundingMode.HALF_EVEN
    def round(amount: BigDecimal): BigDecimal = amount.setScale(precision, roundingMode)
    lazy val zero = make(BigDecimal(0))
    lazy val penny = make(BigDecimal(s"1e${-precision}"))

    val isFiat: Boolean
    val isCrypto: Boolean
    val systemOrderLimit: OrderLimit
  }

  def currencyMonoid[T <: Currency : CurrencyFactory]: Monoid[T] = new Monoid[T] {
    private val factory = implicitly[CurrencyFactory[T]]
    def zero = factory(0)
    def append(x: T, y: => T) = factory(x.amount + y.amount)
  }

  def currencyFactory[T <: Currency](settings: CurrencySettings[T]): CurrencyFactory[T] = {
    val currencyConfig = ConfigFactory.load.getConfig("currency." + settings.code)
    val orderConfig = currencyConfig.getConfig("limits.order")

    new CurrencyFactory[T] {
      val code = settings.code
      val pattern = settings.pattern
      val make = { (amt: BigDecimal) => settings.maker(round(amt)) }

      val precision = ConfigFactory.load.getConfig("currency").getInt("precision")
      val isFiat = !currencyConfig.getBoolean("crypto")
      val isCrypto = currencyConfig.getBoolean("crypto")
      val systemOrderLimit = OrderLimit( BigDecimal(orderConfig.getString("min")), BigDecimal(orderConfig.getString("max")) )
    }
  }

  trait AsCurrency[TFrom] {
    def toCurrency[TCur <: Currency : CurrencyFactory](value: TFrom): TCur
  }

  object CurrencyImplicits {
    implicit def stringAsBigDecimal(str: String): BigDecimal = { BigDecimal(str) }

    implicit object StringAsCurrency extends AsCurrency[String] {
      def toCurrency[TCur <: Currency : CurrencyFactory](value: String) =
        implicitly[CurrencyFactory[TCur]].make(BigDecimal(value))
    }
  }
}

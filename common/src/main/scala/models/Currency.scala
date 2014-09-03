package org.buttercoin.common.models

import org.buttercoin.common.util._
import money._
import scala.language.implicitConversions
import org.buttercoin.common.models.money._
import java.text.DecimalFormat
import scala.language.implicitConversions
import scalaz._
import Scalaz._

object currency {
  val zero = BigDecimal(0)

  val df = new DecimalFormat()
  df.setMinimumFractionDigits(2)
  final case class USD private (amount: BigDecimal) extends Currency {
    def format(): String = { "$" + df.format(amount.setScale(2, BigDecimal.RoundingMode.HALF_EVEN)) }
  }
  object USDSettings extends CurrencySettings[USD] {
    val code: String = "USD"
    val maker: BigDecimal => USD = USD
    val pattern = """^\d{1,9}(\.\d{1,2})?$"""
  }
  implicit val usdFactory = currencyFactory[USD](USDSettings)
  implicit val usdMonoid = currencyMonoid[USD]
  def usd(amount: BigDecimal) = usdFactory(amount)

  final case class BTC private (amount: BigDecimal) extends Currency {
    // Strip trailing zeros from any decimal amounts, which returns a whole number in scientific notation 2E4
    //  Run the result back through a big decimal with no trailing decimal zeros and print result as plain string
    def format(): String =
      (if (amount.bigDecimal.compareTo(zero.bigDecimal) == 0) zero.bigDecimal.toPlainString else BigDecimal(amount.bigDecimal.stripTrailingZeros).bigDecimal.toPlainString) +
      " Bitcoin" +
      (if (amount > 1) "s" else "")
  }
  object BTCSettings extends CurrencySettings[BTC] {
    val code: String = "BTC"
    val maker: BigDecimal => BTC = BTC
    val pattern = """^\d{1,9}(\.\d{1,8})?$"""
  }
  implicit val btcFactory = currencyFactory(BTCSettings)
  implicit val btcMonoid = currencyMonoid[BTC]
  def btc(amount: BigDecimal) = btcFactory(amount)

  import scala.reflect._
  //import scala.reflect.runtime.universe._
  def factoryFor(sym: TypeSymbol): Option[CurrencyFactory[_ <: Currency]] = sym match {
    case TypeSymbol("USD", Seq()) => Some(usdFactory)
    case TypeSymbol("BTC", Seq()) => Some(btcFactory)
    case _ => None
  }

  def factoryFor(code: String): Option[CurrencyFactory[_ <: Currency]] = {
    typeSymFor(code).flatMap(factoryFor)
  }

  def factoryFor(cur: Currency): CurrencyFactory[_ <: Currency] = factoryFor(typeSymFor(cur)).get

  // ISSUE - Unsafe due to non-concurrent nature of reflection API
  // Might be fixed in 2.10.4
  //def typeTagFor(code: String): Option[TypeTag[_ <: Currency]] = code match {
    //case "USD" => Some(typeTag[USD])
    //case "BTC" => Some(typeTag[BTC])
    //case _ => None
  //}

  // HACK - Workaround using symbols instead of typetags
  case class TypeSymbol(symType: String, symArgs: Seq[TypeSymbol] = Seq())
  def typeSymFor(code: String): Option[TypeSymbol] = code match {
    case "USD" => Some(TypeSymbol("USD"))
    case "BTC" => Some(TypeSymbol("BTC"))
    case _ => None
  }

  // ISSUE - Unsafe due to non-concurrent nature of reflection API
  //def typeTagFor(cur: Currency): TypeTag[_ <: Currency] = cur match {
    //case _: USD => typeTag[USD]
    //case _: BTC => typeTag[BTC]
  //}

  // HACK - Workaround using symbols instead of typetags
  def typeSymFor(cur: Currency): TypeSymbol = cur match {
    case _: USD => TypeSymbol("USD")
    case _: BTC => TypeSymbol("BTC")
  }

  def classTagFor(code: String): ClassTag[_ <: Currency] = code match {
    case "USD" => classTag[USD]
    case "BTC" => classTag[BTC]
  }
  def classTagFor(cur: Currency): ClassTag[_ <: Currency] = cur match {
    case _: USD => classTag[USD]
    case _: BTC => classTag[BTC]
  }

  def codeFor(cur: Currency): String = cur match {
    case _: USD => "USD"
    case _: BTC => "BTC"
  }

  class CurrencyMath[T <: Currency](val self: T, private val factory: CurrencyFactory[T]) {
    def +(n: T): T = factory(self.amount + n.amount)
    def -(n: T): T = factory(self.amount - n.amount)
    def unary_-(): T = factory(-self.amount)
    def *(n: BigDecimal): T = factory(self.amount * n)
    def /(n: BigDecimal): T = factory(self.amount / n)
  }

  implicit def currencyWithMath[T <: Currency : CurrencyFactory](x: T): CurrencyMath[T] = {
    new CurrencyMath[T](x, implicitly[CurrencyFactory[T]])
  }

  implicit def CurrencyOrder[T <: Currency] = new Order[T] {
    def order(a: T, b: T) = {
      if (a.amount > b.amount) { Ordering.GT }
      else if (a.amount < b.amount) { Ordering.LT }
      else { Ordering.EQ }
    }
  }

  implicit def CurrencyOrdering[T <: Currency]: math.Ordering[T] =
    implicitly[math.Ordering[BigDecimal]].on(_.amount)

  final class GenericCurrencyOps(val self: Currency) extends AnyVal {
    def factory: CurrencyFactory[_ <: Currency] = factoryFor(self)
    def code: String = factory.code
    def validOrderAmount(): Boolean = factory.systemOrderLimit.isValid(self.amount)
  }

  implicit def currencyWithGenericOps(x: Currency): GenericCurrencyOps = {
    new GenericCurrencyOps(x)
  }
}

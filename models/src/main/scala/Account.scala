package org.buttercoin.engine.models

import java.util.UUID
import org.buttercoin.common.models.currency._
import org.buttercoin.common.models.money._
import org.buttercoin.common.models.money.CurrencyImplicits._
import org.buttercoin.common.models.core.AccountID
import org.buttercoin.common.util._

@SerialVersionUID(1L)
case class Balance[T <: Currency : CurrencyFactory](private var value: T) {
  private var factory = implicitly[CurrencyFactory[T]]
  def current: T = value
  def credit(c: BigDecimal): T = {
    value = factory(value.amount + c)
    value
  }
  def debit(d: BigDecimal): Option[T] = {
    val result = value.amount - d
    if(result >= 0) {
      value = factory(result)
      Some(value)
    }
    else { None }
  }
}

@SerialVersionUID(1L)
case class Account(val id: AccountID) {
  private var usdBalance: Balance[USD] = new Balance(usd("0"))
  private var btcBalance: Balance[BTC] = new Balance(btc("0"))

  def balanceFor[T <: Currency](value: T) : Option[Balance[T]] = value match {
    case USD(_) => Some(usdBalance.asInstanceOf[Balance[T]])
    case BTC(_) => Some(btcBalance.asInstanceOf[Balance[T]])
    case _ => None
  }

  def balance[T <: Currency](name: String): Option[T] = name match {
    case "USD" => Some(usdBalance.asInstanceOf[Balance[T]].current)
    case "BTC" => Some(btcBalance.asInstanceOf[Balance[T]].current)
    case _ => None
  }

  def deposit[T <: Currency](value: T): Option[T] =
    for(bal <- balanceFor(value)) yield bal.credit(value.amount)

  def withdraw[T <: Currency](value: T): Option[T] =
    for(bal <- balanceFor(value);
        result <- bal.debit(value.amount)) yield result

  def currentBalances: Seq[Currency] = {
    Seq("USD", "BTC") flatMap { (x:String) => balance(x) }
  }
}

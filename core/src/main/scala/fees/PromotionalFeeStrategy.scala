package org.buttercoin.engine.fees.strategies

import org.buttercoin.common.fees._
import org.buttercoin.common.messages._
import org.buttercoin.common.models.money._
import com.typesafe.config._
import java.util.Date
import java.text.DateFormat

trait PromotionalFeeStrategy extends FeeStrategy {
  protected val promotional: FeeStrategy
  protected val normal: FeeStrategy

  def isPromotional: Boolean
  def feeBreakdown[T <: Currency](msg: CreditTrade[T]): FeeInfo[T] = {
    if(isPromotional) {
      promotional.feeBreakdown(msg)
    } else {
      normal.feeBreakdown(msg)
    }
  }
}

trait UntilDateStrategy extends PromotionalFeeStrategy {
  protected val validUntil: Date
  def isPromotional = {
    (new Date()).getTime < validUntil.getTime
  }
}

class UntilDateFactory(protected val config: Config) extends StrategyFactory
{
  val df = DateFormat.getDateInstance
  private lazy val promoFactory = FeeStrategies(config.getValue("promotion"))
  private lazy val normalFactory = FeeStrategies(config.getValue("normal"))
  def apply() = {
    new UntilDateStrategy {
      protected val validUntil = try {
        df.parse(config.getString("validUntil"))
      } catch { case _: Throwable => new Date() }
      protected val promotional = promoFactory()
      protected val normal = normalFactory()
    }
  }
}

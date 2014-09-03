package org.buttercoin.jersey.fees.strategies

import org.buttercoin.common.fees._
import org.buttercoin.common._
import messages._
import models.money._
import models.currency._
import models.order.LiquidityMaker
import com.typesafe.config._
import collection.JavaConverters._

class FlatFeeStrategy(val feeRate: BigDecimal) extends FeeStrategy {
  override def feeBreakdown[T <: Currency](msg: CreditTrade[T]): FeeInfo[T] = {
    implicit val factory = factoryFor(msg.amount).asInstanceOf[CurrencyFactory[T]]
    val fee = msg.amount * feeRate
    val credit = msg.amount - fee
    (credit, fee, feeRate)
  }
}

class FlatFeeFactory(protected val config: Config) extends StrategyFactory {
  def apply() = {
    new FlatFeeStrategy(BigDecimal(config.getString("rate")))
  }
}

class TradebotFeeFactory(protected val config: Config) extends StrategyFactory {
  def apply() = new FlatFeeStrategy(BigDecimal(0))
}

object FeeStrategies {
  val feesConfig = ConfigFactory.load.getConfig("jersey.engine.fees")

  def apply(name: String): StrategyFactory = {
    lookup(FeeStrategyID(name))
  }

  def apply(cfgVal: ConfigValue): StrategyFactory = cfgVal.valueType() match {
    case ConfigValueType.STRING =>
      apply(cfgVal.unwrapped.asInstanceOf[String])
    case ConfigValueType.OBJECT =>
      initFeeFactory(cfgVal.asInstanceOf[ConfigObject].toConfig)
    case _ => throw new Exception("Invalid configuration at: " + cfgVal.origin)
  }

  private def initFeeFactory(stratConfig: Config) = {
    val stratClass = Class.forName(stratConfig.getString("factory"))
    val ctor = stratClass.getDeclaredConstructors.find { c =>
      val paramTs = c.getParameterTypes
      ( paramTs.length == 1 &&
        paramTs(0).isAssignableFrom(classOf[Config]))
    }.get

    ctor.newInstance(stratConfig).asInstanceOf[StrategyFactory]
  }

  val lookup = {
    val initialLookup = (feesConfig.getConfig("strategies").root.entrySet.asScala map {
      e: java.util.Map.Entry[String, ConfigValue] =>
        FeeStrategyID(e.getKey) ->
          initFeeFactory(e.getValue.asInstanceOf[ConfigObject].toConfig)
    }).toMap

    val default = initialLookup(FeeStrategyID(feesConfig.getString("defaultStrategy")))
    initialLookup + (FeeStrategyID("default") -> default)
  }
}

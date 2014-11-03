package org.buttercoin.engine.models.market

import org.buttercoin.common.models.order
import org.buttercoin.common.models.currency._
import org.buttercoin.common.models.money._

import scalaz._
import Scalaz._

package object depth {
  def mapWithDefaultLens[K,V](k: K): Map.WithDefault[K,V] @> V =
    Lens.lensg(m => v => m.updated(k,v), m => m(k))

  type CodePair = (String, String)
  type LevelKey = (order.Parity, Currency)
  type Levels = Map[LevelKey, Currency]
  object Levels {
    def apply(): Levels = Map[LevelKey, Currency]()
  }

  type MarketMap = Map.WithDefault[CodePair, Levels]
  object MarketMap {
    def apply(): MarketMap =
      Map[CodePair, Levels]().withDefaultValue(Levels())
      .asInstanceOf[MarketMap]
  }

  def marketL(pair: CodePair): Store @> Levels =
    marketsL >=> mapWithDefaultLens[CodePair, Levels](pair)

  def levelL(key: LevelKey): Levels @> Option[Currency] =
    Lens.mapVLens[LevelKey, Currency](key)

  def levelL(pair: CodePair, key: LevelKey): Store @> Option[Currency] =
    marketL(pair) >=> levelL(key)

  val marketsL: Store @> MarketMap = Lens.lensu((s, newMap) => s.copy(markets = newMap), _.markets)

  case class Store(val markets: MarketMap = MarketMap()) {
    def add(parity: order.Parity, price: Currency, qty: Currency): (Option[Currency], Store) = {
      val priceL = levelL(price.code -> qty.code, parity -> price)
      val newQ = priceL.get(this)
        .map { x => x.factory.make(x.amount + qty.amount) }
        .orElse(Some(qty))
        .flatMap { x => if(x.amount > 0) { Some(x) } else { None } }
      (newQ, priceL.set(this, newQ))
    }

    def set(parity: order.Parity, price: Currency, qty: Currency): Store = {
      val q = if(qty.amount > 0) { Some(qty) } else { None }
      levelL(price.code -> qty.code, parity -> price).set(this, q)
    }

    def levels(pair: CodePair) = {
      marketL(pair).get(this)
    }
  }
}

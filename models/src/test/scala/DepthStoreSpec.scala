package spec

import org.scalatest.{ FunSpec, BeforeAndAfter, Matchers }

import org.buttercoin.engine.models.market.depth
import org.buttercoin.common.models.currency._
import org.buttercoin.common.models.money._
import org.buttercoin.common.models.order

import scalaz._, Lens._
import Scalaz._

class DepthStoreSpec extends FunSpec
  with Matchers
{
  describe("Levels") {
    it("should be able to create an empty level map") {
      val levels = depth.Levels()
      levels should be ( 'empty )
    }

    it("should provide a lens to set a specific level to a price") {
      var key = order.Bid -> usd(1)
      var levels = depth.levelL(key).set(depth.Levels(), Some(btc(1)))

      depth.levelL(key).get(levels) should be ( Some(btc(1)) )
    }
  }

  describe("MarketMap") {
    it("should be able to create an empty market map") {
      val markets = depth.MarketMap()
      markets should be ( 'empty )
    }
  }

  describe("Store") {
    it("should provide a lens which can get levels for any market pair") {
      val levels = depth.marketL("USD" -> "BTC").get(new depth.Store())
      levels should be ( depth.Levels() )
    }

    it("should be able to set a level in a specific market") {
      val key = order.Bid -> usd(1)
      val priceL = depth.levelL("USD" -> "BTC", key)
      val markets = priceL.set(new depth.Store(), Some(btc(1)))

      depth.marketL("USD" -> "BTC").get(markets) shouldNot be ( 'empty )
      priceL.get(markets) should be ( Some(btc(1)) )
    }
  }
}

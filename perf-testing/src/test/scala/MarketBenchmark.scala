import org.scalameter.api._
import org.buttercoin.jersey._
import org.buttercoin.common._
import org.buttercoin.common.models.core.AccountID
import org.buttercoin.common.models.currency._
import scala.util.Random
import scala.math.abs
import java.util.UUID

object MarketBenchmark
extends PerformanceTest.Quickbenchmark {
  val sizes: Gen[Int] = Gen.range("size")(2000, 20000, 2000)
  val ranges: Gen[Range] = for {
    size <- sizes
  } yield 1 until size

  val acctId = AccountID(UUID.randomUUID)
  val rnd = new Random()

  val setSize = 5000
  val stack = market.Stack.apply[USD, BTC]
  import stack.{ Market => _, _ }
  class Market extends stack.Market

  val randomBids = (for(_ <- 0 to setSize) yield {
    val b = (abs(rnd.nextInt()) % 100) + 1
    val d = (abs(rnd.nextInt()) % 1000) + 10
    BidLimitOrder(acctId, usd(d), btc(b), UUID.randomUUID)
  }).toArray

  val randomAsks = (for(_ <- 0 to setSize) yield {
    val b = (abs(rnd.nextInt()) % 100) + 1
    val d = (abs(rnd.nextInt()) % 1000) + 10
    AskLimitOrder(acctId, usd(d), btc(b), UUID.randomUUID)
  }).toArray

  performance of "Market" in {
    measure method "random-pair" in {
      using(ranges) in { r =>
        val market = new Market
        for(i <- r) {
          market.runLimitBid(randomBids(abs(rnd.nextInt()) % setSize))
          market.runLimitAsk(randomAsks(abs(rnd.nextInt()) % setSize))
        }
      }
    }

    measure method "arbitrary" in {
      using(ranges) in { r =>
        val market = new Market
        for(i <- r) {
          if(abs(rnd.nextInt()) % 2 == 0) {
            market.runLimitBid(randomBids(abs(rnd.nextInt()) % setSize))
          } else {
            market.runLimitAsk(randomAsks(abs(rnd.nextInt()) % setSize))
          }
        }
      }
    }

    measure method "random-walk" in {
      using(ranges) in { r =>
        val market = new Market
        var price: Double = (abs(rnd.nextInt()) % 10000) + 1000.0
        for(i <- r) {
          val qty = (abs(rnd.nextInt()) % 10) + 1
          if(abs(rnd.nextInt()) % 2 == 0) {
            market.runLimitBid(BidLimitOrder(acctId, usd(price), btc(qty), UUID.randomUUID))
          } else {
            market.runLimitAsk(AskLimitOrder(acctId, usd(price), btc(qty), UUID.randomUUID))
          }
          price *= 1.0 + (rnd.nextDouble() - 0.45)/300.0
        }
      }
    }

  }
}

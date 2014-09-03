import org.scalameter.api._
import org.buttercoin.jersey._
import org.buttercoin.common._
import org.buttercoin.common.models.core.AccountID
import org.buttercoin.common.models.currency._
import scala.util.Random
import scala.math.abs
import java.util.UUID

object BookBenchmark
extends PerformanceTest.Quickbenchmark {
  val sizes: Gen[Int] = Gen.range("size")(10000, 50000, 10000)
  val ranges: Gen[Range] = for {
    size <- sizes
  } yield 1 until size

  val smSizes: Gen[Int] = Gen.range("size")(2000, 10000, 2000)
  val smRanges: Gen[Range] = for {
    size <- smSizes
  } yield 1 until size

  val rnd = new Random()
  val acctId = AccountID(UUID.randomUUID)
  val stack = book.Stack.apply[USD, BTC]
  import stack._

  val setSize = 5000
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

  performance of "Book" in {
    measure method "insert" in {
      using(ranges) in { r =>
        val book = new BidBook
        for(i <- r) {
          book.insert(BidLimitOrder(acctId, usd(i), btc(1), UUID.randomUUID))
        }
      }
    }

    measure method "fill-miss" in {
      using(ranges) in { r =>
        val book = new BidBook
        val order = AskLimitOrder(acctId, usd(1), btc(1), UUID.randomUUID)
        for(i <- r) {
          book.fill(order)
        }
      }
    }

    measure method "fill-even" in {
      using(ranges) in { r =>
        val book = new BidBook
        for(i <- r) {
          book.insert(BidLimitOrder(acctId, usd(i), btc(1), UUID.randomUUID))
          book.fill(AskLimitOrder(acctId, usd(i), btc(1), UUID.randomUUID))
        }
      }
    }

    measure method "random-pair" in {
      using(smRanges) in { r =>
        val book = new BidBook
        for(i <- r) {
          book.insert(randomBids(abs(rnd.nextInt()) % setSize))
          book.fill(randomAsks(abs(rnd.nextInt()) % setSize))
        }
      }
    }

    measure method "arbitrary" in {
      using(smRanges) in { r =>
        val book = new BidBook
        for(i <- r) {
          if(abs(rnd.nextInt()) % 2 == 0) {
            book.insert(randomBids(abs(rnd.nextInt()) % setSize))
          } else {
            book.fill(randomAsks(abs(rnd.nextInt()) % setSize))
          }
        }
      }
    }
  }
}

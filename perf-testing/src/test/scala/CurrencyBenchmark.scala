import org.scalameter.api._
import org.buttercoin.jersey._
import org.buttercoin.common._
import org.buttercoin.common.models.currency._

object CurrencyBenchmark
extends PerformanceTest.Quickbenchmark {
  val sizes: Gen[Int] = Gen.range("size")(10000, 90000, 40000)
  val ranges: Gen[Range] = for {
    size <- sizes
  } yield 1 until size

  val acctId = java.util.UUID.randomUUID

  performance of "Currency" in {
    performance of "creation" in {
      measure method "USD" in {
        using(ranges) in { r =>
          for(i <- r) {
            usd(i)
          }
        }
      }

      measure method "vs-bigdecimal" in {
        using(ranges) in { r =>
          for(i <- r) {
            BigDecimal(i).setScale(5, BigDecimal.RoundingMode.HALF_EVEN)
          }
        }
      }
    }

    performance of "addition" in {
      measure method "USD" in {
        using(ranges) in { r =>
          for(i <- r) {
            usd(i) + usd(i)
          }
        }
      }

      measure method "vs-bigdecimal" in {
        using(ranges) in { r =>
          for(i <- r) {
            val a = BigDecimal(i).setScale(5, BigDecimal.RoundingMode.HALF_EVEN)
            val b = BigDecimal(i).setScale(5, BigDecimal.RoundingMode.HALF_EVEN)

            val x = a + b
            x.setScale(5, BigDecimal.RoundingMode.HALF_EVEN)
          }
        }
      }
    }

    performance of "division" in {
      measure method "USD" in {
        using(ranges) in { r =>
          for(i <- r) {
            usd(i) / 5
          }
        }
      }

      measure method "vs-bigdecimal" in {
        using(ranges) in { r =>
          for(i <- r) {
            val a = BigDecimal(i).setScale(5, BigDecimal.RoundingMode.HALF_EVEN)
            val x = a / 5
            x.setScale(5, BigDecimal.RoundingMode.HALF_EVEN)
          }
        }
      }
    }
  }
}


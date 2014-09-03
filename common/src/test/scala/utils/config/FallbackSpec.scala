package test.utils.config

import org.scalatest.{ Matchers, FunSpec }
import org.buttercoin.common.util.countryConfiguration._

import com.typesafe.config._
import org.buttercoin.common.util.config.fallback._

class FallbackSpec extends FunSpec with Matchers {
  describe("ConfigWithFallback") {
    it("should check for fallback- prefixed keys if the value is empty") {
      val cfg: Config = ConfigFactory.parseString("""
        foo = []
        fallback-foo = ["bar"]
      """)
      cfg.getStringListWithFallback("foo") should be ( List("bar") )
    }

    it("should prefer the non-fallback config setting") {
      val cfg: Config = ConfigFactory.parseString("""
        foo = ["bar"]
        fallback-foo = ["fail"]
      """)
      cfg.getStringListWithFallback("foo") should be ( List("bar") )
    }
  }
}


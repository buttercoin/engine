package test.utils.models

import org.scalacheck.Arbitrary._
import org.scalatest.{ FunSpec, Matchers }
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import org.buttercoin.common.util.Validator
import org.buttercoin.common.util.validations.{ oneOf => isOneOf, _ }
import org.buttercoin.common.testhelper.Gen._

import scalaz._
import Scalaz._


class ValidatorSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  def validate[T : Validator](x: T) = x :> valid


  implicit val NonEmptyStringValidator = validator[String] {
    check { x =>
      x :> nonEmpty[String]
    }
  }

  implicit val SimpleBooleanValidator = validator[Boolean] {
    check { x =>
      x :> alwaysValid[Boolean]
    }
  }

  implicit val OneOfNumberValidator = validator[Int] {
    check { x =>
      x :> isOneOf(1, 3, 5, 7, 9)
    }
  }

  describe("Validator") {
    it("should validate values appropriately") {
      // Boolean validator above says everything is valid
      forAll { (x: Boolean) =>
        validate(x) should be(Success(x))
      }

      // String validator above only allows nonempty strings
      validate("") should not be(Success(""))
      forAll(nonEmptyString) { x =>
        validate(x) should be(Success(x))
      }

      // Number validator only allows odd numbers less than 10
      validate(1) should be(Success(1))
      validate(2) should not be(Success(2))
      validate(3) should be(Success(3))
      validate(4) should not be(Success(4))
      validate(5) should be(Success(5))
      validate(6) should not be(Success(6))
      validate(7) should be(Success(7))
      validate(8) should not be(Success(8))
      validate(9) should be(Success(9))
      validate(10) should not be(Success(10))
    }
  }

}

package test.utils.models

import org.scalatest.{ FunSpec, Matchers }
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import org.buttercoin.common.util.MultiMapOps._

class MultiMapSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  describe("MultiMap") {

    val key1 = "key1"
    val key2 = "key2"
    val key3 = "key3"

    val value1 = "value1"
    val value2 = "value2"
    val value3 = "value3"
    val value4 = "value4"
    val value5 = "value5"

    var map = Map[String, Set[String]]()

    it("should set and retrieve key value pairs") {
      // Verify the empty map properties
      map.size should be(0)

      // Start adding values
      map = map.addBinding(key1, value1)
      map = map.addBinding(key1, value2)
      map = map.addBinding(key2, value1)
      map = map.addBinding(key2, value2)
      map = map.addBinding(key2, value3)

      map.size should be(2)
      map.get(key1) should be(Some(Set(value1, value2)))
      map.get(key2) should be(Some(Set(value1, value2, value3)))
      map.get(key3) should be(None)

      map = map.addBinding(key3, value4)
      map = map.addBinding(key3, value5)

      map.size should be(3)
      map.get(key1) should be(Some(Set(value1, value2)))
      map.get(key2) should be(Some(Set(value1, value2, value3)))
      map.get(key3) should be(Some(Set(value4, value5)))
    }


    it("should remove key value pairs from set") {
      // Remove values only from the specified key set
      map = map.removeBinding(key1, value1)

      map.size should be(3)
      map.get(key1) should be(Some(Set(value2)))
      map.get(key2) should be(Some(Set(value1, value2, value3)))
      map.get(key3) should be(Some(Set(value4, value5)))

      // No-op if trying to remove a value not found in key set
      map = map.removeBinding(key1, value5)

      map.size should be(3)
      map.get(key1) should be(Some(Set(value2)))
      map.get(key2) should be(Some(Set(value1, value2, value3)))
      map.get(key3) should be(Some(Set(value4, value5)))

      // Removing the last item should leave an empty set
      map = map.removeBinding(key1, value2)

      map.size should be(3)
      map.get(key1) should be(Some(Set()))
      map.get(key2) should be(Some(Set(value1, value2, value3)))
      map.get(key3) should be(Some(Set(value4, value5)))
    }
  }

}

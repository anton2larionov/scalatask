package by.scalalab.ip

import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}

/**
  * Tests of [[by.scalalab.ip.IPAddress]].
  */
class IPAddressTest extends FunSuite with Matchers with PropertyChecks {

  test("testCompareTo") {

    val nums1 = for (n <- Gen.choose(0, 255)) yield n
    val nums2 = for (n <- Gen.choose(0, 255)) yield n

    forAll(nums1, nums2) { (v1: Int, v2: Int) =>

      if (v1 < v2) {
        IPAddress(v1, 0, 0, 0) should be < IPAddress(v2, 0, 0, 0)
        IPAddress(0, v1, 0, 0) should be < IPAddress(0, v2, 0, 0)
        IPAddress(0, 0, v1, 0) should be < IPAddress(0, 0, v2, 0)
        IPAddress(0, 0, 0, v1) should be < IPAddress(0, 0, 0, v2)
      } else if (v1 > v2) {
        IPAddress(v1, 0, 0, 0) should be > IPAddress(v2, 0, 0, 0)
        IPAddress(0, v1, 0, 0) should be > IPAddress(0, v2, 0, 0)
        IPAddress(0, 0, v1, 0) should be > IPAddress(0, 0, v2, 0)
        IPAddress(0, 0, 0, v1) should be > IPAddress(0, 0, 0, v2)
      } else {
        IPAddress(v1, 0, 0, 0) should equal(IPAddress(v2, 0, 0, 0))
        IPAddress(0, v1, 0, 0) should equal(IPAddress(0, v2, 0, 0))
        IPAddress(0, 0, v1, 0) should equal(IPAddress(0, 0, v2, 0))
        IPAddress(0, 0, 0, v1) should equal(IPAddress(0, 0, 0, v2))
      }
    }
  }

  test("testApply") {
    intercept[IllegalArgumentException] {
      IPAddress(-1, 2, 3, 4)
    }
    intercept[IllegalArgumentException] {
      IPAddress(256, 2, 3, 4)
    }
  }

  test("testToString") {
    assertResult("25.69.54.194") {
      IPAddress(25, 69, 54, 194).toString
    }
  }

  test("testToLong") {
    assertResult(2130706433L) {
      IPAddress(127, 0, 0, 1).toLong
    }
  }
}

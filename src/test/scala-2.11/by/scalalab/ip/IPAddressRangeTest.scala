package by.scalalab.ip

import org.scalatest.FunSuite

/**
  * Tests of [[by.scalalab.ip.IPAddressRange]].
  */
class IPAddressRangeTest extends FunSuite {

  test("testToString") {
    assertResult("25.69.54.194-25.69.55.215") {
      IPAddressRange(IPAddress(25, 69, 54, 194), IPAddress(25, 69, 55, 215)).toString
    }
  }

  test("testApply") {
    intercept[IllegalArgumentException] {
      IPAddressRange(IPAddress(35, 69, 54, 194), IPAddress(25, 69, 55, 215))
    }
  }
}

package by.scalalab.ip

import org.scalatest.{BeforeAndAfter, FunSuite}

/**
  * Tests of [[by.scalalab.ip.Segment]].
  */
class SegmentTest extends FunSuite with BeforeAndAfter {

  var range1: IPAddressRange = _
  var range2: IPAddressRange = _

  before {
    range1 = IPAddressRange(IPAddress(21, 155, 217, 215), IPAddress(38, 54, 50, 0))
    range2 = IPAddressRange(IPAddress(51, 46, 13, 75), IPAddress(62, 152, 255, 94))
  }

  test("testApply") {
    Segment(range1, "Name")

    intercept[IllegalArgumentException] {
      Segment(range2, "")
    }

    intercept[IllegalArgumentException] {
      Segment(range1, "--")
    }

    intercept[IllegalArgumentException] {
      Segment(range2, " Network 0 ")
    }
  }

  test("testCompareTo") {
    assert(Segment(range1, "Name1") < Segment(range2, "Name1"))
  }

  test("testToString") {
    assertResult("51.46.13.75-62.152.255.94\tName1") {
      Segment(range2, "Name1").toString
    }
  }
}

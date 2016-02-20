package by.scalalab

import org.scalatest.FunSuite

/**
  * Test of [[by.scalalab.GenSegments]].
  */
class GenSegmentsTest extends FunSuite {

  test("testGet") {
    val seq = GenSegments.get(12, 360)

    assert(seq.size == 360)
    assert(seq.map(_.name).distinct.size == 12)
  }

}

package by.scalalab.ip

import by.scalalab.GenSegments
import org.scalatest.FunSuite

/**
  * Tests of [[by.scalalab.ip.SegmentTree]].
  */
class SegmentTreeTest extends FunSuite {

  test("testApply") {
    assert(SegmentTree(Seq()).segments(IPAddress(127, 0, 0, 1)).isEmpty)
    assert(SegmentTree(null).segments(IPAddress(127, 0, 0, 1)).isEmpty)
  }

  test("testSegmentsWithGen") {

    val segments = GenSegments.get(1000, 100000)
    val ip = IPAddress(64, 0, 0, 1)

    val tree = SegmentTree(segments)

    val seqTree = tree.segments(ip).map(_.name).distinct
    val seqPar = segments.par.filter(_.range.contains(ip)).map(_.name).distinct.seq

    assert(seqPar.size == seqTree.size)

    for {
      name <- seqTree
    } assert(seqPar.contains(name))

  }

  test("testSegments") {
    val st = SegmentTree(List(
      Segment(
        IPAddressRange(
          IPAddress(51, 45, 17, 215),
          IPAddress(78, 54, 50, 18)),
        "nw1"),

      Segment(
        IPAddressRange(
          IPAddress(60, 45, 17, 215),
          IPAddress(70, 54, 50, 18)),
        "nw2"),

      Segment(
        IPAddressRange(
          IPAddress(61, 45, 17, 215),
          IPAddress(69, 54, 50, 18)),
        "nw3")
    ))

    val r1 = st.segments(IPAddress(65, 0, 0, 1)).map(_.name)

    assert(r1.contains("nw1"))
    assert(r1.contains("nw2"))
    assert(r1.contains("nw3"))

    val r2 = st.segments(IPAddress(52, 0, 0, 1)).map(_.name)

    assert(r2.contains("nw1"))
    assert(!r2.contains("nw2"))
    assert(!r2.contains("nw3"))

    val r3 = st.segments(IPAddress(70, 54, 50, 0)).map(_.name)

    assert(r3.contains("nw1"))
    assert(r3.contains("nw2"))
    assert(!r3.contains("nw3"))
  }

}

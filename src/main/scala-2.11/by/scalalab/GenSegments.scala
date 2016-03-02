package by.scalalab

import by.scalalab.ip.{IPAddress, IPAddressRange, Segment}

import scala.util.Random

/**
  * A simple generator of [[by.scalalab.ip.Segment]] instances.
  */
object GenSegments {

  /**
    * Produces sequence of random [[by.scalalab.ip.Segment]] objects.
    * @param totalSegments the total number of segments
    * @param countRanges the amount of ranges; any range can overlap with another range
    */
  def get(totalSegments: Int, countRanges: Int): Seq[Segment] = {

    def num = Random.nextInt(256)

    val total = if (totalSegments > 0) totalSegments else 1
    def name = s"Network${Random.nextInt(total)}"

    val segments = Range.inclusive(1, countRanges).par.map { i =>
      val (v1, v2) = IPAddress(num, num, num, num) -> IPAddress(num, num, num, num)
      val (ip1, ip2) = if (v1 < v2) (v1, v2) else (v2, v1)

      Segment(IPAddressRange(ip1, ip2), name)
    }

    segments.seq
  }

}

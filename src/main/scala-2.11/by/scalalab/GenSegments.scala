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
    val random = Random

    def num = random.nextInt(128)
    def num2 = 64 + num

    def name = s"Network${random.nextInt(totalSegments)}"

    val segments = Range.inclusive(0, countRanges - 1).par.map { i =>
      val (v1, v2) = IPAddress(num, num2, num, num2) -> IPAddress(num2, num, num2, num)
      val (ip1, ip2) = if (v1 < v2) (v1, v2) else (v2, v1)

      Segment(IPAddressRange(ip1, ip2), name)
    }

    segments.seq
  }

}

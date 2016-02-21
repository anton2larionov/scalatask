package by.scalalab.ip

/**
  * An interval tree which contains the network segments.
  * @see https://en.wikipedia.org/wiki/Interval_tree
  */
object SegmentTree {

  /**
    * @param segments any sequence of segments
    * @return an interval tree
    */
  def apply(segments: Seq[Segment]): SegmentNode = {
    val inst = for {
      s <- Option(segments)
      if s.nonEmpty
    } yield new SegmentNodeImpl(s)
    inst getOrElse SegmentNodeEmpty
  }
}

/**
  * A node of the interval tree.
  */
trait SegmentNode {

  /**
    * Find all segments overlapping with any IP address.
    *
    * @param ip a IP address
    * @return sequence of segments overlapping with a given ''ip''.
    */
  def segments(ip: IPAddress): Seq[Segment]
}

/**
  * A segment node of tree with elements.
  *
  * @param segments any non empty sequence of segments
  */
private class SegmentNodeImpl(val segments: Seq[Segment]) extends SegmentNode {

  private val mid = median(segments)

  private var (left, center, right) = (Vector[Segment](), Vector[Segment](), Vector[Segment]())

  // dividing all the 'segments' in half at 'mid'
  // 'center' overlapping 'mid'
  for(s <- segments) {
    if (s.range.ip2 < mid) left +:= s
    else if (s.range.ip1 > mid) right +:= s
    else center +:= s
  }

  val leftNode = SegmentTree(left)   // node containing all segments completely to the left of the 'mid'
  val rightNode = SegmentTree(right) // node containing all segments completely to the right of the 'mid'

  private val cL = center.sortBy(_.range.ip1) // segments overlapping the 'mid' sorted by their beginning ip
  private val cR = center.sortBy(_.range.ip2) // segments overlapping the 'mid' sorted by their ending ip

  override def segments(ip: IPAddress): Vector[Segment] = {
    ip match {
      case v if v < mid  => leftNode.segments(ip)  ++: cL.span(_.range.ip1 <= ip)._1
      case v if v > mid  => rightNode.segments(ip) ++: cR.span(_.range.ip2 <  ip)._2
      case v if v == mid => center
    }
  }

  private def median(segments: Seq[Segment]): IPAddress = {
    val ms = segments.sorted.splitAt(segments.size / 2)._2.headOption
    val m = for {s <- ms} yield s.range.ip1
    m getOrElse IPAddress(0, 0, 0, 0)
  }
}

/** A segment node without elements. */
private object SegmentNodeEmpty extends SegmentNode {
  override def segments(ip: IPAddress): Vector[Segment] = Vector.empty
}


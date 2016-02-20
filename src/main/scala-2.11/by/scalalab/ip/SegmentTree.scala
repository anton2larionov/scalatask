package by.scalalab.ip

/**
  * An interval tree which contains the network segments.
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
    * @param ip a IP address
    * @return sequence of segments overlapping with a given ''ip''.
    */
  def segments(ip: IPAddress): Seq[Segment]
}

/**
  * A segment node of tree with elements.
  * @param segments any non empty sequence of segments
  */
private class SegmentNodeImpl(val segments: Seq[Segment]) extends SegmentNode {
  import scala.collection.SortedSet

  private val mid = median(segments)

  private var center = SortedSet[Segment]()
  private var (left, right) = Seq[Segment]() -> Seq[Segment]()

  // dividing all the 'segments' in half at 'mid'
  // 'center' overlapping 'mid'
  for (s <- segments; r = s.range) {
    if (r.ip2 < mid) left +:= s
    else if (r.ip1 > mid) right +:= s
    else center += s
  }

  // recursively create leafs
  lazy val leftNode = SegmentTree(left)
  lazy val rightNode = SegmentTree(right)

  override def segments(ip: IPAddress): Seq[Segment] = {
    // all segments from the beginning of the sorted set to ip
    var res = center.span(_.range.contains(ip))._1.toVector

    if (ip < mid) res ++:= leftNode.segments(ip)
    if (ip > mid) res ++:= rightNode.segments(ip)

    res
  }

  private def median(segments: Seq[Segment]): IPAddress = {
    val r = segments.map(_.range).sorted.splitAt(segments.size / 2)._2.headOption
    val m = for {range <- r} yield range.ip1
    m getOrElse IPAddress(0, 0, 0, 0)
  }
}

/** A segment node without elements. */
private object SegmentNodeEmpty extends SegmentNode {
  override def segments(ip: IPAddress): Seq[Segment] = Seq.empty
}


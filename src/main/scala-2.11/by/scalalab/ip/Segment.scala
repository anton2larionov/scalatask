package by.scalalab.ip

/**
  * A network segment.
  * Comparable by IP-address ranges.
  *
  * @param range a range of IP addresses
  * @param name the segment's name
  *
  * @throws IllegalArgumentException if ''name'' does not match ''[a­zA­Z0­9]+''
  */
case class Segment(range: IPAddressRange,
                   name: String) extends Ordered[Segment] {

  require(name matches """\w+""", "Illegal the network segment's name")

  override def compare(that: Segment): Int = range compare that.range

  /**
    * @example 13.249.61.153-13.249.62.64	Network25
    */
  override def toString = s"$range\t$name"
}


/**
  * A transaction.
  *
  * @param userId a user's id
  * @param ip a IP address of the transaction.
  */
case class Transaction(userId: Long, ip: IPAddress) {

  /**
    * @example 4596635809354383891	177.162.30.243
    */
  override def toString = s"$userId\t$ip"
}
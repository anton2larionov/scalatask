package by.scalalab.ip

/**
  * Presents a IP address (v4).
  *
  * @throws IllegalArgumentException if any param ''v'' less than 0 and greater than 255
  */
case class IPAddress(v1: Int, v2: Int, v3: Int, v4: Int) extends Ordered[IPAddress] {

  require(List(v1, v2, v3, v4).forall((0 to 255).contains),
    s"Malformed IP address: $v1.$v2.$v3.$v4")

  /** Convert IP address to [[Long]] */
  val toLong: Long = List(v4, v3, v2, v1).zipWithIndex.foldLeft(0L)((acc, pair) =>
    acc + pair._1 * Math.pow(256, pair._2).toLong)

  override def compare(that: IPAddress): Int = toLong compare that.toLong

  /**
    * @example 67.5.159.113
    */
  override def toString = s"$v1.$v2.$v3.$v4"
}


/**
  * A range (interval) of IP addresses.
  * The composition of the two instances of [[by.scalalab.ip.IPAddress]].
  *
  * @param ip1 a lower bound of range
  * @param ip2 a upper bound of range
  *
  * @throws IllegalArgumentException if ''ip2'' less than ''ip1''
  */
case class IPAddressRange(ip1: IPAddress,
                          ip2: IPAddress) extends Ordered[IPAddressRange] {

  require(ip1 <= ip2, s"Malformed IP address range: $ip1-$ip2")

  /**
    * @param ip any IP address
    * @return true if this range contains ''ip''
    */
  def contains(ip: IPAddress): Boolean = ip >= this.ip1 && ip <= this.ip2

  override def compare(that: IPAddressRange): Int = (ip1, ip2) compare (that.ip1, that.ip2)

  /**
    * @example 26.65.125.125-26.65.126.47
    */
  override def toString = s"$ip1-$ip2"
}
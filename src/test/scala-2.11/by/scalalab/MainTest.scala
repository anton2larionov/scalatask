package by.scalalab

import by.scalalab.ip._
import org.scalatest.FunSuite

import scala.io.Source
import scala.util.{Failure, Success}

/**
  * Tests of functions from [[by.scalalab.Main]].
  */
class MainTest extends FunSuite {

  test("testMkIPAddress") {
    Main.mkIPAddress("127.0.0.1") match {
      case Success(ip) => assert(ip == IPAddress(127, 0, 0, 1))
      case Failure(e) => fail("Failed.")
    }

    Main.mkIPAddress("1.13.69.315") match {
      case Success(ip) => fail("Failed.")
      case Failure(e) => assert(e.isInstanceOf[IllegalArgumentException])
    }

    Main.mkIPAddress("some_ip") match {
      case Success(ip) => fail("Failed.")
      case Failure(e) => assert(e.isInstanceOf[IllegalArgumentException])
    }
  }

  test("testMkSegments") {
    val strings =
      "41.108.32.68-41.108.32.99\tNetwork39\n" +
      "26.65.125.125-26.65.126.47\tNetwork33"

    val stream = Source.fromString(strings).getLines().toStream

    val segments = Main.mkSegments(stream)

    assert(segments.size == 2)

    val ips = segments.flatMap(s => Vector(s.range.ip1, s.range.ip2))

    assert(ips.contains(IPAddress(26, 65, 125, 125)))

    val names = segments.map(_.name)

    assert(names.contains("Network39"))

    assert(Main.mkSegments(Stream[String]("empty")).isEmpty)
  }

  test("testMkTransactions") {
    val strings = "-6124327207679313045\t61.75.26.68\n" +
      "4596635809354383891\t177.162.30.243"

    val stream = Source.fromString(strings).getLines().toStream

    val transactions = Main.mkTransactions(stream)

    assert(transactions.size == 2)

    val ips = transactions.map(_.userId)

    assert(ips.contains(4596635809354383891L))

    val names = transactions.map(_.ip)

    assert(names.contains(IPAddress(61, 75, 26, 68)))

    assert(Main.mkTransactions(Stream[String]("empty")).isEmpty)
  }

  test("testMkData") {
    val transactions = Seq(
      Transaction(123L, IPAddress(26, 65, 125, 125)),
      Transaction(-987L, IPAddress(67, 4, 205, 1)),
      Transaction(951L, IPAddress(152, 94, 78, 63))
    )

    val segments = Seq(
      Segment(IPAddressRange(IPAddress(0, 0, 0, 0), IPAddress(127, 127, 127, 127)), "Nw0"),
      Segment(IPAddressRange(IPAddress(25, 40, 129, 1), IPAddress(51, 17, 146, 12)), "Nw1"),
      Segment(IPAddressRange(IPAddress(44, 79, 3, 209), IPAddress(79, 143, 177, 4)), "Nw2")
    )

    val data1 = Main.mkData(segments, transactions)

    assert(data1.contains((123L, Seq("Nw0", "Nw1"))))
    assert(data1.contains((951L, Seq("Unknown"))))

    val data2 = Main.mkData(segments, transactions, "NetworkZ")

    assert(data2.contains((951L, Seq("NetworkZ"))))
  }

}

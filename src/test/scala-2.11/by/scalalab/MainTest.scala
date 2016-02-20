package by.scalalab

import by.scalalab.ip.IPAddress
import org.scalatest.FunSuite

import scala.util.{Failure, Success}

/**
  * Test of functions in [[by.scalalab.Main]].
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

}

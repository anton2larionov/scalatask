package by.scalalab.ip

import org.scalatest.FunSuite

/**
  * Test of [[by.scalalab.ip.Transaction]].
  */
class TransactionTest extends FunSuite {

  test("testToString") {
    assertResult("123\t127.0.0.1") {
      Transaction(123L, IPAddress(127, 0, 0, 1)).toString
    }
  }
}

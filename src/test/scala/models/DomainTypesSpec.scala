package models

import org.scalatest.funsuite.AnyFunSuite
import DomainTypes._

class DomainTypesSpec extends AnyFunSuite {

  test("valid ISBN format should pass") {
    val isbn = ISBN("978-1234567890")
    assert(isbn.isValid)
  }

  test("invalid ISBN format should fail") {
    val isbn = ISBN("123-XYZ")
    assert(!isbn.isValid)
  }

  test("UserID value should be preserved") {
    val userId = UserID("U123")
    assert(userId.value == "U123")
  }
}

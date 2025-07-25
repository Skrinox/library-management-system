package models

import org.scalatest.funsuite.AnyFunSuite

class EnumSpec extends AnyFunSuite {

  test("UserType should match expected values") {
    val librarian = UserType.Librarian
    val student = UserType.Student
    assert(librarian.toString == "Librarian")
    assert(student.toString == "Student")
  }

  test("BookStatus enum comparison should work") {
    val status = BookStatus.Available
    assert(status == BookStatus.Available)
    assert(status != BookStatus.CheckedOut)
  }
}

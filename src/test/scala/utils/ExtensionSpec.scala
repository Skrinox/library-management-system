package utils

import org.scalatest.funsuite.AnyFunSuite
import models._
import models.DomainTypes._
import models.BookStatus
import utils.Extensions._

class ExtensionsSpec extends AnyFunSuite {

  val b1 = Book(ISBN("978-1"), "Book A", List("Alice"), 2000, "Fiction", BookStatus.Available)
  val b2 = Book(ISBN("978-2"), "Book B", List("Bob"), 2005, "Science", BookStatus.CheckedOut)
  val b3 = Book(ISBN("978-3"), "Book C", List("Alice"), 2010, "Fiction", BookStatus.Available)

  val books = List(b1, b2, b3)

  test("available books extension filters correctly") {
    val result = books.available.map(_.title)
    assert(result == List("Book A", "Book C"))
  }

  test("filter books by genre using extension") {
    val result = books.byGenre("Fiction").map(_.title)
    assert(result == List("Book A", "Book C"))
  }

  test("isValidISBN extension method works") {
    assert("978-1234567890".isValidISBN)
    assert(!"123".isValidISBN)
  }
}

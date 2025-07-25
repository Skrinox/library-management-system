package utils

import org.scalatest.funsuite.AnyFunSuite
import models._
import models.DomainTypes._
import models.BookStatus
import utils.Show._
import utils.Show.given

class ShowSpec extends AnyFunSuite {

  test("Show[Int] should format correctly") {
    val output = summon[Show[Int]].show(42)
    assert(output == "Int: 42")
  }

  test("Show[Book] should include title and authors") {
    val book = Book(ISBN("978-111"), "Scala Unleashed", List("John", "Jane"), 2021, "Programming", BookStatus.Available)
    val output = summon[Show[Book]].show(book)
    assert(output.contains("Scala Unleashed"))
    assert(output.contains("John, Jane"))
  }
}

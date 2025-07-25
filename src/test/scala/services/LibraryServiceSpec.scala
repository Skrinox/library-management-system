package services

import org.scalatest.funsuite.AnyFunSuite
import models.*
import models.types.*
import services.LibraryService

class LibraryServiceSpec extends AnyFunSuite {

  val b1 = Book(ISBN("978-001"), "Intro to Scala", List("A"), 2020, Genre.Science, Availability.Available)
  val b2 = Book(ISBN("978-002"), "Advanced FP",     List("B"), 2021, Genre.Science, Availability.CheckedOut)
  val catalog = List(b1, b2)

  test("searchBookByISBN returns Some(book) when found") {
    val result = LibraryService.searchBookByISBN(ISBN("978-001"), catalog)
    assert(result.isDefined)
    assert(result.get.title == "Intro to Scala")
  }

  test("searchBookByISBN returns None when not found") {
    val result = LibraryService.searchBookByISBN(ISBN("999-XXX"), catalog)
    assert(result.isEmpty)
  }
}

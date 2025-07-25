package services

import org.scalatest.funsuite.AnyFunSuite
import models._

class ValidationSpec extends AnyFunSuite {

  val validBook = Book(ISBN("978-2253085799"),"Le Père Goriot",List("Honoré de Balzac"),1835,Genre.Classic,Availability.Available)
  val invalidBook = Book(ISBN(""),"No Title",Nil,-1,Genre.Fiction,Availability.CheckedOut)
  val enzo = Student(UserID("enzo01"),"Enzo Greiner","enzo.greiner@efrei.net","Computer Science")
  val incompleteStudent = Student(UserID("bad01"),"","bademail","")

  val librarian = Librarian(UserID("lib1"), "Marie")
  val namelessLibrarian = Librarian(UserID("lib2"), "")

  test("validates a correct book") {
    val result = Validation.validateBook(validBook)
    assert(result.isRight)
  }

  test("rejects an invalid book") {
    val result = Validation.validateBook(invalidBook)
    assert(result.isLeft)
  }

  test("validates Enzo as a student") {
    val result = Validation.validateUser(enzo)
    assert(result.isRight)
  }

  test("rejects an incomplete student") {
    val result = Validation.validateUser(incompleteStudent)
    assert(result.isLeft)
  }

  test("rejects librarian with empty name") {
    val result = Validation.validateUser(namelessLibrarian)
    assert(result == Left("Librarian name cannot be empty"))
  }

  test("validates a correct transaction with Enzo and valid book") {
    val result = Validation.validateTransaction(validBook, enzo)
    assert(result.isRight)
  }

  test("rejects a transaction when book is unavailable") {
    val unavailableBook = validBook.copy(availability = Availability.CheckedOut)
    val result = Validation.validateTransaction(unavailableBook, enzo)
    assert(result == Left("The book is not available"))
  }
}

package services

import org.scalatest.funsuite.AnyFunSuite
import models._
import models.types._

class ErrorHandlingSpec extends AnyFunSuite {

  val validBook = Book(
    ISBN("978-0-123456-78-9"),
    "Test Book",
    List("Test Author"),
    2020,
    Genre.Fiction,
    Availability.Available
  )

  val checkedOutBook = Book(
    ISBN("978-0-987654-32-1"),
    "Checked Out Book",
    List("Another Author"),
    2019,
    Genre.Fiction,
    Availability.CheckedOut
  )

  val validStudent = Student(
    UserID("student-001"),
    "John Doe",
    "john@example.com",
    "Computer Science"
  )

  val catalog = LibraryCatalog(List(validBook, checkedOutBook), List(validStudent), Nil)

  test("validateISBNDetailed with valid ISBN") {
    val result = ErrorHandling.validateISBNDetailed("978-0-123456-78-9")
    assert(result.isRight)
    assert(result.getOrElse(ISBN("")).value == "978-0-123456-78-9")
  }

  test("validateISBNDetailed with invalid ISBN") {
    val result = ErrorHandling.validateISBNDetailed("invalid-isbn")
    assert(result.isLeft)
    result.left.foreach { error =>
      assert(error.isInstanceOf[services.ValidationError])
      assert(error.message.contains("Invalid ISBN format"))
    }
  }

  test("validateEmail with valid email") {
    val result = ErrorHandling.validateEmail("test@example.com")
    assert(result.isRight)
  }

  test("validateEmail with invalid email") {
    val result = ErrorHandling.validateEmail("invalid-email")
    assert(result.isLeft)
    result.left.foreach { error =>
      assert(error.isInstanceOf[services.ValidationError])
      assert(error.message.contains("Email must contain @ symbol"))
    }
  }

  test("validatePublicationYear with future year") {
    val futureYear = java.time.Year.now().getValue + 10
    val result = ErrorHandling.validatePublicationYear(futureYear)
    assert(result.isLeft)
    result.left.foreach { error =>
      assert(error.isInstanceOf[services.ValidationError])
      assert(error.message.contains("cannot be in the future"))
    }
  }

  test("findBookByISBN with existing book") {
    val result = ErrorHandling.findBookByISBN(catalog.books, validBook.isbn)
    assert(result.isRight)
  }

  test("findBookByISBN with non-existing book") {
    val result = ErrorHandling.findBookByISBN(catalog.books, ISBN("999-9-999999-99-9"))
    assert(result.isLeft)
    result.left.foreach { error =>
      assert(error.isInstanceOf[services.NotFoundError])
    }
  }

  test("checkAvailability with available book") {
    val result = ErrorHandling.checkAvailability(validBook)
    assert(result.isRight)
  }

  test("checkAvailability with unavailable book") {
    val result = ErrorHandling.checkAvailability(checkedOutBook)
    assert(result.isLeft)
    result.left.foreach { error =>
      assert(error.isInstanceOf[services.UnavailableError])
    }
  }

  test("validateBookData with valid data") {
    val result = ErrorHandling.validateBookData(
      "978-0-123456-78-9",
      "Test Book",
      List("Author"),
      2020,
      Genre.Fiction,
      Availability.Available
    )
    assert(result.isRight)
  }

  test("validateBookData with invalid data") {
    val result = ErrorHandling.validateBookData(
      "invalid-isbn",
      "",
      Nil,
      -1,
      Genre.Fiction,
      Availability.Available
    )
    assert(result.isLeft)
  }

  test("validateTransaction with valid data") {
    val result = ErrorHandling.validateTransaction(catalog, validBook.isbn, validStudent.id)
    assert(result.isRight)
  }

  test("validateTransaction with unavailable book") {
    val result = ErrorHandling.validateTransaction(catalog, checkedOutBook.isbn, validStudent.id)
    println(s"ErrorHandling test result: $result")
    result match {
      case Left(error) => 
        println(s"Error: $error")
        assert(error.isInstanceOf[services.UnavailableError])
      case Right(_) => 
        println("Transaction was unexpectedly successful")
        assert(false, "Expected transaction to fail for checked out book")
    }
  }

  test("collectErrors with mixed results") {
    val results = List(
      Right("success1"),
      Left(services.ValidationError("error1")),
      Right("success2"),
      Left(services.NotFoundError("error2"))
    )
    val collected = ErrorHandling.collectErrors(results)
    assert(collected.isLeft)
    collected.left.foreach { errors =>
      assert(errors.length == 2)
    }
  }
}
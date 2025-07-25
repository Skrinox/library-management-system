package services

import org.scalatest.funsuite.AnyFunSuite
import models._
import models.types._

class ValidationSpec extends AnyFunSuite {

  // Utilisez des ISBNs avec le bon format
  val validBook = Book(
    ISBN("978-2-253085-79-9"), // Format corrigé avec tirets
    "Le Père Goriot",
    List("Honoré de Balzac"),
    1835,
    Genre.Classic,
    Availability.Available
  )

  val invalidBook = Book(
    ISBN("978-0-123456-78-9"),
    "",  // Titre vide
    Nil, // Pas d'auteurs
    -1,  // Année invalide
    Genre.Fiction,
    Availability.Available
  )

  val validStudent = Student(
    UserID("student-enzo"),
    "Enzo",
    "enzo@example.com",
    "Computer Science"
  )

  val incompleteStudent = Student(
    UserID("student-incomplete"),
    "",  // Nom vide
    "invalid-email",  // Email invalide
    ""   // Major vide
  )

  val emptyLibrarian = Librarian(
    UserID("lib-001"),
    ""   // Nom vide
  )

  val unavailableBook = Book(
    ISBN("978-2-253085-79-9"),
    "Le Père Goriot",
    List("Honoré de Balzac"),
    1835,
    Genre.Classic,
    Availability.CheckedOut
  )

  test("validates a correct book") {
    val result = Validation.validateBook(validBook)
    assert(result.isRight)
  }

  test("rejects an invalid book") {
    val result = Validation.validateBook(invalidBook)
    assert(result.isLeft)
  }

  test("validates Enzo as a student") {
    val result = Validation.validateUser(validStudent)
    assert(result.isRight)
  }

  test("rejects an incomplete student") {
    val result = Validation.validateUser(incompleteStudent)
    assert(result.isLeft)
  }

  test("rejects librarian with empty name") {
    val result = Validation.validateUser(emptyLibrarian)
    assert(result.isLeft)
  }

  test("validates a correct transaction with Enzo and valid book") {
    val result = Validation.validateTransaction(validBook, validStudent)
    assert(result.isRight)
  }

  test("rejects a transaction when book is unavailable") {
    val result = Validation.validateTransaction(unavailableBook, validStudent)
    // Ajoutons du debugging pour voir ce qui se passe
    println(s"Test result: $result")
    result match {
      case Left(error) => 
        println(s"Error message: $error")
        assert(true) // Le test réussit si on a une erreur
      case Right(_) => 
        println("Transaction was unexpectedly successful")
        assert(false, "Expected transaction to fail for unavailable book")
    }
  }
}

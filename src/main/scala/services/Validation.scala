package services

import models._
import models.types._

/**
 * Contains high-level validation utilities for books, users and transactions
 */
object Validation:

  /**
   * Validates the fields of a Book using the functional validation logic in ErrorHandling
   *
   * @param book The book to validate
   * @return Either a String error message or the original Book if valid
   */
  def validateBook(book: Book): Either[String, Book] =
    ErrorHandling.validateBookData(
      book.isbn.value,
      book.title,
      book.authors,
      book.publicationYear,
      book.genre,
      book.availability
    ).left.map(_.message)

  /**
   * Validates a User based on their specific subtype (Student, Faculty, Librarian)
   *
   * @param user The user to validate
   * @return Either a String error message or the original User if valid
   */
  def validateUser(user: User): Either[String, User] = user match
    case Student(id, name, email, major) =>
      ErrorHandling.validateStudentData(id.value, name, email, major)
        .left.map(_.message)
        .map(_ => user)

    case Faculty(id, name, department) =>
      if name.trim.isEmpty then Left("Faculty name cannot be empty")
      else if department.trim.isEmpty then Left("Department must be specified")
      else Right(user)

    case Librarian(id, name) =>
      if name.trim.isEmpty then Left("Librarian name cannot be empty")
      else Right(user)

  /**
   * Validates a book-user pair before processing a transaction
   * Ensures book is valid, user is valid, and book is available
   *
   * @param book The book to check
   * @param user The user attempting the transaction
   * @return Either a String error message or a tuple (Book, User) if valid
   */
  def validateTransaction(book: Book, user: User): Either[String, (Book, User)] =
    for
      b <- validateBook(book)
      u <- validateUser(user)
      availableBook <- ErrorHandling.checkAvailability(b).left.map(_.message)
    yield (availableBook, u)
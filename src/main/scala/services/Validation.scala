package services

import models._
import models.types._

object Validation:

  def validateBook(book: Book): Either[String, Book] =
    ErrorHandling.validateBookData(
      book.isbn.value,
      book.title,
      book.authors,
      book.publicationYear
    ).left.map(_.message)

  def validateUser(user: User): Either[String, User] = user match
    case Student(id, name, email, major) =>
      ErrorHandling.validateStudentData(id.value, name, email, major)
        .left.map(_.message)
        .map(_ => user) // Retourner l'utilisateur original si validation OK
        
    case Faculty(id, name, department) =>
      if name.trim.isEmpty then Left("Faculty name cannot be empty")
      else if department.trim.isEmpty then Left("Department must be specified")
      else Right(user)

    case Librarian(id, name) =>
      if name.trim.isEmpty then Left("Librarian name cannot be empty")
      else Right(user)

  def validateTransaction(book: Book, user: User): Either[String, (Book, User)] =
    for
      b <- validateBook(book)
      u <- validateUser(user)
      availableBook <- if b.isAvailable then Right(b) 
                      else Left(s"Book '${b.title}' is currently ${b.availability} and cannot be borrowed")
    yield (availableBook, u)

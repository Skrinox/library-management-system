package services

import models._

object Validation:

  def validateBook(book: Book): Either[String, Book] =
    if book.isbn.value.trim.isEmpty then Left("ISBN cannot be empty")
    else if book.authors.isEmpty then Left("Book must have at least one author")
    else if book.publicationYear <= 0 then Left("Invalid publication year")
    else Right(book)

  def validateUser(user: User): Either[String, User] = user match
    case Student(id, name, email, major) =>
      if name.trim.isEmpty then Left("Student name cannot be empty")
      else if email.trim.isEmpty || !email.contains("@") then Left("Invalid student email")
      else if major.trim.isEmpty then Left("Major must be specified")
      else Right(user)

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
      _ <- if b.isAvailable then Right(()) else Left("The book is not available")
    yield (b, u)

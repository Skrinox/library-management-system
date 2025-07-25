package services

import models._
import models.types._
import scala.util.{Try, Success, Failure}

// Types d'erreurs spécifiques
sealed trait LibraryError:
  def message: String

case class ValidationError(message: String) extends LibraryError
case class NotFoundError(message: String) extends LibraryError
case class DuplicateError(message: String) extends LibraryError
case class UnavailableError(message: String) extends LibraryError
case class IOError(message: String, cause: Option[Throwable] = None) extends LibraryError
case class ParseError(message: String, input: String) extends LibraryError

// Type alias pour les résultats d'opérations
type LibraryResult[T] = Either[LibraryError, T]

object ErrorHandling:
  // import models.Extensions._
  
  // Conversion de Try vers Either
  def tryToEither[T](operation: => T): LibraryResult[T] =
    Try(operation) match
      case Success(value) => Right(value)
      case Failure(exception) => Left(IOError(s"Operation failed: ${exception.getMessage}", Some(exception)))
  
  // Validation sécurisée avec Option
  def validateISBN(isbn: String): Option[ISBN] =
    Option.when(isbn.matches("""^\d{3}-\d{1,5}-\d{1,7}-\d{1,7}-\d{1}$"""))(ISBN(isbn))
  
  // Validation avec Either pour plus de détails
  def validateISBNDetailed(isbn: String): LibraryResult[ISBN] =
    if isbn.trim.isEmpty then Left(ValidationError("ISBN cannot be empty"))
    else if !isbn.matches("""^\d{3}-\d{1,5}-\d{1,7}-\d{1,7}-\d{1}$""") then
      Left(ValidationError(s"Invalid ISBN format: $isbn. Expected format: XXX-XXXXX-XXXXXXX-XXXXXXX-X"))
    else Right(ISBN(isbn))
  
  // Validation d'email avec détails
  def validateEmail(email: String): LibraryResult[String] =
    if email.trim.isEmpty then Left(ValidationError("Email cannot be empty"))
    else if !email.contains("@") then Left(ValidationError("Email must contain @ symbol"))
    else if !email.matches("""^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$""") then
      Left(ValidationError(s"Invalid email format: $email"))
    else Right(email)
  
  // Validation d'année de publication
  def validatePublicationYear(year: Int): LibraryResult[Int] =
    val currentYear = java.time.Year.now().getValue
    if year <= 0 then Left(ValidationError("Publication year must be positive"))
    else if year > currentYear then Left(ValidationError(s"Publication year cannot be in the future (current year: $currentYear)"))
    else Right(year)
  
  // Recherche sécurisée dans une liste
  def findBookByISBN(books: List[Book], isbn: ISBN): LibraryResult[Book] =
    books.find(_.isbn == isbn) match
      case Some(book) => Right(book)
      case None => Left(NotFoundError(s"Book with ISBN ${isbn.value} not found"))
  
  def findUserById(users: List[User], userId: UserID): LibraryResult[User] =
    users.find(_.id == userId) match
      case Some(user) => Right(user)
      case None => Left(NotFoundError(s"User with ID ${userId.value} not found"))
  
  // Vérification de disponibilité
  def checkAvailability(book: Book): LibraryResult[Book] =
    if book.isAvailable then Right(book)
    else Left(UnavailableError(s"Book '${book.title}' is currently ${book.availability}"))
  
  // Combinaison de validations
  def validateBookData(isbn: String, title: String, authors: List[String], year: Int): LibraryResult[Book] =
    for
      validISBN <- validateISBNDetailed(isbn)
      validYear <- validatePublicationYear(year)
      validTitle <- if title.trim.nonEmpty then Right(title) else Left(ValidationError("Title cannot be empty"))
      validAuthors <- if authors.nonEmpty then Right(authors) else Left(ValidationError("Book must have at least one author"))
    yield Book(validISBN, validTitle, validAuthors, validYear, Genre.Fiction, Availability.Available)
  
  def validateStudentData(id: String, name: String, email: String, major: String): LibraryResult[Student] =
    for
      validEmail <- validateEmail(email)
      validName <- if name.trim.nonEmpty then Right(name) else Left(ValidationError("Name cannot be empty"))
      validMajor <- if major.trim.nonEmpty then Right(major) else Left(ValidationError("Major cannot be empty"))
    yield Student(UserID(id), validName, validEmail, validMajor)
  
  // Gestion des erreurs avec récupération
  def recoverFromError[T](result: LibraryResult[T], default: T): T =
    result.getOrElse(default)
  
  def recoverWithMessage[T](result: LibraryResult[T], onError: LibraryError => T): T =
    result.fold(onError, identity)
  
  def collectErrors[T](results: List[LibraryResult[T]]): Either[List[LibraryError], List[T]] =
    val errors = results.collect { case Left(error) => error }
    val successes = results.collect { case Right(success) => success }
    
    if errors.nonEmpty then Left(errors)
    else Right(successes)
  
  // Validation de transaction complète
  def validateTransaction(catalog: LibraryCatalog, bookISBN: ISBN, userId: UserID): LibraryResult[(Book, User)] =
    for
      book <- findBookByISBN(catalog.books, bookISBN)
      availableBook <- checkAvailability(book)
      user <- findUserById(catalog.users, userId)
      _ <- user match
        case _: Student | _: Faculty => Right(())
        case _: Librarian => Left(UnavailableError(s"Librarian ${user.name} cannot borrow books"))
    yield (availableBook, user)
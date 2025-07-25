package services

import models._
import models.types._
import scala.util.{Try, Success, Failure}

/**
 * Represents all error types for the library system.
 */
sealed trait LibraryError:
  def message: String

/** Error related to invalid input or validation failure. */
case class ValidationError(message: String) extends LibraryError

/** Error when an entity is not found. */
case class NotFoundError(message: String) extends LibraryError

/** Error when a duplicate entry is detected. */
case class DuplicateError(message: String) extends LibraryError

/** Error when an entity (e.g., book) is not available. */
case class UnavailableError(message: String) extends LibraryError

/** Error related to I/O operations, with optional cause. */
case class IOError(message: String, cause: Option[Throwable] = None) extends LibraryError

/** Error during parsing with context. */
case class ParseError(message: String, input: String) extends LibraryError

/** Type alias representing a result that may contain a LibraryError. */
type LibraryResult[T] = Either[LibraryError, T]

/**
 * Provides safe error-handling and validation utilities for the library system.
 */
object ErrorHandling:

  /**
   * Converts a `Try` into a `LibraryResult`, wrapping any failure as an IOError.
   */
  def tryToEither[T](operation: => T): LibraryResult[T] =
    Try(operation) match
      case Success(value) => Right(value)
      case Failure(exception) => Left(IOError(s"Operation failed: ${exception.getMessage}", Some(exception)))

  /**
   * Validates an ISBN using a regex and wraps it in an `Option`.
   */
  def validateISBN(isbn: String): Option[ISBN] =
    Option.when(isbn.matches("""^\d{3}-\d{1,5}-\d{1,7}-\d{1,7}-\d{1}$"""))(ISBN(isbn))

  /**
   * Validates an ISBN and returns a detailed error if invalid.
   */
  def validateISBNDetailed(isbn: String): LibraryResult[ISBN] =
    if isbn.trim.isEmpty then Left(ValidationError("ISBN cannot be empty"))
    else if !isbn.matches("""^\d{3}-\d{1,5}-\d{1,7}-\d{1,7}-\d{1}$""") then
      Left(ValidationError(s"Invalid ISBN format: $isbn. Expected format: XXX-XXXXX-XXXXXXX-XXXXXXX-X"))
    else Right(ISBN(isbn))

  /**
   * Validates the format of an email address.
   */
  def validateEmail(email: String): LibraryResult[String] =
    if email.trim.isEmpty then Left(ValidationError("Email cannot be empty"))
    else if !email.contains("@") then Left(ValidationError("Email must contain @ symbol"))
    else if !email.matches("""^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$""") then
      Left(ValidationError(s"Invalid email format: $email"))
    else Right(email)

  /**
   * Validates that a publication year is positive and not in the future.
   */
  def validatePublicationYear(year: Int): LibraryResult[Int] =
    val currentYear = java.time.Year.now().getValue
    if year <= 0 then Left(ValidationError("Publication year must be positive"))
    else if year > currentYear then Left(ValidationError(s"Publication year cannot be in the future (current year: $currentYear)"))
    else Right(year)

  /**
   * Finds a book by ISBN in the provided book list.
   */
  def findBookByISBN(books: List[Book], isbn: ISBN): LibraryResult[Book] =
    books.find(_.isbn == isbn) match
      case Some(book) => Right(book)
      case None => Left(NotFoundError(s"Book with ISBN ${isbn.value} not found"))

  /**
   * Finds a user by ID in the provided user list.
   */
  def findUserById(users: List[User], userId: UserID): LibraryResult[User] =
    users.find(_.id == userId) match
      case Some(user) => Right(user)
      case None => Left(NotFoundError(s"User with ID ${userId.value} not found"))

  /**
   * Checks if a book is available. Returns an error if it is not.
   */
  def checkAvailability(book: Book): LibraryResult[Book] =
    if book.isAvailable then Right(book)
    else Left(UnavailableError(s"Book '${book.title}' is currently ${book.availability}"))

  /**
   * Validates book fields and constructs a valid `Book` if all fields pass.
   *
   * @return A validated `Book` wrapped in a `LibraryResult`
   */
  def validateBookData(
    isbn: String,
    title: String,
    authors: List[String],
    year: Int,
    genre: Genre,
    availability: Availability
  ): LibraryResult[Book] =
    for
      validISBN <- validateISBNDetailed(isbn)
      validYear <- validatePublicationYear(year)
      validTitle <- if title.trim.nonEmpty then Right(title) else Left(ValidationError("Title cannot be empty"))
      validAuthors <- if authors.nonEmpty then Right(authors) else Left(ValidationError("Book must have at least one author"))
    yield Book(validISBN, validTitle, validAuthors, validYear, genre, availability)

  /**
   * Validates student input data and constructs a valid `Student` object.
   */
  def validateStudentData(id: String, name: String, email: String, major: String): LibraryResult[Student] =
    for
      validEmail <- validateEmail(email)
      validName <- if name.trim.nonEmpty then Right(name) else Left(ValidationError("Name cannot be empty"))
      validMajor <- if major.trim.nonEmpty then Right(major) else Left(ValidationError("Major cannot be empty"))
    yield Student(UserID(id), validName, validEmail, validMajor)

  /**
   * Fallback strategy: return a default value in case of error.
   */
  def recoverFromError[T](result: LibraryResult[T], default: T): T =
    result.getOrElse(default)

  /**
   * Handles error by applying a fallback function to the error value.
   */
  def recoverWithMessage[T](result: LibraryResult[T], onError: LibraryError => T): T =
    result.fold(onError, identity)

  /**
   * Aggregates a list of `LibraryResult`s and collects all errors if any.
   */
  def collectErrors[T](results: List[LibraryResult[T]]): Either[List[LibraryError], List[T]] =
    val errors = results.collect { case Left(error) => error }
    val successes = results.collect { case Right(success) => success }

    if errors.nonEmpty then Left(errors)
    else Right(successes)

  /**
   * Validates a transaction by checking book and user validity and availability.
   *
   * @param catalog The library catalog
   * @param bookISBN The ISBN of the book to borrow
   * @param userId The ID of the user requesting the book
   * @return A tuple of (Book, User) if validation succeeds
   */
  def validateTransaction(catalog: LibraryCatalog, bookISBN: ISBN, userId: UserID): LibraryResult[(Book, User)] =
    for
      book <- findBookByISBN(catalog.books, bookISBN)
      availableBook <- checkAvailability(book)
      user <- findUserById(catalog.users, userId)
      _ <- user match
        case _: Student | _: Faculty => Right(())
        case _: Librarian => Left(UnavailableError(s"Librarian ${user.name} cannot borrow books"))
    yield (availableBook, user)
package models

import models.types._
import java.time.LocalDateTime

/** Extension methods for working with [[Book]] instances. */
extension (book: Book)

  /** Returns true if the book was published before 1950. */
  def isClassic: Boolean = book.publicationYear < 1950

  /** Returns the number of years since the book was published. */
  def ageInYears: Int = java.time.Year.now().getValue - book.publicationYear

  /** Returns the authors of the book as a comma-separated string. */
  def formattedAuthors: String = book.authors.mkString(", ")

  /** Returns a copy of the book marked as checked out. */
  def checkout: Book = book.copy(availability = Availability.CheckedOut)

  /** Returns a copy of the book marked as available. */
  def makeAvailable: Book = book.copy(availability = Availability.Available)

/** Extension methods for working with [[User]] instances. */
extension (user: User)

  /** Indicates whether the user is allowed to borrow books. */
  def canBorrow: Boolean = user match
    case _: Student => true
    case _: Faculty => true
    case _: Librarian => false

  /** Returns the maximum number of books this user can borrow. */
  def maxBorrowLimit: Int = user match
    case _: Student => 5
    case _: Faculty => 10
    case _: Librarian => 0

  /** Returns a readable role name for the user. */
  def displayRole: String = user match
    case _: Student => "Student"
    case _: Faculty => "Faculty"
    case _: Librarian => "Librarian"

/** Extension methods for querying the [[LibraryCatalog]]. */
extension (catalog: LibraryCatalog)

  /** Returns all books currently available for borrowing. */
  def availableBooks: List[Book] =
    catalog.books.filter(_.availability == Availability.Available)

  /** Returns all books that are currently checked out. */
  def checkedOutBooks: List[Book] =
    catalog.books.filter(_.availability == Availability.CheckedOut)

  /** Finds a book in the catalog by its ISBN. */
  def findBookByISBN(isbn: ISBN): Option[Book] =
    catalog.books.find(_.isbn == isbn)

  /** Finds a user in the catalog by their user ID. */
  def findUserById(userId: UserID): Option[User] =
    catalog.users.find(_.id == userId)

  /** Returns the number of books in the catalog. */
  def booksCount: Int = catalog.books.length

  /** Returns the number of users registered in the catalog. */
  def usersCount: Int = catalog.users.length

/** Extension methods for converting a [[String]] to domain-specific types. */
extension (s: String)

  /** Attempts to convert this string into an ISBN. Returns None if the format is invalid. */
  def toISBN: Option[ISBN] =
    if s.matches("""^\d{3}-\d{1,5}-\d{1,7}-\d{1,7}-\d{1}$""") then Some(ISBN(s))
    else None

  /** Converts this string into a user ID. Format is not validated. */
  def toUserID: UserID = UserID(s)

/** Extension methods for filtering and analyzing lists of books. */
extension (books: List[Book])

  /** Returns books of the given genre. */
  def byGenre(genre: Genre): List[Book] =
    books.filter(_.genre == genre)

  /** Returns only books that are currently available. */
  def availableOnly: List[Book] =
    books.filter(_.isAvailable)

  /** Returns books written by the given author (case-insensitive partial match). */
  def byAuthor(author: String): List[Book] =
    books.filter(_.authors.exists(_.toLowerCase.contains(author.toLowerCase)))

  /** Returns books published after the specified year. */
  def publishedAfter(year: Int): List[Book] =
    books.filter(_.publicationYear > year)

  /** Returns books that are considered classics (published before 1950). */
  def classics: List[Book] =
    books.filter(_.isClassic)
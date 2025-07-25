package models

import models.types._
import java.time.LocalDateTime

// Extension methods pour Book
extension (book: Book)
  def isClassic: Boolean = book.publicationYear < 1950
  
  def ageInYears: Int = 
    java.time.Year.now().getValue - book.publicationYear
  
  def formattedAuthors: String = 
    book.authors.mkString(", ")
  
  def checkout: Book = 
    book.copy(availability = Availability.CheckedOut)
  
  def makeAvailable: Book = 
    book.copy(availability = Availability.Available)

// Extension methods pour User
extension (user: User)
  def canBorrow: Boolean = user match
    case _: Student => true
    case _: Faculty => true
    case _: Librarian => false
  
  def maxBorrowLimit: Int = user match
    case _: Student => 5
    case _: Faculty => 10
    case _: Librarian => 0
  
  def displayRole: String = user match
    case _: Student => "Student"
    case _: Faculty => "Faculty"
    case _: Librarian => "Librarian"

// Extension methods pour LibraryCatalog
extension (catalog: LibraryCatalog)
  def availableBooks: List[Book] = 
    catalog.books.filter(_.availability == Availability.Available)
  
  def checkedOutBooks: List[Book] = 
    catalog.books.filter(_.availability == Availability.CheckedOut)
  
  def findBookByISBN(isbn: ISBN): Option[Book] = 
    catalog.books.find(_.isbn == isbn)
  
  def findUserById(userId: UserID): Option[User] = 
    catalog.users.find(_.id == userId)
  
  def booksCount: Int = catalog.books.length
  
  def usersCount: Int = catalog.users.length

// Extension methods pour String (ISBN validation)
extension (s: String)
  def toISBN: Option[ISBN] = 
    if s.matches("""^\d{3}-\d{1,5}-\d{1,7}-\d{1,7}-\d{1}$""") then
      Some(ISBN(s))
    else 
      None
  
  def toUserID: UserID = UserID(s)

// Extension methods pour List[Book]
extension (books: List[Book])
  def byGenre(genre: Genre): List[Book] = 
    books.filter(_.genre == genre)
  
  def availableOnly: List[Book] = 
    books.filter(_.isAvailable)
  
  def byAuthor(author: String): List[Book] = 
    books.filter(_.authors.exists(_.toLowerCase.contains(author.toLowerCase)))
  
  def publishedAfter(year: Int): List[Book] = 
    books.filter(_.publicationYear > year)
  
  def classics: List[Book] = 
    books.filter(_.isClassic)
package services

import org.scalatest.funsuite.AnyFunSuite
import models._
import models.types._

class Scala3FeaturesSpec extends AnyFunSuite {

  val testBook: Book = Book(
    ISBN("978-0-123456-78-9"),
    "Test Book",
    List("Test Author"),
    2020,
    Genre.Fiction,
    Availability.Available
  )

  val testUser: Student = Student(
    UserID("student-001"),
    "John Doe",
    "john@example.com",
    "Computer Science"
  )

  test("Extension methods for Book") {
    assert(!testBook.isClassic) // Published in 2020, not classic
    assert(testBook.ageInYears >= 4) // At least 4 years old
    assert(testBook.formattedAuthors == "Test Author")
    
    val checkedOut = testBook.checkout
    assert(checkedOut.availability == Availability.CheckedOut)
  }

  test("Extension methods for User") {
    assert(testUser.canBorrow)
    assert(testUser.maxBorrowLimit == 5)
    assert(testUser.displayRole == "Student")
  }

  test("Union types in SearchQuery") {
    val catalog = LibraryCatalog(List(testBook), List(testUser), Nil)
    
    val titleSearch = SearchQuery.search("Test", catalog)
    assert(titleSearch.nonEmpty)
    
    val genreSearch = SearchQuery.search(Genre.Fiction, catalog)
    assert(genreSearch.nonEmpty)
  }

  test("Type classes with given/using") {
    val displayedBook = DisplayService.show(testBook)
    assert(displayedBook.contains("Test Book"))
    
    val serializedUser = SerializationService.save(testUser)
    assert(serializedUser.startsWith("STUDENT"))
  }

  test("Enhanced enums") {
    assert(Genre.Fiction.description == "Fiction literature")
    assert(Availability.Available.canBorrow)
    assert(Permission.Admin.level == 4)
    assert(ReportType.Monthly.daysIncluded == 30)
  }

  test("String extensions for ISBN") {
    val validISBN = "978-0-123456-78-9".toISBN
    assert(validISBN.isDefined)
    
    val invalidISBN = "invalid-isbn".toISBN
    assert(invalidISBN.isEmpty)
  }
}
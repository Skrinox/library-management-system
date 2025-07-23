package models

import org.scalatest.flatspec.AnyFlatSpec
import java.time.LocalDateTime
import models.types.*

class LibraryCatalogSpec extends AnyFlatSpec {

  val testBook: Book = Book(
    ISBN("999-666-777-8"),
    "1984",
    List("George Orwell"),
    1949,
    Genre.Fiction,
    Availability.Available
  )
  val testUser: Student = Student(
    UserID("student-001"),
    "Alice",
    "alice@gmail.com",
    "Data & AI")
  val transaction: Transaction = Transaction(
    testBook,
    testUser,
    LocalDateTime.now(),
    TransactionType.Borrow
  )

  "LibraryCatalog" should "add a book" in {
    val catalog = LibraryCatalog(Nil, Nil, Nil)
    val updated = catalog.addBook(testBook)
    assert(updated.books.contains(testBook))
  }

  it should "add a user" in {
    val catalog = LibraryCatalog(Nil, Nil, Nil)
    val updated = catalog.addUser(testUser)
    assert(updated.users.contains(testUser))
  }

  it should "add a transaction" in {
    val catalog = LibraryCatalog(Nil, Nil, Nil)
    val updated = catalog.addTransaction(transaction)
    assert(updated.transactions.contains(transaction))
  }

  it should "remove a book" in {
    val catalog = LibraryCatalog(List(testBook), Nil, Nil)
    val updated = catalog.removeBook(testBook.isbn)
    assert(!updated.books.contains(testBook))
  }

  it should "remove a user" in {
    val catalog = LibraryCatalog(Nil, List(testUser), Nil)
    val updated = catalog.removeUser(testUser.id)
    assert(!updated.users.contains(testUser))
  }

  it should "remove a transaction" in {
    val catalog = LibraryCatalog(Nil, Nil, List(transaction))
    val updated = catalog.removeTransaction(transaction)
    assert(!updated.transactions.contains(transaction))
  }
}

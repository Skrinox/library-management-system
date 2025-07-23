package models

import models.types.*
import java.time.LocalDateTime
import org.scalatest.flatspec.AnyFlatSpec

class TransactionSpec extends AnyFlatSpec {

  "A Transaction" should "be created with valid data" in {
    val isbn = ISBN("999-666-777-8")
    val book = Book(
      isbn,
      "1984",
      List("George Orwell"),
      1949,
      Genre.Fiction,
      Availability.Available
    )
    val userId = UserID("student-001")
    val user = Student(userId, "Alice", "alice@gmail.com", "Data & AI")

    val timestamp = LocalDateTime.now()
    val transaction = Transaction(book, user, timestamp, TransactionType.Borrow)

    assert(transaction.book.isbn.value == "999-666-777-8")
    assert(transaction.user.id.value == "student-001")
    assert(transaction.transactionType == TransactionType.Borrow)
    assert(transaction.timestamp == timestamp)
  }
}

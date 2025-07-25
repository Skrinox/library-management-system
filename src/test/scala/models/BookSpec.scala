package models

import models.types.*
import org.scalatest.funsuite.AnyFunSuite

class BookSpec extends AnyFunSuite:
  test("Book creation with valid data"){
    val isbn = ISBN("999-666-777-8")
    val book = Book(
      isbn,
      "1984",
      List("George Orwell"),
      1949,
      Genre.Fiction,
      Availability.Available
    )
    assert(book.isbn.value == "999-666-777-8")
    assert(book.title == "1984")
    assert(book.authors == List("George Orwell"))
    assert(book.publicationYear == 1949)
    assert(book.genre == Genre.Fiction)
    assert(book.availability == Availability.Available)
    
  }

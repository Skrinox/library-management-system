package services

import org.scalatest.funsuite.AnyFunSuite
import models._
import models.types._

class StatisticsSpec extends AnyFunSuite {

  val books: List[Book] = List(
    Book(ISBN("978-2253085799"), "Le Père Goriot", List("Honoré de Balzac"), 1835, Genre.Classic, Availability.Available),
    Book(ISBN("978-2070394388"), "La Peste", List("Albert Camus"), 1947, Genre.Fiction, Availability.CheckedOut),
    Book(ISBN("978-2070360024"), "L'Étranger", List("Albert Camus"), 1942, Genre.Fiction, Availability.Available),
    Book(ISBN("979-1035826857"), "Les Misérables", List("Victor Hugo"), 1862, Genre.Classic, Availability.CheckedOut)
  )

  test("total number of books") {
    assert(Statistics.totalBooks(books) == 4)
  }

  test("count available books") {
    assert(Statistics.countAvailableBooks(books) == 2)
  }

  test("list of all titles") {
    val expectedTitles = Set("Le Père Goriot", "La Peste", "L'Étranger", "Les Misérables")
    assert(Statistics.allTitles(books).toSet == expectedTitles)
  }

  test("number of books per genre") {
    val stats = Statistics.booksPerGenre(books)
    assert(stats(Genre.Classic) == 2)
    assert(stats(Genre.Fiction) == 2)
  }

  test("average publication year") {
    val avg = Statistics.averagePublicationYear(books)
    assert(avg == (1835 + 1947 + 1942 + 1862) / 4.0)
  }

  test("oldest book") {
    val result = Statistics.oldestBook(books)
    assert(result.nonEmpty)
    assert(result.get.title == "Le Père Goriot")
  }

  test("newest book") {
    val result = Statistics.newestBook(books)
    assert(result.nonEmpty)
    assert(result.get.title == "La Peste")
  }
}

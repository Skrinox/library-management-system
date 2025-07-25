package services

import org.scalatest.funsuite.AnyFunSuite
import models._
import models.types._

class SearchSpec extends AnyFunSuite {

  val books = List(
    Book(ISBN("978-2-253085-79-9"), "Le Père Goriot", List("Honoré de Balzac"), 1835, Genre.Classic, Availability.Available),
    Book(ISBN("978-2-070394-38-8"), "La Peste", List("Albert Camus"), 1947, Genre.Fiction, Availability.CheckedOut),
    Book(ISBN("978-2-070360-02-4"), "L'Étranger", List("Albert Camus"), 1942, Genre.Fiction, Availability.Available),
    Book(ISBN("979-1-035826-85-7"), "Les Misérables", List("Victor Hugo"), 1862, Genre.Classic, Availability.CheckedOut)
  )

  test("filterBooks by title") {
    val result = Search.filterBooks(books)(Search.byTitle("peste"))
    assert(result.map(_.title) == List("La Peste"))
  }

  test("filterBooks by author") {
    val result = Search.filterBooks(books)(Search.byAuthor("camus"))
    assert(result.map(_.title).toSet == Set("La Peste", "L'Étranger"))
  }

  test("filterBooks by genre") {
    val result = Search.filterBooks(books)(Search.byGenre(Genre.Classic))
    assert(result.map(_.title).toSet == Set("Le Père Goriot", "Les Misérables"))
  }

  test("filterBooks by availability") {
    val result = Search.filterBooks(books)(Search.byAvailability(Availability.Available))
    assert(result.map(_.title).toSet == Set("Le Père Goriot", "L'Étranger"))
  }

  test("search by title and genre") {
    val result = Search.searchByTitleAndGenre(books, "les", Genre.Classic)
    assert(result.map(_.title) == List("Les Misérables"))
  }
}
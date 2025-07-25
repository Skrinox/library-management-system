package services

import org.scalatest.funsuite.AnyFunSuite
import models._

class RecommendationSpec extends AnyFunSuite {

  val book1 = Book(ISBN("978-2253085799"), "Le Père Goriot", List("Honoré de Balzac"), 1835, Genre.Classic, Availability.Available)
  val book2 = Book(ISBN("978-2070394388"), "La Peste", List("Albert Camus"), 1947, Genre.Fiction, Availability.CheckedOut)
  val book3 = Book(ISBN("978-2070360024"), "L'Étranger", List("Albert Camus"), 1942, Genre.Fiction, Availability.Available)
  val book4 = Book(ISBN("979-1035826857"), "Les Misérables", List("Victor Hugo"), 1862, Genre.Classic, Availability.CheckedOut)

  val catalog = List(book1, book2, book3, book4)

  test("recommend books from same genres not already read") {
    val userHistory = List(book1) 
    val recommendations = Recommendation.recommendByGenre(catalog, userHistory)
    val titles = recommendations.map(_.title).toSet
    assert(titles == Set("Les Misérables"))
  }

  test("exclude books already read") {
    val userHistory = List(book1, book4) 
    val recommendations = Recommendation.recommendByGenre(catalog, userHistory)
    assert(recommendations.isEmpty)
  }

  test("recommend from multiple genres") {
    val userHistory = List(book1, book2)
    val recommendations = Recommendation.recommendByGenre(catalog, userHistory)
    val titles = recommendations.map(_.title).toSet
    assert(titles == Set("L'Étranger", "Les Misérables"))
  }

  test("empty history returns no recommendations") {
    val recommendations = Recommendation.recommendByGenre(catalog, Nil)
    assert(recommendations.isEmpty)
  }
}

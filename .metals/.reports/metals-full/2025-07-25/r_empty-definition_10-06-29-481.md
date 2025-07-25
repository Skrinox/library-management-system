error id: file:///C:/Users/hupar/Downloads/library-management-system-master/library-management-system-master/src/test/scala/services/SearchSpec.scala:`<none>`.
file:///C:/Users/hupar/Downloads/library-management-system-master/library-management-system-master/src/test/scala/services/SearchSpec.scala
empty definition using pc, found symbol in pc: `<none>`.
empty definition using semanticdb
empty definition using fallback
non-local guesses:

offset: 424
uri: file:///C:/Users/hupar/Downloads/library-management-system-master/library-management-system-master/src/test/scala/services/SearchSpec.scala
text:
```scala
package services

import org.scalatest.funsuite.AnyFunSuite
import models._

class SearchSpec extends AnyFunSuite {

  val books = List(
    Book(ISBN("978-2253085799"), "Le Père Goriot", List("Honoré de Balzac"), 1835, Genre.Classic, Availability.Available),
    Book(ISBN("978-2-253-16482-7"), "La Peste", List("Albert Camus"), 1947, Genre.Fiction, Availability.CheckedOut),
    Book(ISBN("978-2-07-037444-1"), "@@L'Étranger", List("Albert Camus"), 1942, Genre.Fiction, Availability.Available),
    Book(ISBN("978-2-07-038588-1"), "Les Misérables", List("Victor Hugo"), 1862, Genre.Classic, Availability.CheckedOut)
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

  test("search by title AND genre") {
    val result = Search.searchByTitleAndGenre(books, "les", Genre.Classic)
    assert(result.map(_.title) == List("Les Misérables"))
  }
}

```


#### Short summary: 

empty definition using pc, found symbol in pc: `<none>`.
error id: file:///C:/Users/hupar/Downloads/library-management-system-master/library-management-system-master/src/main/scala/services/Statistics.scala:`<none>`.
file:///C:/Users/hupar/Downloads/library-management-system-master/library-management-system-master/src/main/scala/services/Statistics.scala
empty definition using pc, found symbol in pc: `<none>`.
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -models.
	 -models#
	 -models().
	 -.
	 -#
	 -().
	 -scala/Predef.
	 -scala/Predef#
	 -scala/Predef().
offset: 536
uri: file:///C:/Users/hupar/Downloads/library-management-system-master/library-management-system-master/src/main/scala/services/Statistics.scala
text:
```scala
package services

import models._

object Statistics {

  def totalBooks(books: List[Book]): Int =
    books.length

  def countAvailableBooks(books: List[Book]): Int =
    books.count(_.availability == Availability.Available)

  def allTitles(books: List[Book]): List[String] =
    books.map(_.title)

  // 📊 Nombre de livres par genre
  def booksPerGenre(books: List[Book]): Map[Genre, Int] =
    books.groupBy(_.genre).view.mapValues(_.size).toMap

  // 👤 Auteurs uniques dans la collection
  def uniqueAuthors(@@books: List[Book]): Set[String] =
    books.flatMap(_.authors).map(_.trim.toLowerCase).toSet

  // 📈 Année moyenne de publication
  def averagePublicationYear(books: List[Book]): Double =
    if books.isEmpty then 0.0
    else books.map(_.publicationYear).sum.toDouble / books.length

  // 🧓 Livre le plus ancien
  def oldestBook(books: List[Book]): Option[Book] =
    books.reduceOption((b1, b2) => if b1.publicationYear < b2.publicationYear then b1 else b2)

  // 📖 Livre le plus récent
  def newestBook(books: List[Book]): Option[Book] =
    books.reduceOption((b1, b2) => if b1.publicationYear > b2.publicationYear then b1 else b2)
}

```


#### Short summary: 

empty definition using pc, found symbol in pc: `<none>`.
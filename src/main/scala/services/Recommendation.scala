package services

import models._
import models.types.*

object Recommendation:

  def recommendByGenre(catalog: List[Book], userHistory: List[Book]): List[Book] =
    val extractGenres: List[Book] => Set[Genre] =
      _.map(_.genre).toSet

    val extractReadISBNs: List[Book] => Set[ISBN] =
      _.map(_.isbn).toSet

    val genresRead = extractGenres(userHistory)
    val isbnsRead = extractReadISBNs(userHistory)

    val isInReadGenres: Book => Boolean = book => genresRead.contains(book.genre)
    val isNotAlreadyRead: Book => Boolean = book => !isbnsRead.contains(book.isbn)

    catalog
      .filter(isInReadGenres)
      .filter(isNotAlreadyRead)

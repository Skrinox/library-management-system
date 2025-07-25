package services

import models._
import models.types._

/**
 * Provides a simple book recommendation engine based on user reading history
 */
object Recommendation:

  /**
   * Recommends books from the catalog that share genres with previously read books
   * Excludes books the user has already read
   *
   * @param catalog List of all available books
   * @param userHistory List of books the user has already read
   * @return List of recommended books
   */
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
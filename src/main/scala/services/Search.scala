package services

import models._

object Search {

  def filterBooks(books: List[Book])(predicate: Book => Boolean): List[Book] =
    books.filter(predicate)

  def byTitle(title: String): Book => Boolean =
    _.title.toLowerCase.contains(title.toLowerCase)

  def byAuthor(author: String): Book => Boolean =
    _.authors.exists(_.toLowerCase.contains(author.toLowerCase))

  def byGenre(genre: Genre): Book => Boolean =
    _.genre == genre

  def byAvailability(status: Availability): Book => Boolean =
    _.availability == status

  def composeFilters(f1: Book => Boolean, f2: Book => Boolean): Book => Boolean =
    book => f1(book) && f2(book)

  def searchByTitleAndGenre(books: List[Book], title: String, genre: Genre): List[Book] = {
    val combined = composeFilters(byTitle(title), byGenre(genre))
    filterBooks(books)(combined)
  }
}
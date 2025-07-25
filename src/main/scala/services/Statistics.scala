package services

import models._

object Statistics {

  def totalBooks(books: List[Book]): Int =
    books.length

  def countAvailableBooks(books: List[Book]): Int =
    books.count(_.availability == Availability.Available)

  def allTitles(books: List[Book]): List[String] =
    books.map(_.title)

  def booksPerGenre(books: List[Book]): Map[Genre, Int] =
    books.groupBy(_.genre).view.mapValues(_.size).toMap

  def averagePublicationYear(books: List[Book]): Double =
    if books.isEmpty then 0.0
    else books.map(_.publicationYear).sum.toDouble / books.length

  def oldestBook(books: List[Book]): Option[Book] =
    books.reduceOption((b1, b2) => if b1.publicationYear < b2.publicationYear then b1 else b2)

  def newestBook(books: List[Book]): Option[Book] =
    books.reduceOption((b1, b2) => if b1.publicationYear > b2.publicationYear then b1 else b2)
}

package services

import models._

/**
 * Provides statistical utilities for analyzing a collection of books
 */
object Statistics:

  /**
   * Returns the total number of books in the list
   *
   * @param books List of books
   * @return Number of books
   */
  def totalBooks(books: List[Book]): Int =
    books.length

  /**
   * Counts how many books are currently available
   *
   * @param books List of books
   * @return Number of available books
   */
  def countAvailableBooks(books: List[Book]): Int =
    books.count(_.availability == Availability.Available)

  /**
   * Extracts the titles of all books
   *
   * @param books List of books
   * @return List of titles
   */
  def allTitles(books: List[Book]): List[String] =
    books.map(_.title)

  /**
   * Counts the number of books per genre
   *
   * @param books List of books
   * @return Map from genre to count
   */
  def booksPerGenre(books: List[Book]): Map[Genre, Int] =
    books.groupBy(_.genre).view.mapValues(_.size).toMap

  /**
   * Calculates the average publication year of the books
   *
   * @param books List of books
   * @return Average year as a Double
   */
  def averagePublicationYear(books: List[Book]): Double =
    if books.isEmpty then 0.0
    else books.map(_.publicationYear).sum.toDouble / books.length

  /**
   * Finds the oldest book (with earliest publication year)
   *
   * @param books List of books
   * @return Option of the oldest book
   */
  def oldestBook(books: List[Book]): Option[Book] =
    books.reduceOption((b1, b2) => if b1.publicationYear < b2.publicationYear then b1 else b2)

  /**
   * Finds the newest book (with latest publication year)
   *
   * @param books List of books
   * @return Option of the newest book
   */
  def newestBook(books: List[Book]): Option[Book] =
    books.reduceOption((b1, b2) => if b1.publicationYear > b2.publicationYear then b1 else b2)